#include "../config.h"
#include "macro.h"

#include <assert.h>
#include <limits.h>  // INT_MAX
#include <stdlib.h>  // malloc
#include <string.h>

#include "pp_parser.h"  // pp_parse_error
#include "table.h"
#include "util.h"

Macro *new_macro(Vector *params, bool va_args, Vector *body) {
  Macro *macro = malloc(sizeof(*macro));
  int len = params != NULL ? params->len : -1;
  macro->params_len = len;
  macro->va_args = va_args;
  macro->body = body;

  Table *table = NULL;
  if (params != NULL) {
    // Store parameter index.
    table = alloc_table();
    for (int i = 0; i < len; ++i) {
      const Name *ident = params->data[i];
      table_put(table, ident, (void*)(intptr_t)i);
    }
    if (macro->va_args) {
      const Name *ident = alloc_name("__VA_ARGS__", NULL, false);
      table_put(table, ident, (void*)(intptr_t)len);
    }
  }
  macro->param_table = table;

  return macro;
}

//

typedef Table HideSet;

static Vector hideset_array;

static int search_hideset(const Token *tok) {
  int n = hideset_array.len >> 1;
  int lo = -1, hi = n;
  while (hi - lo > 1) {
    int m = lo + ((hi - lo) >> 1);
    const Token *t = hideset_array.data[m << 1];
    if (t < tok)
      lo = m;
    else
      hi = m;
  }
  return hi << 1;
}

static HideSet *get_hideset(const Token *tok) {
  int i = search_hideset(tok);
  assert(i >= 0 && i <= hideset_array.len);
  if (i < hideset_array.len) {
    const Token *t = hideset_array.data[i];
    if (t == tok)
      return hideset_array.data[i + 1];
  }
  return NULL;
}

static void set_token_hideset(const Token *tok, HideSet *hs) {
  int i = search_hideset(tok);
  assert(i >= 0 && i <= hideset_array.len);
  if (i < hideset_array.len &&
      ((Token*)hideset_array.data[i]) == tok) {
    hideset_array.data[i + 1] = hs;
  } else {
    vec_insert(&hideset_array, i, tok);
    vec_insert(&hideset_array, i + 1, hs);
  }
}

static HideSet *clone_hideset(HideSet *hs) {
  HideSet *cloned = alloc_table();
  if (hs != NULL) {
    const Name *name;
    void *value;
    for (int it = 0; (it = table_iterate(hs, it, &name, &value)) != -1; )
      table_put(cloned, name, value);
  }
  return cloned;
}

static void hideset_put(HideSet *hs, const Name *token) {
  assert(hs != NULL);
  table_put(hs, token, (void*)token);
}

static void union_hideset(HideSet *hs, HideSet *hs2) {
  assert(hs != NULL);
  if (hs2 != NULL) {
    const Name *name;
    void *value;
    for (int it = 0; (it = table_iterate(hs2, it, &name, &value)) != -1; )
      table_put(hs, name, (void*)name);
  }
}

static void intersection_hideset(HideSet *hs, HideSet *hs2) {
  assert(hs != NULL);
  if (hs2 == NULL) {
    table_init(hs);
  } else {
    const Name *name;
    void *value;
    for (int it = 0; (it = table_iterate(hs, it, &name, &value)) != -1; ) {
      if (!table_try_get(hs2, name, NULL))
        table_delete(hs, name);
    }
  }
}

static Token *glue(const Token *tok1, const Token *tok2) {
  // TODO: Check whether the concatenated is ident.
  size_t len1 = tok1->end - tok1->begin;
  size_t len2 = tok2->end - tok2->begin;
  char *str = malloc(len1 + len2 + 1);
  assert(str != NULL);
  memcpy(str, tok1->begin, len1);
  memcpy(str + len1, tok2->begin, len2);
  str[len1 + len2] = '\0';
  const Name *name = alloc_name(str, str + (len1 + len2), false);
  Token *tok = alloc_token(TK_IDENT, str, str + (len1 + len2));
  tok->ident = name;
  return tok;
}

static Token *stringize(const Vector *arg) {
  StringBuffer sb;
  sb_init(&sb);
  for (int i = 0; i < arg->len; ++i) {
    const Token *tok = arg->data[i];
    sb_append(&sb, tok->begin, tok->end);
  }
  const char *str = sb_to_string(&sb);
  size_t len = strlen(str);

  sb_clear(&sb);
  static const char DQUOTE[] = "\"";
  sb_append(&sb, DQUOTE, NULL);
  escape_string(str, len, &sb);
  sb_append(&sb, DQUOTE, NULL);
  const char *escaped = sb_to_string(&sb);

  Token *tok = alloc_token(TK_STR, escaped, NULL);
  tok->str.buf = str + 1;
  tok->str.size = len + (- 2 /*for ""*/ + 1 /*for \0*/);
  return tok;
}

static Token *clone_token(const Token *tok) {
  Token *dup = malloc(sizeof(*dup));
  *dup = *tok;
  return dup;
}

static void hsadd(HideSet *hs, Vector *ts) {
  for (int i = 0; i < ts->len; ++i) {
    const Token *tok = ts->data[i];
    if (tok->kind == TK_IDENT || tok->kind == TK_RPAR) {
      HideSet *h = get_hideset(tok);
      if (h == NULL) {
        h = clone_hideset(hs);
        set_token_hideset(tok, h);
      } else {
        union_hideset(h, hs);
      }
    }
  }
}

static Vector *subst(Vector *body, Table *param_table, Vector *args, HideSet *hs) {
  Vector *os = new_vector();
  for (int i = 0; i < body->len; ++i) {
    const Token *tok = body->data[i];

    if (tok->kind == PPTK_STRINGIFY && i + 1 < body->len) {
      const Token *next = body->data[i + 1];
      intptr_t j;
      if (next->kind == TK_IDENT &&
          param_table != NULL && table_try_get(param_table, next->ident, (void*)&j)) {
        assert(j < args->len);
        const Vector *arg = args->data[j];
        const Token *str = stringize(arg);
        vec_push(os, str);
        ++i;
        continue;
      }
    }

    if (tok->kind == PPTK_CONCAT && os->len > 0 && i + 1 < body->len) {
      const Token *prev = os->data[os->len - 1];
      const Token *next = body->data[i + 1];
      intptr_t j;
      if (prev->kind == TK_IDENT && next->kind == TK_IDENT &&
          param_table != NULL && table_try_get(param_table, next->ident, (void*)&j)) {
        assert(j < args->len);
        const Vector *arg = args->data[j];
        if (arg->len > 0) {
          os->data[os->len - 1] = glue(prev, arg->data[0]);
          for (int k = 1; k < arg->len; ++k)
            vec_push(os, arg->data[k]);
          ++i;
          continue;
        }
      }
    }

    if (tok->kind == TK_IDENT) {
      intptr_t j;
      if (param_table != NULL && table_try_get(param_table, tok->ident, (void*)&j)) {
        assert(j < args->len);
        Vector *arg = args->data[j];
        Vector *duplicated = new_vector();
        for (int k = 0; k < arg->len; ++k) {
          const Token *t = arg->data[k];
          vec_push(duplicated, clone_token(t));
        }
        macro_expand(duplicated);
        for (int k = 0; k < duplicated->len; ++k) {
          const Token *t = duplicated->data[k];
          vec_push(os, t);
        }
        continue;
      }
    }

    vec_push(os, clone_token(tok));
  }
  hsadd(hs, os);
  return os;
}

//

static Table macro_table;  // <Name, Macro*>

void macro_add(const Name *name, Macro *macro) {
  table_put(&macro_table, name, macro);
}

Macro *macro_get(const Name *name) {
  return table_get(&macro_table, name);
}

void macro_delete(const Name *name) {
  table_delete(&macro_table, name);
}

void macro_expand(Vector *tokens) {
  for (int i = 0; i < tokens->len; ++i) {
    const Token *tok = tokens->data[i];
    if (tok->kind != TK_IDENT)
      continue;
    Macro *macro = macro_get(tok->ident);
    if (macro == NULL)
      continue;
    HideSet *hs = get_hideset(tok);
    if (hs != NULL &&
        table_try_get(hs, tok->ident, NULL)) {
      continue;
    }

    int next = i + 1;
    const Vector *replaced = NULL;
    if (macro->params_len < 0) {  // "()-less macro"
      hs = clone_hideset(hs);
      hideset_put(hs, tok->ident);
      replaced = subst(macro->body, NULL, NULL, hs);
    } else {  // "()'d macro"
      Vector *args = pp_funargs(tokens, &next, macro->va_args ? macro->params_len : INT_MAX);
      if (args != NULL) {
        int count = macro->params_len + (macro->va_args ? 1 : 0);  // variadic argument is concatenated.
        if (count != args->len) {
          const char *cmp = args->len > count ? "many" : "few";
          pp_parse_error(tok, "Too %s arguments for macro `%.*s'", cmp, tok->ident->bytes, tok->ident->chars);
        }

        assert(next > 0);
        HideSet *hs2 = get_hideset(tokens->data[next - 1]);
        hs = clone_hideset(hs);
        intersection_hideset(hs, hs2);
        hideset_put(hs, tok->ident);
        replaced = subst(macro->body, macro->param_table, args, hs);
      }
    }

    if (replaced != NULL) {
      for (int n = next - i; n > 0; --n)
        vec_remove_at(tokens, i);
      for (int j = 0; j < replaced->len; ++j)
        vec_insert(tokens, i + j, replaced->data[j]);
      --i;
    }
  }
}
