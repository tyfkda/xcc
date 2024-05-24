#include "../config.h"
#include "macro.h"

#include <assert.h>
#include <limits.h>  // INT_MAX
#include <stdlib.h>  // malloc
#include <string.h>

#include "pp_parser.h"  // pp_parse_error
#include "table.h"
#include "util.h"

Macro *new_macro(Vector *params, const Name *vaargs_ident, Vector *body) {
  Macro *macro = malloc_or_die(sizeof(*macro));
  int len = params != NULL ? params->len : -1;
  macro->params_len = len;
  macro->vaargs_ident = vaargs_ident;
  macro->body = body;

  Table *table = NULL;
  if (params != NULL) {
    // Store parameter index.
    table = alloc_table();
    for (int i = 0; i < len; ++i) {
      const Name *ident = params->data[i];
      table_put(table, ident, INT2VOIDP(i));
    }
    if (macro->vaargs_ident) {
      const Name *ident = macro->vaargs_ident;
      table_put(table, ident, INT2VOIDP(len));
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
  if (i < hideset_array.len && ((Token*)hideset_array.data[i]) == tok) {
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

inline void hideset_put(HideSet *hs, const Name *token) {
  assert(hs != NULL);
  table_put(hs, token, (void*)token);
}

static void union_hideset(HideSet *hs, HideSet *hs2) {
  assert(hs != NULL);
  if (hs2 != NULL) {
    const Name *name;
    void *value;
    for (int it = 0; (it = table_iterate(hs2, it, &name, &value)) != -1; )
      table_put(hs, name, value);
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

static void glue1(Vector *ls, const Token *tok2) {
  if (ls->len > 0) {
    const Token *tok1 = ls->data[ls->len - 1];
    // TODO: Check whether the concatenated is ident.
    size_t len1 = tok1->end - tok1->begin;
    size_t len2 = tok2->end - tok2->begin;
    char *str = malloc_or_die(len1 + len2 + 1);
    memcpy(str, tok1->begin, len1);
    memcpy(str + len1, tok2->begin, len2);
    str[len1 + len2] = '\0';
    const Name *name = alloc_name(str, str + (len1 + len2), false);
    Token *tok = alloc_token(TK_IDENT, NULL, str, str + (len1 + len2));
    tok->ident = name;
    ls->data[ls->len - 1] = tok;
  } else {
    vec_push(ls, tok2);
  }
}

static void glue(Vector *ls, const Vector *rs) {
  int start = 0;
  if (ls->len > 0) {
    assert(rs->len > 0);
    const Token *tok2 = rs->data[0];
    glue1(ls, tok2);
    start = 1;
  }
  for (int i = start; i < rs->len; ++i)
    vec_push(ls, rs->data[i]);
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

  Token *tok = alloc_token(TK_STR, NULL, escaped, NULL);
  tok->str.buf = str + 1;
  tok->str.len = len + (-2 /*for ""*/ + 1 /*for \0*/);
  return tok;
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

static Vector *subst(Macro *macro, Table *param_table, Vector *args, HideSet *hs) {
  Vector *os = new_vector();
  Vector *body = macro->body;
  if (body == NULL)
    return os;

  Vector **expanded_args = NULL;
  if (args != NULL && args->len > 0) {
    expanded_args = alloca(sizeof(*expanded_args) * args->len);
    if (expanded_args == NULL)
      error("alloca failed");
    for (int i = 0; i < args->len; ++i)
      expanded_args[i] = NULL;
  }

  for (int i = 0; i < body->len; ++i) {
    const Token *tok = body->data[i];

    switch (tok->kind) {
    case PPTK_STRINGIFY:
      if (i + 1 < body->len) {
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
      break;

    case PPTK_CONCAT:
      if (os->len > 0 && i + 1 < body->len) {
        const Token *next = body->data[i + 1];
        if (next->kind == TK_IDENT) {
          intptr_t j;
          if (param_table != NULL && table_try_get(param_table, next->ident, (void*)&j)) {
            assert(j < args->len);
            const Vector *arg = args->data[j];
            if (arg->len > 0)
              glue(os, arg);
            ++i;
            continue;
          }
        }
        glue1(os, next);
        ++i;
        continue;
      }
      break;

    case TK_IDENT:
      if (body->len > i + 1 && ((Token*)body->data[i + 1])->kind == PPTK_CONCAT) {
        intptr_t j;
        if (param_table != NULL && table_try_get(param_table, tok->ident, (void*)&j)) {
          assert(j < args->len);
          const Vector *arg = args->data[j];
          if (arg->len == 0) {  // only if actuals can be empty
            intptr_t k;
            if (body->len > i + 2 && ((Token*)body->data[i + 2])->kind == TK_IDENT &&
                table_try_get(param_table, ((Token*)body->data[i + 2])->ident, (void*)&k)) {
              assert(k < args->len);
              const Vector *arg2 = args->data[k];
              for (int l = 0; l < arg2->len; ++l)
                vec_push(os, arg2->data[l]);
            }
            i += 2;
          } else {
            for (int l = 0; l < arg->len; ++l)
              vec_push(os, arg->data[l]);
            // Handle `##` at next iteration.
          }
          continue;
        }
      }

      if (param_table != NULL) {
        intptr_t j;
        if (table_try_get(param_table, tok->ident, (void*)&j)) {
          assert(j < args->len);
          Vector *expanded = expanded_args[j];
          if (expanded == NULL) {
            Vector *arg = args->data[j];
            expanded = new_vector();
            for (int i = 0; i < arg->len; ++i)
              vec_push(expanded, arg->data[i]);
            macro_expand(expanded);
            expanded_args[j] = expanded;
          }
          for (int k = 0; k < expanded->len; ++k) {
            const Token *t = expanded->data[k];
            vec_push(os, t);
          }
          continue;
        }
      }
      break;

    case TK_COMMA:
      // Handle GNU extension: , ## __VA_ARGS__
      if (body->len > i + 2 && ((Token*)body->data[i + 1])->kind == PPTK_CONCAT &&
          macro->vaargs_ident != NULL &&
          ((Token*)body->data[i + 2])->kind == TK_IDENT && param_table != NULL) {
        const Token *next = body->data[i + 2];
        intptr_t j;
        if (table_try_get(param_table, next->ident, (void*)&j) && j == macro->params_len) {
          const Vector *arg = args->data[j];
          if (arg->len == 0) {
            i += 2;  // Argument is empty, so omit `,` and `##`.
            continue;
          }
          ++i;  // Argument is not empty, so put `,` followed by the argument (omit `##`).
        }
      }
      break;

    default: break;
    }

    vec_push(os, tok);
  }
  hsadd(hs, os);
  return os;
}

//

static Table macro_table;  // <Name, Macro*>

void macro_init(void) {
  table_init(&macro_table);
  vec_init(&hideset_array);
}

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
    if (hs != NULL && table_try_get(hs, tok->ident, NULL))
      continue;

    int next = i + 1;
    const Vector *replaced = NULL;
    if (macro->params_len < 0) {  // "()-less macro"
      hs = clone_hideset(hs);
      hideset_put(hs, tok->ident);
      replaced = subst(macro, NULL, NULL, hs);
    } else {  // "()'d macro"
      Vector *args = pp_funargs(tokens, &next,
                                macro->vaargs_ident != NULL ? macro->params_len : INT_MAX);
      if (args != NULL) {
        int count = macro->params_len +
                    (macro->vaargs_ident != NULL);  // variadic argument is concatenated.
        if (count != args->len) {
          const char *cmp = args->len > count ? "many" : "few";
          pp_parse_error(tok, "Too %s arguments for macro `%.*s'", cmp, NAMES(tok->ident));
        }

        assert(next > 0);
        HideSet *hs2 = get_hideset(tokens->data[next - 1]);
        hs = clone_hideset(hs);
        intersection_hideset(hs, hs2);
        hideset_put(hs, tok->ident);
        replaced = subst(macro, macro->param_table, args, hs);
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
