#include "../config.h"
#include "macro.h"

#include <assert.h>
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

bool expand_macro(Macro *macro, Vector *args, const Token *token, Vector *expanded) {
  int plen = macro->params_len;
  if (macro->param_table != NULL) {
    if (args == NULL)
      return false;

    if ((!macro->va_args && args->len != plen) ||
        (macro->va_args && args->len < plen)) {
      const char *cmp = args->len > plen ? "many" : "few";
      pp_parse_error(token, "Too %s arguments for macro `%.*s'", cmp, token->ident->bytes, token->ident->chars);
    }
  } else {
    if (args != NULL) {
      pp_parse_error(token, "Illegal argument for macro `%.*s'", token->ident->bytes, token->ident->chars);
    }
  }

  // __VA_ARGS__
  if (macro->va_args) {
    // Concat.
    Vector *vaargs = new_vector();
    Token *tok_comma = NULL;
    for (int i = plen; i < args->len; ++i) {
      if (i > plen) {
        if (tok_comma == NULL)
          tok_comma = alloc_token(TK_COMMA, ",", NULL);
        vec_push(vaargs, tok_comma);
      }
      const Vector *arg = args->data[i];
      for (int j = 0; j < arg->len; ++j)
        vec_push(vaargs, arg->data[j]);
    }

    // Side effect: args modified.
    if (args->len <= plen)
      vec_push(args, vaargs);
    else
      args->data[plen] = vaargs;
  }

  if (macro->body != NULL) {
    for (int i = 0; i < macro->body->len; ++i) {
      const Token *tok = macro->body->data[i];
      switch (tok->kind) {
      case TK_IDENT:
        {
          intptr_t index;
          if (macro->param_table != NULL &&
              table_try_get(macro->param_table, tok->ident, (void**)&index)) {
            const Vector *arg = args->data[index];
            for (int i = 0; i < arg->len; ++i) {
              const Token *tok = arg->data[i];
              vec_push(expanded, tok);
            }
            continue;
          }
        }
        break;
      case PPTK_CONCAT:
        assert(false);
        continue;
      case PPTK_STRINGIFY:
        if (i + 1 >= macro->body->len) {
          pp_parse_error(tok, "Macro parameter required");
        } else {
          const Token *next = macro->body->data[i + 1];
          intptr_t index;
          if (macro->param_table != NULL &&
              table_try_get(macro->param_table, next->ident, (void**)&index)) {
            StringBuffer sb;
            sb_init(&sb);
            const Vector *arg = args->data[index];
            for (int i = 0; i < arg->len; ++i) {
              const Token *tok = arg->data[i];
              sb_append(&sb, tok->begin, tok->end);
            }
            const char *str = sb_to_string(&sb);
            size_t size = strlen(str);

            sb_clear(&sb);
            static const char DQUOTE[] = "\"";
            sb_append(&sb, DQUOTE, NULL);
            escape_string(str, size, &sb);
            sb_append(&sb, DQUOTE, NULL);
            const char *escaped = sb_to_string(&sb);

            Token *tok = alloc_token(TK_STR, escaped, NULL);
            tok->str.buf = str;
            tok->str.size = size;

            vec_push(expanded, tok);
            ++i;
            continue;
          } else {
            pp_parse_error(next, "Macro parameter required");
          }
        }
        break;
      default:
        break;
      }

      vec_push(expanded, tok);
    }
  }
  return true;
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
