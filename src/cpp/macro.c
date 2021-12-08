#include "macro.h"

#include <assert.h>
#include <stdlib.h>  // malloc
#include <string.h>

#include "pp_parser.h"  // pp_parse_error
#include "table.h"
#include "util.h"

Macro *new_macro(Vector *params, bool va_args, Vector *segments) {
  Macro *macro = malloc(sizeof(*macro));
  macro->params = params;
  macro->va_args = va_args;
  macro->segments = segments;
  return macro;
}

Macro *new_macro_single(const char *text) {
  Vector *segments = new_vector();
  Segment *seg = malloc(sizeof(*seg));
  seg->kind = SK_TEXT;
  seg->text = text;
  vec_push(segments, seg);
  return new_macro(NULL, false, segments);
}

bool expand_macro(Macro *macro, const Token *token, Vector *args, const Name *name, StringBuffer *sb) {
  if (macro->params != NULL) {
    if (args == NULL)
      return false;

    if ((!macro->va_args && args->len != macro->params->len) ||
        (macro->va_args && args->len <= macro->params->len)) {
      const char *cmp = args->len < macro->params->len ? "few" : "many";
      pp_parse_error(token, "Too %s arguments for macro `%.*s'", cmp, name->bytes, name->chars);
    }
  } else {
    if (args != NULL) {
      pp_parse_error(token, "Illegal argument for macro `%.*s'", name->bytes, name->chars);
    }
  }

  // __VA_ARGS__
  if (macro->va_args) {
    // Concat.
    StringBuffer sb;
    sb_init(&sb);
    int plen = macro->params->len;
    for (int i = plen; i < args->len; ++i) {
      if (i > plen)
        sb_append(&sb, ",", NULL);
      sb_append(&sb, (char*)args->data[i], NULL);
    }
    char *vaargs = sb_to_string(&sb);

    if (args->len <= plen)
      vec_push(args, vaargs);
    else
      args->data[plen] = vaargs;
  }

  if (macro->segments != NULL) {
    for (int i = 0; i < macro->segments->len; ++i) {
      Segment *seg = macro->segments->data[i];
      switch (seg->kind) {
      case SK_TEXT:
        sb_append(sb, seg->text, NULL);
        break;
      case SK_PARAM:
        sb_append(sb, (char*)args->data[seg->param], NULL);
        break;
      case SK_STRINGIFY:
        {
          static const char DQUOTE[] = "\"";
          sb_append(sb, DQUOTE, NULL);
          const char *text = args->data[seg->param];
          escape_string(text, strlen(text), sb);
          sb_append(sb, DQUOTE, NULL);
        }
        break;
      default:
        assert(false);
        break;
      }
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
