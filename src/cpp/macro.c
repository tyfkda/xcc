#include "macro.h"

#include <stdlib.h>  // malloc

#include "lexer.h"  // parse_error
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
  seg->type = ST_TEXT;
  seg->text = text;
  vec_push(segments, seg);
  return new_macro(NULL, false, segments);
}

void expand(Macro *macro, Vector *args, const Name *name, StringBuffer *sb) {
  if (macro->params != NULL) {
    if (args == NULL) {
      parse_error(NULL, "arguments expected for macro `%.*s'", name->bytes, name->chars);
    } else {
      if ((!macro->va_args && args->len != macro->params->len) ||
          (macro->va_args && args->len <= macro->params->len)) {
        const char *cmp = args->len < macro->params->len ? "less" : "few";
        parse_error(NULL, "Too %s arguments for macro `%.*s'", cmp, name->bytes, name->chars);
      }
    }
  } else {
    if (args != NULL) {
      parse_error(NULL, "Illegal argument for macro `%.*s'", name->bytes, name->chars);
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
      switch (seg->type) {
      case ST_TEXT:
        sb_append(sb, seg->text, NULL);
        break;
      case ST_PARAM:
        sb_append(sb, (char*)args->data[seg->param], NULL);
        break;
      default:
        break;
      }
    }
  }
}
