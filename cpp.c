#include "ctype.h"
#include "libgen.h"  // dirname
#include "stdarg.h"
#include "stdbool.h"
#include "stdio.h"
#include "stdlib.h"
#include "string.h"

#include "util.h"
#include "lexer.h"

#define MAX(x, y)  ((x) >= (y) ? (x) : (y))

enum SegmentType {
  ST_TEXT,
  ST_PARAM,
};

typedef struct {
  enum SegmentType type;
  union {
    const char *text;
    int param;
  } u;
} Segment;

typedef struct {
  Vector *params;  // <const char*>
  bool va_args;
  Vector *segments;  // <Segment*>
} Macro;

Macro *new_macro(Vector *params, bool va_args, Vector *segments) {
  Macro *macro = malloc(sizeof(*macro));
  macro->params = params;
  macro->va_args = va_args;
  macro->segments = segments;
  return macro;
}

const char *skip_whitespaces(const char *s) {
  while (isspace(*s))
    ++s;
  return s;
}

const char *keyword(const char *s, const char *word) {
  size_t len = strlen(word);
  if (strncmp(s, word, len) != 0 || (s[len] != '\0' && !isspace(s[len])))
    return NULL;
  return skip_whitespaces(s + (len + 1));
}

const char *find_directive(const char *line) {
  const char *p = skip_whitespaces(line);
  if (*p != '#')
    return NULL;
  return skip_whitespaces(p + 1);
}

Map *macro_map;

void pp(FILE *fp, const char *filename);

void handle_include(const char *p, const char *srcname) {
  char close;
  switch (*p++) {
  case '"':
    close = '"';
    break;
  case '<':
    close = '>';
    break;
  default:
    error("syntax error");
    break;
  }

  const char *q;
  for (q = p; *q != close; ++q) {
    if (*q == '\0')
      error("not closed");
  }

  char *fn = cat_path(dirname(strdup_(srcname)), strndup_(p, q - p));

  FILE *fp = fopen(fn, "r");
  if (fp == NULL)
    error("Cannot open file: %s", fn);
  printf("/* \"%s\" start */\n", fn);
  pp(fp, fn);
  printf("/* \"%s\" end */\n", fn);
  fclose(fp);
}

char *append(char *str, const char *begin, const char *end) {
  size_t add;
  if (end == NULL) {
    add = strlen(begin);
    end = begin + add;
  } else {
    add = end - begin;
  }
  if (add == 0)
    return str;
  size_t len = str != NULL ? strlen(str) : 0;
  char *newstr = realloc(str, len + add + 1);
  memcpy(newstr + len, begin, add);
  newstr[len + add] = '\0';
  return newstr;
}

Vector *parse_macro_body(const char *p, const Vector *params, bool va_args, const char *filename, int lineno) {
  Vector *segments = new_vector();
  init_lexer_string(p, filename, lineno);
  int param_len = params != NULL ? params->len : 0;
  char *text = NULL;
  for (;;) {
    Token *tok;
    if ((tok = consume(TK_IDENT)) != NULL) {
      int index = -1;
      if (va_args && strcmp(tok->u.ident, "__VA_ARGS__") == 0) {
        index = param_len;
      } else {
        for (int i = 0; i < param_len; ++i) {
          if (strcmp(tok->u.ident, params->data[i]) == 0) {
            index = i;
            break;
          }
        }
      }
      if (index >= 0) {
        if (text != NULL) {
          Segment *seg = malloc(sizeof(*seg));
          seg->type = ST_TEXT;
          seg->u.text = text;
          vec_push(segments, seg);
        }

        Segment *seg2 = malloc(sizeof(*seg2));
        seg2->type = ST_PARAM;
        seg2->u.param = index;
        vec_push(segments, seg2);

        text = NULL;
        continue;
      }
    } else {
      tok = consume(-1);
      if (tok->type == TK_EOF)
        break;
    }
    text = append(text, tok->begin, tok->end);
    text = append(text, " ", NULL);
  }

  if (text != NULL) {
    Segment *seg = malloc(sizeof(*seg));
    seg->type = ST_TEXT;
    seg->u.text = text;
    vec_push(segments, seg);
  }
  return segments;
}

void handle_define(const char *p, const char *filename, int lineno) {
  char *name = read_ident(&p);
  if (name == NULL)
    error("`ident' expected");

  Vector *params = NULL;
  bool va_args = false;
  if (*p == '(') {
    // Macro with parameter.
    params = new_vector();
    init_lexer_string(p + 1, filename, lineno);
    if (!consume(TK_RPAR)) {
      for (;;) {
        Token *tok;
        if ((tok = consume(TK_DOTDOTDOT)) != NULL) {
          va_args = true;
          if (!consume(TK_RPAR))
            parse_error(NULL, "`)' expected");
          break;
        } else if ((tok = consume(TK_IDENT)) != NULL) {
          vec_push(params, tok->u.ident);
          if (consume(TK_RPAR))
            break;
          if (!consume(TK_COMMA))
            parse_error(NULL, "`,' or `)' expected");
        } else {
          parse_error(NULL, "`ident' expected");
        }
      }
    }
    p = get_lex_p();
  }

  Vector *segments = NULL;
  p = skip_whitespaces(p);
  if (*p != '\0') {
    segments = parse_macro_body(skip_whitespaces(p), params, va_args, filename, lineno);
  }
  map_put(macro_map, name, new_macro(params, va_args, segments));
}

Token *consume2(enum TokenType type) {
  // TODO: Handle end of line.
  return consume(type);
}

void expand(Macro *macro, const char *name) {
  Vector *args = NULL;
  if (macro->params != NULL) {
    if (!consume2(TK_LPAR))
      parse_error(NULL, "`(' expected for macro `%s'", name);
    args = new_vector();
    if (!consume2(TK_RPAR)) {
      int paren = 0;
      const char *begin = NULL;
      for (;;) {
        if (consume2(TK_EOF))
          parse_error(NULL, "`)' expected");

        Token *tok;
        if ((tok = consume2(TK_COMMA)) != NULL || (tok = consume2(TK_RPAR)) != NULL)  {
          if (tok->type == TK_RPAR && paren > 0) {
            --paren;
            continue;
          }
          if (begin == NULL)
            parse_error(tok, "expression expected");
          vec_push(args, strndup_(begin, tok->begin - begin));
          begin = NULL;
          if (tok->type == TK_RPAR)
            break;
          continue;
        }
        tok = consume2(-1);
        if (begin == NULL)
          begin = tok->begin;
        if (tok->type == TK_LPAR)
          ++paren;
      }
    }

    if ((!macro->va_args && args->len != macro->params->len) ||
        (macro->va_args && args->len <= macro->params->len)) {
      const char *cmp = args->len < macro->params->len ? "less" : "few";
      parse_error(NULL, "Too %s arguments for macro `%s'", cmp, name);
    }
  }

  // __VA_ARGS__
  if (macro->va_args) {
    // Concat.
    char *vaargs = NULL;
    for (int i = macro->params->len; i < args->len; ++i) {
      if (i > macro->params->len)
        vaargs = append(vaargs, ",", NULL);
      vaargs = append(vaargs, (char*)args->data[i], NULL);
    }
    if (args->len <= macro->params->len)
      vec_push(args, vaargs);
    else
      args->data[macro->params->len] = vaargs;
  }

  char *str = NULL;
  if (macro->segments != NULL) {
    str = NULL;
    for (int i = 0; i < macro->segments->len; ++i) {
      Segment *seg = macro->segments->data[i];
      switch (seg->type) {
      case ST_TEXT:
        str = append(str, seg->u.text, NULL);
        break;
      case ST_PARAM:
        str = append(str, (char*)args->data[seg->u.param], NULL);
        break;
      default:
        break;
      }
    }
  }
  str = append(str, get_lex_p(), NULL);

  init_lexer_string(str, NULL, -1);
}

void process_line(const char *line, const char *filename, int lineno) {
  init_lexer_string(line, filename, lineno);

  const char *begin = get_lex_p();
  for (;;) {
    if (consume(TK_EOF))
      break;

    Token *ident = consume(TK_IDENT);
    Macro *macro;
    if (ident != NULL && (macro = map_get(macro_map, ident->u.ident)) != NULL) {
      if (ident->begin != begin)
        fwrite(begin, ident->begin - begin, 1, stdout);

      expand(macro, ident->u.ident);
      begin = get_lex_p();
      continue;
    }

    consume(-1);
  }

  printf("%s\n", begin);
}

bool handle_ifdef(const char *p) {
  char *name = read_ident(&p);
  if (name == NULL)
    error("`ident' expected");
  return map_get(macro_map, name) != NULL;
}

#define CF_ENABLE  (1 << 0)
#define CF_ELSE    (1 << 1)

void pp(FILE *fp, const char *filename) {
  Vector *condstack = new_vector();
  bool enable = true;
  for (int lineno = 1;; ++lineno) {
    char *line = NULL;
    size_t capa = 0;

    if (getline_(&line, &capa, fp) == EOF)
      break;

    // Find '#'
    const char *directive = find_directive(line);
    if (directive == NULL) {
      if (enable)
        process_line(line, filename, lineno);
      continue;
    }

    const char *next;
    if ((next = keyword(directive, "ifdef")) != NULL) {
      bool defined = handle_ifdef(next);
      intptr_t flag = enable ? CF_ENABLE : 0;
      vec_push(condstack, (void*)flag);
      enable = enable && defined;
    } else if ((next = keyword(directive, "ifndef")) != NULL) {
      bool defined = handle_ifdef(next);
      intptr_t flag = enable ? CF_ENABLE : 0;
      vec_push(condstack, (void*)flag);
      enable = enable && !defined;
    } else if ((next = keyword(directive, "else")) != NULL) {
      int last = condstack->len - 1;
      if (last < 0)
        error("`#else' used without `#if'");
      intptr_t flag = (intptr_t)condstack->data[last];
      if (flag & CF_ELSE)
        error("Illegal #else");
      condstack->data[last] = (void*)(flag ^ CF_ELSE);
      enable = !enable && ((flag & CF_ENABLE) != 0);
    } else if ((next = keyword(directive, "endif")) != NULL) {
      int len = condstack->len;
      if (len <= 0)
        error("`#endif' used without `#if'");
      enable = (((intptr_t)condstack->data[--len]) & CF_ENABLE) != 0;
      condstack->len = len;
    } else if ((next = keyword(directive, "include")) != NULL) {
      if (enable)
        handle_include(next, filename);
    } else if ((next = keyword(directive, "define")) != NULL) {
      if (enable)
        handle_define(next, filename, lineno);
    } else {
      printf("unknown directive: %s", directive);
    }
  }

  if (condstack->len > 0)
    error("#if not closed");
}

int main(int argc, char* argv[]) {
  macro_map = new_map();

  if (argc > 1) {
    for (int i = 1; i < argc; ++i) {
      const char *filename = argv[i];
      FILE *fp = fopen(filename, "rb");
      if (fp == NULL)
        error("Cannot open file: %s\n", argv[i]);
      pp(fp, filename);
      fclose(fp);
    }
  } else {
    pp(stdin, "*stdin*");
  }
  return 0;
}
