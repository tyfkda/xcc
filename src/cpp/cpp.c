#include "assert.h"
#include "ctype.h"
#include "libgen.h"  // dirname
#include "stdarg.h"
#include "stdbool.h"
#include "stdio.h"
#include "stdlib.h"
#include "string.h"
#include "unistd.h"

#include "expr.h"
#include "lexer.h"
#include "type.h"
#include "util.h"

char *abspath_cwd(const char *dir, const char *path) {
  char *cwd = getcwd(NULL, 0);
  char *root = abspath(cwd, dir);
  free(cwd);
  return abspath(root, path);
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

Macro *new_macro_single(const char *text) {
  Vector *segments = new_vector();
  Segment *seg = malloc(sizeof(*seg));
  seg->type = ST_TEXT;
  seg->u.text = text;
  vec_push(segments, seg);
  return new_macro(NULL, false, segments);
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

Map *macro_map;  // <Macro*>

Vector *sys_inc_paths;  // <const char*>
Vector *pragma_once_files;  // <const char*>

typedef struct {
  const char *filename;
  FILE *fp;
  int lineno;
} Stream;

static Stream *s_stream;

bool registered_pragma_once(const char *filename) {
  for (int i = 0, len = pragma_once_files->len; i < len; ++i) {
    const char *fn = pragma_once_files->data[i];
    if (strcmp(fn, filename) == 0)
      return true;
  }
  return false;
}

void register_pragma_once(const char *filename) {
  vec_push(pragma_once_files, filename);
}

int pp(FILE *fp, const char *filename);

void handle_include(const char *p, const char *srcname) {
  char close;
  bool sys = false;
  switch (*p++) {
  case '"':
    close = '"';
    break;
  case '<':
    close = '>';
    sys = true;
    break;
  default:
    error("syntax error");
    return;
  }

  const char *q;
  for (q = p; *q != close; ++q) {
    if (*q == '\0')
      error("not closed");
  }

  char *path = strndup_(p, q - p);
  char *fn = NULL;
  FILE *fp = NULL;
  // Search from current directory.
  if (!sys) {
    fn = abspath_cwd(dirname(strdup_(srcname)), path);
    fp = fopen(fn, "r");
  }
  if (fp == NULL) {
    // Search from system include directries.
    for (int i = 0; i < sys_inc_paths->len; ++i) {
      fn = abspath_cwd(sys_inc_paths->data[i], path);
      fp = fopen(fn, "r");
      if (fp != NULL)
        break;
    }
    if (fp == NULL) {
      error("Cannot open file: %s", path);
      return;
    }
  }

  if (registered_pragma_once(fn))
    return;

  printf("# 1 \"%s\" 1\n", fn);
  int lineno = pp(fp, fn);
  printf("# %d \"%s\" 2\n", lineno, fn);
  fclose(fp);
}

void handle_pragma(const char *p, const char *filename) {
  char *name = read_ident(&p);
  if (strcmp(name, "once") == 0) {
    if (!registered_pragma_once(filename))
      register_pragma_once(filename);
  } else {
    fprintf(stderr, "Warning: unhandled #pragma: %s\n", p);
  }
}

Vector *parse_macro_body(const char *p, const Vector *params, bool va_args, Stream *stream) {
  Vector *segments = new_vector();
  init_lexer_string(p, stream->filename, stream->lineno);
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

void handle_define(const char *p, Stream *stream) {
  char *name = read_ident(&p);
  if (name == NULL)
    error("`ident' expected");

  Vector *params = NULL;
  bool va_args = false;
  if (*p == '(') {
    // Macro with parameter.
    params = new_vector();
    init_lexer_string(p + 1, stream->filename, stream->lineno);
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
    segments = parse_macro_body(skip_whitespaces(p), params, va_args, stream);
  }
  map_put(macro_map, name, new_macro(params, va_args, segments));
}

void handle_undef(const char *p) {
  char *name = read_ident(&p);
  if (name == NULL)
    error("`ident' expected");

  map_remove(macro_map, name);
}

Token *consume2(enum TokenType type) {
  Token *tok;
  for (;;) {
    tok = consume(type);
    if (tok == NULL || tok->type != TK_EOF)
      return tok;

    char *line = NULL;
    size_t capa = 0;
    ssize_t len = getline_(&line, &capa, s_stream->fp, 0);
    if (len == EOF)
      return tok;  // EOF
    ++s_stream->lineno;
    init_lexer_string(line, s_stream->filename, s_stream->lineno);
  }
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
          if (paren > 0) {
            if (tok->type == TK_RPAR)
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

bool handle_block_comment(const char *begin, const char **pp, Stream *stream) {
  const char *p = skip_whitespaces(*pp);
  if (*p != '/' || p[1] != '*')
    return false;

  p += 2;
  for (;;) {
    if (*p == '\0') {
      fwrite(begin, p - begin, 1, stdout);
      fputc('\n', stdout);

      char *line = NULL;
      size_t capa = 0;
      ssize_t len = getline_(&line, &capa, stream->fp, 0);
      if (len == EOF) {
        *pp = p;
        return true;
      }
      ++stream->lineno;
      begin = p = line;
      continue;
    }

    if (*p == '*' && p[1] == '/') {
      p += 2;
      fwrite(begin, p - begin, 1, stdout);
      *pp = p;
      return true;
    }

    ++p;
  }
}

void process_line(const char *line, Stream *stream) {
  init_lexer_string(line, stream->filename, stream->lineno);

  const char *begin = get_lex_p();
  for (;;) {
    const char *p = get_lex_p();
    if (p != NULL) {
      if (handle_block_comment(begin, &p, stream)) {
        begin = p;
        init_lexer_string(begin, stream->filename, stream->lineno);
      }
    }

    if (consume(TK_EOF))
      break;

    Token *ident = consume(TK_IDENT);
    Macro *macro;
    if (ident != NULL && (macro = map_get(macro_map, ident->u.ident)) != NULL) {
      if (ident->begin != begin)
        fwrite(begin, ident->begin - begin, 1, stdout);

      s_stream = stream;
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

intptr_t reduce(Expr *expr) {
  switch (expr->type) {
  case EX_NUM:
    switch (expr->valType->u.num.type) {
    case NUM_CHAR:
    case NUM_SHORT:
    case NUM_INT:
    case NUM_LONG:
      return expr->u.num.ival;
    default: assert(false); break;
    }
    break;
  case EX_FUNCALL:
    {
      const Expr *func = expr->u.funcall.func;
      const Vector *args = expr->u.funcall.args;
      if (func->type == EX_VARREF &&
          strcmp(func->u.varref.ident, "defined") == 0 &&
          args != NULL && args->len == 1 &&
          ((Expr*)args->data[0])->type == EX_VARREF) {  // defined(IDENT)
        Expr *arg = (Expr*)args->data[0];
        void *dummy = 0;
        return map_try_get(macro_map, arg->u.varref.ident, &dummy) ? 1 : 0;
      }
    }
    break;
  case EX_NOT:
    return reduce(expr->u.unary.sub) ? 0 : 1;
  case EX_LOGAND:
    return reduce(expr->u.bop.lhs) && reduce(expr->u.bop.rhs);
  case EX_LOGIOR:
    return reduce(expr->u.bop.lhs) || reduce(expr->u.bop.rhs);
  default:
    break;
  }
  error("expression not handled in preprocessor: type=%d", expr->type);
  return 0;
}

bool handle_if(const char *p, Stream *stream) {
  init_lexer_string(p, stream->filename, stream->lineno);
  Expr *expr = parse_expr();
  return reduce(expr) != 0;
}

#define CF_ENABLE         (1 << 0)
#define CF_SATISFY_SHIFT  (1)
#define CF_SATISFY_MASK   (3 << CF_SATISFY_SHIFT)

intptr_t cond_value(bool enable, int satisfy) {
  return (enable ? CF_ENABLE : 0) | (satisfy << CF_SATISFY_SHIFT);
}

static void define_file_macro(const char *filename) {
  size_t len = strlen(filename);
  char *buf = malloc(len + 2 + 1);
  snprintf(buf, len + 2 + 1, "\"%s\"", filename);
  map_put(macro_map, "__FILE__", new_macro_single(buf));
}

int pp(FILE *fp, const char *filename) {
  Vector *condstack = new_vector();
  bool enable = true;
  int satisfy = 0;  // #if condition: 0=not satisfied, 1=satisfied, 2=else
  //char linenobuf[sizeof(int) * 3 + 1];  // Buffer for __LINE__
  char linenobuf[32];  // Buffer for __LINE__

  Macro *old_file_macro = map_get(macro_map, "__FILE__");
  Macro *old_line_macro = map_get(macro_map, "__LINE__");

  define_file_macro(filename);
  map_put(macro_map, "__LINE__", new_macro_single(linenobuf));

  Stream stream;
  stream.filename = filename;
  stream.fp = fp;

  for (stream.lineno = 1;; ++stream.lineno) {
    char *line = NULL;
    size_t capa = 0;
    ssize_t len = getline_(&line, &capa, fp, 0);
    if (len == EOF)
      break;

    snprintf(linenobuf, sizeof(linenobuf), "%d", stream.lineno);

    while (len > 0 && line[len - 1] == '\\') {  // Continue line.
      ++stream.lineno;
      len = getline_(&line, &capa, fp, len - 1);  // -1 for overwrite on '\'
    }

    // Find '#'
    const char *directive = find_directive(line);
    if (directive == NULL) {
      if (enable)
        process_line(line, &stream);
      else
        printf("\n");
      continue;
    }
    printf("\n");

    const char *next;
    if ((next = keyword(directive, "ifdef")) != NULL) {
      vec_push(condstack, (void*)cond_value(enable, satisfy));
      bool defined = handle_ifdef(next);
      satisfy = defined ? 1 : 0;
      enable = enable && satisfy == 1;
    } else if ((next = keyword(directive, "ifndef")) != NULL) {
      vec_push(condstack, (void*)cond_value(enable, satisfy));
      bool defined = handle_ifdef(next);
      satisfy = defined ? 0 : 1;
      enable = enable && satisfy == 1;
    } else if ((next = keyword(directive, "if")) != NULL) {
      vec_push(condstack, (void*)cond_value(enable, satisfy));
      bool cond = handle_if(next, &stream);
      satisfy = cond ? 1 : 0;
      enable = enable && satisfy == 1;
    } else if ((next = keyword(directive, "else")) != NULL) {
      int last = condstack->len - 1;
      if (last < 0)
        error("`#else' used without `#if'");
      intptr_t flag = (intptr_t)condstack->data[last];
      if (satisfy == 2)
        error("Illegal #else");
      enable = !enable && satisfy == 0 && ((flag & CF_ENABLE) != 0);
      satisfy = 2;
    } else if ((next = keyword(directive, "elif")) != NULL) {
      int last = condstack->len - 1;
      if (last < 0)
        error("`#elif' used without `#if'");
      intptr_t flag = (intptr_t)condstack->data[last];
      if (satisfy == 2)
        error("Illegal #elif");

      bool cond = false;
      if (satisfy == 0) {
        cond = handle_if(next, &stream);
        if (cond)
          satisfy = 1;
      }

      enable = !enable && cond && ((flag & CF_ENABLE) != 0);
    } else if ((next = keyword(directive, "endif")) != NULL) {
      int len = condstack->len;
      if (len <= 0)
        error("`#endif' used without `#if'");
      --len;
      int flag = (intptr_t)condstack->data[len];
      enable = (flag & CF_ENABLE) != 0;
      satisfy = (flag & CF_SATISFY_MASK) >> CF_SATISFY_SHIFT;
      condstack->len = len;
    } else if (enable) {
      if ((next = keyword(directive, "include")) != NULL) {
        handle_include(next, filename);
        printf("# %d \"%s\" 1\n", stream.lineno + 1, filename);
      } else if ((next = keyword(directive, "define")) != NULL) {
        handle_define(next, &stream);
      } else if ((next = keyword(directive, "undef")) != NULL) {
        handle_undef(next);
      } else if ((next = keyword(directive, "pragma")) != NULL) {
        handle_pragma(next, filename);
      } else if ((next = keyword(directive, "error")) != NULL) {
        error("#error: %s", next);
      } else {
        error("unknown directive: %s", directive);
      }
    }
  }

  if (condstack->len > 0)
    error("#if not closed");

  map_put(macro_map, "__FILE__", old_file_macro);
  map_put(macro_map, "__LINE__", old_line_macro);

  return stream.lineno;
}

static void define_macro(const char *arg) {
  char *p = strchr(arg, '=');
  if (p == NULL) {
    map_put(macro_map, arg, new_macro(NULL, false, NULL));
  } else {
    char *name = strndup_(arg, p - arg);
    map_put(macro_map, name, new_macro_single(p + 1));
  }
}

int main(int argc, char* argv[]) {
  macro_map = new_map();
  sys_inc_paths = new_vector();
  pragma_once_files = new_vector();

  // Predefeined macros.
  map_put(macro_map, "__XCC", new_macro(NULL, false, NULL));
#if defined(__XV6)
  map_put(macro_map, "__XV6", new_macro(NULL, false, NULL));
#elif defined(__linux__)
  map_put(macro_map, "__linux__", new_macro(NULL, false, NULL));
#endif

  int i = 1;
  for (; i < argc; ++i) {
    if (*argv[i] != '-')
      break;
    if (strncmp(argv[i], "-I", 2) == 0) {
      vec_push(sys_inc_paths, strdup_(argv[i] + 2));
    }
    if (strncmp(argv[i], "-D", 2) == 0)
      define_macro(argv[i] + 2);
  }

  if (i < argc) {
    for (; i < argc; ++i) {
      const char *filename = argv[i];
      FILE *fp = fopen(filename, "r");
      if (fp == NULL)
        error("Cannot open file: %s\n", filename);
      printf("# 1 \"%s\" 1\n", filename);
      pp(fp, filename);
      fclose(fp);
    }
  } else {
    pp(stdin, "*stdin*");
  }
  return 0;
}
