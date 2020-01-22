#include <assert.h>
#include <ctype.h>
#include <libgen.h>  // dirname
#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "ast.h"
#include "lexer.h"
#include "parser.h"
#include "table.h"
#include "type.h"
#include "util.h"

char *abspath_cwd(const char *dir, const char *path) {
  char *cwd = getcwd(NULL, 0);
  char *root = abspath(cwd, dir);
  free(cwd);
  return abspath(root, path);
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
  };
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
  seg->text = text;
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

Table macro_table;  // <Name, Macro*>

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
  const char *begin = p;
  const char *end = read_ident(p);
  if ((end - begin) == 4 && strncmp(begin, "once", 4) == 0) {
    if (!registered_pragma_once(filename))
      register_pragma_once(filename);
  } else {
    fprintf(stderr, "Warning: unhandled #pragma: %s\n", p);
  }
}

Vector *parse_macro_body(const char *p, const Vector *params, bool va_args, Stream *stream) {
  Vector *segments = new_vector();
  set_source_string(p, stream->filename, stream->lineno);
  int param_len = params != NULL ? params->len : 0;
  const Name *key_va_args = alloc_name("__VA_ARGS__", NULL, false);
  StringBuffer sb;
  sb_init(&sb);
  for (;;) {
    Token *tok;
    if ((tok = match(TK_IDENT)) != NULL) {
      int index = -1;
      if (va_args && equal_name(tok->ident, key_va_args)) {
        index = param_len;
      } else {
        for (int i = 0; i < param_len; ++i) {
          if (equal_name(tok->ident, params->data[i])) {
            index = i;
            break;
          }
        }
      }
      if (index >= 0) {
        if (!sb_empty(&sb)) {
          Segment *seg = malloc(sizeof(*seg));
          seg->type = ST_TEXT;
          seg->text = sb_to_string(&sb);
          vec_push(segments, seg);
          sb_clear(&sb);
        }

        Segment *seg2 = malloc(sizeof(*seg2));
        seg2->type = ST_PARAM;
        seg2->param = index;
        vec_push(segments, seg2);

        continue;
      }
    } else {
      tok = match(-1);
      if (tok->kind == TK_EOF)
        break;
    }

    if (!sb_empty(&sb))
      sb_append(&sb, " ", NULL);
    sb_append(&sb, tok->begin, tok->end);
  }

  if (!sb_empty(&sb)) {
    Segment *seg = malloc(sizeof(*seg));
    seg->type = ST_TEXT;
    seg->text = sb_to_string(&sb);
    vec_push(segments, seg);
  }
  return segments;
}

void handle_define(const char *p, Stream *stream) {
  const char *begin = p;
  const char *end = read_ident(p);
  if (end == NULL)
    error("`ident' expected");
  const Name *name = alloc_name(begin, end, false);
  p = end;

  Vector *params = NULL;
  bool va_args = false;
  if (*p == '(') {
    // Macro with parameter.
    params = new_vector();
    set_source_string(p + 1, stream->filename, stream->lineno);
    if (!match(TK_RPAR)) {
      for (;;) {
        Token *tok;
        if ((tok = match(TK_DOTDOTDOT)) != NULL) {
          va_args = true;
          consume(TK_RPAR, "`)' expected");
          break;
        } else {
          tok = consume(TK_IDENT, "`ident' expected");
          vec_push(params, tok->ident);
          if (match(TK_RPAR))
            break;
          consume(TK_COMMA, "`,' or `)' expected");
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
  table_put(&macro_table, name, new_macro(params, va_args, segments));
}

void handle_undef(const char *p) {
  const char *begin = p;
  const char *end = read_ident(p);
  if (end == NULL)
    error("`ident' expected");
  const Name *name = alloc_name(begin, end, false);

  table_delete(&macro_table, name);
}

Token *match2(enum TokenKind kind) {
  Token *tok;
  for (;;) {
    tok = match(kind);
    if (tok == NULL || tok->kind != TK_EOF)
      return tok;

    char *line = NULL;
    size_t capa = 0;
    ssize_t len = getline_(&line, &capa, s_stream->fp, 0);
    if (len == EOF)
      return tok;  // EOF
    ++s_stream->lineno;
    set_source_string(line, s_stream->filename, s_stream->lineno);
  }
}

char *expand(Macro *macro, Vector *args, const Name *name) {
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

  StringBuffer sb;
  sb_init(&sb);
  if (macro->segments != NULL) {
    for (int i = 0; i < macro->segments->len; ++i) {
      Segment *seg = macro->segments->data[i];
      switch (seg->type) {
      case ST_TEXT:
        sb_append(&sb, seg->text, NULL);
        break;
      case ST_PARAM:
        sb_append(&sb, (char*)args->data[seg->param], NULL);
        break;
      default:
        break;
      }
    }
  }
  const char *left = get_lex_p();
  if (left != NULL)
    sb_append(&sb, get_lex_p(), NULL);

  return sb_to_string(&sb);
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

static Vector *parse_funargs(void) {
  Vector *args = NULL;
  if (match2(TK_LPAR)) {
    args = new_vector();
    StringBuffer sb;
    sb_init(&sb);
    if (!match2(TK_RPAR)) {
      int paren = 0;
      for (;;) {
        if (match2(TK_EOF))
          parse_error(NULL, "`)' expected");

        Token *tok;
        if ((tok = match2(TK_COMMA)) != NULL || (tok = match2(TK_RPAR)) != NULL)  {
          if (paren > 0) {
            sb_append(&sb, tok->begin, tok->end);
            if (tok->kind == TK_RPAR)
              --paren;
            continue;
          }
          if (sb_empty(&sb))
            parse_error(tok, "expression expected");

          vec_push(args, sb_to_string(&sb));
          sb_clear(&sb);

          if (tok->kind == TK_RPAR)
            break;
          continue;
        }
        tok = match2(-1);
        if (tok->kind == TK_LPAR)
          ++paren;
        if (!sb_empty(&sb))
          sb_append(&sb, " ", NULL);
        sb_append(&sb, tok->begin, tok->end);
      }
    }
  }
  return args;
}

void process_line(const char *line, Stream *stream) {
  set_source_string(line, stream->filename, stream->lineno);

  const char *begin = get_lex_p();
  for (;;) {
    const char *p = get_lex_p();
    if (p != NULL) {
      if (handle_block_comment(begin, &p, stream)) {
        begin = p;
        set_source_string(begin, stream->filename, stream->lineno);
      }
    }

    if (match(TK_EOF))
      break;

    Token *ident = match(TK_IDENT);
    Macro *macro;
    if (ident != NULL && (macro = table_get(&macro_table, ident->ident)) != NULL) {
      if (ident->begin != begin)
        fwrite(begin, ident->begin - begin, 1, stdout);

      s_stream = stream;
      Vector *args = NULL;
      if (macro->params != NULL)
        args = parse_funargs();
      char *expanded = expand(macro, args, ident->ident);
      set_source_string(expanded, NULL, -1);
      begin = get_lex_p();
      continue;
    }

    match(-1);
  }

  printf("%s\n", begin);
}

bool handle_ifdef(const char *p) {
  const char *begin = p;
  const char *end = read_ident(p);
  if (end == NULL)
    error("`ident' expected");
  const Name *name = alloc_name(begin, end, false);
  return table_get(&macro_table, name) != NULL;
}

intptr_t reduce(Expr *expr) {
  switch (expr->kind) {
  case EX_NUM:
    switch (expr->type->num.kind) {
    case NUM_CHAR:
    case NUM_SHORT:
    case NUM_INT:
    case NUM_LONG:
      return expr->num.ival;
    default: assert(false); break;
    }
    break;
  case EX_VARIABLE:
    {
      Macro *macro = table_get(&macro_table, expr->variable.name);
      if (macro == NULL) {
        parse_error(NULL, "`%.s' not defined", expr->variable.name->bytes, expr->variable.name->chars);
      }
      char *expanded = expand(macro, NULL, expr->variable.name);
      int flag = 1;
      if (*expanded == '-') {
        ++expanded;
        flag = -1;
      }
      char *p;
      intptr_t value = strtol(expanded, &p, 10);
      if (p == expanded)
        parse_error(NULL, "number expected");
      return value * flag;
    }
    break;
  case EX_ADD:    return reduce(expr->bop.lhs) + reduce(expr->bop.rhs);
  case EX_SUB:    return reduce(expr->bop.lhs) - reduce(expr->bop.rhs);
  case EX_DIV:    return reduce(expr->bop.lhs) * reduce(expr->bop.rhs);
  case EX_MUL:    return reduce(expr->bop.lhs) / reduce(expr->bop.rhs);
  case EX_MOD:    return reduce(expr->bop.lhs) % reduce(expr->bop.rhs);
  case EX_BITAND: return reduce(expr->bop.lhs) & reduce(expr->bop.rhs);
  case EX_BITOR:  return reduce(expr->bop.lhs) | reduce(expr->bop.rhs);
  case EX_BITXOR: return reduce(expr->bop.lhs) ^ reduce(expr->bop.rhs);
  case EX_LSHIFT: return reduce(expr->bop.lhs) << reduce(expr->bop.rhs);
  case EX_RSHIFT: return reduce(expr->bop.lhs) >> reduce(expr->bop.rhs);
  case EX_EQ:     return reduce(expr->bop.lhs) == reduce(expr->bop.rhs);
  case EX_NE:     return reduce(expr->bop.lhs) != reduce(expr->bop.rhs);
  case EX_LT:     return reduce(expr->bop.lhs) < reduce(expr->bop.rhs);
  case EX_LE:     return reduce(expr->bop.lhs) <= reduce(expr->bop.rhs);
  case EX_GE:     return reduce(expr->bop.lhs) >= reduce(expr->bop.rhs);
  case EX_GT:     return reduce(expr->bop.lhs) > reduce(expr->bop.rhs);
  case EX_LOGAND: return reduce(expr->bop.lhs) && reduce(expr->bop.rhs);
  case EX_LOGIOR: return reduce(expr->bop.lhs) || reduce(expr->bop.rhs);
  case EX_POS:    return reduce(expr->unary.sub);
  case EX_NEG:    return -reduce(expr->unary.sub);
  case EX_NOT:    return reduce(expr->unary.sub) ? 0 : 1;
  case EX_BITNOT: return ~reduce(expr->unary.sub);
  case EX_GROUP:  return reduce(expr->unary.sub);
  case EX_CAST:   return reduce(expr->unary.sub);
  case EX_TERNARY:return reduce(expr->ternary.cond) ? reduce(expr->ternary.tval) : reduce(expr->ternary.fval);
  case EX_FUNCALL:
    {
      const Expr *func = expr->funcall.func;
      const Vector *args = expr->funcall.args;
      if (func->kind == EX_VARIABLE &&
          equal_name(func->variable.name, alloc_name("defined", NULL, false)) &&
          args != NULL && args->len == 1 &&
          ((Expr*)args->data[0])->kind == EX_VARIABLE) {  // defined(IDENT)
        Expr *arg = (Expr*)args->data[0];
        void *dummy = 0;
        return table_try_get(&macro_table, arg->variable.name, &dummy) ? 1 : 0;
      }
    }
    break;
  default:
    break;
  }
  error("expression not handled in preprocessor: type=%d", expr->kind);
  return 0;
}

bool handle_if(const char *p, Stream *stream) {
  set_source_string(p, stream->filename, stream->lineno);
  Expr *expr = parse_expr();
  return reduce(expr) != 0;
}

#define CF_ENABLE         (1 << 0)
#define CF_SATISFY_SHIFT  (1)
#define CF_SATISFY_MASK   (3 << CF_SATISFY_SHIFT)

intptr_t cond_value(bool enable, int satisfy) {
  return (enable ? CF_ENABLE : 0) | (satisfy << CF_SATISFY_SHIFT);
}

static void define_file_macro(const char *filename, const Name *key_file) {
  size_t len = strlen(filename);
  char *buf = malloc(len + 2 + 1);
  snprintf(buf, len + 2 + 1, "\"%s\"", filename);
  table_put(&macro_table, key_file, new_macro_single(buf));
}

int pp(FILE *fp, const char *filename) {
  Vector *condstack = new_vector();
  bool enable = true;
  int satisfy = 0;  // #if condition: 0=not satisfied, 1=satisfied, 2=else
  //char linenobuf[sizeof(int) * 3 + 1];  // Buffer for __LINE__
  char linenobuf[32];  // Buffer for __LINE__

  const Name *key_file = alloc_name("__FILE__", NULL, false);
  const Name *key_line = alloc_name("__LINE__", NULL, false);

  Macro *old_file_macro = table_get(&macro_table, key_file);
  Macro *old_line_macro = table_get(&macro_table, key_line);

  define_file_macro(filename, key_file);
  table_put(&macro_table, key_line, new_macro_single(linenobuf));

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

  table_put(&macro_table, key_file, old_file_macro);
  table_put(&macro_table, key_line, old_line_macro);

  return stream.lineno;
}

static void define_macro(const char *arg) {
  char *p = strchr(arg, '=');
  if (p == NULL) {
    table_put(&macro_table, alloc_name(arg, NULL, true), new_macro(NULL, false, NULL));
  } else {
    const Name *name = alloc_name(arg, p, true);
    table_put(&macro_table, name, new_macro_single(p + 1));
  }
}

int main(int argc, char *argv[]) {
  sys_inc_paths = new_vector();
  pragma_once_files = new_vector();

  // Predefeined macros.
  table_put(&macro_table, alloc_name("__XCC", NULL, true), new_macro(NULL, false, NULL));
#if defined(__XV6)
  table_put(&macro_table, alloc_name("__XV6", NULL, true), new_macro(NULL, false, NULL));
#elif defined(__linux__)
  table_put(&macro_table, alloc_name("__linux__", NULL, true), new_macro(NULL, false, NULL));
#elif defined(__APPLE__)
  table_put(&macro_table, alloc_name("__APPLE__", NULL, true), new_macro(NULL, false, NULL));
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

  init_lexer();

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
