#include "ctype.h"
#include "libgen.h"  // dirname
#include "stdarg.h"
#include "stdbool.h"
#include "stdio.h"
#include "stdlib.h"
#include "string.h"
#include "unistd.h"

#include "../cc/expr.h"
#include "../cc/lexer.h"
#include "../cc/util.h"

char *abspath(const char *root, const char *path) {
  //EXPECT_STREQ("Relative", "/user/foo/inc/stdio.h", abspath("/user/foo", "inc/stdio.h"));
  //EXPECT_STREQ("Absolute", "/inc/stdio.h", abspath("/user/foo", "/inc/stdio.h"));
  //EXPECT_STREQ("Current", "/user/foo/inc/stdio.h", abspath("/user/foo", "./inc/stdio.h"));
  //EXPECT_STREQ("Parent", "/user/inc/stdio.h", abspath("/user/foo", "../inc/stdio.h"));
  //EXPECT_STREQ("Dir", "/user/foo/bar/baz/", abspath("/user/foo", "bar/baz/"));
  //EXPECT_STREQ("Redundant slash", "/user/foo/bar/baz", abspath("/user/foo", "bar//baz"));
  //EXPECT_STREQ("Root 1", "/", abspath("/", "."));  //
  //EXPECT_STREQ("Root 2", "/", abspath("/user/foo", "../.."));
  //EXPECT_STREQ("Root 3", "/", abspath("/user/foo", "../../"));
  //EXPECT_STR_NULL("Illegal", abspath("/user/foo", "../../.."));
  //EXPECT_STREQ("Root end with '/'", "/user/foo/inc/stdio.h", abspath("/user/foo/", "inc/stdio.h"));
  //EXPECT_STREQ("Not root", "user/foo/inc/stdio.h", abspath("user/foo", "inc/stdio.h"));

  if (*path == '/')
    return strdup_(path);

  bool is_root = *root == '/';

  Vector *dirs = new_vector();  // [start, end]
  for (const char *p = root; *p != '\0'; ) {
    if (*p == '/')
      if (*(++p) == '\0')
        break;
    vec_push(dirs, p);
    const char *q = strchr(p, '/');
    if (q == NULL) {
      vec_push(dirs, p + strlen(p));
      break;
    }
    vec_push(dirs, q);
    p = q;
  }

  for (const char *p = path; *p != '\0'; ) {
    if (*p == '/') {
      while (*p == '/')
        ++p;
      if (*p == '\0') {
        // End with '/'.
        vec_push(dirs, p);
        vec_push(dirs, p);
        break;
      }
    }
    const char *q = strchr(p, '/');
    if (q == NULL)
      q = p + strlen(p);
    size_t size = q - p;
    if (size == 1 && strncmp(p, ".", size) == 0) {
      // Skip
    } else if (size == 2 && strncmp(p, "..", size) == 0) {
      if (dirs->len < 2)
        return NULL;  // Illegal
      dirs->len -= 2;
    } else {
      vec_push(dirs, p);
      vec_push(dirs, q);
    }
    p = q;
  }

  if (dirs->len == 0)
    return strdup_("/");

  size_t total_len = 1;  // 1 for NUL-terminate.
  for (int i = 0; i < dirs->len; i += 2) {
    if (i != 0 || is_root)
      total_len += 1;
    total_len += ((char*)dirs->data[i + 1] - (char*)dirs->data[i]);
  }

  char *buf = malloc(total_len);
  char *p = buf;
  for (int i = 0; i < dirs->len; i += 2) {
    if (i != 0 || is_root)
      *p++ = '/';
    size_t size = (char*)dirs->data[i + 1] - (char*)dirs->data[i];
    memcpy(p, dirs->data[i], size);
    p += size;
  }
  *p = '\0';
  return buf;
}

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

void pp(FILE *fp, const char *filename);

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

  printf("/* \"%s\" start */\n", fn);
  pp(fp, fn);
  printf("/* \"%s\" end */\n", fn);
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

intptr_t reduce(Expr *expr) {
  switch (expr->type) {
  case EX_CHAR:
  case EX_SHORT:
  case EX_INT:
  case EX_LONG:
    return expr->u.value;
  case EX_FUNCALL:
    {
      const Expr *func = expr->u.funcall.func;
      const Vector *args = expr->u.funcall.args;
      if (func->type == EX_VARREF &&
          strcmp(func->u.varref.ident, "defined") == 0 &&
          args != NULL && args->len == 1 &&
          ((Expr*)args->data[0])->type == EX_VARREF) {  // defined(IDENT)
        Expr *arg = (Expr*)args->data[0];
        return map_get(macro_map, arg->u.varref.ident) != NULL ? 1 : 0;
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

bool handle_if(const char *p, const char *filename, int lineno) {
  init_lexer_string(p, filename, lineno);
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

void pp(FILE *fp, const char *filename) {
  Vector *condstack = new_vector();
  bool enable = true;
  int satisfy = 0;  // #if condition: 0=not satisfied, 1=satisfied, 2=else
  //char linenobuf[sizeof(int) * 3 + 1];  // Buffer for __LINE__
  char linenobuf[32];  // Buffer for __LINE__

  Macro *old_file_macro = map_get(macro_map, "__FILE__");
  Macro *old_line_macro = map_get(macro_map, "__LINE__");

  define_file_macro(filename);
  map_put(macro_map, "__LINE__", new_macro_single(linenobuf));

  for (int lineno = 1;; ++lineno) {
    char *line = NULL;
    size_t capa = 0;
    ssize_t len = getline_(&line, &capa, fp, 0);
    if (len == EOF)
      break;

    snprintf(linenobuf, sizeof(linenobuf), "%d", lineno);

    while (len > 0 && line[len - 1] == '\\') {  // Continue line.
      ++lineno;
      len = getline_(&line, &capa, fp, len - 1);  // -1 for overwrite on '\'
    }

    // Find '#'
    const char *directive = find_directive(line);
    if (directive == NULL) {
      if (enable)
        process_line(line, filename, lineno);
      continue;
    }

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
      bool cond = handle_if(next, filename, lineno);
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
        cond = handle_if(next, filename, lineno);
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
      } else if ((next = keyword(directive, "define")) != NULL) {
        handle_define(next, filename, lineno);
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
  }

  if (i < argc) {
    for (; i < argc; ++i) {
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
