#include "preprocessor.h"

#include <assert.h>
#include <ctype.h>
#include <libgen.h>  // dirname
#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "lexer.h"
#include "macro.h"
#include "pp_parser.h"
#include "table.h"
#include "type.h"
#include "util.h"

char *cat_path_cwd(const char *dir, const char *path) {
  char *cwd = getcwd(NULL, 0);
  char *root = cat_path(cwd, dir);
  free(cwd);
  return cat_path(root, path);
}

char *fullpath(const char *filename) {
  return cat_path_cwd(dirname(strdup_(filename)), basename((char*)filename));
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

static FILE *pp_ofp;
static Vector *sys_inc_paths;  // <const char*>
static Vector *pragma_once_files;  // <const char*>

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
  if (!is_fullpath(filename))
    filename = fullpath(filename);
  vec_push(pragma_once_files, filename);
}

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
    fn = cat_path_cwd(dirname(strdup_(srcname)), path);
    if (registered_pragma_once(fn))
      return;
    fp = fopen(fn, "r");
  }
  if (fp == NULL) {
    // Search from system include directries.
    for (int i = 0; i < sys_inc_paths->len; ++i) {
      fn = cat_path_cwd(sys_inc_paths->data[i], path);
      if (registered_pragma_once(fn))
        return;
      fp = fopen(fn, "r");
      if (fp != NULL)
        break;
    }
    if (fp == NULL) {
      error("Cannot open file: %s", path);
      return;
    }
  }

  fprintf(pp_ofp, "# 1 \"%s\" 1\n", fn);
  int lineno = preprocess(fp, fn);
  fprintf(pp_ofp, "# %d \"%s\" 2\n", lineno, fn);
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

static void push_text_segment(Vector *segments, const char *start, const char *token_begin) {
  if (token_begin > start) {
    Segment *seg = malloc(sizeof(*seg));
    seg->kind = SK_TEXT;
    seg->text = strndup_(start, token_begin - start);
    vec_push(segments, seg);
  }
}

Vector *parse_macro_body(const char *p, const Vector *params, bool va_args, Stream *stream) {
  Vector *segments = new_vector();
  set_source_string(p, stream->filename, stream->lineno);
  int param_len = params != NULL ? params->len : 0;
  const Name *key_va_args = alloc_name("__VA_ARGS__", NULL, false);
  const char *start = p;
  const char *end = start;
  for (;;) {
    Token *tok = match(-1);
    if (tok->kind == TK_EOF)
      break;
    switch (tok->kind) {
    case TK_IDENT:
      {
        int param_index = -1;
        if (va_args && equal_name(tok->ident, key_va_args)) {
          param_index = param_len;
        } else {
          for (int i = 0; i < param_len; ++i) {
            if (equal_name(tok->ident, params->data[i])) {
              param_index = i;
              break;
            }
          }
        }
        if (param_index >= 0) {
          push_text_segment(segments, start, tok->begin);

          Segment *seg = malloc(sizeof(*seg));
          seg->kind = SK_PARAM;
          seg->param = param_index;
          vec_push(segments, seg);

          start = end = tok->end;
          continue;
        }
      }
      break;
    case PPTK_CONCAT:
      push_text_segment(segments, start, end);

      start = end = skip_whitespaces(tok->end);
      continue;
    case PPTK_STRINGIFY:
      {
        Token *ident;
        if ((ident = match(TK_IDENT)) != NULL) {
          int param_index = -1;
          for (int i = 0; i < param_len; ++i) {
            if (equal_name(ident->ident, params->data[i])) {
              param_index = i;
              break;
            }
          }
          if (param_index >= 0) {
            push_text_segment(segments, start, tok->begin);

            Segment *seg = malloc(sizeof(*seg));
            seg->kind = SK_STRINGIFY;
            seg->param = param_index;
            vec_push(segments, seg);

            start = end = ident->end;
            continue;
          }
      }
      }
      break;
    default:
      break;
    }
    end = tok->end;
  }

  if (start != end) {
    Segment *seg = malloc(sizeof(*seg));
    seg->kind = SK_TEXT;
    seg->text = strndup_(start, end - start);
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
        if ((tok = match(TK_ELLIPSIS)) != NULL) {
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

bool handle_block_comment(const char *begin, const char **pp, Stream *stream) {
  const char *p = skip_whitespaces(*pp);
  if (*p != '/' || p[1] != '*')
    return false;

  p += 2;
  for (;;) {
    if (*p == '\0') {
      fwrite(begin, p - begin, 1, pp_ofp);
      fputc('\n', pp_ofp);

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
      fwrite(begin, p - begin, 1, pp_ofp);
      *pp = p;
      return true;
    }

    ++p;
  }
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
      s_stream = stream;
      Vector *args = NULL;
      if (macro->params != NULL)
        args = pp_funargs(s_stream);

      StringBuffer sb;
      sb_init(&sb);
      if (!expand(macro, ident, args, ident->ident, &sb))
        continue;

      if (ident->begin != begin)
        fwrite(begin, ident->begin - begin, 1, pp_ofp);

      const char *left = get_lex_p();
      if (left != NULL)
        sb_append(&sb, left, NULL);
      char *expanded = sb_to_string(&sb);

      set_source_string(expanded, NULL, -1);
      begin = get_lex_p();
      continue;
    }

    match(-1);
  }

  fprintf(pp_ofp, "%s\n", begin);
}

bool handle_ifdef(const char *p) {
  const char *begin = p;
  const char *end = read_ident(p);
  if (end == NULL)
    error("`ident' expected");
  const Name *name = alloc_name(begin, end, false);
  return table_get(&macro_table, name) != NULL;
}

bool handle_if(const char *p, Stream *stream) {
  set_source_string(p, stream->filename, stream->lineno);
  return pp_expr() != 0;
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

void init_preprocessor(FILE *ofp) {
  pp_ofp = ofp;
  sys_inc_paths = new_vector();
  pragma_once_files = new_vector();

  init_lexer();
}

int preprocess(FILE *fp, const char *filename) {
  Vector *condstack = new_vector();
  bool enable = true;
  int satisfy = 0;  // #if condition: 0=not satisfied, 1=satisfied, 2=else
  char linenobuf[sizeof(int) * 3 + 1];  // Buffer for __LINE__

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
        fprintf(pp_ofp, "\n");
      continue;
    }
    fprintf(pp_ofp, "\n");

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
        fprintf(pp_ofp, "# %d \"%s\" 1\n", stream.lineno + 1, filename);
      } else if ((next = keyword(directive, "define")) != NULL) {
        handle_define(next, &stream);
      } else if ((next = keyword(directive, "undef")) != NULL) {
        handle_undef(next);
      } else if ((next = keyword(directive, "pragma")) != NULL) {
        handle_pragma(next, filename);
      } else if ((next = keyword(directive, "error")) != NULL) {
        fprintf(stderr, "%s(%d): error\n", filename, stream.lineno);
        error("%s", line);
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

void define_macro(const char *arg) {
  char *p = strchr(arg, '=');
  if (p == NULL) {
    table_put(&macro_table, alloc_name(arg, NULL, true), new_macro(NULL, false, NULL));
  } else {
    const Name *name = alloc_name(arg, p, true);
    table_put(&macro_table, name, new_macro_single(p + 1));
  }
}

void define_macro_simple(const char *label) {
  table_put(&macro_table, alloc_name(label, NULL, true), new_macro(NULL, false, NULL));
}

void add_system_inc_path(const char *path) {
  vec_push(sys_inc_paths, strdup_(path));
}
