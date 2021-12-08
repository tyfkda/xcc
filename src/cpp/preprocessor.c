#include "preprocessor.h"

#include <assert.h>
#include <ctype.h>
#include <libgen.h>  // dirname
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "lexer.h"
#include "macro.h"
#include "pp_parser.h"
#include "table.h"
#include "util.h"

static char *cat_path_cwd(const char *dir, const char *path) {
  char *cwd = getcwd(NULL, 0);
  char *root = cat_path(cwd, dir);
  free(cwd);
  return cat_path(root, path);
}

static char *fullpath(const char *filename) {
  return cat_path_cwd(dirname(strdup_(filename)), basename((char*)filename));
}

static const char *keyword(const char *s, const char *word) {
  size_t len = strlen(word);
  if (strncmp(s, word, len) != 0 || isalnum_(s[len]))
    return NULL;
  return skip_whitespaces(s + len);
}

static const char *find_directive(const char *line) {
  const char *p = skip_whitespaces(line);
  if (*p != '#')
    return NULL;
  return skip_whitespaces(p + 1);
}

static FILE *pp_ofp;
static Vector *sys_inc_paths;  // <const char*>
static Vector *pragma_once_files;  // <const char*>

static bool registered_pragma_once(const char *filename) {
  for (int i = 0, len = pragma_once_files->len; i < len; ++i) {
    const char *fn = pragma_once_files->data[i];
    if (strcmp(fn, filename) == 0)
      return true;
  }
  return false;
}

static void register_pragma_once(const char *filename) {
  if (!is_fullpath(filename))
    filename = fullpath(filename);
  vec_push(pragma_once_files, filename);
}

static void handle_include(const char **pp, const char *srcname) {
  const char *p = *pp;
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
  *pp = q + 1;

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

static void handle_pragma(const char **pp, const char *filename) {
  const char *p = *pp;
  const char *begin = p;
  const char *end = read_ident(p);
  if ((end - begin) == 4 && strncmp(begin, "once", 4) == 0) {
    if (!registered_pragma_once(filename))
      register_pragma_once(filename);
    *pp = end;
  } else {
    fprintf(stderr, "Warning: unhandled #pragma: %s\n", p);
    *pp = NULL;  // TODO:
  }
}

static const char *handle_line_directive(const char **pp, const char *filename, int *plineno) {
  const char *p = *pp;
  const char *next = p;
  unsigned long num = strtoul(next, (char**)&next, 10);
  if (next > p) {
    *plineno = num;
    if (isspace(*next) && (p = skip_whitespaces(next), *p == '"')) {
      p += 1;
      const char *q = strchr(p, '"');
      if (q != NULL) {
        filename = strndup_(p, q - p);
        p = q + 1;
      }
      next = p;
    }
  }
  *pp = next;
  return filename;
}

static void push_text_segment(Vector *segments, const char *start, const char *end) {
  if (end > start) {
    Segment *seg = malloc(sizeof(*seg));
    seg->kind = SK_TEXT;
    seg->text = strndup_(start, end - start);
    vec_push(segments, seg);
  }
}

static Vector *parse_macro_body(const char *p, const Vector *params, bool va_args, Stream *stream) {
  Vector *segments = new_vector();
  set_source_string(p, stream->filename, stream->lineno);
  int param_len = params != NULL ? params->len : 0;
  const Name *key_va_args = alloc_name("__VA_ARGS__", NULL, false);
  const char *start = p;
  const char *end = start;
  for (;;) {
    const char *q = block_comment_start(get_lex_p());
    if (q != NULL) {
      const char *comment_start = q;
      push_text_segment(segments, start, q);
      for (;;) {
        q = block_comment_end(q);
        if (q != NULL)
          break;

        char *line = NULL;
        size_t capa = 0;
        ssize_t len = getline_cont(&line, &capa, stream->fp, &stream->lineno);
        if (len == -1) {
          lex_error(comment_start, "Block comment not closed");
        }
        q = line;
      }
      set_source_string(q, stream->filename, stream->lineno);
      start = end = q;
      continue;
    }

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

  push_text_segment(segments, start, end);
  return segments;
}

static void handle_define(const char *p, Stream *stream) {
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
          pp_consume(TK_RPAR, "`)' expected");
          break;
        } else {
          tok = pp_consume(TK_IDENT, "`ident' expected");
          vec_push(params, tok->ident);
          if (match(TK_RPAR))
            break;
          pp_consume(TK_COMMA, "`,' or `)' expected");
        }
      }
    }
    p = get_lex_p();
  }

  Vector *segments = NULL;
  p = skip_whitespaces(p);
  if (*p != '\0') {
    segments = parse_macro_body(p, params, va_args, stream);
  }
  macro_add(name, new_macro(params, va_args, segments));
}

static void handle_undef(const char **pp) {
  const char *p = *pp;
  const char *begin = p;
  const char *end = read_ident(p);
  if (end == NULL)
    error("`ident' expected");
  const Name *name = alloc_name(begin, end, false);

  macro_delete(name);

  *pp = end;
}

static bool handle_block_comment(const char *begin, const char **pp, Stream *stream, bool enable) {
  const char *p = *pp;
  for (;;) {
    p = skip_whitespaces(p);
    if (*p != '\0')
      break;
    begin = p;
    if (!lex_eof_continue())
      return false;
    *pp = begin = p = get_lex_p();
  }

  p = block_comment_start(p);
  if (p == NULL)
    return false;

  const char *comment_start = p;
  p += 2;
  for (;;) {
    const char *q = block_comment_end(p);
    if (q != NULL) {
      if (enable)
        fwrite(begin, q - begin, 1, pp_ofp);
      *pp = q;
      break;
    }

    fprintf(pp_ofp, "%s\n", enable ? begin : "");

    char *line = NULL;
    size_t capa = 0;
    ssize_t len = getline_cont(&line, &capa, stream->fp, &stream->lineno);
    if (len == -1) {
      lex_error(comment_start, "Block comment not closed");
    }
    begin = p = line;
  }
  return true;
}

static const char **line_begin_ptr;

static void on_eof_callback(void) {
  fputs(*line_begin_ptr, pp_ofp);
  *line_begin_ptr = get_lex_p();
}

static void process_line(const char *line, bool enable, Stream *stream) {
  set_source_string(line, stream->filename, stream->lineno);

  const char *begin = get_lex_p();
  line_begin_ptr = &begin;

  for (;;) {
    const char *p = get_lex_p();
    if (p != NULL) {
      if (handle_block_comment(begin, &p, stream, enable)) {
        begin = p;
        set_source_string(begin, stream->filename, stream->lineno);
        continue;
      }
    }

    if (match(TK_EOF))
      break;

    if (enable) {
      Token *ident = match(TK_IDENT);
      Macro *macro;
      if (ident != NULL) {
        if ((macro = can_expand_ident(ident->ident)) != NULL) {
          Vector *args = NULL;
          if (macro->params != NULL)
            args = pp_funargs();

          StringBuffer sb;
          sb_init(&sb);
          if (!expand_macro(macro, ident, args, ident->ident, &sb))
            continue;

          if (ident->begin != begin)
            fwrite(begin, ident->begin - begin, 1, pp_ofp);

          push_lex(ident->ident, &on_eof_callback);

          char *expanded = sb_to_string(&sb);
          set_source_string(expanded, NULL, -1);
          begin = expanded;
        }
        continue;
      }
    }

    match(-1);
  }

  if (enable)
    fprintf(pp_ofp, "%s\n", begin);
  else
    fprintf(pp_ofp, "\n");

  line_begin_ptr = NULL;
}

static bool handle_ifdef(const char **pp) {
  const char *p = *pp;
  const char *begin = p;
  const char *end = read_ident(p);
  *pp = end;
  if (end == NULL)
    error("`ident' expected");
  const Name *name = alloc_name(begin, end, false);
  return macro_get(name) != NULL;
}

static bool handle_if(const char **pp, Stream *stream) {
  const char *p = *pp;
  set_source_string(p, stream->filename, stream->lineno);
  PpResult result = pp_expr();
  *pp = get_lex_p();
  return result != 0;
}

#define CF_ENABLE         (1 << 0)
#define CF_SATISFY_SHIFT  (1)
#define CF_SATISFY_MASK   (3 << CF_SATISFY_SHIFT)

static intptr_t cond_value(bool enable, int satisfy) {
  return (enable ? CF_ENABLE : 0) | (satisfy << CF_SATISFY_SHIFT);
}

static void define_file_macro(const char *filename, const Name *key_file) {
  size_t len = strlen(filename);
  char *buf = malloc(len + 2 + 1);
  snprintf(buf, len + 2 + 1, "\"%s\"", filename);
  macro_add(key_file, new_macro_single(buf));
}

void init_preprocessor(FILE *ofp) {
  pp_ofp = ofp;
  sys_inc_paths = new_vector();
  pragma_once_files = new_vector();

  init_lexer();
}

int preprocess(FILE *fp, const char *filename_) {
  Vector *condstack = new_vector();
  bool enable = true;
  int satisfy = 0;  // #if condition: 0=not satisfied, 1=satisfied, 2=else
  char linenobuf[sizeof(int) * 3 + 1];  // Buffer for __LINE__

  const Name *key_file = alloc_name("__FILE__", NULL, false);
  const Name *key_line = alloc_name("__LINE__", NULL, false);

  Macro *old_file_macro = macro_get(key_file);
  Macro *old_line_macro = macro_get(key_line);

  Stream stream;
  stream.filename = filename_;
  stream.fp = fp;
  Stream *old_stream = set_pp_stream(&stream);

  define_file_macro(stream.filename, key_file);
  macro_add(key_line, new_macro_single(linenobuf));

  stream.lineno = 0;
  for (;;) {
    char *line = NULL;
    size_t capa = 0;
    ssize_t len = getline_cont(&line, &capa, fp, &stream.lineno);
    if (len == -1)
      break;

    snprintf(linenobuf, sizeof(linenobuf), "%d", stream.lineno);

    // Find '#'
    const char *directive = find_directive(line);
    if (directive == NULL) {
      process_line(line, enable, &stream);
      continue;
    }
    fprintf(pp_ofp, "\n");

    const char *next;
    if ((next = keyword(directive, "ifdef")) != NULL) {
      vec_push(condstack, (void*)cond_value(enable, satisfy));
      bool defined = handle_ifdef(&next);
      satisfy = defined ? 1 : 0;
      enable = enable && satisfy == 1;
    } else if ((next = keyword(directive, "ifndef")) != NULL) {
      vec_push(condstack, (void*)cond_value(enable, satisfy));
      bool defined = handle_ifdef(&next);
      satisfy = defined ? 0 : 1;
      enable = enable && satisfy == 1;
    } else if ((next = keyword(directive, "if")) != NULL) {
      vec_push(condstack, (void*)cond_value(enable, satisfy));
      bool cond = handle_if(&next, &stream);
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
        cond = handle_if(&next, &stream);
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
        handle_include(&next, stream.filename);
        fprintf(pp_ofp, "# %d \"%s\" 1\n", stream.lineno + 1, stream.filename);
      } else if ((next = keyword(directive, "define")) != NULL) {
        handle_define(next, &stream);
        next = NULL;  // `#define' consumes the line all.
      } else if ((next = keyword(directive, "undef")) != NULL) {
        handle_undef(&next);
      } else if ((next = keyword(directive, "pragma")) != NULL) {
        handle_pragma(&next, stream.filename);
      } else if ((next = keyword(directive, "error")) != NULL) {
        fprintf(stderr, "%s(%d): error\n", stream.filename, stream.lineno);
        error("%s", line);
        next = NULL;  // TODO:
      } else if ((next = keyword(directive, "line")) != NULL) {
        stream.filename = handle_line_directive(&next, stream.filename, &stream.lineno);
        int flag = 1;
        fprintf(pp_ofp, "# %d \"%s\" %d\n", stream.lineno, stream.filename, flag);
        define_file_macro(stream.filename, key_file);
        --stream.lineno;
      } else {
        error("unknown directive: %s", directive);
        next = NULL;
      }
    }

    if (next != NULL) {
      process_line(next, enable, &stream);
    }
  }

  if (condstack->len > 0)
    error("#if not closed");

  macro_add(key_file, old_file_macro);
  macro_add(key_line, old_line_macro);

  set_pp_stream(old_stream);

  return stream.lineno;
}

void define_macro(const char *arg) {
  char *p = strchr(arg, '=');
  Macro *macro = p == NULL ? new_macro(NULL, false, NULL) : new_macro_single(p + 1);
  macro_add(alloc_name(arg, p, true), macro);
}

void define_macro_simple(const char *label) {
  macro_add(alloc_name(label, NULL, true), new_macro(NULL, false, NULL));
}

void add_system_inc_path(const char *path) {
  vec_push(sys_inc_paths, strdup_(path));
}
