#include "../config.h"
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
  return JOIN_PATHS(cwd, dir, path);
}

static char *fullpath(const char *filename) {
  return cat_path_cwd(dirname(strdup(filename)), basename((char*)filename));
}

static const char *keyword(const char *s, const char *word) {
  size_t len = strlen(word);
  if (strncmp(s, word, len) != 0 || isalnum_(s[len]))
    return NULL;
  return skip_whitespaces(s + len);
}

static const char *find_directive(const char *line) {
  bool hash = false;
  const char *p = line;
  for (;;) {
    p = skip_whitespaces(p);
    if (*p == '#' && !hash) {
      hash = true;
      ++p;
      continue;
    }
    if (p[0] != '/')
      break;

    if (p[1] == '/') {  // Line comment
      if (hash)
        p += strlen(p);
      break;
    }
    const char *comstart = block_comment_start(p);
    if (comstart == NULL)
      break;
    const char *q = block_comment_end(comstart + 2);
    if (q == NULL) {
      if (hash)
        error("block comment not closed: %s", comstart);
      break;
    }
    p = q;
  }
  return hash ? p : NULL;
}

#define INC_ORDERS  (INC_AFTER + 1)

static FILE *pp_ofp;
static Vector sys_inc_paths[INC_ORDERS];  // <const char*>
static Vector pragma_once_files;  // <const char*>

static bool registered_pragma_once(const char *filename) {
  for (int i = 0, len = pragma_once_files.len; i < len; ++i) {
    const char *fn = pragma_once_files.data[i];
    if (strcmp(fn, filename) == 0)
      return true;
  }
  return false;
}

static void register_pragma_once(const char *filename) {
  if (!is_fullpath(filename))
    filename = fullpath(filename);
  vec_push(&pragma_once_files, filename);
}

static FILE *search_sysinc_next(const char *dir, const char *path, char **pfn) {
  int ord = 0, idx = 0;
  Vector *v;

  bool found = false;

  for (ord = 0; dir && ord < INC_ORDERS; ++ord) {
    v = &sys_inc_paths[ord];
    for (idx = 0; idx < v->len; ++idx) {
      if (!strcmp(fullpath(v->data[idx]), dir)) {
        ++idx;
        found = true;
        break;
      }
    }
    if (found) break;
  }

  for (; ord < INC_ORDERS; ++ord) {
    v = &sys_inc_paths[ord];
    for (; idx < v->len; ++idx) {
      FILE *fp = NULL;
      char *fn = cat_path_cwd(v->data[idx], path);
      if (registered_pragma_once(fn) ||
          (is_file(fn) && (fp = fopen(fn, "r")) != NULL)) {
        *pfn = fn;
        return fp;
      }
    }
    idx = 0;
  }
  *pfn = NULL;
  return NULL;
}

static FILE *search_sysinc(const char *path, char **pfn) {
  return search_sysinc_next(NULL, path, pfn);
}

static void handle_include(const char *p, Stream *stream, bool next) {
  const char *orgp = p;
  char close;
  bool sys = false;

  for (;;) {
    p = skip_whitespaces(p);
    char c = *p;
    if (c == '"' || c == '<' || c == '\0' || !isalnum_(c))
      break;

    set_source_string(p, stream->filename, stream->lineno);
    Token *ident = match(TK_IDENT);
    assert(ident != NULL);
    if (can_expand_ident(ident->ident)) {
      Vector *tokens = new_vector();
      vec_push(tokens, ident);
      macro_expand(tokens);
      StringBuffer sb;
      sb_init(&sb);
      for (int i = 0; i < tokens->len; ++i) {
        const Token *tok = tokens->data[i];
        sb_append(&sb, tok->begin, tok->end);
      }

      char *expanded = sb_to_string(&sb);
      set_source_string(expanded, NULL, -1);
      p = expanded;

      // TODO: Recursive back.
    }
  }

  switch (*p++) {
  case '"':
    close = '"';
    break;
  case '<':
    close = '>';
    sys = true;
    break;
  default:
    error("illegal include: %s", orgp);
    return;
  }

  const char *q;
  for (q = p; *q != close; ++q) {
    if (*q == '\0')
      error("not closed");
  }

  // Ensure line end after include.
  {
    const char *after = q + 1;
    set_source_string(after, stream->filename, stream->lineno);
    bool err = false;
    for (;;) {
      Token *tok = pp_match(-1);
      if (tok->kind == TK_EOF)
        break;
      if (!err) {
        error("Illegal token after include: %s", tok->begin);
        err = true;
      }
    }
  }

  char *path = strndup(p, q - p);
  char *fn = NULL;
  FILE *fp = NULL;
  char *dir = strdup(dirname(strdup(stream->filename)));
  // Search from current directory.
  if (!next && !sys) {
    fn = cat_path_cwd(dir, path);
    if (registered_pragma_once(fn))
      return;
    if (is_file(fn))
      fp = fopen(fn, "r");
  }
  if (fp == NULL) {
    if (next) fp = search_sysinc_next(dir, path, &fn);
    else fp = search_sysinc(path, &fn);

    if (fp == NULL) {
      if (fn == NULL)  // Except pragma once.
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
        filename = strndup(p, q - p);
        p = q + 1;
      }
      next = p;
    }
  }
  *pp = next;
  return filename;
}

static Vector *parse_macro_body(const char *p, Stream *stream) {
  p = skip_whitespaces(p);
  if (*p == '\0')
    return NULL;

  Vector *tokens = new_vector();
  set_source_string(p, stream == NULL ? "?" : stream->filename, stream == NULL ? 0 : stream->lineno);
  Token *tok_space = NULL;
  bool need_space = false;
  for (;;) {
    const char *start = get_lex_p();
    const char *q = block_comment_start(start);
    if (q != NULL) {
      const char *comment_start = q;
      q += 2;
      for (;;) {
        q = block_comment_end(q);
        if (q != NULL)
          break;

        ssize_t len = -1;
        char *line = NULL;
        if (stream != NULL) {
          size_t capa = 0;
          len = getline_cont(&line, &capa, stream->fp, &stream->lineno);
        }
        if (len == -1) {
          lex_error(comment_start, "Block comment not closed");
        }
        q = line;
      }
      set_source_string(q, stream == NULL ? "?" : stream->filename, stream == NULL ? 0 : stream->lineno);
      // need_space = true;  // GNUC insert whitespace, but old compilers not and it is used to concatenate identifiers
      continue;
    }

    Token *tok = match(-1);
    if (tok->kind == TK_EOF)
      break;
    if (need_space || tok->begin != start) {
      if (tok->kind != PPTK_CONCAT) {
        if (tok_space == NULL)
          tok_space = alloc_token(PPTK_SPACE, NULL, " ", NULL);
        vec_push(tokens, tok_space);
      }
      need_space = false;
    }
    vec_push(tokens, tok);

    if (tok->kind == PPTK_CONCAT || tok->kind == PPTK_STRINGIFY) {
      // Ignore after space.
      const char *s = skip_whitespaces(get_lex_p());
      set_source_string(s, stream == NULL ? "?" : stream->filename, stream == NULL ? 0 : stream->lineno);
    }
  }
  return tokens;
}

static void handle_define(const char *p, Stream *stream) {
  const char *begin = p;
  const char *end = read_ident(p);
  if (end == NULL)
    error("ident expected");
  const Name *name = alloc_name(begin, end, false);
  p = end;

  Vector *params = NULL;
  const Name *vaargs_ident = NULL;
  if (*p == '(') {
    // Macro with parameter.
    params = new_vector();
    set_source_string(p + 1, stream->filename, stream->lineno);
    if (!match(TK_RPAR)) {
      for (;;) {
        if (match(TK_ELLIPSIS)) {
          vaargs_ident = alloc_name("__VA_ARGS__", NULL, false);
          break;
        }

        Token *tok = pp_consume(TK_IDENT, "ident expected");
        if (match(TK_ELLIPSIS)) {
          vaargs_ident = tok->ident;
          break;
        }

        vec_push(params, tok->ident);
        if (match(TK_RPAR))
          break;
        pp_consume(TK_COMMA, "`,' or `)' expected");
      }
      if (vaargs_ident != NULL)
        pp_consume(TK_RPAR, "`)' expected");
    }
    p = get_lex_p();
  }

  Vector *tokens = parse_macro_body(p, stream);
  macro_add(name, new_macro(params, vaargs_ident, tokens));
}

static void handle_undef(const char **pp) {
  const char *p = *pp;
  const char *begin = p;
  const char *end = read_ident(p);
  if (end == NULL)
    error("ident expected");
  const Name *name = alloc_name(begin, end, false);

  macro_delete(name);

  *pp = end;
}

static bool handle_block_comment(const char *begin, const char **pp, Stream *stream) {
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
      fwrite(begin, q - begin, 1, pp_ofp);
      *pp = q;
      break;
    }

    fprintf(pp_ofp, "%s\n", begin);

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

static const char *find_double_quote_end(const char *p) {
  const char *start = p;
  for (;;) {
    switch (*p++) {
    case '\0':
      lex_error(start, "Quote not closed");
      return p - 1;
    case '"':
      return p;
    case '\\':
      ++p;
      break;
    default:
      break;
    }
  }
}

static const char *find_block_comment_end(const char *comment_start, Stream *stream) {
  const char *p = comment_start;
  for (;;) {
    const char *e = block_comment_end(p);
    if (e != NULL)
      return e;

    char *line = NULL;
    size_t capa = 0;
    ssize_t len = getline_cont(&line, &capa, stream->fp, &stream->lineno);
    if (len == -1) {
      lex_error(comment_start, "Block comment not closed");
      return strchr(p, '\0');
    }
    p = line;
    fputc('\n', pp_ofp);
  }
}

static void process_disabled_line(const char *p, Stream *stream) {
  for (;;) {
    switch (*p++) {
    case '\0':
      return;
    case '"':
      p = find_double_quote_end(p);
      break;
    case '\'':
      for (bool closed = false; !closed;) {
        switch (*p++) {
        case '\0':
          return;
        case '\\':
          if (*p != '\0')
            ++p;
          break;
        case '\'':
          closed = true;
          break;
        }
      }
      break;
    case '/':
      switch (*p) {
      case '*':
        p = find_block_comment_end(p + 1, stream);
        break;
      case '/':
        return;
      default: break;
      }
      break;
    default:
      break;
    }
  }
}

static void process_line(const char *line, bool enable, Stream *stream) {
  if (!enable) {
    process_disabled_line(line, stream);
    return;
  }

  set_source_string(line, stream->filename, stream->lineno);

  const char *begin = get_lex_p();

  for (;;) {
    const char *p = get_lex_p();
    if (p != NULL) {
      if (handle_block_comment(begin, &p, stream)) {
        begin = p;
        set_source_string(begin, stream->filename, stream->lineno);
        continue;
      }
    }

    if (match(TK_EOF))
      break;

    if (enable) {
      Token *ident = match(TK_IDENT);
      if (ident != NULL) {
        if (can_expand_ident(ident->ident)) {
          const char *p = begin;
          begin = ident->end;  // Update for EOF callback.

          Vector *tokens = new_vector();
          vec_push(tokens, ident);
          macro_expand(tokens);

          if (ident->begin != p)
            fwrite(p, ident->begin - p, 1, pp_ofp);

          // Everything should have been expanded, so output
          for (int i = 0; i < tokens->len; ++i) {
            const Token *tok = tokens->data[i];
            fwrite(tok->begin, tok->end - tok->begin, 1, pp_ofp);
          }
          begin = get_lex_p();
        }
        continue;
      }
    }

    match(-1);
  }

  if (enable && begin != NULL)
    fprintf(pp_ofp, "%s\n", begin);
  else
    fprintf(pp_ofp, "\n");
}

static bool handle_ifdef(const char **pp) {
  const char *p = *pp;
  const char *begin = p;
  const char *end = read_ident(p);
  *pp = end;
  if (end == NULL)
    error("ident expected");
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
  char *buf = malloc_or_die(len + 2 + 1);
  snprintf(buf, len + 2 + 1, "\"%s\"", filename);
  macro_add(key_file, new_macro(NULL, NULL, parse_macro_body(buf, NULL)));
}

void init_preprocessor(FILE *ofp) {
  pp_ofp = ofp;

  init_lexer_for_preprocessor();
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

  // __LINE__ : Dirty hack.
  Token *tok_lineno = alloc_token(TK_STR, NULL, linenobuf, linenobuf);
  Vector *lineno_tokens = new_vector();
  vec_push(lineno_tokens, tok_lineno);
  macro_add(key_line, new_macro(NULL, NULL, lineno_tokens));

  stream.lineno = 0;
  for (;;) {
    char *line = NULL;
    size_t capa = 0;
    ssize_t len = getline_cont(&line, &capa, fp, &stream.lineno);
    if (len == -1)
      break;

    tok_lineno->end = tok_lineno->begin + snprintf(linenobuf, sizeof(linenobuf), "%d", stream.lineno);

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
        handle_include(next, &stream, false);
        fprintf(pp_ofp, "# %d \"%s\" 1\n", stream.lineno + 1, stream.filename);
        next = NULL;
      } else if ((next = keyword(directive, "include_next")) != NULL) {
        handle_include(next, &stream, true);
        fprintf(pp_ofp, "# %d \"%s\" 1\n", stream.lineno + 1, stream.filename);
        next = NULL;
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
        if (*directive != '\0')
          error("unknown directive: [%s]", directive);
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
  Macro *macro = new_macro(NULL, NULL, parse_macro_body(p != NULL ? p + 1 : "1", NULL));
  macro_add(alloc_name(arg, p, true), macro);
}

void add_inc_path(enum IncludeOrder order, const char *path) {
  assert(order < INC_ORDERS);
  vec_push(&sys_inc_paths[order], strdup(path));
}
