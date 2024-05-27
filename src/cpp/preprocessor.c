#include "../config.h"
#include "preprocessor.h"

#include <assert.h>
#include <ctype.h>
#include <libgen.h>  // dirname
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "lexer.h"
#include "macro.h"
#include "pp_parser.h"
#include "table.h"
#include "util.h"

#define CF_ENABLE         (1 << 0)
#define CF_SATISFY_SHIFT  (1)
#define CF_SATISFY_MASK   (3 << CF_SATISFY_SHIFT)

static FILE *pp_ofp;

// Is `#if` condition satisfied?
enum Satisfy {
  NotSatisfied,
  Satisfied,
  ElseAppeared,
};

typedef struct PreprocessFile {
  Vector *condstack;
  Token *tok_lineno;
  Stream stream;
  bool enable;
  enum Satisfy satisfy;
  int out_lineno;
  char linenobuf[sizeof(int) * 3 + 1];  // Buffer for __LINE__
} PreprocessFile;

static PreprocessFile *curpf;

#define OUTPUT_PPLINE(...)  do { fprintf(pp_ofp, __VA_ARGS__); ++curpf->out_lineno; } while (0)

static char *cat_path_cwd(const char *dir, const char *path) {
  char *cwd = platform_getcwd(NULL, 0);
  // Enforce forward slashes, so we never generate backslashes in the C code
  return platform_pathslashes(JOIN_PATHS(cwd, dir, path));
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

    OUTPUT_PPLINE("%s\n", begin);

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

static void process_line(const char *line, Stream *stream) {
  set_source_string(line, stream->filename, stream->lineno);

  const char *begin = get_lex_p();

  const Name *defined = alloc_name("defined", NULL, false);
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

    Token *ident = match(TK_IDENT);
    if (ident != NULL) {
      if (equal_name(ident->ident, defined)) {
        // TODO: Raise error if not matched.
        match(TK_LPAR);
        match(TK_IDENT);
        match(TK_RPAR);
      } else if (can_expand_ident(ident->ident)) {
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

    match(-1);
  }

  if (begin != NULL)
    OUTPUT_PPLINE("%s\n", begin);
}

// Preprocess one line and receive result into memory.
static char *preprocess_one_line(const char *line, Stream *stream, size_t *psize) {
  char *expanded;
  size_t sizebuf;
  if (psize == NULL)
    psize = &sizebuf;
  FILE *memfp = open_memstream(&expanded, psize);
  if (memfp == NULL)
    error("open_memstream failed");
  FILE *bak_fp = pp_ofp;
  int bak_lineno = curpf->out_lineno;
  pp_ofp = memfp;

  process_line(line, stream);
  pp_ofp = bak_fp;
  curpf->out_lineno = bak_lineno;
  flush_memstream(memfp, &expanded, psize);
  fclose(memfp);
  return expanded;
}

#define INC_ORDERS  (INC_AFTER + 1)

static Vector sys_inc_paths[INC_ORDERS];  // <const char*>
static Vector pragma_once_files;  // <const char*>

static const Name *key_file;
static const Name *key_line;

static bool registered_pragma_once(const char *filename) {
  for (int i = 0, len = pragma_once_files.len; i < len; ++i) {
    const char *fn = pragma_once_files.data[i];
    if (platform_cmp_path(fn, filename))
      return true;
  }
  return false;
}

static void register_pragma_once(const char *filename) {
  if (!is_fullpath(filename))
    filename = fullpath(filename);
  vec_push(&pragma_once_files, filename);
}

// Search include file from system include paths.
//   result!=NULL: Found (returns found path into *pfn)
//   result==NULL, *pfn!=NULL: Found, but blocked because of pragma once.
//   result==NULL, *pfn==NULL: Not found.
static FILE *search_sysinc(const char *prevdir, const char *path, char **pfn) {
  for (int ord = 0; ord < INC_ORDERS; ++ord) {
    Vector *v = &sys_inc_paths[ord];
    for (int idx = 0; idx < v->len; ++idx) {
      if (prevdir != NULL) {  // Searching previous directory.
        if (platform_cmp_path(fullpath(v->data[idx]), prevdir))
          prevdir = NULL;
        continue;
      }

      FILE *fp = NULL;
      char *fn = cat_path_cwd(v->data[idx], path);
      if (registered_pragma_once(fn) ||  // If pragma once hit, then fp keeps NULL.
          (is_file(fn) && (fp = fopen(fn, "r")) != NULL)) {
        *pfn = fn;
        return fp;
      }
    }
  }
  *pfn = NULL;
  return NULL;
}

static void handle_include(const char *p, Stream *stream, bool is_next) {
  const char *orgp = p = skip_whitespaces(p);

  if (*p != '<')
    p = preprocess_one_line(p, stream, NULL);
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
      }
    }
  }

  char *path = strndup(p, q - p);
  char *fn = NULL;
  FILE *fp = NULL;
  char *dir = strdup(dirname(strdup(stream->filename)));
  // Search from current directory.
  if (!is_next && !sys) {
    fn = cat_path_cwd(dir, path);
    if (registered_pragma_once(fn))
      return;
    if (is_file(fn))
      fp = fopen(fn, "r");
  }
  if (fp == NULL) {
    fp = search_sysinc(is_next ? dir : NULL, path, &fn);
    if (fp == NULL) {
      if (fn == NULL)  // Raise error except pragma once.
        error("Cannot open file: %s", path);
      return;
    }
  }

  preprocess(fp, fn);
  fclose(fp);

  // Put linemarker to restore line and filename.
  fprintf(pp_ofp, "# %d \"%s\" 2\n", stream->lineno + 1, stream->filename);
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
  set_source_string(p, stream == NULL ? "?" : stream->filename,
                    stream == NULL ? 0 : stream->lineno);
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
      set_source_string(q, stream == NULL ? "?" : stream->filename,
                        stream == NULL ? 0 : stream->lineno);
      // GNUC insert whitespace, but old compilers not and it is used to concatenate identifiers.
      // need_space = true;
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
      set_source_string(s, stream == NULL ? "?" : stream->filename,
                        stream == NULL ? 0 : stream->lineno);
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

static const char *find_double_quote_end(const char *p) {
  const char *start = p;
  for (;;) {
    switch (*p++) {
    case '\0':
      lex_error(start, "Quote not closed");
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
    }
    p = line;
    OUTPUT_PPLINE("\n");
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
  size_t size;
  char *expanded = preprocess_one_line(*pp, stream, &size);

  // Parse expression.
  PpResult result;
  {
    FILE *memfp = fmemopen(expanded, size, "r");
    assert(memfp != NULL);

    Stream tmp_stream;
    tmp_stream.fp = memfp;
    tmp_stream.filename = stream->filename;
    tmp_stream.lineno = stream->lineno;
    Stream *bak_stream = set_pp_stream(&tmp_stream);
    set_source_file(memfp, stream->filename);
    result = pp_expr();
    set_pp_stream(bak_stream);
    fclose(memfp);
    *pp = get_lex_p();
  }
  return result != 0;
}

static intptr_t cond_value(bool enable, enum Satisfy satisfy) {
  return (enable ? CF_ENABLE : 0) | (satisfy << CF_SATISFY_SHIFT);
}

static void define_file_macro(const char *filename) {
  size_t len = strlen(filename);
  char *buf = malloc_or_die(len + 2 + 1);
  snprintf(buf, len + 2 + 1, "\"%s\"", filename);
  macro_add(key_file, new_macro(NULL, NULL, parse_macro_body(buf, NULL)));
}

void init_preprocessor(FILE *ofp) {
  pp_ofp = ofp;
  key_file = alloc_name("__FILE__", NULL, false);
  key_line = alloc_name("__LINE__", NULL, false);

  // Keep sys_inc_paths.

  vec_init(&pragma_once_files);

  macro_init();
  init_lexer_for_preprocessor();
}

static const char *process_directive(PreprocessFile *ppf, const char *line) {
  // Find '#'
  const char *directive = find_directive(line);
  if (directive == NULL)
    return line;

  if (isdigit(*directive)) {
    // Assume linemarkers: output as is.
    fprintf(pp_ofp, "%s\n", line);
    return NULL;
  }

  const char *next;
  if ((next = keyword(directive, "ifdef")) != NULL) {
    vec_push(ppf->condstack, (void*)cond_value(ppf->enable, ppf->satisfy));
    bool defined = handle_ifdef(&next);
    ppf->satisfy = defined ? Satisfied : NotSatisfied;
    ppf->enable = ppf->enable && ppf->satisfy == Satisfied;
  } else if ((next = keyword(directive, "ifndef")) != NULL) {
    vec_push(ppf->condstack, (void*)cond_value(ppf->enable, ppf->satisfy));
    bool defined = handle_ifdef(&next);
    ppf->satisfy = defined ? NotSatisfied : Satisfied;
    ppf->enable = ppf->enable && ppf->satisfy == Satisfied;
  } else if ((next = keyword(directive, "if")) != NULL) {
    vec_push(ppf->condstack, (void*)cond_value(ppf->enable, ppf->satisfy));
    bool cond = handle_if(&next, &ppf->stream);
    ppf->satisfy = cond ? Satisfied : NotSatisfied;
    ppf->enable = ppf->enable && ppf->satisfy == Satisfied;
  } else if ((next = keyword(directive, "else")) != NULL) {
    int last = ppf->condstack->len - 1;
    if (last < 0)
      error("`#else' used without `#if'");
    intptr_t flag = VOIDP2INT(ppf->condstack->data[last]);
    if (ppf->satisfy == ElseAppeared)
      error("Illegal #else");
    ppf->enable = !ppf->enable && ppf->satisfy == NotSatisfied && ((flag & CF_ENABLE) != 0);
    ppf->satisfy = ElseAppeared;
  } else if ((next = keyword(directive, "elif")) != NULL) {
    int last = ppf->condstack->len - 1;
    if (last < 0)
      error("`#elif' used without `#if'");
    intptr_t flag = VOIDP2INT(ppf->condstack->data[last]);
    if (ppf->satisfy == ElseAppeared)
      error("Illegal #elif");

    bool cond = false;
    bool cond2 = handle_if(&next, &ppf->stream);
    if (ppf->satisfy == NotSatisfied) {
      cond = cond2;
      if (cond)
        ppf->satisfy = Satisfied;
    }

    ppf->enable = !ppf->enable && cond && ((flag & CF_ENABLE) != 0);
  } else if ((next = keyword(directive, "endif")) != NULL) {
    if (ppf->condstack->len <= 0)
      error("`#endif' used without `#if'");
    int flag = VOIDP2INT(vec_pop(ppf->condstack));
    ppf->enable = (flag & CF_ENABLE) != 0;
    ppf->satisfy = (flag & CF_SATISFY_MASK) >> CF_SATISFY_SHIFT;
  } else if (ppf->enable) {
    if ((next = keyword(directive, "include")) != NULL) {
      handle_include(next, &ppf->stream, false);
      ++ppf->out_lineno;
      next = NULL;
    } else if ((next = keyword(directive, "include_next")) != NULL) {
      handle_include(next, &ppf->stream, true);
      next = NULL;
    } else if ((next = keyword(directive, "define")) != NULL) {
      handle_define(next, &ppf->stream);
      next = NULL;  // `#define' consumes the line all.
    } else if ((next = keyword(directive, "undef")) != NULL) {
      handle_undef(&next);
    } else if ((next = keyword(directive, "pragma")) != NULL) {
      handle_pragma(&next, ppf->stream.filename);
    } else if ((next = keyword(directive, "error")) != NULL) {
      fprintf(stderr, "%s(%d): error\n", ppf->stream.filename, ppf->stream.lineno);
      error("%s", line);
    } else if ((next = keyword(directive, "line")) != NULL) {
      ppf->stream.filename = handle_line_directive(&next, ppf->stream.filename,
                                                   &ppf->stream.lineno);
      int flag = 1;
      fprintf(pp_ofp, "# %d \"%s\" %d\n", ppf->stream.lineno, ppf->stream.filename, flag);
      define_file_macro(ppf->stream.filename);
      ppf->out_lineno = --ppf->stream.lineno - 1;
    } else {
      if (*directive != '\0')
        error("unknown directive: [%s]", directive);
      next = NULL;
    }
  }
  return next;
}

static void adjust_output_lineno(PreprocessFile *ppf) {
  assert(ppf->out_lineno <= ppf->stream.lineno);
  for (; ppf->out_lineno < ppf->stream.lineno; ++ppf->out_lineno)
    fprintf(pp_ofp, "\n");
}

const char *get_processed_next_line(void) {
  PreprocessFile *ppf = curpf;
  for (;;) {
    char *line = NULL;
    size_t capa = 0;
    ssize_t len = getline_cont(&line, &capa, ppf->stream.fp, &ppf->stream.lineno);
    if (len == -1)
      return NULL;

    int ln = snprintf(ppf->linenobuf, sizeof(ppf->linenobuf), "%d", ppf->stream.lineno);
    ppf->tok_lineno->end = ppf->tok_lineno->begin + ln;

    const char *next = process_directive(ppf, line);
    if (next != NULL) {
      if (ppf->enable)
        return next;
      process_disabled_line(next, &ppf->stream);
    }

    adjust_output_lineno(ppf);
  }
}

void preprocess(FILE *fp, const char *filename) {
  Macro *old_file_macro = macro_get(key_file);
  Macro *old_line_macro = macro_get(key_line);

  PreprocessFile pf;
  pf.condstack = new_vector();
  pf.stream = (Stream){.filename = filename, .fp = fp, .lineno = 0};
  pf.enable = true;
  pf.out_lineno = 0;
  pf.satisfy = NotSatisfied;

  Stream *old_stream = set_pp_stream(&pf.stream);
  PreprocessFile *oldpf = curpf;
  curpf = &pf;

  define_file_macro(pf.stream.filename);

  // __LINE__ : Dirty hack.
  pf.tok_lineno = alloc_token(TK_STR, NULL, pf.linenobuf, pf.linenobuf);
  Vector *lineno_tokens = new_vector();
  vec_push(lineno_tokens, pf.tok_lineno);
  macro_add(key_line, new_macro(NULL, NULL, lineno_tokens));

  fprintf(pp_ofp, "# 1 \"%s\" 1\n", filename);

  for (const char *line; (line = get_processed_next_line()) != NULL;) {
    process_line(line, &pf.stream);
    adjust_output_lineno(&pf);
  }

  if (pf.condstack->len > 0)
    error("#if not closed");

  curpf = oldpf;
  set_pp_stream(old_stream);

  macro_add(key_file, old_file_macro);
  macro_add(key_line, old_line_macro);
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
