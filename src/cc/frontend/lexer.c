#include "../../config.h"
#include "lexer.h"

#include <assert.h>
#include <ctype.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdlib.h>  // malloc, strtoul
#include <string.h>
#include <sys/types.h>  // ssize_t

#include "table.h"
#include "util.h"

Token *alloc_token(enum TokenKind kind, Line *line, const char *begin, const char *end) {
  if (end == NULL) {
    assert(begin != NULL);
    end = begin + strlen(begin);
  }
  Token *token = malloc_or_die(sizeof(*token));
  token->kind = kind;
  token->line = line;
  token->begin = begin;
  token->end = end;
  return token;
}

static bool for_preprocess;

static const struct {
  const char *str;
  enum TokenKind kind;
} kReservedWords[] = {
  {"if", TK_IF},
  {"else", TK_ELSE},
  {"switch", TK_SWITCH},
  {"case", TK_CASE},
  {"default", TK_DEFAULT},
  {"do", TK_DO},
  {"while", TK_WHILE},
  {"for", TK_FOR},
  {"break", TK_BREAK},
  {"continue", TK_CONTINUE},
  {"goto", TK_GOTO},
  {"return", TK_RETURN},
  {"void", TK_VOID},
  {"char", TK_CHAR},
  {"short", TK_SHORT},
  {"int", TK_INT},
  {"long", TK_LONG},
  {"const", TK_CONST},
  {"unsigned", TK_UNSIGNED},
  {"signed", TK_SIGNED},
  {"static", TK_STATIC},
  {"inline", TK_INLINE},
  {"extern", TK_EXTERN},
  {"volatile", TK_VOLATILE},
  {"restrict", TK_RESTRICT},
  {"auto", TK_AUTO},
  {"register", TK_REGISTER},
  {"struct", TK_STRUCT},
  {"union", TK_UNION},
  {"enum", TK_ENUM},
  {"_Bool", TK_BOOL},
  {"sizeof", TK_SIZEOF},
  {"_Alignof", TK_ALIGNOF},
  {"typedef", TK_TYPEDEF},
  {"__asm", TK_ASM},
#ifndef __NO_FLONUM
  {"float", TK_FLOAT},
  {"double", TK_DOUBLE},
#endif
  {"__FUNCTION__", TK_FUNCNAME},
  {"__func__", TK_FUNCNAME},
  {"__attribute__", TK_ATTRIBUTE},
};

static const struct {
  const char ident[4];
  enum TokenKind kind;
} kMultiOperators[] = {
  {"<<=", TK_LSHIFT_ASSIGN},
  {">>=", TK_RSHIFT_ASSIGN},
  {"...", TK_ELLIPSIS},
  {"==", TK_EQ},
  {"!=", TK_NE},
  {"<=", TK_LE},
  {">=", TK_GE},
  {"+=", TK_ADD_ASSIGN},
  {"-=", TK_SUB_ASSIGN},
  {"*=", TK_MUL_ASSIGN},
  {"/=", TK_DIV_ASSIGN},
  {"%=", TK_MOD_ASSIGN},
  {"&=", TK_AND_ASSIGN},
  {"|=", TK_OR_ASSIGN},
  {"^=", TK_HAT_ASSIGN},
  {"++", TK_INC},
  {"--", TK_DEC},
  {"->", TK_ARROW},
  {"&&", TK_LOGAND},
  {"||", TK_LOGIOR},
  {"<<", TK_LSHIFT},
  {">>", TK_RSHIFT},
};

static const char kOperatorMap[] = {  // enum TokenKind
  ['+'] = TK_ADD,
  ['-'] = TK_SUB,
  ['*'] = TK_MUL,
  ['/'] = TK_DIV,
  ['%'] = TK_MOD,
  ['&'] = TK_AND,
  ['|'] = TK_OR,
  ['^'] = TK_HAT,
  ['<'] = TK_LT,
  ['>'] = TK_GT,
  ['!'] = TK_NOT,
  ['='] = TK_ASSIGN,
  ['.'] = TK_DOT,
};

static const char kPunctMap[] = {  // enum TokenKind
  ['('] = TK_LPAR,
  [')'] = TK_RPAR,
  ['{'] = TK_LBRACE,
  ['}'] = TK_RBRACE,
  ['['] = TK_LBRACKET,
  [']'] = TK_RBRACKET,
  [':'] = TK_COLON,
  [';'] = TK_SEMICOL,
  [','] = TK_COMMA,
  ['?'] = TK_QUESTION,
  ['~'] = TK_TILDA,
};

Lexer lexer;
static Table reserved_word_table;
static LexEofCallback lex_eof_callback;

void lex_error(const char *p, const char *fmt, ...) {
  fprintf(stderr, "%s(%d): ", lexer.filename, lexer.lineno);

  va_list ap;
  va_start(ap, fmt);
  vfprintf(stderr, fmt, ap);
  va_end(ap);
  fprintf(stderr, "\n");

  if (lexer.line != NULL)
    show_error_line(lexer.line->buf, p, 1);

  exit(1);
}

static Token *alloc_ident(const Name *name, Line *line, const char *begin, const char *end) {
  Token *tok = alloc_token(TK_IDENT, line, begin, end);
  tok->ident = name;
  return tok;
}

Token *alloc_dummy_ident(void) {
  const Name *label = alloc_label();
  return alloc_ident(label, NULL, label->chars, label->chars + label->bytes);
}

static void init_reserved_word_table(void) {
  table_init(&reserved_word_table);

  if (!for_preprocess) {
    // Reserved words.
    for (int i = 0; i < (int)ARRAY_SIZE(kReservedWords); ++i) {
      const Name *key = alloc_name(kReservedWords[i].str, NULL, false);
      table_put(&reserved_word_table, key, INT2VOIDP(kReservedWords[i].kind));
    }
  }

  // Multi-char operators.
  for (int i = 0; i < (int)ARRAY_SIZE(kMultiOperators); ++i) {
    const Name *key = alloc_name(kMultiOperators[i].ident, NULL, false);
    table_put(&reserved_word_table, key, INT2VOIDP(kMultiOperators[i].kind));
  }
}

static enum TokenKind reserved_word(const Name *name) {
  void *kind = table_get(&reserved_word_table, name);
  return kind != NULL ? (enum TokenKind)VOIDP2INT(kind) : TK_EOF;
}

static int backslash(int c, const char **pp) {
  switch (c) {
  case '0':
    if (!isoctal((*pp)[1]))
      return '\0';
    // Fallthrough
  case '1': case '2': case '3': case '4': case '5': case '6': case '7':
    {
      const char *p = *pp + 1;
      int v = c - '0';
      for (int i = 0; i < 2; ++i, ++p) {
        char c2 = *p;
        if (!isoctal(c2))
          break;
        v = (v << 3) | (c2 - '0');
      }
      *pp = p - 1;
      return v;
    }
  case 'x':
    {
      const char *p = *pp + 1;
      c = 0;
      for (int i = 0; i < 2; ++i, ++p) {
        int v = xvalue(*p);
        if (v < 0)
          break;  // TODO: Error
        c = (c << 4) | v;
      }
      *pp = p - 1;
      return c;
    }
  case 'a':  return '\a';
  case 'b':  return '\b';
  case 'f':  return '\f';
  case 'n':  return '\n';
  case 'r':  return '\r';
  case 't':  return '\t';
  case 'v':  return '\v';

  default:
    lex_error(*pp, "Illegal escape");
    // Fallthrough
  case '\'': case '"': case '\\':
    return c;
  }
}

LexEofCallback set_lex_eof_callback(LexEofCallback callback) {
  LexEofCallback old = lex_eof_callback;
  lex_eof_callback = callback;
  return old;
}

bool lex_eof_continue(void) {
  return (lex_eof_callback != NULL && (*lex_eof_callback)());
}

static void init_lexer_with_flag(bool for_preprocess_) {
  for_preprocess = for_preprocess_;
  init_reserved_word_table();
}

void init_lexer(void) {
  init_lexer_with_flag(false);
}

void init_lexer_for_preprocessor(void) {
  init_lexer_with_flag(true);
}

void set_source_file(FILE *fp, const char *filename) {
  lexer.fp = fp;
  lexer.filename = filename;
  lexer.line = NULL;
  lexer.p = "";
  lexer.idx = -1;
  lexer.lineno = 0;
}

void set_source_string(const char *line, const char *filename, int lineno) {
  Line *p = malloc_or_die(sizeof(*p));
  p->filename = lexer.filename;
  p->buf = line;
  p->lineno = lineno;

  lexer.fp = NULL;
  lexer.filename = filename;
  lexer.line = p;
  lexer.p = line;
  lexer.idx = -1;
  lexer.lineno = lineno;
}

const char *get_lex_p(void) {
  if (lexer.idx < 0)
    return lexer.p;
  else
    return lexer.fetched[lexer.idx]->begin;
}

static int scan_linemarker(const char *line, long *pnum, char **pfn, int *pflag) {
  const char *p = line;
  if (p[0] != '#' || !isspace(p[1]))
    return 0;
  p = skip_whitespaces(p + 2);

  int n = 0;
  const char *next = p;
  unsigned long num = strtoul(next, (char**)&next, 10);
  if (next > p) {
    ++n;
    *pnum = num;
    if (isspace(*next) && (p = skip_whitespaces(next), *p == '"')) {
      p += 1;
      const char *q = strchr(p, '"');
      if (q != NULL) {
        ++n;
        *pfn = strndup(p, q - p);
        p = q + 1;

        if (isspace(*p)) {
          p = skip_whitespaces(p);
          next = p;
          int flag = strtol(next, (char**)&next, 10);
          if (next > p) {
            ++n;
            *pflag = flag;
          }
        }
      }
    }
  }
  return n;
}

static bool read_next_line(void) {
  if (lexer.fp == NULL || feof(lexer.fp))
    return lex_eof_continue();

  char *line = NULL;
  size_t capa = 0;
  for (;;) {
    ssize_t len = getline_cont(&line, &capa, lexer.fp, &lexer.lineno);
    if (len == -1) {
      if (lex_eof_continue())
        continue;

      return false;
    }

    if (line[0] != '#')
      break;

    // linemarkers: # linenum filename flags
    long num = -1;
    char *fn;
    int flag = -1;
    int n = scan_linemarker(line, &num, &fn, &flag);
    if (n >= 2) {
      lexer.lineno = num - 1;
      lexer.filename = fn;
    }
  }

  Line *p = malloc_or_die(sizeof(*p));
  p->filename = lexer.filename;
  p->buf = line;
  p->lineno = lexer.lineno;
  lexer.line = p;
  lexer.p = lexer.line->buf;
  return true;
}

static const char *skip_block_comment(const char *p) {
  for (;;) {
    p = block_comment_end(p);
    if (p != NULL)
      return p;

    if (!read_next_line())
      return NULL;
    p = lexer.p;
  }
}

static const char *skip_line_comment(void) {
  return read_next_line() ? lexer.p : NULL;
}

static const char *skip_whitespace_or_comment(const char *p) {
  for (;;) {
    p = skip_whitespaces(p);
    switch (*p) {
    case '\0':
      if (!read_next_line())
        return NULL;
      p = lexer.p;
      continue;
    case '/':
      switch (p[1]) {
      case '*':
        {
          const char *q = skip_block_comment(p + 2);
          if (q == NULL)
            lex_error(p, "Block comment not closed");
          p = q;
        }
        continue;
      case '/':
        p = skip_line_comment();
        if (p == NULL)
          return NULL;
        continue;
      default:  break;
      }
      break;
    default:  break;
    }
    break;
  }
  return p;
}

#ifndef __NO_FLONUM
static Token *read_flonum(const char **pp, int base) {
  const char *start = *pp;
  char *next;
  Flonum val = strtold(start, &next);
  enum TokenKind tk = TK_DOUBLELIT;
  switch (tolower(*next)) {
  case 'f':
    tk = TK_FLOATLIT;
    ++next;
    break;
  case 'l':
    tk = TK_LDOUBLELIT;
    ++next;
    break;
  default: break;
  }
  Token *tok = alloc_token(tk, lexer.line, start, next);
  tok->flonum = val;
  *pp = next;

  if (base == 16) {
    // Check exponent part exists.
    const char *q;
    for (q = start; q < next; ++q) {
      if (tolower(*q) == 'p')
        break;
    }
    if (q >= next) {
      lex_error(start, "Hex float literal must have exponent part");
    }
  }

  return tok;
}
#endif

static Token *read_num(const char **pp) {
  const char *start = *pp, *p = start;
  int base = 10;
  bool is_unsigned = false;
  if (*p == '0') {
    char c = tolower(p[1]);
    if (c == 'x') {
      base = 16;
      is_unsigned = true;
      p += 2;
#ifndef __NO_FLONUM
      if (*p == '.')  // Hex float literal.
        return read_flonum(pp, 16);
#endif
    } else if (c == 'b') {
      base = 2;
      is_unsigned = true;
      p += 2;
    } else if (isdigit(c)) {
      base = 8;
      is_unsigned = true;
    }
  }
  const char *q = p;
  unsigned long long val = strtoull(p, (char**)&p, base);
  if (p == q)
    lex_error(p, "Illegal literal");

#ifndef __NO_FLONUM
  if (base == 16 && (*p == '.' || tolower(*p) == 'p'))
    return read_flonum(pp, 16);
  if (*p == '.' || tolower(*p) == 'e') {
    if (base != 10)
      lex_error(p, "Illegal literal");
    return read_flonum(pp, 10);
  }
#endif
  enum TokenKind tt = TK_INTLIT;
  int unsigned_count = 0, long_count = 0;
  for (;; ++p) {
    int c = tolower(*p);
    if (c == 'u') {
      if (unsigned_count > 0)
        lex_error(p, "Illegal unsigned literal");
      is_unsigned = true;
      ++unsigned_count;
      continue;
    } else if (c == 'l') {
      switch (long_count) {
      case 0:  tt = TK_LONGLIT; break;
      case 1:  tt = TK_LLONGLIT; break;
      default:
        lex_error(p, "Illegal long literal");
        break;
      }
      ++long_count;
      continue;
    }
    break;
  }
  if (tt == TK_INTLIT) {
    const int INT_BYTES = 4;  // TODO: Detect.
    int bits = INT_BYTES * TARGET_CHAR_BIT;
    unsigned long long threshold = 1ULL << (bits - (is_unsigned ? 0 : 1));
    if (val >= threshold)
      tt = TK_LONGLIT;
  }
  Token *tok = alloc_token(tt + (is_unsigned ? (TK_UINTLIT - TK_INTLIT) : 0), lexer.line, start, p);
  tok->fixnum = val;
  *pp = p;
  return tok;
}

const char *read_ident(const char *p_) {
  const unsigned char *p = (const unsigned char *)p_;
  unsigned char uc = *p;
  int ucc = isutf8first(uc) - 1;
  if (!(ucc > 0 || isalpha(uc) || uc == '_'))
    return NULL;

  for (;;) {
    uc = *++p;
    if (ucc > 0) {
      if (!isutf8follow(uc)) {
        lex_error(p_, "Illegal byte sequence");
      }
      --ucc;
      continue;
    }
    if ((ucc = isutf8first(uc) - 1) > 0)
      continue;
    if (!isalnum_(uc))
      break;
  }
  return (const char*)p;
}

static const char *read_utf8_char(const char *p, int *result) {
  int c = *(unsigned char*)p;
  if (c == '\\') {
    c = *(unsigned char*)(++p);
    if (c == '\0')
      --p;
    else
      c = backslash(c, &p);
  }
#ifndef __NO_WCHAR
  else {
    int ucc = isutf8first(c);
    if (ucc > 0) {
      c &= ((1 << (8 - ucc)) - 1);
      for (int i = 1; i < ucc; ++i) {
        int c2 = *(unsigned char*)(++p);
        if (!isutf8follow(c2)) {
          lex_error(p, "Illegal byte sequence");
        }
        c = (c << 6) | (c2 & 0x3f);
      }
    }
  }
#endif
  *result = c;
  return p + 1;
}

static Token *read_char(const char **pp) {
  const char *p = *pp;
  const char *begin = p++;
#ifndef __NO_WCHAR
  bool is_wide = false;
  if (*begin == 'L') {
    is_wide = true;
    assert(*p == '\'');
    ++p;
  }
#endif

  if (*p == '\'')
    lex_error(p, "Empty character");

  int c;
  p = read_utf8_char(p, &c);
  if (*p != '\'')
    lex_error(p, "Character not closed");

  ++p;
  enum TokenKind kind = TK_CHARLIT;
#ifndef __NO_WCHAR
  if (is_wide)
    kind = TK_WCHARLIT;
#endif
  Token *tok = alloc_token(kind, lexer.line, begin, p);
  tok->fixnum = c;
  *pp = p;
  return tok;
}

#ifndef __NO_WCHAR
static void *convert_str_to_wstr(const char *src, size_t *plen) {
  // Count characters.
  size_t len = 0;  // Include '\0'.
  for (const char *p = src;; ) {
    int c;
    p = read_utf8_char(p, &c);
    ++len;
    if (c == '\0')
      break;
  }

  int *wstr = malloc_or_die(len * sizeof(*wstr));
  int *q = wstr;
  for (const char *p = src;; ) {
    int c;
    p = read_utf8_char(p, &c);
    *q++ = c;
    if (c == '\0')
      break;
  }
  *plen = len;
  return wstr;
}
#endif

static Token *read_string(const char **pp) {
  const int ADD = 16;
  const char *p = *pp;
  const char *begin, *end;
  size_t capa = 16, len = 0;
  char *str = malloc_or_die(capa * sizeof(*str));
#ifndef __NO_WCHAR
  bool is_wide = false;
#endif
  for (;;) {
    begin = p++;  // Skip first '"'
#ifndef __NO_WCHAR
  if (*begin == 'L') {
    is_wide = true;
    if (*p != '"') {
      lex_error(p - 1, "`\"' expected");
    }
    ++p;
  }
#endif
    for (int c; (c = *(unsigned char*)p++) != '"'; ) {
      if (c == '\0')
        lex_error(p - 1, "String not closed");
      if (len + 1 >= capa) {
        capa += ADD;
        str = realloc_or_die(str, capa * sizeof(*str));
      }

      if (c == '\\') {
        c = *(unsigned char*)p;
        if (c == '\0')
          lex_error(p, "String not closed");
        c = backslash(c, &p);
        ++p;
      }
      assert(len < capa);
      str[len++] = c;
    }
    end = p;
    if (for_preprocess)
      break;

    // Continue string literal when next character is '"'
    const char *q = skip_whitespace_or_comment(p);
    if (q == NULL || (p = q, *q != '"'
#ifndef __NO_WCHAR
        && !(*q == 'L' && q[1] == '"')
#endif
    ))
      break;
  }
  assert(len < capa);
  str[len++] = '\0';

  enum StrKind kind = STR_CHAR;
#ifndef __NO_WCHAR
  if (is_wide) {
    str = convert_str_to_wstr(str, &len);
    kind = STR_WIDE;
  }
#endif
  Token *tok = alloc_token(TK_STR, lexer.line, begin, end);
  tok->str.buf = str;
  tok->str.len = len;
  tok->str.kind = kind;
  *pp = p;
  return tok;
}

static Token *get_op_token(const char **pp) {
  const char *p = *pp;
  unsigned char c = *(unsigned char*)p;

  if (for_preprocess && c == '#') {
    enum TokenKind kind = PPTK_STRINGIFY;
    const char *q = p + 1;
    if (*q == '#') {
      ++q;
      kind = PPTK_CONCAT;
    }
    *pp = q;
    return alloc_token(kind, lexer.line, p, q);
  }

  if (c < sizeof(kPunctMap)) {
    enum TokenKind kind = kPunctMap[c];
    if (kind != 0) {
      const char *q = p + 1;
      *pp = q;
      return alloc_token(kind, lexer.line, p, q);
    }
  }

  if (c < sizeof(kOperatorMap)) {
    enum TokenKind single = kOperatorMap[c];
    if (single != 0) {
      int n;
      for (n = 1; n < 3; ++n) {
        unsigned char c = *(unsigned char*)(p + n);
        if (c >= sizeof(kOperatorMap) || kOperatorMap[c] == 0)
          break;
      }

      for (int len = n; len > 1; --len) {
        const Name *op = alloc_name(p, p + len, false);
        enum TokenKind kind = reserved_word(op);
        if (kind != TK_EOF) {
          const char *q = p + len;
          *pp = q;
          return alloc_token(kind, lexer.line, p, q);
        }
      }

      const char *q = p + 1;
      *pp = q;
      return alloc_token(single, lexer.line, p, q);
    }
  }
  return NULL;
}

static Token *get_token(void) {
  static Line kEofLine = {.buf = ""};
  static Token kEofToken = {.kind = TK_EOF, .line = &kEofLine};

  const char *p = lexer.p;
  if (p == NULL || (p = skip_whitespace_or_comment(p)) == NULL) {
    if ((p = lexer.p) != NULL && *p != '\0')
      lexer.p += strlen(p);  // Point to nul-chr.
    kEofLine.filename = lexer.filename;
    kEofLine.lineno = lexer.lineno;
    return &kEofToken;
  }

  Token *tok = NULL;
#ifndef __NO_WCHAR
  if (*p == 'L' && p[1] == '\'') {
    tok = read_char(&p);
  } else if (*p == 'L' && p[1] == '"') {
    tok = read_string(&p);
  } else
#endif
  if (*p == '\'') {
    tok = read_char(&p);
  } else if (*p == '"') {
    tok = read_string(&p);
  } else if (isdigit(*p)) {
    tok = read_num(&p);
#ifndef __NO_FLONUM
  } else if (*p == '.' && isdigit(p[1])) {
    tok = read_flonum(&p, 10);
#endif
  } else if ((tok = get_op_token(&p)) != NULL) {
    // Ok.
  } else {
    const char *begin = p;
    const char *ident_end = read_ident(p);
    if (ident_end != NULL) {
      const Name *name = alloc_name(begin, ident_end, false);
      enum TokenKind kind = reserved_word(name);
      tok = kind != TK_EOF ? alloc_token(kind, lexer.line, begin, ident_end)
                          : alloc_ident(name, lexer.line, begin, ident_end);
      p = ident_end;
    } else {
      if (!for_preprocess) {
        lex_error(p, "Unexpected character `%c'(%d)", *p, *p);
      }

      assert(*p != '\0');
      const char *q = p + 1;
      if (isutf8first(*p)) {
        for (; isutf8follow(*q); ++q)
          ;
      }
      tok = alloc_token(PPTK_OTHERCHAR, lexer.line, p, q);
      p = q;
    }
  }

  assert(tok != NULL);
  lexer.p = p;
  return tok;
}

Token *fetch_token(void) {
  if (lexer.idx < 0) {
    Token *tok = get_token();
    lexer.idx = lexer.idx < 0 ? 0 : lexer.idx + 1;
    lexer.fetched[lexer.idx] = tok;
  }
  return lexer.fetched[lexer.idx];
}

Token *match(enum TokenKind kind) {
  Token *tok = fetch_token();
  if (tok->kind != kind && (int)kind != -1)
    return NULL;
  if (tok->kind != TK_EOF)
    --lexer.idx;
  return tok;
}

void unget_token(Token *token) {
  if (token->kind == TK_EOF)
    return;
  ++lexer.idx;
  assert(lexer.idx < MAX_LEX_LOOKAHEAD);
  lexer.fetched[lexer.idx] = token;
}
