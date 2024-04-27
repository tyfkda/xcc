#include "../config.h"
#include "parse_asm.h"

#include <assert.h>
#include <ctype.h>
#include <stdint.h>
#include <stdlib.h>  // strtoul
#include <string.h>
#include <strings.h>

#include "gen_section.h"
#include "ir_asm.h"
#include "table.h"
#include "util.h"

bool err;
int current_section = SEC_CODE;

static const char *kDirectiveTable[] = {
  "ascii",
  "section",
  "text",
  "data",
  "align",
  "p2align",
  "type",
  "byte",
  "short",
  "long",
  "quad",
  "comm",
  "globl",
  "local",
  "extern",
#ifndef __NO_FLONUM
  "float",
  "double",
#endif
};

static LabelInfo *new_label(int section, uintptr_t address) {
  LabelInfo *info = malloc_or_die(sizeof(*info));
  info->section = section;
  info->flag = 0;
  info->address = address;
  info->kind = LK_NONE;
  return info;
}

LabelInfo *add_label_table(Table *label_table, const Name *label, int section, bool define, bool global) {
  LabelInfo *info = table_get(label_table, label);
  if (info != NULL) {
    if (define) {
      if ((info->flag & LF_DEFINED) != 0) {
        fprintf(stderr, "`%.*s' already defined\n", NAMES(label));
        return NULL;
      }
      info->address = 1;
      info->section = section;
    }
  } else {
    info = new_label(section, 0);
    table_put(label_table, label, info);
  }
  if (define)
    info->flag |= LF_DEFINED;
  if (global)
    info->flag |= LF_GLOBAL;
  return info;
}

void parse_error(const ParseInfo *info, const char *message) {
  fprintf(stderr, "%s(%d): %s\n", info->filename, info->lineno, message);
  fprintf(stderr, "%s\n", info->rawline);
  err = true;
}

static enum DirectiveType find_directive(const char *p, size_t n) {
  const char **table = kDirectiveTable;
  for (size_t i = 0; i < ARRAY_SIZE(kDirectiveTable); ++i) {
    const char *name = table[i];
    if (strncasecmp(p, name, n) == 0 && name[n] == '\0') {
      return i + 1;
    }
  }
  return NODIRECTIVE;
}

bool immediate(const char **pp, int64_t *value) {
  const char *p = *pp;
  bool negative = false;
  if (*p == '-') {
    negative = true;
    ++p;
  }

  int base = 10;
  if (*p == '0') {
    char c = tolower(p[1]);
    if (c == 'x') {
      base = 16;
      p += 2;
      c = tolower(*p);
      if (!isxdigit(c))
        return false;
    } else if (isdigit(c)) {
      if (c >= '8')
        return false;
      base = 8;
    }
  }
  const char *q = p;
  unsigned long val = strtoul(p, (char**)&p, base);
  if (p == q)
    return false;

  *value = negative ? -val : val;
  *pp = p;
  return true;
}

bool is_label_first_chr(char c) {
  return isalpha(c) || c == '_' || c == '.';
}

bool is_label_chr(char c) {
  return is_label_first_chr(c) || isdigit(c);
}

static const char *get_label_end(ParseInfo *info) {
  const char *start = info->p;
  const char *p = start;
  if (*p == '"') {
    ++p;
    for (;;) {
      char c = *p;
      if (c == '\0') {
        parse_error(info, "String not closed");
        break;
      }

      ++p;
      if (c == '"')
        break;
      if (c == '\\')
        ++p;
    }
    start += 2;
  } else {
    const unsigned char *q = (const unsigned char*)p;
    int ucc = 0;
    for (;;) {
      int uc = *++q;
      if (ucc > 0) {
        if (!isutf8follow(uc)) {
          parse_error(info, "Illegal byte sequence");
          return NULL;
        }
        --ucc;
        continue;
      }
      if ((ucc = isutf8first(uc) - 1) > 0)
        continue;
      if (!is_label_chr(uc))
        break;
    }
    p = (const char*)q;
  }
  if (p <= start)
    parse_error(info, "Empty label");
  return p;
}

const Name *unquote_label(const char *p, const char *q) {
  if (*p != '"')
    return alloc_name(p, q, false);
  if (q[-1] != '"' || q == p + 2)
    return NULL;
  // TODO: Unescape
  return alloc_name(p + 1, q - 1, false);
}

static const Name *parse_label(ParseInfo *info) {
  const char *start = info->p;
  const char *p = get_label_end(info);
  if (p == start)
    return NULL;

  info->p = p;
  return unquote_label(start, p);
}

static const Name *parse_section_name(ParseInfo *info) {
  const char *p = info->p;
  const char *start = p;

  if (!is_label_first_chr(*p))
    return NULL;

  do {
    ++p;
  } while (isalnum_(*p) || *p == '.');
  info->p = p;
  return alloc_name(start, p, false);
}

enum TokenKind {
  TK_EOF,
  TK_UNKNOWN,
  TK_LABEL,
  TK_FIXNUM,
  TK_ADD,
  TK_SUB,
  TK_MUL,
  TK_DIV,
  TK_FLONUM,
};

typedef struct Token {
  enum TokenKind kind;
  union {
    const Name *label;
    int64_t fixnum;
#ifndef __NO_FLONUM
    Flonum flonum;
#endif
  };
} Token;

static Token *new_token(enum TokenKind kind) {
  Token *token = calloc_or_die(sizeof(*token));
  token->kind = kind;
  return token;
}

#ifndef __NO_FLONUM
static Token *read_flonum(ParseInfo *info, int base) {
  const char *start = info->p;
  char *next;
  Flonum val = strtold(start, &next);
  Token *tok = new_token(TK_FLONUM);
  tok->flonum = val;
  info->p = next;

  if (base == 16) {
    // Check exponent part exists.
    const char *q;
    for (q = start; q < next; ++q) {
      if (tolower(*q) == 'p')
        break;
    }
    if (q >= next) {
      parse_error(info, "Hex float literal must have exponent part");
    }
  }

  return tok;
}
#endif

static const Token *fetch_token(ParseInfo *info) {
  static const Token kTokEOF = {.kind = TK_EOF};
  const char *start = skip_whitespaces(info->p);
  const char *p = start;
  char c = *p;
  switch (c) {
  case '\0':
    return &kTokEOF;
  case '0': case '1': case '2': case '3': case '4': case '5': case '6': case '7': case '8': case '9':
    {
      int base = 10;
      if (c == '0' && tolower(p[1]) == 'x') {
#ifndef __NO_FLONUM
        if (c == '.')  // Hex float literal.
          return read_flonum(info, 16);
#endif
        if (isxdigit(p[2])) {
          p += 2;
          base = 16;
        }
      }
      char *q;
      unsigned long long v = strtoull(p, &q, base);
#ifndef __NO_FLONUM
      if (*q == '.' || tolower(*q) == 'e') {
        info->p = p;
        return read_flonum(info, 10);
      }
#endif
      Token *token = new_token(TK_FIXNUM);
      token->fixnum = v;
      info->p = q;
      return token;
    }
  case '"':
    {
      info->p = p;
      const Name *label = parse_label(info);
      if (label != NULL) {
        Token *token = new_token(TK_LABEL);
        token->label = label;
        return token;
      }
    }
    break;
  case '+': case '-': case '*': case '/':
    {
      enum TokenKind kind;
      switch (c) {
      default: assert(false); // To suppress warning.
      case '+':  kind = TK_ADD; break;
      case '-':  kind = TK_SUB; break;
      case '*':  kind = TK_MUL; break;
      case '/':  kind = TK_DIV; break;
      }
      info->p = p + 1;
      return new_token(kind);
    }
#ifndef __NO_FLONUM
  case '.':
    if (isdigit(p[1])) {
      info->p = p;
      return read_flonum(info, 10);
    }
    break;
#endif
  default: break;
  }

  if (is_label_first_chr(c)) {
    while (c = *++p, is_label_chr(c))
      ;
    Token *token = new_token(TK_LABEL);
    token->label = alloc_name(start, p, false);
    info->p = p;
    return token;
  }

  static const Token kTokUnknown = {.kind = TK_UNKNOWN};
  return &kTokUnknown;
}

static const Token *match(ParseInfo *info, enum TokenKind kind) {
  const Token *token = info->prefetched;
  if (token == NULL)
    token = fetch_token(info);

  if (token->kind != kind) {
    info->prefetched = token;
    return NULL;
  }
  info->prefetched = NULL;
  return token;
}

Expr *new_expr(enum ExprKind kind) {
  Expr *expr = malloc_or_die(sizeof(*expr));
  expr->kind = kind;
  return expr;
}

static Expr *prim(ParseInfo *info) {
  Expr *expr = NULL;
  const Token *tok;
  if ((tok = match(info, TK_LABEL)) != NULL) {
    expr = new_expr(EX_LABEL);
    expr->label = tok->label;
  } else if ((tok = match(info, TK_FIXNUM)) != NULL) {
    expr = new_expr(EX_FIXNUM);
    expr->fixnum = tok->fixnum;
#ifndef __NO_FLONUM
  } else if ((tok = match(info, TK_FLONUM)) != NULL) {
    expr = new_expr(EX_FLONUM);
    expr->flonum = tok->flonum;
#endif
  }
  return expr;
}

static Expr *unary(ParseInfo *info) {
  const Token *tok;
  if ((tok = match(info, TK_ADD)) != NULL) {
    Expr *expr = unary(info);
    if (expr == NULL)
      return NULL;
    switch (expr->kind) {
    case EX_FIXNUM:
    case EX_FLONUM:
      return expr;
    default:
      {
        Expr *op = new_expr(EX_POS);
        op->unary.sub = expr;
        return op;
      }
    }
  }

  if ((tok = match(info, TK_SUB)) != NULL) {
    Expr *expr = unary(info);
    if (expr == NULL)
      return NULL;
    switch (expr->kind) {
    case EX_FIXNUM:
      expr->fixnum = -expr->fixnum;
      return expr;
#ifndef __NO_FLONUM
    case EX_FLONUM:
      expr->flonum = -expr->flonum;
      return expr;
#endif
    default:
      {
        Expr *op = new_expr(EX_NEG);
        op->unary.sub = expr;
        return op;
      }
    }
  }

  return prim(info);
}

static Expr *parse_mul(ParseInfo *info) {
  Expr *expr = unary(info);
  if (expr == NULL)
    return expr;

  const Token *tok;
  while ((tok = match(info, TK_MUL)) != NULL ||
         (tok = match(info, TK_DIV)) != NULL) {
    Expr *rhs = unary(info);
    if (rhs == NULL) {
      parse_error(info, "expression error");
      break;
    }

    Expr *lhs = expr;
    if (lhs->kind == EX_FIXNUM && rhs->kind == EX_FIXNUM) {
      switch (tok->kind) {
      case TK_MUL:  lhs->fixnum *= rhs->fixnum; break;
      case TK_DIV:  lhs->fixnum += rhs->fixnum; break;
      default:  assert(false); break;
      }
    } else {
      expr = new_expr((enum ExprKind)(tok->kind + (EX_MUL - TK_MUL)));  // Assume ExprKind is same order with TokenKind.
      expr->bop.lhs = lhs;
      expr->bop.rhs = rhs;
    }
  }
  return expr;
}

static Expr *parse_add(ParseInfo *info) {
  Expr *expr = parse_mul(info);
  if (expr == NULL)
    return expr;

  const Token *tok;
  while ((tok = match(info, TK_ADD)) != NULL ||
         (tok = match(info, TK_SUB)) != NULL) {
    Expr *rhs = parse_mul(info);
    if (rhs == NULL) {
      parse_error(info, "expression error");
      break;
    }

    Expr *lhs = expr;
    if (lhs->kind == EX_FIXNUM && rhs->kind == EX_FIXNUM) {
      switch (tok->kind) {
      case TK_ADD:  lhs->fixnum += rhs->fixnum; break;
      case TK_SUB:  lhs->fixnum += rhs->fixnum; break;
      default:  assert(false); break;
      }
    } else {
      // Assume ExprKind is same order with TokenKind.
      expr = new_expr((enum ExprKind)(tok->kind + (EX_ADD - TK_ADD)));
      expr->bop.lhs = lhs;
      expr->bop.rhs = rhs;
    }
  }
  return expr;
}

Expr *parse_expr(ParseInfo *info) {
  info->prefetched = NULL;
  return parse_add(info);
}

static void check_line_end(ParseInfo *info) {
  const char *p = info->p;
  for (;;) {
    const char *q = block_comment_start(p);
    if (q == NULL)
      break;
    p = block_comment_end(q);
    if (p == NULL) {
      parse_error(info, "Block comment not closed");
      return;
    }
  }
  p = skip_whitespaces(p);
  if (*p != '\0' && !(*p == '/' && p[1] == '/')) {
    parse_error(info, "Syntax error");
  }
}

Line *parse_line(ParseInfo *info) {
  Line *line = calloc_or_die(sizeof(*line));
  line->label = NULL;
  line->inst.op = NOOP;
  line->dir = NODIRECTIVE;

  const char *p = skip_whitespaces(info->rawline);
  info->p = p;
  const char *q = get_label_end(info);
  const char *r = skip_whitespaces(q);
  if (*r == ':') {
    const Name *label = unquote_label(p, q);
    if (label == NULL) {
      parse_error(info, "Illegal label");
    } else {
      info->p = p;
      line->label = label;
      info->p = r + 1;
    }
  } else {
    if (*p == '.') {
      enum DirectiveType dir = find_directive(p + 1, q - p - 1);
      if (dir == NODIRECTIVE) {
        parse_error(info, "Unknown directive");
        return NULL;
      }
      line->dir = dir;
      info->p = r;
    } else if (*p != '\0') {
      info->p = p;
      parse_inst(info, line);
      check_line_end(info);
    }
  }
  return line;
}

static char unescape_char(ParseInfo *info) {
  const char *p = info->p;
  char c = *p++;
  switch (c) {
  case '0':  return '\0';
  case 'x':
    {
      c = 0;
      for (int i = 0; i < 2; ++i, ++p) {
        int v = xvalue(*p);
        if (v < 0)
          break;  // TODO: Error
        c = (c << 4) | v;
      }
      info->p = p - 1;
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
    parse_error(info, "Illegal escape");
    // Fallthrough
  case '\'': case '"': case '\\':
    return c;
  }
}

static size_t unescape_string(ParseInfo *info, char *dst) {
  size_t len = 0;
  for (; *info->p != '"'; ++info->p, ++len) {
    char c = *info->p;
    if (c == '\0')
      parse_error(info, "string not closed");
    if (c == '\\') {
      ++info->p;
      c = unescape_char(info);
    }
    if (dst != NULL)
      *dst++ = c;
  }
  ++info->p;
  return len;
}

void handle_directive(ParseInfo *info, enum DirectiveType dir, Vector **section_irs,
                      Table *label_table) {
  Vector *irs = section_irs[current_section];

  switch (dir) {
  case NODIRECTIVE:
    break;
  case DT_ASCII:
    {
      if (*info->p != '"')
        parse_error(info, "`\"' expected");
      ++info->p;
      const char *p = info->p;
      size_t len = unescape_string(info, NULL);
      char *str = malloc_or_die(len);
      info->p = p;  // Again.
      unescape_string(info, str);

      vec_push(irs, new_ir_data(str, len));
    }
    break;

  case DT_COMM:
    {
      const Name *label = parse_label(info);
      if (label == NULL)
        parse_error(info, ".comm: label expected");
      info->p = skip_whitespaces(info->p);
      if (*info->p != ',')
        parse_error(info, ".comm: `,' expected");
      info->p = skip_whitespaces(info->p + 1);
      int64_t count;
      if (!immediate(&info->p, &count)) {
        parse_error(info, ".comm: count expected");
        return;
      }

      int64_t align = 0;
      if (*info->p == ',') {
        info->p = skip_whitespaces(info->p + 1);
        if (!immediate(&info->p, &align) || align < 1) {
          parse_error(info, ".comm: optional alignment expected");
          return;
        }
      }

      enum SectionType sec = SEC_BSS;
      irs = section_irs[sec];
      if (align > 1)
        vec_push(irs, new_ir_align(align));
      vec_push(irs, new_ir_label(label));
      vec_push(irs, new_ir_bss(count));

      if (!add_label_table(label_table, label, sec, true, false))
        return;
    }
    break;

  case DT_TEXT:
    current_section = SEC_CODE;
    break;

  case DT_DATA:
    current_section = SEC_DATA;
    break;

  case DT_ALIGN:
    {
      int64_t align;
      if (!immediate(&info->p, &align))
        parse_error(info, ".align: number expected");
      vec_push(irs, new_ir_align(align));
    }
    break;
  case DT_P2ALIGN:
    {
      int64_t align;
      if (!immediate(&info->p, &align))
        parse_error(info, ".align: number expected");
      vec_push(irs, new_ir_align(1 << align));
    }
    break;

  case DT_TYPE:
    {
      const Name *label = parse_label(info);
      if (label == NULL) {
        parse_error(info, ".type: label expected");
        break;
      }
      if (*info->p != ',') {
        parse_error(info, ".type: `,' expected");
        break;
      }
      info->p = skip_whitespaces(info->p + 1);
      enum LabelKind kind = LK_NONE;
      if (strcmp(info->p, "@function") == 0) {
        kind = LK_FUNC;
      } else if (strcmp(info->p, "@object") == 0) {
        kind = LK_OBJECT;
      } else {
        parse_error(info, "illegal .type");
        break;
      }

      LabelInfo *info = add_label_table(label_table, label, current_section, false, false);
      if (info != NULL) {
        info->kind = kind;
      }
    }
    break;

  case DT_BYTE:
  case DT_SHORT:
  case DT_LONG:
  case DT_QUAD:
    {
      Expr *expr = parse_expr(info);
      if (expr == NULL) {
        parse_error(info, "expression expected");
        break;
      }

      assert(expr->kind != EX_FLONUM);
      if (expr->kind == EX_FIXNUM) {
        // TODO: Target endian.
        long value = expr->fixnum;
        int size = 1 << (dir - DT_BYTE);
        unsigned char *buf = malloc_or_die(size);
        for (int i = 0; i < size; ++i)
          buf[i] = value >> (8 * i);
        vec_push(irs, new_ir_data(buf, size));
      } else {
        vec_push(irs, new_ir_expr((enum IrKind)(IR_EXPR_BYTE + (dir - DT_BYTE)), expr));
      }
    }
    break;

  case DT_FLOAT:
  case DT_DOUBLE:
#ifndef __NO_FLONUM
    {
      Expr *expr = parse_expr(info);
      if (expr == NULL) {
        parse_error(info, "expression expected");
        break;
      }

      Flonum value;
      switch (expr->kind) {
      case EX_FIXNUM:  value = expr->fixnum; break;
      case EX_FLONUM:  value = expr->flonum; break;
      default:
        assert(false);
        value = -1;
        break;
      }
      int size;
      switch (dir) {
      default: assert(false); // Fallthrough
      case DT_DOUBLE:  size = sizeof(double); break;
      case DT_FLOAT:   size = sizeof(float); break;
      }
      unsigned char *buf = malloc_or_die(size);
      if (dir == DT_FLOAT) {
        float fval = value;
        memcpy(buf, (void*)&fval, sizeof(fval));  // TODO: Endian
      } else {
        double dval = value;
        memcpy(buf, (void*)&dval, sizeof(dval));  // TODO: Endian
      }
      vec_push(irs, new_ir_data(buf, size));
    }
#else
    assert(false);
#endif
    break;

  case DT_GLOBL:
  case DT_LOCAL:
    {
      const Name *label = parse_label(info);
      if (label == NULL) {
        char buf[32];
        snprintf(buf, sizeof(buf), "%s: label expected", dir == DT_GLOBL ? ".globl" : ".local");
        parse_error(info, buf);
        return;
      }

      if (!add_label_table(label_table, label, current_section, false, dir == DT_GLOBL))
        err = true;
    }
    break;

  case DT_SECTION:
    {
      const Name *name = parse_section_name(info);
      if (name == NULL) {
        parse_error(info, ".section: section name expected");
        return;
      }
      if (equal_name(name, alloc_name(".rodata", NULL, false))) {
        current_section = SEC_RODATA;
      } else {
        parse_error(info, "Unknown section name");
        return;
      }
    }
    break;

  case DT_EXTERN:
    break;
  }
}

Value calc_expr(Table *label_table, const Expr *expr) {
  assert(expr != NULL);
  switch (expr->kind) {
  case EX_LABEL:
    return (Value){.label = expr->label, .offset = 0};
  case EX_FIXNUM:
    return (Value){.label = NULL, .offset = expr->fixnum};
  case EX_ADD:
  case EX_SUB:
  case EX_MUL:
  case EX_DIV:
    {
      Value lhs = calc_expr(label_table, expr->bop.lhs);
      Value rhs = calc_expr(label_table, expr->bop.rhs);
      if (rhs.label != NULL) {
        if (expr->kind == EX_SUB && lhs.label != NULL) {
          LabelInfo *llabel, *rlabel;
          if (table_try_get(label_table, lhs.label, (void**)&llabel) &&
              table_try_get(label_table, rhs.label, (void**)&rlabel)) {
            return (Value){.label = NULL, .offset = llabel->address - rlabel->address};
          } else {
            error("Unresolved");
          }
        }
        if (expr->kind != EX_ADD || lhs.label != NULL) {
          error("Illegal expression");
        }
        // offset + label
        return (Value){.label = rhs.label, .offset = lhs.offset + rhs.offset};
      }
      if (lhs.label != NULL) {
        if (expr->kind != EX_ADD) {
          error("Illegal expression");
        }
        // label + offset
        return (Value){.label = lhs.label, .offset = lhs.offset + rhs.offset};
      }

      assert(lhs.label == NULL && rhs.label == NULL);
      switch (expr->kind) {
      case EX_ADD:  lhs.offset += rhs.offset; break;
      case EX_SUB:  lhs.offset -= rhs.offset; break;
      case EX_MUL:  lhs.offset *= rhs.offset; break;
      case EX_DIV:  lhs.offset /= rhs.offset; break;
      default: assert(false); break;
      }
      return lhs;
    }

  case EX_POS:
  case EX_NEG:
    {
      Value value = calc_expr(label_table, expr->unary.sub);
      if (value.label != NULL) {
        error("Illegal expression");
      }
      if (expr->kind == EX_NEG)
        value.offset = -value.offset;
      return value;
    }

  default: assert(false); break;
  }
  return (Value){.label = NULL, .offset = 0};
}
