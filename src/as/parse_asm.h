// Parser for assembler

#pragma once

#include <stdbool.h>
#include <stdint.h>  // int64_t

#include "inst.h"  // Inst

typedef struct Name Name;
typedef struct Table Table;
typedef struct Token Token;
typedef struct Vector Vector;

enum DirectiveType {
  NODIRECTIVE,
  DT_ASCII,
  DT_SECTION,
  DT_TEXT,
  DT_DATA,
  DT_ALIGN,
  DT_P2ALIGN,
  DT_TYPE,
  DT_BYTE,
  DT_SHORT,
  DT_LONG,
  DT_QUAD,
  DT_COMM,
  DT_GLOBL,
  DT_LOCAL,
  DT_EXTERN,
  DT_FLOAT,
  DT_DOUBLE,
};

typedef struct ParseInfo {
  const char *filename;
  int lineno;
  const char *rawline;
  const char *p;

  const Token *prefetched;
} ParseInfo;

typedef struct Line {
  const Name *label;
  Inst inst;
  enum DirectiveType dir;
} Line;

enum ExprKind {
  EX_LABEL,
  EX_FIXNUM,
  EX_POS,
  EX_NEG,
  EX_ADD,
  EX_SUB,
  EX_MUL,
  EX_DIV,
  EX_FLONUM,
};

#ifndef __NO_FLONUM
typedef long double Flonum;
#endif

#define LF_PAGE     (1 << 0)
#define LF_PAGEOFF  (1 << 1)
#define LF_LO12     (1 << 2)
#define LF_GOT      (1 << 3)

typedef struct Expr {
  enum ExprKind kind;
  union {
    struct {
      const Name *name;
      int flag;
    } label;
    int64_t fixnum;
    struct {
      struct Expr *lhs;
      struct Expr *rhs;
    } bop;
    struct {
      struct Expr *sub;
    } unary;
#ifndef __NO_FLONUM
    Flonum flonum;
#endif
  };
} Expr;

extern int current_section;  // enum SectionType
extern bool err;

Line *parse_line(ParseInfo *info);
void handle_directive(ParseInfo *info, enum DirectiveType dir, Vector **section_irs,
                      Table *label_table);
void parse_error(const ParseInfo *info, const char *message);

typedef struct {
  enum Opcode op;
  int opr_flags[4 + 1];
} ParseOpArray;

typedef struct {
  int count;
  const ParseOpArray **array;
} ParseInstTable;

extern const char *kRawOpTable[];
extern const ParseInstTable kParseInstTable[];

unsigned int parse_operand(ParseInfo *info, unsigned int opr_flag, Operand *operand);

bool immediate(const char **pp, int64_t *value);
const Name *unquote_label(const char *p, const char *q);
Expr *parse_expr(ParseInfo *info);
Expr *new_expr(enum ExprKind kind);

typedef struct {
  const Name *label;
  int64_t offset;
  int flag;
} Value;

Value calc_expr(Table *label_table, const Expr *expr);

bool is_label_first_chr(char c);
bool is_label_chr(char c);

#define LF_GLOBAL    (1 << 0)
#define LF_DEFINED   (1 << 1)
#define LF_REFERRED  (1 << 2)

enum LabelKind {
  LK_NONE,
  LK_FUNC,
  LK_OBJECT,
};

typedef struct {
  int section;
  int flag;
  uintptr_t address;
  enum LabelKind kind;
} LabelInfo;

LabelInfo *add_label_table(Table *label_table, const Name *label, int section, bool define, bool global);
