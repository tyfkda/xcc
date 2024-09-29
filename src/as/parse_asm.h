// Parser for assembler

#pragma once

#include <stdbool.h>
#include <stdint.h>  // int64_t

#include "inst.h"  // Inst

typedef struct DataStorage DataStorage;
typedef struct Name Name;
typedef struct Table Table;
typedef struct Token Token;
typedef struct Vector Vector;

enum DirectiveType {
  NODIRECTIVE,
  DT_ASCII,
  DT_STRING,
  DT_SECTION,
  DT_TEXT,
  DT_DATA,
  DT_BSS,
  DT_ALIGN,
  DT_P2ALIGN,
  DT_TYPE,
  DT_BYTE,
  DT_SHORT,
  DT_LONG,
  DT_QUAD,
  DT_COMM,
  DT_ZERO,
  DT_GLOBL,
  DT_LOCAL,
  DT_EXTERN,
  DT_FLOAT,
  DT_DOUBLE,
};

#define SF_EXECUTABLE  (1 << 0)
#define SF_WRITABLE    (1 << 1)
#define SF_BSS         (1 << 2)
#define SF_INIT_FUNCS  (1 << 8)

typedef struct SectionInfo {
  const Name *name;
  const char *segname;
  Vector *irs;
  int flag;

  int index;
  uintptr_t start_address;
  DataStorage *ds;
  size_t align;
  size_t bss_size;
  uintptr_t offset;  // File offset.

  int rela_count;
  void *rela_buf;
  uintptr_t rela_ofs;
} SectionInfo;

typedef struct ParseInfo {
  const char *filename;
  const char *rawline;
  const char *p;
  int lineno;
  int error_count;

  const Token *prefetched;

  Table *section_infos;  // <SectionInfo*>
  Table *label_table;
  SectionInfo *current_section;
} ParseInfo;

SectionInfo *set_current_section(ParseInfo *info, const char *name, const char *segname, int flag);
SectionInfo *get_section_info(ParseInfo *info, const char *name, const char *segname, int flag);

extern const char kSecText[];
extern const char kSecRodata[];
extern const char kSecData[];
extern const char kSecBss[];

#if XCC_TARGET_PLATFORM == XCC_PLATFORM_APPLE
extern const char kSegText[];
extern const char kSegRodata[];
extern const char kSegData[];
extern const char kSegBss[];
#else
#define kSegText    NULL
#define kSegRodata  NULL
#define kSegData    NULL
#define kSegBss     NULL
#endif

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

typedef struct Expr {
  enum ExprKind kind;
  union {
    struct {
      const Name *name;
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

Line *parse_line(ParseInfo *info);
void parse_set_p(ParseInfo *info, const char *p);
void handle_directive(ParseInfo *info, enum DirectiveType dir);
void parse_error(ParseInfo *info, const char *message);

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
} Value;

Value calc_expr(Table *label_table, const Expr *expr);

bool is_label_first_chr(char c);
bool is_label_chr(char c);

#define LF_GLOBAL    (1 << 0)
#define LF_DEFINED   (1 << 1)
#define LF_REFERRED  (1 << 2)
#define LF_COMM      (1 << 3)

enum LabelKind {
  LK_NONE,
  LK_FUNC,
  LK_OBJECT,
};

typedef struct {
  SectionInfo *section;
  int flag;
  uintptr_t address;
  enum LabelKind kind;
  int size;
  int align;
} LabelInfo;

LabelInfo *add_label_table(Table *label_table, const Name *label, SectionInfo *section, bool define, bool global);
