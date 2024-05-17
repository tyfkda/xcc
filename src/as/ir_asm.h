// Intermediate Representation for assembly

#pragma once

#include <stddef.h>  // size_t
#include <stdint.h>  // uint64_t

#include "asm_code.h"  // Code

#define BYTE_SIZE   (1)
#define SHORT_SIZE  (2)
#define LONG_SIZE   (4)
#define QUAD_SIZE   (8)

typedef struct Expr Expr;
typedef struct Name Name;
typedef struct SectionInfo SectionInfo;
typedef struct Table Table;
typedef struct Vector Vector;

enum UnresolvedKind {
  UNRES_EXTERN,
  UNRES_EXTERN_PC32,
  UNRES_ABS64,
  UNRES_CALL,
  UNRES_PCREL_HI,
  UNRES_PCREL_LO,
  UNRES_GOT_HI,
  UNRES_GOT_LO,

  UNRES_X64_GOT_LOAD,

  UNRES_RISCV_BRANCH,
  UNRES_RISCV_JAL,
  UNRES_RISCV_HI20,
  UNRES_RISCV_LO12_I,
  UNRES_RISCV_RVC_BRANCH,
  UNRES_RISCV_RVC_JUMP,
  UNRES_RISCV_RELAX,
};

typedef struct {
  const Name *label;
  uintptr_t offset;
  SectionInfo *src_section;
  int add;
  enum UnresolvedKind kind;
} UnresolvedInfo;

typedef struct {
  size_t len;
  unsigned char *buf;
} Data;

enum IrKind {
  IR_LABEL,
  IR_CODE,
  IR_DATA,
  IR_BSS,
  IR_ZERO,
  IR_ALIGN,
  IR_EXPR_BYTE,
  IR_EXPR_SHORT,
  IR_EXPR_LONG,
  IR_EXPR_QUAD,
};

typedef struct {
  enum IrKind kind;
  union {
    const Name *label;
    Code code;
    Data data;
    struct {
      const Expr *expr;
      int64_t addend;  // Calculated in `resolve_relative_address`
    } expr;
    size_t bss;
    size_t zero;
    int align;
    int section;
  };
  uint64_t address;
} IR;

IR *new_ir_label(const Name *label);
IR *new_ir_code(const Code *code);
IR *new_ir_data(const void *data, size_t size);
IR *new_ir_bss(size_t size);
IR *new_ir_zero(size_t size);
IR *new_ir_align(int align);
IR *new_ir_expr(enum IrKind kind, const Expr *expr);

bool calc_label_address(uint64_t start_address, Vector *sections, Table *label_table);
bool resolve_relative_address(Vector *sections, Table *label_table, Vector *unresolved);
void emit_irs(Vector *sections);
