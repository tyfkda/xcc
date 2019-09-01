// Intermediate Representation

#pragma once

#include <stdbool.h>
#include <stddef.h>  // size_t
#include <stdint.h>  // intptr_t

typedef struct BB BB;
typedef struct Vector Vector;

// Virtual register

typedef struct VReg {
  int v;
  int r;
} VReg;

VReg *new_vreg(int vreg_no);

// Intermediate Representation

enum IrType {
  IR_IMM,   // Immediate value
  IR_BOFS,  // basereg+ofs
  IR_IOFS,  // label(rip)
  IR_LOAD,
  IR_STORE,
  IR_MEMCPY,
  IR_ADD,
  IR_SUB,
  IR_MUL,
  IR_DIV,
  IR_MOD,
  IR_BITAND,
  IR_BITOR,
  IR_BITXOR,
  IR_LSHIFT,
  IR_RSHIFT,
  IR_CMP,
  IR_INCDEC,
  IR_NEG,
  IR_NOT,
  IR_SET,   // SETxx: flag => 0 or 1
  IR_CMPI,
  IR_PUSH,
  IR_JMP,
  IR_CALL,
  IR_ADDSP,
  IR_CAST,
  IR_LABEL,
  IR_SAVE_LVAL,
  IR_ASSIGN_LVAL,
  IR_CLEAR,
  IR_RESULT,
};

enum ConditionType {
  COND_ANY,
  COND_EQ,
  COND_NE,
  COND_LT,
  COND_LE,
  COND_GE,
  COND_GT,
};

typedef struct {
  enum IrType type;
  VReg *dst;
  VReg *opr1;
  int size;
  intptr_t value;

  union {
    struct {
      const char *label;
    } iofs;
    struct {
      bool inc;
      bool pre;
    } incdec;
    struct {
      enum ConditionType cond;
    } set;
    struct {
      BB *bb;
      enum ConditionType cond;
    } jmp;
    struct {
      const char *label;
      size_t arg_count;
    } call;
    struct {
      int srcsize;
    } cast;
  } u;
} IR;

VReg *new_ir_imm(intptr_t value, int size);
IR *new_ir_bofs(int offset);
IR *new_ir_iofs(const char *label);
IR *new_ir_load(int size);
IR *new_ir_store(int size);
IR *new_ir_memcpy(size_t size);
IR *new_ir_op(enum IrType type, int size);
IR *new_ir_cmpi(intptr_t value, int size);
IR *new_ir_incdec(bool inc, bool pre, int size, intptr_t value);
IR *new_ir_st(enum IrType type);
IR *new_ir_set(enum ConditionType cond);
IR *new_ir_jmp(enum ConditionType cond, BB *bb);
IR *new_ir_call(const char *label, int arg_count);
IR *new_ir_addsp(int value);
IR *new_ir_cast(int dstsize, int srcsize);
IR *new_ir_assign_lval(int size);
IR *new_ir_clear(size_t size);
void new_ir_result(VReg *reg, int size);

void ir_alloc_reg(IR *ir);
void ir_out(const IR *ir);

// Basci Block:
//   Chunk of IR codes without branching in the middle (except at the bottom).

typedef struct BB {
  struct BB *next;
  const char *label;
  Vector *irs;  // <IR*>
} BB;

extern BB *curbb;

BB *new_bb(void);
BB *bb_split(BB *bb);
void bb_insert(BB *bb, BB *cc);

// Basic blocks in a function
typedef struct BBContainer {
  Vector *bbs;  // <BB*>
} BBContainer;

BBContainer *new_func_blocks(void);
void remove_unnecessary_bb(BBContainer *bbcon);
void emit_bb_irs(BBContainer *bbcon);
