// Intermediate Representation

#pragma once

#include <stdbool.h>
#include <stddef.h>  // size_t
#include <stdint.h>  // intptr_t

typedef struct BB BB;
typedef struct Defun Defun;
typedef struct Type Type;
typedef struct Vector Vector;

// Virtual register

typedef struct VReg {
  int v;
  int r;
  const Type *type;
  int offset;  // Local offset for spilled register.
} VReg;

VReg *new_vreg(int vreg_no);
void vreg_spill(VReg *vreg);

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
  IR_INC,
  IR_DEC,
  IR_NEG,
  IR_NOT,
  IR_SET,   // SETxx: flag => 0 or 1
  IR_TEST,
  IR_JMP,
  IR_PRECALL,
  IR_PUSHARG,
  IR_CALL,
  IR_ADDSP,
  IR_CAST,
  IR_CLEAR,
  IR_COPY,
  IR_RESULT,
  IR_ASM,
  IR_UNREG,

  IR_MOV,
  IR_LOAD_SPILLED,
  IR_STORE_SPILLED,
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
  VReg *opr2;
  int size;
  intptr_t value;

  union {
    struct {
      const char *label;
    } iofs;
    struct {
      enum ConditionType cond;
    } set;
    struct {
      BB *bb;
      enum ConditionType cond;
    } jmp;
    struct {
      const char *label;
      int arg_count;
    } call;
    struct {
      int srcsize;
    } cast;
    struct {
      const char *str;
    } asm_;
  } u;
} IR;

VReg *new_ir_imm(intptr_t value, int size);
VReg *new_ir_bop(enum IrType type, VReg *opr1, VReg *opr2, int size);
VReg *new_ir_unary(enum IrType type, VReg *opr, int size);
void new_ir_mov(VReg *dst, VReg *src, int size);
VReg *new_ir_bofs(VReg *src);
VReg *new_ir_iofs(const char *label);
void new_ir_store(VReg *dst, VReg *src, int size);
void new_ir_memcpy(VReg *dst, VReg *src, int size);
void new_ir_cmp(VReg *opr1, VReg *opr2, int size);
void new_ir_test(VReg *reg, int size);
void new_ir_incdec(enum IrType type, VReg *reg, int size, intptr_t value);
VReg *new_ir_set(enum ConditionType cond);
void new_ir_jmp(enum ConditionType cond, BB *bb);
void new_ir_precall(int arg_count);
void new_ir_pusharg(VReg *vreg);
VReg *new_ir_call(const char *label, VReg *freg, int arg_count, int result_size);
void new_ir_addsp(int value);
void new_ir_cast(VReg *vreg, int dstsize, int srcsize);
void new_ir_clear(VReg *reg, size_t size);
void new_ir_copy(VReg *dst, VReg *src, int size);
void new_ir_result(VReg *reg, int size);
void new_ir_asm(const char *asm_);
void new_ir_unreg(VReg *reg);

void ir_alloc_reg(IR *ir);
void ir_out(const IR *ir);

#if !defined(SELF_HOSTING)
void dump_ir(IR *ir);
#endif

// Register allocator

void init_reg_alloc(void);
VReg *add_new_reg(void);
void check_all_reg_unused(void);

// Basci Block:
//   Chunk of IR codes without branching in the middle (except at the bottom).

typedef struct BB {
  struct BB *next;
  const char *label;
  Vector *irs;  // <IR*>

  Vector *in_regs;  // <VReg*>
  Vector *out_regs;  // <VReg*>
  Vector *assigned_regs;  // <VReg*>
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
size_t alloc_real_registers(Defun *defun);
void emit_bb_irs(BBContainer *bbcon);
