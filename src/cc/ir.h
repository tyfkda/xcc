// Intermediate Representation

#pragma once

#include <stdbool.h>
#include <stddef.h>  // size_t
#include <stdint.h>  // intptr_t

typedef struct BB BB;
typedef struct Function Function;
typedef struct Name Name;
typedef struct RegAlloc RegAlloc;
typedef struct Vector Vector;

#define REG_COUNT  (7 - 1)
#define SPILLED_REG_NO  (REG_COUNT)

// Virtual register

#define VRF_PARAM  (1 << 0)  // Function parameter
#define VRF_LOCAL  (1 << 1)  // Local variable
#define VRF_REF    (1 << 2)  // Reference(&) taken
#define VRF_CONST  (1 << 3)  // Constant

typedef struct VRegType {
  int size;
  int align;
  bool is_unsigned;
} VRegType;

typedef struct VReg {
  int v;
  intptr_t r;  // Real register no. or constant value.
  const VRegType *vtype;
  int flag;
  int param_index;  // Function parameter index: -1=not a param
  int offset;  // Local offset for spilled register.
} VReg;

VReg *new_vreg(int vreg_no, const VRegType *vtype, int flag);
void vreg_spill(VReg *vreg);

// Intermediate Representation

enum IrKind {
  IR_BOFS,    // dst = [rbp + offset]
  IR_IOFS,    // dst = [rip + label]
  IR_LOAD,    // dst = [opr1]
  IR_STORE,   // [opr2] = opr1
  IR_ADD,     // dst = opr1 + opr2
  IR_SUB,
  IR_MUL,
  IR_DIV,
  IR_MOD,
  IR_BITAND,
  IR_BITOR,
  IR_BITXOR,
  IR_LSHIFT,
  IR_RSHIFT,
  IR_DIVU,
  IR_MODU,
  IR_PTRADD,  // dst = opr1 + opr2 * size
  IR_CMP,     // opr1 - opr2
  IR_INC,     // opr1 += size
  IR_DEC,     // dst = -opr1
  IR_NEG,
  IR_NOT,
  IR_BITNOT,
  IR_COND,    // dst <- flag
  IR_TEST,    // opr1 - 0
  IR_JMP,     // Jump with condition
  IR_PRECALL, // Prepare for call
  IR_PUSHARG,
  IR_CALL,    // Call label or opr1
  IR_RESULT,  // retval = opr1
  IR_ADDSP,   // RSP += value
  IR_CAST,    // dst <= opr1
  IR_MOV,     // dst = opr1
  IR_MEMCPY,  // memcpy(opr2, opr1, size)
  IR_CLEAR,   // memset(opr1, 0, size)
  IR_ASM,     // assembler code

  IR_LOAD_SPILLED,   // dst(spilled) = [ofs]
  IR_STORE_SPILLED,  // [ofs] = opr1(spilled)
};

enum ConditionKind {
  COND_ANY,
  COND_EQ,
  COND_NE,
  COND_LT,
  COND_LE,
  COND_GE,
  COND_GT,
};

typedef struct {
  enum IrKind kind;
  VReg *dst;
  VReg *opr1;
  VReg *opr2;
  int size;
  intptr_t value;

  union {
    struct {
      const Name *label;
      bool global;
    } iofs;
    struct {
      int offset;
      int scale;
    } ptradd;
    struct {
      enum ConditionKind kind;
    } cond;
    struct {
      BB *bb;
      enum ConditionKind cond;
    } jmp;
    struct {
      const Name *label;
      bool *stack_aligned;
      int arg_count;
      bool global;
    } call;
    struct {
      int srcsize;
      bool is_unsigned;
    } cast;
    struct {
      const char *str;
    } asm_;
  };
} IR;

VReg *new_const_vreg(intptr_t value, const VRegType *vtype);
VReg *new_ir_bop(enum IrKind kind, VReg *opr1, VReg *opr2, const VRegType *vtype);
VReg *new_ir_unary(enum IrKind kind, VReg *opr, const VRegType *vtype);
VReg *new_ir_ptradd(int offset, VReg *base, VReg *index, int scale, const VRegType *vtype);
void new_ir_mov(VReg *dst, VReg *src, int size);
VReg *new_ir_bofs(VReg *src);
VReg *new_ir_iofs(const Name *label, bool global);
void new_ir_store(VReg *dst, VReg *src, int size);
void new_ir_cmp(VReg *opr1, VReg *opr2, int size);
void new_ir_test(VReg *reg, int size);
void new_ir_incdec(enum IrKind kind, VReg *reg, int size, intptr_t value);
VReg *new_ir_cond(enum ConditionKind cond);
void new_ir_jmp(enum ConditionKind cond, BB *bb);
void new_ir_precall(int arg_count, bool *stack_aligned);
void new_ir_pusharg(VReg *vreg);
VReg *new_ir_call(const Name *label, bool global, VReg *freg, int arg_count, const VRegType *result_type, bool *stack_aligned);
void new_ir_result(VReg *reg, int size);
void new_ir_addsp(int value);
VReg *new_ir_cast(VReg *vreg, const VRegType *dsttype, int srcsize, bool is_unsigned);
void new_ir_memcpy(VReg *dst, VReg *src, int size);
void new_ir_clear(VReg *reg, size_t size);
void new_ir_asm(const char *asm_);

IR *new_ir_load_spilled(VReg *reg, int offset, int size);
IR *new_ir_store_spilled(VReg *reg, int offset, int size);

#if !defined(SELF_HOSTING)
void dump_func_ir(Function *func);
#endif

// Register allocator

extern RegAlloc *curra;

// Basci Block:
//   Chunk of IR codes without branching in the middle (except at the bottom).

typedef struct BB {
  struct BB *next;
  const Name *label;
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
void push_callee_save_regs(Function *func);
void pop_callee_save_regs(Function *func);
void emit_bb_irs(BBContainer *bbcon);
