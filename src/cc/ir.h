// Intermediate Representation

#pragma once

#include <stdbool.h>
#include <stddef.h>  // size_t
#include <stdint.h>  // intptr_t

typedef struct BB BB;
typedef struct Name Name;
typedef struct RegAlloc RegAlloc;
typedef struct Vector Vector;

#define MAX_REG_ARGS  (6)
#define WORD_SIZE  (8)  /*sizeof(void*)*/

#define PHYSICAL_REG_MAX  (7 - 1)

#ifndef __NO_FLONUM
#define MAX_FREG_ARGS  (8)
#define PHYSICAL_FREG_MAX  (7 - 1)
#endif

// Virtual register

#define VRTF_UNSIGNED  (1 << 0)
#ifndef __NO_FLONUM
#define VRTF_FLONUM    (1 << 1)
#endif

typedef struct VRegType {
  int size;
  int align;
  int flag;
} VRegType;

#define VRF_PARAM  (1 << 0)  // Function parameter
#define VRF_LOCAL  (1 << 1)  // Local variable
#define VRF_REF    (1 << 2)  // Reference(&) taken
#define VRF_CONST  (1 << 3)  // Constant

typedef struct VReg {
  const VRegType *vtype;
  int virt;         // Virtual reg no.
  int phys;         // Physical reg no.
  int flag;
  int param_index;  // Function parameter index: -1=not a param
  int offset;       // Local offset for spilled register.
  intptr_t fixnum;  // Constant value.
} VReg;

VReg *new_vreg(int vreg_no, const VRegType *vtype, int flag);

// Intermediate Representation

enum IrKind {
  IR_BOFS,    // dst = [rbp + offset]
  IR_IOFS,    // dst = [rip + label]
  IR_SOFS,    // dst = [rsp + offset]
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
  IR_CMP,     // opr1 - opr2
  IR_INC,     // opr1 += size
  IR_DEC,     // dst = -opr1
  IR_NEG,
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
  COND_NONE,
  COND_ANY,
  COND_EQ,
  COND_NE,
  COND_LT,
  COND_LE,
  COND_GE,
  COND_GT,
  COND_ULT,  // Unsigned
  COND_ULE,
  COND_UGE,
  COND_UGT,
};

typedef struct IR {
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
      enum ConditionKind kind;
    } cond;
    struct {
      BB *bb;
      enum ConditionKind cond;
    } jmp;
    struct {
      int arg_count;
      int stack_args_size;
      int stack_aligned;
      unsigned int living_pregs;
    } precall;
    struct {
      const Name *label;
      struct IR *precall;
      int reg_arg_count;
      bool global;
      VRegType **arg_vtypes;
    } call;
    struct {
      int flag;  // VRTF_FLOAT
    } spill;
    struct {
      const char *str;
    } asm_;
  };
} IR;

VReg *new_const_vreg(intptr_t value, const VRegType *vtype);
VReg *new_ir_bop(enum IrKind kind, VReg *opr1, VReg *opr2, const VRegType *vtype);
VReg *new_ir_unary(enum IrKind kind, VReg *opr, const VRegType *vtype);
void new_ir_mov(VReg *dst, VReg *src);
VReg *new_ir_bofs(VReg *src);
VReg *new_ir_iofs(const Name *label, bool global);
VReg *new_ir_sofs(VReg *src);
void new_ir_store(VReg *dst, VReg *src);
void new_ir_cmp(VReg *opr1, VReg *opr2);
void new_ir_test(VReg *reg);
void new_ir_incdec(enum IrKind kind, VReg *reg, int size, intptr_t value);
VReg *new_ir_cond(enum ConditionKind cond);
void new_ir_jmp(enum ConditionKind cond, BB *bb);
IR *new_ir_precall(int arg_count, int stack_args_size);
void new_ir_pusharg(VReg *vreg, const VRegType *vtype);
VReg *new_ir_call(const Name *label, bool global, VReg *freg, int reg_arg_count, const VRegType *result_type, IR *precall, VRegType **arg_vtypes);
void new_ir_result(VReg *reg);
void new_ir_addsp(int value);
VReg *new_ir_cast(VReg *vreg, const VRegType *dsttype);
void new_ir_memcpy(VReg *dst, VReg *src, int size);
void new_ir_clear(VReg *reg, size_t size);
void new_ir_asm(const char *asm_);

IR *new_ir_load_spilled(VReg *reg, int offset, int size, int flag);
IR *new_ir_store_spilled(VReg *reg, int offset, int size, int flag);

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

// Basic blocks in a function
typedef struct BBContainer {
  Vector *bbs;  // <BB*>
} BBContainer;

BBContainer *new_func_blocks(void);
void remove_unnecessary_bb(BBContainer *bbcon);
void push_callee_save_regs(unsigned short used);
void pop_callee_save_regs(unsigned short used);

void emit_bb_irs(BBContainer *bbcon);

//

#define PUSH_STACK_POS()  do { stackpos += WORD_SIZE; } while (0)
#define POP_STACK_POS()   do { stackpos -= WORD_SIZE; } while (0)

extern int stackpos;

void convert_3to2(BBContainer *bbcon);  // Make 3 address code to 2.
