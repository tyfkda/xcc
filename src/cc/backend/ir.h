// Intermediate Representation

#pragma once

#include <stdbool.h>
#include <stddef.h>  // size_t
#include <stdint.h>  // int64_t

typedef struct BB BB;
typedef struct Name Name;
typedef struct RegAlloc RegAlloc;
typedef struct RegAllocSettings RegAllocSettings;
typedef struct Vector Vector;

#define WORD_SIZE  (8)  /*sizeof(void*)*/

typedef struct FrameInfo {
  int offset;
} FrameInfo;

// Virtual register

enum VRegSize {
  VRegSize1,
  VRegSize2,
  VRegSize4,
  VRegSize8,
};

#define VRF_PARAM     (1 << 0)  // Function parameter
#define VRF_REF       (1 << 1)  // Reference(&) taken
#define VRF_CONST     (1 << 2)  // Constant
#define VRF_SPILLED   (1 << 3)  // Spilled
#define VRF_NO_SPILL  (1 << 4)  // No Spill
#define VRF_UNSIGNED  (1 << 5)  // Unsigned?
#define VRF_FLONUM    (1 << 6)  // Floating-point register?
#define VRF_UNUSED    (1 << 9)  // Unused

#define VRF_MASK      (VRF_UNSIGNED | VRF_FLONUM)

typedef struct VReg {
  enum VRegSize vsize;
  int virt;         // Virtual reg no.
  int phys;         // Physical reg no.
  int flag;
  int reg_param_index;  // Index of function parameter through register: -1=not a register param.
  int64_t fixnum;   // Constant value.
  FrameInfo frame;  // FrameInfo for spilled register.
} VReg;

void spill_vreg(VReg *vreg);

// Intermediate Representation

enum IrKind {
  IR_BOFS,    // dst = [rbp + offset]
  IR_IOFS,    // dst = [rip + label]
  IR_SOFS,    // dst = [rsp + offset]
  IR_LOAD,    // dst = [opr1]
  IR_LOAD_S,  // dst = [opr1(spilled)]
  IR_STORE,   // [opr2] = opr1
  IR_STORE_S, // [opr2(spilled)] = opr1
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
  IR_CMP,     // opr1 - opr2
  IR_NEG,
  IR_BITNOT,
  IR_COND,    // dst <- flag
  IR_JMP,     // Jump with condition
  IR_TJMP,    // Table jump
  IR_PRECALL, // Prepare for call
  IR_PUSHARG,
  IR_CALL,    // Call label or opr1
  IR_RESULT,  // retval = opr1  (Or mov to dst if set)
  IR_SUBSP,   // RSP -= value
  IR_CAST,    // dst <= opr1
  IR_MOV,     // dst = opr1
  IR_ASM,     // assembler code
};

// ConditionKind occupies lower bits and bitOR-ed with COND_UNSIGNED or COND_FLONUM.
// The value can be extracted with bitAND with COND_MASK.
enum ConditionKind {
  COND_NONE,
  COND_ANY,
  COND_EQ,
  COND_NE,
  COND_LT,
  COND_LE,
  COND_GE,
  COND_GT,
};
enum {
  COND_MASK = 0x07,
  COND_UNSIGNED = 1 << 3,
  COND_FLONUM = 1 << 4,
};

typedef struct IR {
  enum IrKind kind;
  VReg *dst;
  VReg *opr1;
  VReg *opr2;
  int64_t value;

  union {
    struct {
      FrameInfo *frameinfo;
    } bofs;
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
      BB **bbs;
      size_t len;
    } tjmp;
    struct {
      int arg_count;
      int stack_args_size;
      int stack_aligned;
      unsigned long living_pregs;
      Vector *caller_saves;  // <const char*>
    } precall;
    struct {
      int index;
    } pusharg;
    struct {
      const Name *label;
      struct IR *precall;
      VReg **args;
      int total_arg_count;
      int reg_arg_count;
      int vaarg_start;
      bool global;
    } call;
    struct {
      size_t size;
    } memcpy;
    struct {
      size_t size;
    } clear;
    struct {
      const char *str;
    } asm_;
  };
} IR;

VReg *new_const_vreg(int64_t value, enum VRegSize vsize, int vflag);
VReg *new_ir_bop(enum IrKind kind, VReg *opr1, VReg *opr2, enum VRegSize vsize);
VReg *new_ir_unary(enum IrKind kind, VReg *opr, enum VRegSize vsize, int vflag);
IR *new_ir_mov(VReg *dst, VReg *src);
VReg *new_ir_bofs(FrameInfo *fi, VReg *src);
VReg *new_ir_iofs(const Name *label, bool global);
VReg *new_ir_sofs(VReg *src);
void new_ir_store(VReg *dst, VReg *src);
void new_ir_cmp(VReg *opr1, VReg *opr2);
VReg *new_ir_cond(enum ConditionKind cond);
void new_ir_jmp(enum ConditionKind cond, BB *bb);
void new_ir_tjmp(VReg *val, BB **bbs, size_t len);
IR *new_ir_precall(int arg_count, int stack_args_size);
void new_ir_pusharg(VReg *vreg, int index);
VReg *new_ir_call(const Name *label, bool global, VReg *freg, int total_arg_count,
                  int reg_arg_count, enum VRegSize result_size, int result_flag, IR *precall,
                  VReg **args, int vaarg_start);
void new_ir_result(VReg *dst, VReg *vreg);
void new_ir_subsp(VReg *value, VReg *dst);
VReg *new_ir_cast(VReg *vreg, enum VRegSize dstsize, int vflag);
void new_ir_asm(const char *asm_, VReg *dst);

IR *new_ir_load_spilled(VReg *vreg, VReg *src);
IR *new_ir_store_spilled(VReg *dst, VReg *vreg);

// Register allocator

extern RegAlloc *curra;

// Basci Block:
//   Chunk of IR codes without branching in the middle (except at the bottom).

typedef struct BB {
  struct BB *next;
  Vector *from_bbs;
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
void analyze_reg_flow(BBContainer *bbcon);
int push_callee_save_regs(unsigned long used, unsigned long fused);
void pop_callee_save_regs(unsigned long used, unsigned long fused);

void emit_bb_irs(BBContainer *bbcon);

// Function info for backend

typedef struct FuncBackend {
  RegAlloc *ra;
  BBContainer *bbcon;
  BB *ret_bb;
  VReg *retval;
  VReg *result_dst;
  size_t frame_size;
} FuncBackend;

//

extern const RegAllocSettings kArchRegAllocSettings;

void tweak_irs(FuncBackend *fnbe);
