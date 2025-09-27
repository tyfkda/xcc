// Intermediate Representation

#pragma once

#include <stdbool.h>
#include <stddef.h>  // size_t
#include <stdint.h>  // int64_t

typedef struct BB BB;
typedef struct Name Name;
typedef struct RegAlloc RegAlloc;
typedef struct VarInfo VarInfo;
typedef struct Vector Vector;

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

#define VRF_CONST        (1 << 0)  // Constant
#define VRF_FLONUM       (1 << 1)  // Floating-point register?
#define VRF_REF          (1 << 2)  // Reference(&) taken
#define VRF_PARAM        (1 << 3)  // Function parameter
#define VRF_STACK_PARAM  (1 << 4)  // Function parameter, but through stack (spilled by default, so no regalloc needed)
#define VRF_SPILLED      (1 << 5)  // Spilled
#define VRF_NO_SPILL     (1 << 6)  // No Spill
#define VRF_UNUSED       (1 << 7)  // Unused
#define VRF_VOLATILE     (1 << 8)  // Volatile
#define VRF_VOLATILEREG  (1 << 9)  // Volatile, but register

#define VRF_MASK         (VRF_FLONUM)
#define VRF_FORCEMEMORY  (VRF_REF | VRF_SPILLED | VRF_VOLATILE)

typedef struct VReg {
  enum VRegSize vsize;
  int flag;
  union {
    // Non-const:
    struct {
      struct VReg *original;  // If this member is same as itself, it is the original.
      int virt;             // Virtual reg no.
      int phys;             // Physical reg no.
      int reg_param_index;  // Index of function parameter through register: -1=non reg param.
      FrameInfo frame;      // FrameInfo for spilled register.
    };

    // Const:
    int64_t fixnum;
#ifndef __NO_FLONUM
    struct {
      double value;
      const Name *label;
    } flonum;
#endif
  };
} VReg;

void spill_vreg(VReg *vreg);

// Intermediate Representation

enum IrKind {
  IR_BOFS,    // dst = [rbp + offset]
  IR_IOFS,    // dst = [rip + label]
  IR_SOFS,    // dst = [rsp + opr1(offset)]
  IR_LOAD,    // dst = [opr1]
  IR_LOAD_S,  // dst = [opr1(spilled)]
  IR_STORE,   // [opr2] = opr1
  IR_STORE_S, // [opr2(spilled)] = opr1

  // Binary operators.
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
  IR_COND,    // dst <- (opr1 @@ opr2) ? 1 : 0
  // Unary operators.
  IR_NEG,
  IR_BITNOT,
  IR_CAST,    // dst <- opr1
  IR_MOV,     // dst = opr1
  IR_RESULT,  // retval = opr1

  IR_JMP,     // Non conditional jump, or conditional jmp (opr1 @@ opr2)
  IR_TJMP,    // Table jump (opr1).  opr2 is NULL, but it might be used to keep temporary vreg.
  IR_PUSHARG,
  IR_CALL,    // Call label or opr1
  IR_SUBSP,   // RSP -= value
  IR_KEEP,    // To keep live vregs.
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

enum ConditionKind swap_cond(enum ConditionKind cond);
enum ConditionKind invert_cond(enum ConditionKind cond);

#define IRF_UNSIGNED  (1 << 0)

typedef struct {
  // Precall
  size_t stack_args_size;
  int arg_count;
  unsigned long living_pregs;
  Vector *caller_saves;  // <const char*>

  // Call
  const Name *label;
  VReg **args;  // [total_arg_count]
  int total_arg_count;
  int reg_arg_count;
  int vaarg_start;
  bool global;
} IrCallInfo;

typedef struct IR {
  enum IrKind kind;
  int flag;
  VReg *dst;
  VReg *opr1;
  VReg *opr2;
  Vector *additional_operands;  // <VReg*>

  union {
    struct {
      FrameInfo *frameinfo;
      int64_t offset;
    } bofs;
    struct {
      const Name *label;
      int64_t offset;
      bool global;
    } iofs;
    struct {
      enum ConditionKind kind;
    } cond;
    struct {
      // (ir->flag & IRF_UNSIGNED) indicates whether the destination value is unsigned.
      bool src_unsigned;
    } cast;
    struct {
      BB *bb;
      enum ConditionKind cond;
    } jmp;
    struct {
      BB **bbs;
      size_t len;
    } tjmp;
    struct {
      int index;
#if VAARG_FP_AS_GP
      bool fp_as_gp;
#endif
    } pusharg;
    IrCallInfo *call;
    struct {
      Vector *templates;  // [const char*, (intptr_t)register-index, ...]
    } asm_;
  };
} IR;

typedef struct {
  VReg *dst;
  Vector *params;  // <VReg*>
} Phi;

Phi *new_phi(VReg *dst, Vector *params);

VReg *new_const_vreg(int64_t value, enum VRegSize vsize);
#ifndef __NO_FLONUM
VReg *new_const_vfreg(double value, enum VRegSize vsize);
#endif
VReg *new_ir_bop(enum IrKind kind, VReg *opr1, VReg *opr2, enum VRegSize vsize, int flag);
IR *new_ir_bop_raw(enum IrKind kind, VReg *dst, VReg *opr1, VReg *opr2, int flag);
VReg *new_ir_unary(enum IrKind kind, VReg *opr, enum VRegSize vsize, int flag);
IR *new_ir_load(VReg *opr, enum VRegSize vsize, int vflag, int irflag);
IR *new_ir_mov(VReg *dst, VReg *src, int flag);
IR *new_ir_bofs(FrameInfo *fi);
IR *new_ir_iofs(const Name *label, bool global);
IR *new_ir_sofs(VReg *src);
IR *new_ir_store(VReg *dst, VReg *src, int flag);
IR *new_ir_cond(VReg *opr1, VReg *opr2, enum ConditionKind cond);
IR *new_ir_jmp(BB *bb);  // Non-conditional jump
void new_ir_cjmp(VReg *opr1, VReg *opr2, enum ConditionKind cond, BB *bb);  // Conditional jump
void new_ir_tjmp(VReg *val, BB **bbs, size_t len);
IR *new_ir_pusharg(VReg *vreg, int index);
IR *new_ir_call(IrCallInfo *info, VReg *dst, VReg *freg);
void new_ir_result(VReg *vreg, int flag);
void new_ir_subsp(VReg *value, VReg *dst);
IR *new_ir_cast(VReg *vreg, bool src_unsigned, enum VRegSize dstsize, int vflag);
IR *new_ir_keep(VReg *dst, VReg *opr1, VReg *opr2);
void new_ir_asm(Vector *templates, VReg *dst, Vector *registers);

IR *new_ir_load_spilled(VReg *vreg, VReg *src, int flag);
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
  Vector *phis;
} BB;

extern BB *curbb;

BB *new_bb(void);

// Basic blocks in a function
typedef struct Vector BBContainer;  // <BB*>

BBContainer *new_func_blocks(void);
void detect_from_bbs(BBContainer *bbcon);
void analyze_reg_flow(BBContainer *bbcon);

void emit_bb_irs(BBContainer *bbcon);

//

typedef struct FuncallInfo {
  IR *call;
} FuncallInfo;

// Function info for backend

typedef struct FuncBackend {
  RegAlloc *ra;
  BBContainer *bbcon;
  BB *ret_bb;
  VarInfo *retvarinfo;
  VReg *retval;
  VReg *result_dst;
  Vector *funcalls;
  size_t frame_size;
  FrameInfo vaarg_frame_info;  // Used for va_start.
  size_t stack_work_size;
  VReg *stack_work_size_vreg;
} FuncBackend;
