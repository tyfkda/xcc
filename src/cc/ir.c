#include "ir.h"

#include <assert.h>
#include <stdlib.h>  // malloc

#include "regalloc.h"
#include "table.h"
#include "util.h"
#include "x86_64.h"

#define WORK_REG_NO  (PHYSICAL_REG_MAX)

static void push_caller_save_regs(unsigned short living, int base);
static void pop_caller_save_regs(unsigned short living);

static VRegType vtVoidPtr = {.size = WORD_SIZE, .align = WORD_SIZE, .flag = 0};
static VRegType vtBool    = {.size = 4, .align = 4, .flag = 0};

int stackpos = 8;

static enum ConditionKind invert_cond(enum ConditionKind cond) {
  assert(COND_EQ <= cond && cond <= COND_UGT);
  if (cond <= COND_NE)
    return COND_NE + COND_EQ - cond;
  if (cond <= COND_ULT)
    return COND_LT + ((cond - COND_LT) ^ 2);
  return COND_ULT + ((cond - COND_ULT) ^ 2);
}

// Virtual register

VReg *new_vreg(int vreg_no, const VRegType *vtype, int flag) {
  VReg *vreg = malloc(sizeof(*vreg));
  vreg->virt = vreg_no;
  vreg->phys = -1;
  vreg->fixnum = 0;
  vreg->vtype = vtype;
  vreg->flag = flag;
  vreg->param_index = -1;
  vreg->offset = 0;
  return vreg;
}

// Register allocator

const char *kRegSizeTable[][7] = {
  { BL, R10B, R11B, R12B, R13B, R14B, R15B},
  { BX, R10W, R11W, R12W, R13W, R14W, R15W},
  {EBX, R10D, R11D, R12D, R13D, R14D, R15D},
  {RBX, R10,  R11,  R12,  R13,  R14,  R15},
};

#define kReg8s   (kRegSizeTable[0])
#define kReg32s  (kRegSizeTable[2])
#define kReg64s  (kRegSizeTable[3])

const char *kRegATable[] = {AL, AX, EAX, RAX};
const char *kRegDTable[] = {DL, DX, EDX, RDX};

#ifndef __NO_FLONUM
#define SZ_FLOAT   (4)
#define SZ_DOUBLE  (8)
const char *kFReg64s[7] = {XMM8, XMM9, XMM10, XMM11, XMM12, XMM13, XMM14};
#endif

#define CALLEE_SAVE_REG_COUNT  ((int)(sizeof(kCalleeSaveRegs) / sizeof(*kCalleeSaveRegs)))
const int kCalleeSaveRegs[] = {
  0,  // RBX
  3,  // R12
  4,  // R13
  5,  // R14
};

#define CALLER_SAVE_REG_COUNT  ((int)(sizeof(kCallerSaveRegs) / sizeof(*kCallerSaveRegs)))
const int kCallerSaveRegs[] = {
  1,  // R10
  2,  // R11
};

#ifndef __NO_FLONUM
#define CALLER_SAVE_FREG_COUNT  ((int)(sizeof(kCallerSaveFRegs) / sizeof(*kCallerSaveFRegs)))
const int kCallerSaveFRegs[] = {0, 1, 2, 3, 4, 5};
#endif

//
RegAlloc *curra;

// Intermediate Representation

static IR *new_ir(enum IrKind kind) {
  IR *ir = malloc(sizeof(*ir));
  ir->kind = kind;
  ir->dst = ir->opr1 = ir->opr2 = NULL;
  ir->size = -1;
  if (curbb != NULL)
    vec_push(curbb->irs, ir);
  return ir;
}

static const int kPow2Table[] = {-1, 0, 1, -1, 2, -1, -1, -1, 3};
#define kPow2TableSize  ((int)(sizeof(kPow2Table) / sizeof(*kPow2Table)))

VReg *new_const_vreg(intptr_t value, const VRegType *vtype) {
  VReg *vreg = reg_alloc_spawn(curra, vtype, VRF_CONST);
  vreg->fixnum = value;
  return vreg;
}

static intptr_t clamp_value(intptr_t value, const VRegType *vtype) {
  if (vtype->flag & VRTF_UNSIGNED) {
    switch (vtype->size) {
    case 1:  value = (unsigned char)value; break;
    case 2:  value = (unsigned short)value; break;
    case 4:  value = (unsigned int)value; break;
    default:  break;
    }
  } else {
    switch (vtype->size) {
    case 1:  value = (char)value; break;
    case 2:  value = (short)value; break;
    case 4:  value = (int)value; break;
    default:  break;
    }
  }
  return value;
}

VReg *new_ir_bop(enum IrKind kind, VReg *opr1, VReg *opr2, const VRegType *vtype) {
  if (opr1->flag & VRF_CONST) {
    if (opr2->flag & VRF_CONST) {
      intptr_t value = 0;
      switch (kind) {
      case IR_ADD:     value = opr1->fixnum + opr2->fixnum; break;
      case IR_SUB:     value = opr1->fixnum - opr2->fixnum; break;
      case IR_MUL:     value = opr1->fixnum * opr2->fixnum; break;

      case IR_DIV:
      case IR_DIVU:
      case IR_MOD:
      case IR_MODU:
        if (opr2->fixnum == 0)
          error("Divide by 0");
        switch (kind) {
        case IR_DIV:  value = opr1->fixnum / opr2->fixnum; break;
        case IR_DIVU: value = (uintptr_t)opr1->fixnum / opr2->fixnum; break;
        case IR_MOD:  value = opr1->fixnum / opr2->fixnum; break;
        case IR_MODU: value = (uintptr_t)opr1->fixnum / opr2->fixnum; break;
        default: assert(false); break;
        }
        break;

      case IR_BITAND:  value = opr1->fixnum & opr2->fixnum; break;
      case IR_BITOR:   value = opr1->fixnum | opr2->fixnum; break;
      case IR_BITXOR:  value = opr1->fixnum ^ opr2->fixnum; break;
      case IR_LSHIFT:  value = opr1->fixnum << opr2->fixnum; break;
      case IR_RSHIFT:
        //assert(opr1->type->kind == TY_FIXNUM);
        if (opr1->vtype->flag & VRTF_UNSIGNED)
          value = (uintptr_t)opr1->fixnum >> opr2->fixnum;
        else
          value = opr1->fixnum >> opr2->fixnum;
        break;
      default: assert(false); break;
      }
      return new_const_vreg(clamp_value(value, vtype), vtype);
    } else {
      switch (kind) {
      case IR_ADD:
      case IR_SUB:
        if (opr1->fixnum == 0)
          return opr2;
        break;
      case IR_MUL:
        if (opr1->fixnum == 1)
          return opr2;
        break;
      case IR_DIV:
      case IR_DIVU:
      case IR_MOD:
      case IR_MODU:
        if (opr1->fixnum == 0)
          return opr1;  // TODO: whether opr2 is zero.
        break;
      case IR_BITAND:
        if (opr1->fixnum == 0)
          return opr1;
        break;
      case IR_BITOR:
        if (opr1->fixnum == 0)
          return opr2;
        break;
      case IR_BITXOR:
        if (opr1->fixnum == 0)
          return opr2;
        break;
      case IR_LSHIFT:
      case IR_RSHIFT:
        if (opr1->fixnum == 0)
          return opr1;
        break;
      default:
        break;
      }
    }
  } else {
    if (opr2->flag & VRF_CONST) {
      switch (kind) {
      case IR_ADD:
      case IR_SUB:
        if (opr2->fixnum == 0)
          return opr1;
        break;
      case IR_MUL:
      case IR_DIV:
      case IR_DIVU:
        if (opr2->fixnum == 0)
          error("Divide by 0");
        if (opr2->fixnum == 1)
          return opr1;
        break;
      case IR_BITAND:
        if (opr2->fixnum == 0)
          return opr2;
        break;
      case IR_BITOR:
        if (opr2->fixnum == 0)
          return opr1;
        break;
      case IR_BITXOR:
        if (opr2->fixnum == 0)
          return opr1;
        break;
      case IR_LSHIFT:
      case IR_RSHIFT:
        if (opr2->fixnum == 0)
          return opr1;
        break;
      default:
        break;
      }
    }
  }

  IR *ir = new_ir(kind);
  ir->opr1 = opr1;
  ir->opr2 = opr2;
  ir->size = vtype->size;
  return ir->dst = reg_alloc_spawn(curra, vtype, 0);
}

VReg *new_ir_unary(enum IrKind kind, VReg *opr, const VRegType *vtype) {
  if (opr->flag & VRF_CONST) {
    intptr_t value = 0;
    switch (kind) {
    case IR_NEG:     value = -opr->fixnum; break;
    case IR_BITNOT:  value = ~opr->fixnum; break;
    default: assert(false); break;
    }
    return new_const_vreg(clamp_value(value, vtype), vtype);
  }

  IR *ir = new_ir(kind);
  ir->opr1 = opr;
  ir->size = vtype->size;
  return ir->dst = reg_alloc_spawn(curra, vtype, 0);
}

VReg *new_ir_bofs(VReg *src) {
  IR *ir = new_ir(IR_BOFS);
  ir->opr1 = src;
  ir->size = WORD_SIZE;
  return ir->dst = reg_alloc_spawn(curra, &vtVoidPtr, 0);
}

VReg *new_ir_iofs(const Name *label, bool global) {
  IR *ir = new_ir(IR_IOFS);
  ir->iofs.label = label;
  ir->iofs.global = global;
  ir->size = WORD_SIZE;
  return ir->dst = reg_alloc_spawn(curra, &vtVoidPtr, 0);
}

VReg *new_ir_sofs(VReg *src) {
  IR *ir = new_ir(IR_SOFS);
  ir->opr1 = src;
  ir->size = WORD_SIZE;
  return ir->dst = reg_alloc_spawn(curra, &vtVoidPtr, 0);
}

void new_ir_store(VReg *dst, VReg *src) {
  IR *ir = new_ir(IR_STORE);
  ir->opr1 = src;
  ir->size = src->vtype->size;
  ir->opr2 = dst;  // `dst` is used by indirect, so it is not actually `dst`.
}

void new_ir_cmp(VReg *opr1, VReg *opr2) {
  IR *ir = new_ir(IR_CMP);
  ir->opr1 = opr1;
  ir->opr2 = opr2;
  ir->size = opr1->vtype->size;
}

void new_ir_test(VReg *reg) {
  IR *ir = new_ir(IR_TEST);
  ir->opr1 = reg;
  ir->size = reg->vtype->size;
}

void new_ir_incdec(enum IrKind kind, VReg *reg, int size, intptr_t value) {
  IR *ir = new_ir(kind);
  ir->opr1 = reg;
  ir->size = size;
  ir->value = value;
}

VReg *new_ir_cond(enum ConditionKind cond) {
  IR *ir = new_ir(IR_COND);
  ir->cond.kind = cond;
  return ir->dst = reg_alloc_spawn(curra, &vtBool, 0);
}

void new_ir_jmp(enum ConditionKind cond, BB *bb) {
  if (cond == COND_NONE)
    return;
  IR *ir = new_ir(IR_JMP);
  ir->jmp.bb = bb;
  ir->jmp.cond = cond;
}

void new_ir_pusharg(VReg *vreg, const VRegType *vtype) {
  IR *ir = new_ir(IR_PUSHARG);
  ir->opr1 = vreg;
  ir->size = vtype->size;
}

IR *new_ir_precall(int arg_count, int stack_args_size) {
  IR *ir = new_ir(IR_PRECALL);
  ir->precall.arg_count = arg_count;
  ir->precall.stack_args_size = stack_args_size;
  ir->precall.stack_aligned = false;
  ir->precall.living_pregs = 0;
  return ir;
}

VReg *new_ir_call(const Name *label, bool global, VReg *freg, int reg_arg_count,
                  const VRegType *result_type, IR *precall, VRegType **arg_vtypes) {
  IR *ir = new_ir(IR_CALL);
  ir->call.label = label;
  ir->call.global = global;
  ir->opr1 = freg;
  ir->call.precall = precall;
  ir->call.reg_arg_count = reg_arg_count;
  ir->call.arg_vtypes = arg_vtypes;
  ir->size = result_type->size;
  return ir->dst = reg_alloc_spawn(curra, result_type, 0);
}

void new_ir_result(VReg *reg) {
  IR *ir = new_ir(IR_RESULT);
  ir->opr1 = reg;
  ir->size = reg->vtype->size;
}

void new_ir_addsp(int value) {
  IR *ir = new_ir(IR_ADDSP);
  ir->value = value;
}

VReg *new_ir_cast(VReg *vreg, const VRegType *dsttype) {
  IR *ir = new_ir(IR_CAST);
  ir->opr1 = vreg;
  ir->size = dsttype->size;
  return ir->dst = reg_alloc_spawn(curra, dsttype, 0);
}

void new_ir_mov(VReg *dst, VReg *src) {
  IR *ir = new_ir(IR_MOV);
  ir->dst = dst;
  ir->opr1 = src;
  ir->size = dst->vtype->size;
}

void new_ir_memcpy(VReg *dst, VReg *src, int size) {
  if (size > 0) {
    IR *ir = new_ir(IR_MEMCPY);
    ir->opr1 = src;
    ir->opr2 = dst;
    ir->size = size;
  }
}

void new_ir_clear(VReg *reg, size_t size) {
  IR *ir = new_ir(IR_CLEAR);
  ir->size = size;
  ir->opr1 = reg;
}

void new_ir_asm(const char *asm_) {
  IR *ir = new_ir(IR_ASM);
  ir->asm_.str = asm_;
}

IR *new_ir_load_spilled(VReg *reg, int offset, int size, int flag) {
  IR *ir = new_ir(IR_LOAD_SPILLED);
  ir->value = offset;
  ir->size = size;
  ir->dst = reg;
  ir->spill.flag = flag;
  return ir;
}

IR *new_ir_store_spilled(VReg *reg, int offset, int size, int flag) {
  IR *ir = new_ir(IR_STORE_SPILLED);
  ir->value = offset;
  ir->size = size;
  ir->opr1 = reg;
  ir->spill.flag = flag;
  return ir;
}

static void ir_memcpy(int dst_reg, int src_reg, ssize_t size) {
  const char *dst = kReg64s[dst_reg];
  const char *src = kReg64s[src_reg];

  // Break %rcx, %dl
  switch (size) {
  case 1:
    MOV(INDIRECT(src, NULL, 1), DL);
    MOV(DL, INDIRECT(dst, NULL, 1));
    break;
  case 2:
    MOV(INDIRECT(src, NULL, 1), DX);
    MOV(DX, INDIRECT(dst, NULL, 1));
    break;
  case 4:
    MOV(INDIRECT(src, NULL, 1), EDX);
    MOV(EDX, INDIRECT(dst, NULL, 1));
    break;
  case 8:
    MOV(INDIRECT(src, NULL, 1), RDX);
    MOV(RDX, INDIRECT(dst, NULL, 1));
    break;
  default:
    {
      const Name *name = alloc_label();
      const char *label = fmt_name(name);
      PUSH(src);
      MOV(IM(size), RCX);
      EMIT_LABEL(label);
      MOV(INDIRECT(src, NULL, 1), DL);
      MOV(DL, INDIRECT(dst, NULL, 1));
      INC(src);
      INC(dst);
      DEC(RCX);
      JNE(label);
      POP(src);
    }
    break;
  }
}

static void ir_out(IR *ir) {
  switch (ir->kind) {
  case IR_BOFS:
    assert(!(ir->opr1->flag & VRF_CONST));
    LEA(OFFSET_INDIRECT(ir->opr1->offset, RBP, NULL, 1), kReg64s[ir->dst->phys]);
    break;

  case IR_IOFS:
    {
      const char *label = fmt_name(ir->iofs.label);
      if (ir->iofs.global)
        label = MANGLE(label);
      LEA(LABEL_INDIRECT(label, RIP), kReg64s[ir->dst->phys]);
    }
    break;

  case IR_SOFS:
    assert(ir->opr1->flag & VRF_CONST);
    LEA(OFFSET_INDIRECT(ir->opr1->fixnum, RSP, NULL, 1), kReg64s[ir->dst->phys]);
    break;

  case IR_LOAD:
#ifndef __NO_FLONUM
    if (ir->dst->vtype->flag & VRTF_FLONUM) {
      switch (ir->size) {
      case SZ_FLOAT:   MOVSS(INDIRECT(kReg64s[ir->opr1->phys], NULL, 1), kFReg64s[ir->dst->phys]); break;
      case SZ_DOUBLE:  MOVSD(INDIRECT(kReg64s[ir->opr1->phys], NULL, 1), kFReg64s[ir->dst->phys]); break;
      default: assert(false); break;
      }
      break;
    }
#endif
    {
      assert(!(ir->opr1->flag & VRF_CONST));
      assert(0 <= ir->size && ir->size < kPow2TableSize);
      int pow = kPow2Table[ir->size];
      assert(0 <= pow && pow < 4);
      const char **regs = kRegSizeTable[pow];
      MOV(INDIRECT(kReg64s[ir->opr1->phys], NULL, 1), regs[ir->dst->phys]);
    }
    break;

  case IR_STORE:
#ifndef __NO_FLONUM
    if (ir->opr1->vtype->flag & VRTF_FLONUM) {
      switch (ir->size) {
      case SZ_FLOAT:   MOVSS(kFReg64s[ir->opr1->phys], INDIRECT(kReg64s[ir->opr2->phys], NULL, 1)); break;
      case SZ_DOUBLE:  MOVSD(kFReg64s[ir->opr1->phys], INDIRECT(kReg64s[ir->opr2->phys], NULL, 1)); break;
      default: assert(false); break;
      }
      break;
    }
#endif
    {
      assert(!(ir->opr1->flag & VRF_CONST));
      assert(!(ir->opr2->flag & VRF_CONST));
      assert(0 <= ir->size && ir->size < kPow2TableSize);
      int pow = kPow2Table[ir->size];
      assert(0 <= pow && pow < 4);
      const char **regs = kRegSizeTable[pow];
      MOV(regs[ir->opr1->phys], INDIRECT(kReg64s[ir->opr2->phys], NULL, 1));
    }
    break;

  case IR_ADD:
    {
      assert(ir->dst->phys == ir->opr1->phys);
#ifndef __NO_FLONUM
      if (ir->dst->vtype->flag & VRTF_FLONUM) {
        const char **regs = kFReg64s;
        switch (ir->size) {
        case SZ_FLOAT:   ADDSS(regs[ir->opr2->phys], regs[ir->dst->phys]); break;
        case SZ_DOUBLE:  ADDSD(regs[ir->opr2->phys], regs[ir->dst->phys]); break;
        default: assert(false); break;
        }
        break;
      }
#endif
      assert(!(ir->opr1->flag & VRF_CONST));
      assert(0 <= ir->size && ir->size < kPow2TableSize);
      int pow = kPow2Table[ir->size];
      assert(0 <= pow && pow < 4);
      const char **regs = kRegSizeTable[pow];
      if (ir->opr2->flag & VRF_CONST)
        ADD(im(ir->opr2->fixnum), regs[ir->dst->phys]);
      else
        ADD(regs[ir->opr2->phys], regs[ir->dst->phys]);
    }
    break;

  case IR_SUB:
    {
      assert(ir->dst->phys == ir->opr1->phys);
#ifndef __NO_FLONUM
      if (ir->dst->vtype->flag & VRTF_FLONUM) {
        const char **regs = kFReg64s;
        switch (ir->size) {
        case SZ_FLOAT:   SUBSS(regs[ir->opr2->phys], regs[ir->dst->phys]); break;
        case SZ_DOUBLE:  SUBSD(regs[ir->opr2->phys], regs[ir->dst->phys]); break;
        default: assert(false); break;
        }
        break;
      }
#endif
      assert(!(ir->opr1->flag & VRF_CONST));
      assert(0 <= ir->size && ir->size < kPow2TableSize);
      int pow = kPow2Table[ir->size];
      assert(0 <= pow && pow < 4);
      const char **regs = kRegSizeTable[pow];
      if (ir->opr2->flag & VRF_CONST)
        SUB(im(ir->opr2->fixnum), regs[ir->dst->phys]);
      else
        SUB(regs[ir->opr2->phys], regs[ir->dst->phys]);
    }
    break;

  case IR_MUL:
    {
#ifndef __NO_FLONUM
      if (ir->dst->vtype->flag & VRTF_FLONUM) {
        assert(ir->dst->phys == ir->opr1->phys);
        const char **regs = kFReg64s;
        switch (ir->size) {
        case SZ_FLOAT:   MULSS(regs[ir->opr2->phys], regs[ir->dst->phys]); break;
        case SZ_DOUBLE:  MULSD(regs[ir->opr2->phys], regs[ir->dst->phys]); break;
        default: assert(false); break;
        }
        break;
      }
#endif
      assert(0 <= ir->size && ir->size < kPow2TableSize);
      assert(!(ir->opr1->flag & VRF_CONST));
      int pow = kPow2Table[ir->size];
      assert(0 <= pow && pow < 4);
      const char **regs = kRegSizeTable[pow];
      const char *a = kRegATable[pow];
      MOV(regs[ir->opr1->phys], a);
      const char *opr2;
      if (ir->opr2->flag & VRF_CONST) {
        MOV(im(ir->opr2->fixnum), regs[WORK_REG_NO]);
        opr2 = regs[WORK_REG_NO];
      } else {
        opr2 = regs[ir->opr2->phys];
      }
      MUL(opr2);
      MOV(a, regs[ir->dst->phys]);
    }
    break;

  case IR_DIV:
  case IR_DIVU:
    assert(!(ir->opr1->flag & VRF_CONST));
#ifndef __NO_FLONUM
    if (ir->dst->vtype->flag & VRTF_FLONUM) {
      assert(ir->dst->phys == ir->opr1->phys);
      const char **regs = kFReg64s;
      switch (ir->size) {
      case SZ_FLOAT:   DIVSS(regs[ir->opr2->phys], regs[ir->dst->phys]); break;
      case SZ_DOUBLE:  DIVSD(regs[ir->opr2->phys], regs[ir->dst->phys]); break;
      default: assert(false); break;
      }
      break;
    }
#endif
    if (ir->size == 1) {
      if (ir->kind == IR_DIV) {
        MOVSX(kReg8s[ir->opr1->phys], AX);
        const char *opr2;
        if (ir->opr2->flag & VRF_CONST) {
          opr2 = kReg8s[WORK_REG_NO];
          MOV(im(ir->opr2->fixnum), opr2);
        } else {
          opr2 = kReg8s[ir->opr2->phys];
        }
        IDIV(opr2);
      } else {
        MOVZX(kReg8s[ir->opr1->phys], AX);
        const char *opr2;
        if (ir->opr2->flag & VRF_CONST) {
          opr2 = kReg8s[WORK_REG_NO];
          MOV(im(ir->opr2->fixnum), opr2);
        } else {
          opr2 = kReg8s[ir->opr2->phys];
        }
        DIV(opr2);
      }
      MOV(AL, kReg8s[ir->dst->phys]);
    } else {
      assert(0 <= ir->size && ir->size < kPow2TableSize);
      int pow = kPow2Table[ir->size];
      assert(0 <= pow && pow < 4);
      const char **regs = kRegSizeTable[pow];
      const char *a = kRegATable[pow];
      MOV(regs[ir->opr1->phys], a);
      const char *opr2;
      if (ir->opr2->flag & VRF_CONST) {
        opr2 = regs[WORK_REG_NO];
        MOV(im(ir->opr2->fixnum), opr2);
      } else {
        opr2 = regs[ir->opr2->phys];
      }
      if (ir->kind == IR_DIV) {
        switch (pow) {
        case 1:  CWTL(); break;
        case 2:  CLTD(); break;
        case 3:  CQTO(); break;
        default: assert(false); break;
        }
        IDIV(opr2);
      } else {
        switch (pow) {
        case 1:  XOR(DX, DX); break;
        case 2:  XOR(EDX, EDX); break;
        case 3:  XOR(EDX, EDX); break;  // Clear 64bit register.
        default: assert(false); break;
        }
        DIV(opr2);
      }
      MOV(a, regs[ir->dst->phys]);
    }
    break;

  case IR_MOD:
  case IR_MODU:
    assert(!(ir->opr1->flag & VRF_CONST));
    if (ir->size == 1) {
      if (ir->kind == IR_MOD) {
        MOVSX(kReg8s[ir->opr1->phys], AX);
        const char *opr2;
        if (ir->opr2->flag & VRF_CONST) {
          opr2 = kReg8s[WORK_REG_NO];
          MOV(im(ir->opr2->fixnum), opr2);
        } else {
          opr2 = kReg8s[ir->opr2->phys];
        }
        IDIV(opr2);
      } else {
        MOVZX(kReg8s[ir->opr1->phys], AX);
        const char *opr2;
        if (ir->opr2->flag & VRF_CONST) {
          opr2 = kReg8s[WORK_REG_NO];
          MOV(im(ir->opr2->fixnum), opr2);
        } else {
          opr2 = kReg8s[ir->opr2->phys];
        }
        DIV(opr2);
      }
      MOV(AH, kReg8s[ir->dst->phys]);
    } else {
      assert(0 <= ir->size && ir->size < kPow2TableSize);
      int pow = kPow2Table[ir->size];
      assert(0 <= pow && pow < 4);
      const char **regs = kRegSizeTable[pow];
      const char *a = kRegATable[pow];
      const char *d = kRegDTable[pow];
      MOV(regs[ir->opr1->phys], a);
      const char *opr2;
      if (ir->opr2->flag & VRF_CONST) {
        opr2 = regs[WORK_REG_NO];
        MOV(im(ir->opr2->fixnum), opr2);
      } else {
        opr2 = regs[ir->opr2->phys];
      }
      if (ir->kind == IR_MOD) {
        switch (pow) {
        case 1:  CWTL(); break;
        case 2:  CLTD(); break;
        case 3:  CQTO(); break;
        default: assert(false); break;
        }
        IDIV(opr2);
      } else {
        switch (pow) {
        case 1:  XOR(DX, DX); break;
        case 2:  XOR(EDX, EDX); break;
        case 3:  XOR(EDX, EDX); break;  // Clear 64bit register.
        default: assert(false); break;
        }
        DIV(opr2);
      }
      MOV(d, regs[ir->dst->phys]);
    }
    break;

  case IR_BITAND:
    {
      assert(ir->dst->phys == ir->opr1->phys);
      assert(!(ir->opr1->flag & VRF_CONST));
      assert(0 <= ir->size && ir->size < kPow2TableSize);
      int pow = kPow2Table[ir->size];
      assert(0 <= pow && pow < 4);
      const char **regs = kRegSizeTable[pow];
      if (ir->opr2->flag & VRF_CONST)
        AND(im(ir->opr2->fixnum), regs[ir->dst->phys]);
      else
        AND(regs[ir->opr2->phys], regs[ir->dst->phys]);
    }
    break;

  case IR_BITOR:
    {
      assert(ir->dst->phys == ir->opr1->phys);
      assert(!(ir->opr1->flag & VRF_CONST));
      assert(0 <= ir->size && ir->size < kPow2TableSize);
      int pow = kPow2Table[ir->size];
      assert(0 <= pow && pow < 4);
      const char **regs = kRegSizeTable[pow];
      if (ir->opr2->flag & VRF_CONST)
        OR(im(ir->opr2->fixnum), regs[ir->dst->phys]);
      else
        OR(regs[ir->opr2->phys], regs[ir->dst->phys]);
    }
    break;

  case IR_BITXOR:
    {
      assert(ir->dst->phys == ir->opr1->phys);
      assert(!(ir->opr1->flag & VRF_CONST));
      assert(0 <= ir->size && ir->size < kPow2TableSize);
      int pow = kPow2Table[ir->size];
      assert(0 <= pow && pow < 4);
      const char **regs = kRegSizeTable[pow];
      if (ir->opr2->flag & VRF_CONST)
        XOR(im(ir->opr2->fixnum), regs[ir->dst->phys]);
      else
        XOR(regs[ir->opr2->phys], regs[ir->dst->phys]);
    }
    break;

  case IR_LSHIFT:
    {
      assert(ir->dst->phys == ir->opr1->phys);
      assert(!(ir->opr1->flag & VRF_CONST));
      assert(0 <= ir->size && ir->size < kPow2TableSize);
      int pow = kPow2Table[ir->size];
      assert(0 <= pow && pow < 4);
      const char **regs = kRegSizeTable[pow];
      if (ir->opr2->flag & VRF_CONST) {
        SHL(im(ir->opr2->fixnum), regs[ir->dst->phys]);
      } else {
        MOV(kReg8s[ir->opr2->phys], CL);
        SHL(CL, regs[ir->dst->phys]);
      }
    }
    break;
  case IR_RSHIFT:
    {
      assert(ir->dst->phys == ir->opr1->phys);
      assert(!(ir->opr1->flag & VRF_CONST));
      assert(0 <= ir->size && ir->size < kPow2TableSize);
      int pow = kPow2Table[ir->size];
      assert(0 <= pow && pow < 4);
      const char **regs = kRegSizeTable[pow];
      if (ir->opr1->vtype->flag & VRTF_UNSIGNED) {
        if (ir->opr2->flag & VRF_CONST) {
          SHR(im(ir->opr2->fixnum), regs[ir->dst->phys]);
        } else {
          MOV(kReg8s[ir->opr2->phys], CL);
          SHR(CL, regs[ir->dst->phys]);
        }
      } else {
        if (ir->opr2->flag & VRF_CONST) {
          SAR(im(ir->opr2->fixnum), regs[ir->dst->phys]);
        } else {
          MOV(kReg8s[ir->opr2->phys], CL);
          SAR(CL, regs[ir->dst->phys]);
        }
      }
    }
    break;

  case IR_CMP:
    {
#ifndef __NO_FLONUM
      if (ir->opr1->vtype->flag & VRTF_FLONUM) {
        assert(ir->opr2->vtype->flag & VRTF_FLONUM);
        switch (ir->size) {
        case SZ_FLOAT:   UCOMISS(kFReg64s[ir->opr2->phys], kFReg64s[ir->opr1->phys]); break;
        case SZ_DOUBLE:  UCOMISD(kFReg64s[ir->opr2->phys], kFReg64s[ir->opr1->phys]); break;
        default: assert(false); break;
        }
        break;
      }
#endif
      assert(0 <= ir->size && ir->size < kPow2TableSize);
      int pow = kPow2Table[ir->size];
      assert(0 <= pow && pow < 4);
      const char **regs = kRegSizeTable[pow];
      const char *opr1;
      if (ir->opr1->flag & VRF_CONST) {
        opr1 = kRegATable[pow];
        MOV(im(ir->opr1->fixnum), opr1);
      } else {
        opr1 = regs[ir->opr1->phys];
      }
      const char *opr2 = (ir->opr2->flag & VRF_CONST) ? im(ir->opr2->fixnum) : regs[ir->opr2->phys];
      CMP(opr2, opr1);
    }
    break;

  case IR_INC:
    {
      assert(!(ir->opr1->flag & VRF_CONST));
      const char *reg = INDIRECT(kReg64s[ir->opr1->phys], NULL, 1);
      if (ir->value == 1) {
        switch (ir->size) {
        case 1:  INCB(reg); break;
        case 2:  INCW(reg); break;
        case 4:  INCL(reg); break;
        case 8:  INCQ(reg); break;
        default:  assert(false); break;
        }
      } else {
        assert(ir->size == 8);
        intptr_t value = ir->value;
        if (value <= ((1L << 31) - 1)) {
          ADDQ(IM(value), reg);
        } else {
          MOV(IM(value), RAX);
          ADD(RAX, reg);
        }
      }
    }
    break;

  case IR_DEC:
    {
      assert(!(ir->opr1->flag & VRF_CONST));
      const char *reg = INDIRECT(kReg64s[ir->opr1->phys], NULL, 1);
      if (ir->value == 1) {
        switch (ir->size) {
        case 1:  DECB(reg); break;
        case 2:  DECW(reg); break;
        case 4:  DECL(reg); break;
        case 8:  DECQ(reg); break;
        default:  assert(false); break;
        }
      } else {
        assert(ir->size == 8);
        intptr_t value = ir->value;
        if (value <= ((1L << 31) - 1)) {
          SUBQ(IM(value), reg);
        } else {
          MOV(IM(value), RAX);
          SUB(RAX, reg);
        }
      }
    }
    break;

  case IR_NEG:
    {
      assert(ir->dst->phys == ir->opr1->phys);
      assert(!(ir->dst->flag & VRF_CONST));
      assert(0 <= ir->size && ir->size < kPow2TableSize);
      int pow = kPow2Table[ir->size];
      assert(0 <= pow && pow < 4);
      const char **regs = kRegSizeTable[pow];
      NEG(regs[ir->dst->phys]);
    }
    break;

  case IR_BITNOT:
    {
      assert(ir->dst->phys == ir->opr1->phys);
      assert(!(ir->dst->flag & VRF_CONST));
      assert(0 <= ir->size && ir->size < kPow2TableSize);
      int pow = kPow2Table[ir->size];
      assert(0 <= pow && pow < 4);
      const char **regs = kRegSizeTable[pow];
      NOT(regs[ir->dst->phys]);
    }
    break;

  case IR_COND:
    {
      assert(!(ir->dst->flag & VRF_CONST));
      const char *dst = kReg8s[ir->dst->phys];
      switch (ir->cond.kind) {
      case COND_EQ:  SETE(dst); break;
      case COND_NE:  SETNE(dst); break;
      case COND_LT:  SETL(dst); break;
      case COND_GT:  SETG(dst); break;
      case COND_LE:  SETLE(dst); break;
      case COND_GE:  SETGE(dst); break;
      case COND_ULT: SETB(dst); break;
      case COND_UGT: SETA(dst); break;
      case COND_ULE: SETBE(dst); break;
      case COND_UGE: SETAE(dst); break;
      default: assert(false); break;
      }
      MOVSX(dst, kReg32s[ir->dst->phys]);  // Assume bool is 4 byte.
    }
    break;

  case IR_TEST:
    {
      assert(0 <= ir->size && ir->size < kPow2TableSize);
      int pow = kPow2Table[ir->size];
      assert(0 <= pow && pow < 4);
      const char **regs = kRegSizeTable[pow];
      const char *opr1;
      if (ir->opr1->flag & VRF_CONST) {
        opr1 = kRegATable[pow];
        MOV(im(ir->opr1->fixnum), opr1);
      } else {
        opr1 = regs[ir->opr1->phys];
      }
      TEST(opr1, opr1);
    }
    break;

  case IR_JMP:
    switch (ir->jmp.cond) {
    case COND_ANY:  JMP(fmt_name(ir->jmp.bb->label)); break;
    case COND_EQ:   JE(fmt_name(ir->jmp.bb->label)); break;
    case COND_NE:   JNE(fmt_name(ir->jmp.bb->label)); break;
    case COND_LT:   JL(fmt_name(ir->jmp.bb->label)); break;
    case COND_GT:   JG(fmt_name(ir->jmp.bb->label)); break;
    case COND_LE:   JLE(fmt_name(ir->jmp.bb->label)); break;
    case COND_GE:   JGE(fmt_name(ir->jmp.bb->label)); break;
    case COND_ULT:  JB(fmt_name(ir->jmp.bb->label)); break;
    case COND_UGT:  JA(fmt_name(ir->jmp.bb->label)); break;
    case COND_ULE:  JBE(fmt_name(ir->jmp.bb->label)); break;
    case COND_UGE:  JAE(fmt_name(ir->jmp.bb->label)); break;
    default:  assert(false); break;
    }
    break;

  case IR_PRECALL:
    {
      // Make room for caller save.
      int add = 0;
      unsigned short living_pregs = ir->precall.living_pregs;
      for (int i = 0; i < CALLER_SAVE_REG_COUNT; ++i) {
        int ireg = kCallerSaveRegs[i];
        if (living_pregs & (1 << ireg))
          add += WORD_SIZE;
      }
#ifndef __NO_FLONUM
      for (int i = 0; i < CALLER_SAVE_FREG_COUNT; ++i) {
        int freg = kCallerSaveFRegs[i];
        if (living_pregs & (1 << (freg + PHYSICAL_REG_MAX)))
          add += WORD_SIZE;
      }
#endif

      int align_stack = (16 - (stackpos + add + ir->precall.stack_args_size)) & 15;
      ir->precall.stack_aligned = align_stack;
      add += align_stack;

      if (add > 0) {
        SUB(IM(add), RSP); stackpos += add;
      }
    }
    break;

  case IR_PUSHARG:
#ifndef __NO_FLONUM
    if (ir->opr1->vtype->flag & VRTF_FLONUM) {
      SUB(IM(WORD_SIZE), RSP); PUSH_STACK_POS();
      switch (ir->opr1->vtype->size) {
      case SZ_FLOAT:   MOVSS(kFReg64s[ir->opr1->phys], INDIRECT(RSP, NULL, 1)); break;
      case SZ_DOUBLE:  MOVSD(kFReg64s[ir->opr1->phys], INDIRECT(RSP, NULL, 1)); break;
      default: assert(false); break;
      }
      break;
    }
#endif
    if (ir->opr1->flag & VRF_CONST) {
      if (is_im32(ir->opr1->fixnum)) {
        PUSH(im(ir->opr1->fixnum)); PUSH_STACK_POS();
      } else {
        MOV(im(ir->opr1->fixnum), RAX);  // TODO: Check.
        PUSH(RAX); PUSH_STACK_POS();
      }
    } else {
      PUSH(kReg64s[ir->opr1->phys]); PUSH_STACK_POS();
    }
    break;

  case IR_CALL:
    {
      int reg_args = ir->call.reg_arg_count;
      push_caller_save_regs(ir->call.precall->precall.living_pregs, reg_args * WORD_SIZE + ir->call.precall->precall.stack_args_size + ir->call.precall->precall.stack_aligned);

      static const char *kArgReg64s[] = {RDI, RSI, RDX, RCX, R8, R9};
#ifndef __NO_FLONUM
      static const char *kArgFReg64s[] = {XMM0, XMM1, XMM2, XMM3, XMM4, XMM5, XMM6, XMM7};
      int freg = 0;
#endif

      // Pop register arguments.
      int ireg = 0;
      for (int i = 0; i < reg_args; ++i) {
#ifndef __NO_FLONUM
        if (ir->call.arg_vtypes[i]->flag & VRTF_FLONUM) {
          switch (ir->call.arg_vtypes[i]->size) {
          case SZ_FLOAT:   MOVSS(INDIRECT(RSP, NULL, 1), kArgFReg64s[freg]); break;
          case SZ_DOUBLE:  MOVSD(INDIRECT(RSP, NULL, 1), kArgFReg64s[freg]); break;
          default: assert(false); break;
          }
          ++freg;
          ADD(IM(WORD_SIZE), RSP); POP_STACK_POS();
          continue;
        }
#endif
        POP(kArgReg64s[ireg++]); POP_STACK_POS();
      }

      if (ir->call.label != NULL) {
        const char *label = fmt_name(ir->call.label);
        if (ir->call.global)
          CALL(MANGLE(label));
        else
          CALL(label);
      } else {
        assert(!(ir->opr1->flag & VRF_CONST));
        CALL(fmt("*%s", kReg64s[ir->opr1->phys]));
      }

      int align_stack = ir->call.precall->precall.stack_aligned + ir->call.precall->precall.stack_args_size;
      if (align_stack != 0) {
        ADD(IM(align_stack), RSP);
        stackpos -= align_stack;
      }

      // Resore caller save registers.
      pop_caller_save_regs(ir->call.precall->precall.living_pregs);

#ifndef __NO_FLONUM
      if (ir->dst->vtype->flag & VRTF_FLONUM) {
        switch (ir->size) {
        case SZ_FLOAT:   MOVSS(XMM0, kFReg64s[ir->dst->phys]); break;
        case SZ_DOUBLE:  MOVSD(XMM0, kFReg64s[ir->dst->phys]); break;
        default: assert(false); break;
        }
        break;
      }
#endif
      assert(0 <= ir->size && ir->size < kPow2TableSize);
      if (ir->size > 0) {
        int pow = kPow2Table[ir->size];
        assert(0 <= pow && pow < 4);
        const char **regs = kRegSizeTable[pow];
        MOV(kRegATable[pow], regs[ir->dst->phys]);
      }
    }
    break;

  case IR_RESULT:
#ifndef __NO_FLONUM
    if (ir->opr1->vtype->flag & VRTF_FLONUM) {
      switch (ir->size) {
      case SZ_FLOAT:   MOVSS(kFReg64s[ir->opr1->phys], XMM0); break;
      case SZ_DOUBLE:  MOVSD(kFReg64s[ir->opr1->phys], XMM0); break;
      default: assert(false); break;
      }
      break;
    }
#endif
    {
      assert(0 <= ir->size && ir->size < kPow2TableSize);
      int pow = kPow2Table[ir->size];
      assert(0 <= pow && pow < 4);
      const char **regs = kRegSizeTable[pow];
      if (ir->opr1->flag & VRF_CONST)
        MOV(im(ir->opr1->fixnum), kRegATable[pow]);
      else
        MOV(regs[ir->opr1->phys], kRegATable[pow]);
    }
    break;

  case IR_ADDSP:
    if (ir->value > 0)
      ADD(IM(ir->value), RSP);
    else if (ir->value < 0)
      SUB(IM(-ir->value), RSP);
    stackpos -= ir->value;
    break;

  case IR_CAST:
    assert((ir->opr1->flag & VRF_CONST) == 0);
#ifndef __NO_FLONUM
    if (ir->dst->vtype->flag & VRTF_FLONUM) {
      if (ir->opr1->vtype->flag & VRTF_FLONUM) {
        // flonum->flonum
        // Assume flonum are just two types.
        switch (ir->size) {
        case SZ_FLOAT:   CVTSD2SS(kFReg64s[ir->opr1->phys], kFReg64s[ir->dst->phys]); break;
        case SZ_DOUBLE:  CVTSS2SD(kFReg64s[ir->opr1->phys], kFReg64s[ir->dst->phys]); break;
        default:  assert(false); break;
        }
      } else {
        // fix->flonum
        assert(0 <= ir->opr1->vtype->size && ir->opr1->vtype->size < kPow2TableSize);
        int pows = kPow2Table[ir->opr1->vtype->size];
        if (pows < 2) {
          if (ir->opr1->vtype->flag & VRTF_UNSIGNED)
            MOVZX(kRegSizeTable[pows][ir->opr1->phys], kRegSizeTable[2][ir->opr1->phys]);
          else
            MOVSX(kRegSizeTable[pows][ir->opr1->phys], kRegSizeTable[2][ir->opr1->phys]);
          pows = 2;
        }
        switch (ir->size) {
        case SZ_FLOAT:   CVTSI2SS(kRegSizeTable[pows][ir->opr1->phys], kFReg64s[ir->dst->phys]); break;
        case SZ_DOUBLE:  CVTSI2SD(kRegSizeTable[pows][ir->opr1->phys], kFReg64s[ir->dst->phys]); break;
        default:  assert(false); break;
        }
      }
      break;
    } else if (ir->opr1->vtype->flag & VRTF_FLONUM) {
      // flonum->fix
      int powd = kPow2Table[ir->dst->vtype->size];
      switch (ir->opr1->vtype->size) {
      case SZ_FLOAT:   CVTTSS2SI(kFReg64s[ir->opr1->phys], kRegSizeTable[powd][ir->dst->phys]); break;
      case SZ_DOUBLE:  CVTTSD2SI(kFReg64s[ir->opr1->phys], kRegSizeTable[powd][ir->dst->phys]); break;
      default:  assert(false); break;
      }
      break;
    }
#endif
    if (ir->size <= ir->opr1->vtype->size) {
      if (ir->dst->phys != ir->opr1->phys) {
        assert(0 <= ir->size && ir->size < kPow2TableSize);
        int pow = kPow2Table[ir->size];
        assert(0 <= pow && pow < 4);
        const char **regs = kRegSizeTable[pow];
        MOV(regs[ir->opr1->phys], regs[ir->dst->phys]);
      }
    } else {
      assert(0 <= ir->opr1->vtype->size && ir->opr1->vtype->size < kPow2TableSize);
      int pows = kPow2Table[ir->opr1->vtype->size];
      assert(0 <= ir->size && ir->size < kPow2TableSize);
      int powd = kPow2Table[ir->size];
      assert(0 <= pows && pows < 4);
      assert(0 <= powd && powd < 4);
      if (ir->opr1->vtype->flag & VRTF_UNSIGNED) {
        if (pows == 2 && powd == 3) {
          // MOVZX %64bit, %32bit doesn't exist!
          MOV(kRegSizeTable[pows][ir->opr1->phys], kRegSizeTable[pows][ir->dst->phys]);
        } else {
          MOVZX(kRegSizeTable[pows][ir->opr1->phys], kRegSizeTable[powd][ir->dst->phys]);
        }
      } else {
        MOVSX(kRegSizeTable[pows][ir->opr1->phys], kRegSizeTable[powd][ir->dst->phys]);
      }
    }
    break;

  case IR_MOV:
    {
#ifndef __NO_FLONUM
      if (ir->dst->vtype->flag & VRTF_FLONUM) {
        if (ir->opr1->phys != ir->dst->phys) {
          switch (ir->size) {
          case SZ_FLOAT:   MOVSS(kFReg64s[ir->opr1->phys], kFReg64s[ir->dst->phys]); break;
          case SZ_DOUBLE:  MOVSD(kFReg64s[ir->opr1->phys], kFReg64s[ir->dst->phys]); break;
          default: assert(false); break;
          }
          break;
        }
      }
#endif
      assert(0 <= ir->size && ir->size < kPow2TableSize);
      assert(!(ir->dst->flag & VRF_CONST));
      int pow = kPow2Table[ir->size];
      assert(0 <= pow && pow < 4);
      const char **regs = kRegSizeTable[pow];
      if (ir->opr1->flag & VRF_CONST) {
        MOV(im(ir->opr1->fixnum), regs[ir->dst->phys]);
      } else {
        if (ir->opr1->phys != ir->dst->phys)
          MOV(regs[ir->opr1->phys], regs[ir->dst->phys]);
      }
    }
    break;

  case IR_MEMCPY:
    assert(!(ir->opr1->flag & VRF_CONST));
    assert(!(ir->opr2->flag & VRF_CONST));
    ir_memcpy(ir->opr2->phys, ir->opr1->phys, ir->size);
    break;

  case IR_CLEAR:
    {
      assert(!(ir->opr1->flag & VRF_CONST));
      const char *loop = fmt_name(alloc_label());
      MOV(kReg64s[ir->opr1->phys], RSI);
      MOV(IM(ir->size), EDI);
      XOR(AL, AL);
      EMIT_LABEL(loop);
      MOV(AL, INDIRECT(RSI, NULL, 1));
      INC(RSI);
      DEC(EDI);
      JNE(loop);
    }
    break;

  case IR_ASM:
    EMIT_ASM0(ir->asm_.str);
    break;

  case IR_LOAD_SPILLED:
#ifndef __NO_FLONUM
    if (ir->spill.flag & VRTF_FLONUM) {
      const char **regs = kFReg64s;
      switch (ir->size) {
      case SZ_FLOAT:   MOVSS(OFFSET_INDIRECT(ir->value, RBP, NULL, 1), regs[WORK_REG_NO]); break;
      case SZ_DOUBLE:  MOVSD(OFFSET_INDIRECT(ir->value, RBP, NULL, 1), regs[WORK_REG_NO]); break;
      default: assert(false); break;
      }
      break;
    }
#endif
    {
      assert(0 <= ir->size && ir->size < kPow2TableSize);
      int pow = kPow2Table[ir->size];
      assert(0 <= pow && pow < 4);
      const char **regs = kRegSizeTable[pow];
      MOV(OFFSET_INDIRECT(ir->value, RBP, NULL, 1), regs[WORK_REG_NO]);
    }
    break;

  case IR_STORE_SPILLED:
#ifndef __NO_FLONUM
    if (ir->spill.flag & VRTF_FLONUM) {
      const char **regs = kFReg64s;
      switch (ir->size) {
      case SZ_FLOAT:   MOVSS(regs[WORK_REG_NO], OFFSET_INDIRECT(ir->value, RBP, NULL, 1)); break;
      case SZ_DOUBLE:  MOVSD(regs[WORK_REG_NO], OFFSET_INDIRECT(ir->value, RBP, NULL, 1)); break;
      default: assert(false); break;
      }
      break;
    }
#endif
    {
      assert(0 <= ir->size && ir->size < kPow2TableSize);
      int pow = kPow2Table[ir->size];
      assert(0 <= pow && pow < 4);
      const char **regs = kRegSizeTable[pow];
      MOV(regs[WORK_REG_NO], OFFSET_INDIRECT(ir->value, RBP, NULL, 1));
    }
    break;

  default:
    assert(false);
    break;
  }
}

// Basic Block

BB *curbb;

BB *new_bb(void) {
  BB *bb = malloc(sizeof(*bb));
  bb->next = NULL;
  bb->label = alloc_label();
  bb->irs = new_vector();
  bb->in_regs = NULL;
  bb->out_regs = NULL;
  bb->assigned_regs = NULL;
  return bb;
}

//

BBContainer *new_func_blocks(void) {
  BBContainer *bbcon = malloc(sizeof(*bbcon));
  bbcon->bbs = new_vector();
  return bbcon;
}

static IR *is_last_jmp(BB *bb) {
  int len;
  IR *ir;
  if ((len = bb->irs->len) > 0 &&
      (ir = bb->irs->data[len - 1])->kind == IR_JMP)
    return ir;
  return NULL;
}

static IR *is_last_any_jmp(BB *bb) {
  IR *ir = is_last_jmp(bb);
  return ir != NULL && ir->jmp.cond == COND_ANY ? ir : NULL;
}

static void replace_jmp_destination(BBContainer *bbcon, BB *src, BB *dst) {
  Vector *bbs = bbcon->bbs;
  for (int j = 0; j < bbs->len; ++j) {
    BB *bb = bbs->data[j];
    if (bb == src)
      continue;

    IR *ir = is_last_jmp(bb);
    if (ir != NULL && ir->jmp.bb == src)
      ir->jmp.bb = dst;
  }
}

void remove_unnecessary_bb(BBContainer *bbcon) {
  Vector *bbs = bbcon->bbs;
  for (;;) {
    bool again = false;
    for (int i = 0; i < bbs->len - 1; ++i) {  // Make last one keeps alive.
      BB *bb = bbs->data[i];
      IR *ir;
      if (bb->irs->len == 0) {  // Empty BB.
        replace_jmp_destination(bbcon, bb, bb->next);
      } else if (bb->irs->len == 1 && (ir = is_last_any_jmp(bb)) != NULL && !equal_name(bb->label, ir->jmp.bb->label)) {  // jmp only.
        replace_jmp_destination(bbcon, bb, ir->jmp.bb);
        if (i == 0)
          continue;
        BB *pbb = bbs->data[i - 1];
        if (!is_last_jmp(pbb))
          continue;
        if (!is_last_any_jmp(pbb)) {  // Fallthrough pass exists.
          IR *ir0 = pbb->irs->data[pbb->irs->len - 1];
          if (ir0->jmp.bb != bb->next)  // Non skip jmp: Keep bb connection.
            continue;
          // Invert prev jmp condition and change jmp destination.
          ir0->jmp.cond = invert_cond(ir0->jmp.cond);
          ir0->jmp.bb = ir->jmp.bb;
        }
      } else {
        continue;
      }

      if (i > 0) {
        BB *pbb = bbs->data[i - 1];
        pbb->next = bb->next;
      }

      vec_remove_at(bbs, i);
      --i;
      again = true;
    }
    if (!again)
      break;
  }

  // Remove jmp to next instruction.
  for (int i = 0; i < bbs->len - 1; ++i) {  // Make last one keeps alive.
    BB *bb = bbs->data[i];
    IR *ir = is_last_any_jmp(bb);
    if (ir != NULL && ir->jmp.bb == bb->next)
      vec_pop(bb->irs);
  }
}

void push_callee_save_regs(unsigned short used) {
  for (int i = 0; i < CALLEE_SAVE_REG_COUNT; ++i) {
    int ireg = kCalleeSaveRegs[i];
    if (used & (1 << ireg)) {
      PUSH(kReg64s[ireg]); PUSH_STACK_POS();
    }
  }
}

void pop_callee_save_regs(unsigned short used) {
  for (int i = CALLEE_SAVE_REG_COUNT; --i >= 0;) {
    int ireg = kCalleeSaveRegs[i];
    if (used & (1 << ireg)) {
      POP(kReg64s[ireg]); POP_STACK_POS();
    }
  }
}

static void push_caller_save_regs(unsigned short living, int base) {
#ifndef __NO_FLONUM
  {
    for (int i = CALLER_SAVE_FREG_COUNT; i > 0; ) {
      int ireg = kCallerSaveFRegs[--i];
      if (living & (1U << (ireg + PHYSICAL_REG_MAX))) {
        // TODO: Detect register size.
        MOVSD(kFReg64s[ireg], OFFSET_INDIRECT(base, RSP, NULL, 1));
        base += WORD_SIZE;
      }
    }
  }
#endif

  for (int i = CALLER_SAVE_REG_COUNT; i > 0; ) {
    int ireg = kCallerSaveRegs[--i];
    if (living & (1 << ireg)) {
      MOV(kReg64s[ireg], OFFSET_INDIRECT(base, RSP, NULL, 1));
      base += WORD_SIZE;
    }
  }
}

static void pop_caller_save_regs(unsigned short living) {
#ifndef __NO_FLONUM
  {
    int count = 0;
    for (int i = CALLER_SAVE_FREG_COUNT; i > 0; ) {
      int ireg = kCallerSaveFRegs[--i];
      if (living & (1U << (ireg + PHYSICAL_REG_MAX))) {
        // TODO: Detect register size.
        MOVSD(OFFSET_INDIRECT(count * WORD_SIZE, RSP, NULL, 1), kFReg64s[ireg]);
        ++count;
      }
    }
    if (count > 0) {
      ADD(IM(WORD_SIZE * count), RSP); stackpos -= WORD_SIZE * count;
    }
  }
#endif

  for (int i = CALLER_SAVE_REG_COUNT; --i >= 0;) {
    int ireg = kCallerSaveRegs[i];
    if (living & (1 << ireg)) {
      POP(kReg64s[ireg]); POP_STACK_POS();
    }
  }
}

void emit_bb_irs(BBContainer *bbcon) {
  for (int i = 0; i < bbcon->bbs->len; ++i) {
    BB *bb = bbcon->bbs->data[i];
#ifndef NDEBUG
    // Check BB connection.
    if (i < bbcon->bbs->len - 1) {
      BB *nbb = bbcon->bbs->data[i + 1];
      UNUSED(nbb);
      assert(bb->next == nbb);
    } else {
      assert(bb->next == NULL);
    }
#endif

    EMIT_LABEL(fmt_name(bb->label));
    for (int j = 0; j < bb->irs->len; ++j) {
      IR *ir = bb->irs->data[j];
      ir_out(ir);
    }
  }
}

// Rewrite `A = B op C` to `A = B; A = A op C`.
static void three_to_two(BB *bb) {
  Vector *irs = bb->irs;
  for (int i = 0; i < irs->len; ++i) {
    IR *ir = bb->irs->data[i];

    switch (ir->kind) {
    case IR_ADD:  // binops
    case IR_SUB:
    case IR_MUL:
    case IR_DIV:
    case IR_DIVU:
    case IR_MOD:
    case IR_MODU:
    case IR_BITAND:
    case IR_BITOR:
    case IR_BITXOR:
    case IR_LSHIFT:
    case IR_RSHIFT:
    case IR_NEG:  // unary ops
    case IR_BITNOT:
      {
        assert(!(ir->dst->flag & VRF_CONST));
        IR *ir2 = malloc(sizeof(*ir2));
        ir2->kind = IR_MOV;
        ir2->dst = ir->dst;
        ir2->opr1 = ir->opr1;
        ir2->opr2 = NULL;
        ir2->size = ir->size;
        vec_insert(irs, i, ir2);

        ir->opr1 = ir->dst;
        ++i;
      }
      break;

    default:
      break;
    }
  }
  bb->irs = irs;
}

void convert_3to2(BBContainer *bbcon) {
  for (int i = 0; i < bbcon->bbs->len; ++i) {
    BB *bb = bbcon->bbs->data[i];
    three_to_two(bb);
  }
}
