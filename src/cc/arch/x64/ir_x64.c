#include "ir.h"

#include <assert.h>
#include <stdlib.h>  // malloc

#include "regalloc.h"
#include "table.h"
#include "util.h"
#include "x64.h"

#define WORK_REG_NO  (PHYSICAL_REG_MAX)

static void push_caller_save_regs(unsigned short living, int base);
static void pop_caller_save_regs(unsigned short living);

int stackpos = 8;

static enum ConditionKind invert_cond(enum ConditionKind cond) {
  assert(COND_EQ <= cond && cond <= COND_UGT);
  if (cond <= COND_NE)
    return COND_NE + COND_EQ - cond;
  if (cond <= COND_ULT)
    return COND_LT + ((cond - COND_LT) ^ 2);
  return COND_ULT + ((cond - COND_ULT) ^ 2);
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

static const int kPow2Table[] = {-1, 0, 1, -1, 2, -1, -1, -1, 3};
#define kPow2TableSize ((int)(sizeof(kPow2Table) / sizeof(*kPow2Table)))

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
      PUSH(dst);
      MOV(IM(size), RCX);
      EMIT_LABEL(label);
      MOV(INDIRECT(src, NULL, 1), DL);
      MOV(DL, INDIRECT(dst, NULL, 1));
      INC(src);
      INC(dst);
      DEC(RCX);
      JNE(label);
      POP(dst);
      POP(src);
    }
    break;
  }
}

static void ir_out(IR *ir) {
  switch (ir->kind) {
  case IR_BOFS:
    if (ir->opr1->flag & VRF_CONST)
      LEA(OFFSET_INDIRECT(ir->opr1->fixnum, RBP, NULL, 1), kReg64s[ir->dst->phys]);
    else
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
      case SZ_FLOAT:
        MOVSS(INDIRECT(kReg64s[ir->opr1->phys], NULL, 1), kFReg64s[ir->dst->phys]);
        break;
      case SZ_DOUBLE:
        MOVSD(INDIRECT(kReg64s[ir->opr1->phys], NULL, 1), kFReg64s[ir->dst->phys]);
        break;
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
        MOV(INDIRECT(NUM(ir->opr1->fixnum), NULL, 1), regs[ir->dst->phys]);
      else
        MOV(INDIRECT(kReg64s[ir->opr1->phys], NULL, 1), regs[ir->dst->phys]);
    }
    break;

  case IR_STORE:
#ifndef __NO_FLONUM
    if (ir->opr1->vtype->flag & VRTF_FLONUM) {
      switch (ir->size) {
      case SZ_FLOAT:
        MOVSS(kFReg64s[ir->opr1->phys], INDIRECT(kReg64s[ir->opr2->phys], NULL, 1));
        break;
      case SZ_DOUBLE:
        MOVSD(kFReg64s[ir->opr1->phys], INDIRECT(kReg64s[ir->opr2->phys], NULL, 1));
        break;
      default: assert(false); break;
      }
      break;
    }
#endif
    {
      assert(!(ir->opr2->flag & VRF_CONST));
      assert(0 <= ir->size && ir->size < kPow2TableSize);
      int pow = kPow2Table[ir->size];
      assert(0 <= pow && pow < 4);
      const char **regs = kRegSizeTable[pow];
      if (ir->opr1->flag & VRF_CONST) {
        const char *dst = INDIRECT(kReg64s[ir->opr2->phys], NULL, 1);
        switch (pow) {
        case 0: MOVB(IM(ir->opr1->fixnum), dst); break;
        case 1: MOVW(IM(ir->opr1->fixnum), dst); break;
        case 2: MOVL(IM(ir->opr1->fixnum), dst); break;
        case 3: MOVQ(IM(ir->opr1->fixnum), dst); break;
        default: assert(false); break;
        }
      } else {
        MOV(regs[ir->opr1->phys], INDIRECT(kReg64s[ir->opr2->phys], NULL, 1));
      }
    }
    break;

  case IR_ADD:
    {
      assert(ir->dst->phys == ir->opr1->phys);
#ifndef __NO_FLONUM
      if (ir->dst->vtype->flag & VRTF_FLONUM) {
        const char **regs = kFReg64s;
        switch (ir->size) {
        case SZ_FLOAT: ADDSS(regs[ir->opr2->phys], regs[ir->dst->phys]); break;
        case SZ_DOUBLE: ADDSD(regs[ir->opr2->phys], regs[ir->dst->phys]); break;
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
      const char *dst = regs[ir->dst->phys];
      if (ir->opr2->flag & VRF_CONST) {
        switch (ir->opr2->fixnum) {
        case 0: break;
        case 1:  INC(dst); break;
        case -1: DEC(dst); break;
        default:
          ADD(IM(ir->opr2->fixnum), dst);
          break;
        }
      } else {
        ADD(regs[ir->opr2->phys], dst);
      }
    }
    break;

  case IR_SUB:
    {
      assert(ir->dst->phys == ir->opr1->phys);
#ifndef __NO_FLONUM
      if (ir->dst->vtype->flag & VRTF_FLONUM) {
        const char **regs = kFReg64s;
        switch (ir->size) {
        case SZ_FLOAT: SUBSS(regs[ir->opr2->phys], regs[ir->dst->phys]); break;
        case SZ_DOUBLE: SUBSD(regs[ir->opr2->phys], regs[ir->dst->phys]); break;
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
      const char *dst = regs[ir->dst->phys];
      if (ir->opr2->flag & VRF_CONST) {
        switch (ir->opr2->fixnum) {
        case 0: break;
        case 1:  DEC(dst); break;
        case -1: INC(dst); break;
        default:
          SUB(IM(ir->opr2->fixnum), dst);
          break;
        }
      } else {
        SUB(regs[ir->opr2->phys], dst);
      }
    }
    break;

  case IR_MUL:
    {
#ifndef __NO_FLONUM
      if (ir->dst->vtype->flag & VRTF_FLONUM) {
        assert(ir->dst->phys == ir->opr1->phys);
        const char **regs = kFReg64s;
        switch (ir->size) {
        case SZ_FLOAT: MULSS(regs[ir->opr2->phys], regs[ir->dst->phys]); break;
        case SZ_DOUBLE: MULSD(regs[ir->opr2->phys], regs[ir->dst->phys]); break;
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
        MOV(IM(ir->opr2->fixnum), regs[WORK_REG_NO]);
        opr2 = regs[WORK_REG_NO];
      } else {
        opr2 = regs[ir->opr2->phys];
      }
      MUL(opr2);
      MOV(a, regs[ir->dst->phys]);
    }
    break;

  case IR_DIV:
  case IR_DIVU: assert(!(ir->opr1->flag & VRF_CONST));
#ifndef __NO_FLONUM
    if (ir->dst->vtype->flag & VRTF_FLONUM) {
      assert(ir->dst->phys == ir->opr1->phys);
      const char **regs = kFReg64s;
      switch (ir->size) {
      case SZ_FLOAT: DIVSS(regs[ir->opr2->phys], regs[ir->dst->phys]); break;
      case SZ_DOUBLE: DIVSD(regs[ir->opr2->phys], regs[ir->dst->phys]); break;
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
          MOV(IM(ir->opr2->fixnum), opr2);
        } else {
          opr2 = kReg8s[ir->opr2->phys];
        }
        IDIV(opr2);
      } else {
        MOVZX(kReg8s[ir->opr1->phys], AX);
        const char *opr2;
        if (ir->opr2->flag & VRF_CONST) {
          opr2 = kReg8s[WORK_REG_NO];
          MOV(IM(ir->opr2->fixnum), opr2);
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
        MOV(IM(ir->opr2->fixnum), opr2);
      } else {
        opr2 = regs[ir->opr2->phys];
      }
      if (ir->kind == IR_DIV) {
        switch (pow) {
        case 1: CWTL(); break;
        case 2: CLTD(); break;
        case 3: CQTO(); break;
        default: assert(false); break;
        }
        IDIV(opr2);
      } else {
        switch (pow) {
        case 1: XOR(DX, DX); break;
        case 2: XOR(EDX, EDX); break;
        case 3: XOR(EDX, EDX); break;  // Clear 64bit register.
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
          MOV(IM(ir->opr2->fixnum), opr2);
        } else {
          opr2 = kReg8s[ir->opr2->phys];
        }
        IDIV(opr2);
      } else {
        MOVZX(kReg8s[ir->opr1->phys], AX);
        const char *opr2;
        if (ir->opr2->flag & VRF_CONST) {
          opr2 = kReg8s[WORK_REG_NO];
          MOV(IM(ir->opr2->fixnum), opr2);
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
        MOV(IM(ir->opr2->fixnum), opr2);
      } else {
        opr2 = regs[ir->opr2->phys];
      }
      if (ir->kind == IR_MOD) {
        switch (pow) {
        case 1: CWTL(); break;
        case 2: CLTD(); break;
        case 3: CQTO(); break;
        default: assert(false); break;
        }
        IDIV(opr2);
      } else {
        switch (pow) {
        case 1: XOR(DX, DX); break;
        case 2: XOR(EDX, EDX); break;
        case 3: XOR(EDX, EDX); break;  // Clear 64bit register.
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
        AND(IM(ir->opr2->fixnum), regs[ir->dst->phys]);
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
        OR(IM(ir->opr2->fixnum), regs[ir->dst->phys]);
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
        XOR(IM(ir->opr2->fixnum), regs[ir->dst->phys]);
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
        SHL(IM(ir->opr2->fixnum), regs[ir->dst->phys]);
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
          SHR(IM(ir->opr2->fixnum), regs[ir->dst->phys]);
        } else {
          MOV(kReg8s[ir->opr2->phys], CL);
          SHR(CL, regs[ir->dst->phys]);
        }
      } else {
        if (ir->opr2->flag & VRF_CONST) {
          SAR(IM(ir->opr2->fixnum), regs[ir->dst->phys]);
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
        case SZ_FLOAT: UCOMISS(kFReg64s[ir->opr2->phys], kFReg64s[ir->opr1->phys]); break;
        case SZ_DOUBLE: UCOMISD(kFReg64s[ir->opr2->phys], kFReg64s[ir->opr1->phys]); break;
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
      const char *opr1;
      opr1 = regs[ir->opr1->phys];
      if ((ir->opr2->flag & VRF_CONST) && ir->opr2->fixnum == 0) {
        TEST(opr1, opr1);
      } else {
        const char *opr2 = (ir->opr2->flag & VRF_CONST) ? IM(ir->opr2->fixnum) : regs[ir->opr2->phys];
        CMP(opr2, opr1);
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
      case COND_EQ: SETE(dst); break;
      case COND_NE: SETNE(dst); break;
      case COND_LT: SETL(dst); break;
      case COND_GT: SETG(dst); break;
      case COND_LE: SETLE(dst); break;
      case COND_GE: SETGE(dst); break;
      case COND_ULT: SETB(dst); break;
      case COND_UGT: SETA(dst); break;
      case COND_ULE: SETBE(dst); break;
      case COND_UGE: SETAE(dst); break;
      default: assert(false); break;
      }
      MOVSX(dst, kReg32s[ir->dst->phys]);  // Assume bool is 4 byte.
    }
    break;

  case IR_JMP:
    switch (ir->jmp.cond) {
    case COND_ANY: JMP(fmt_name(ir->jmp.bb->label)); break;
    case COND_EQ: JE(fmt_name(ir->jmp.bb->label)); break;
    case COND_NE: JNE(fmt_name(ir->jmp.bb->label)); break;
    case COND_LT: JL(fmt_name(ir->jmp.bb->label)); break;
    case COND_GT: JG(fmt_name(ir->jmp.bb->label)); break;
    case COND_LE: JLE(fmt_name(ir->jmp.bb->label)); break;
    case COND_GE: JGE(fmt_name(ir->jmp.bb->label)); break;
    case COND_ULT: JB(fmt_name(ir->jmp.bb->label)); break;
    case COND_UGT: JA(fmt_name(ir->jmp.bb->label)); break;
    case COND_ULE: JBE(fmt_name(ir->jmp.bb->label)); break;
    case COND_UGE: JAE(fmt_name(ir->jmp.bb->label)); break;
    default: assert(false); break;
    }
    break;

  case IR_TJMP:
    {
      const Name *table_label = alloc_label();
      int phys = ir->opr1->phys;
      const int powd = 3;
      assert(0 <= ir->opr1->vtype->size && ir->opr1->vtype->size < kPow2TableSize);
      int pows = kPow2Table[ir->opr1->vtype->size];
      assert(0 <= pows && pows < 4);
      if (pows < powd) {
        if (pows == 2) {
          // MOVZX %64bit, %32bit doesn't exist!
          MOV(kRegSizeTable[pows][phys], kRegSizeTable[pows][phys]);
        } else {
          MOVZX(kRegSizeTable[pows][phys], kRegSizeTable[powd][phys]);
        }
      }

      LEA(LABEL_INDIRECT(fmt_name(table_label), RIP), RAX);
      JMP(fmt("*%s", OFFSET_INDIRECT(0, RAX, kReg64s[phys], 8)));

      _RODATA();  // gcc warns, should be put into .data section?
      EMIT_ALIGN(8);
      EMIT_LABEL(fmt_name(table_label));
      for (size_t i = 0, len = ir->tjmp.len; i < len; ++i) {
        BB *bb = ir->tjmp.bbs[i];
        _QUAD(fmt("%.*s", bb->label->bytes, bb->label->chars));
      }
      _TEXT();
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
        SUB(IM(add), RSP);
        stackpos += add;
      }
    }
    break;

  case IR_PUSHARG:
#ifndef __NO_FLONUM
    if (ir->opr1->vtype->flag & VRTF_FLONUM) {
      SUB(IM(WORD_SIZE), RSP);
      PUSH_STACK_POS();
      switch (ir->opr1->vtype->size) {
      case SZ_FLOAT: MOVSS(kFReg64s[ir->opr1->phys], INDIRECT(RSP, NULL, 1)); break;
      case SZ_DOUBLE: MOVSD(kFReg64s[ir->opr1->phys], INDIRECT(RSP, NULL, 1)); break;
      default: assert(false); break;
      }
      break;
    }
#endif
    if (ir->opr1->flag & VRF_CONST) {
      if (is_im32(ir->opr1->fixnum)) {
        PUSH(IM(ir->opr1->fixnum));
        PUSH_STACK_POS();
      } else {
        MOV(IM(ir->opr1->fixnum), RAX);  // TODO: Check.
        PUSH(RAX);
        PUSH_STACK_POS();
      }
    } else {
      PUSH(kReg64s[ir->opr1->phys]);
      PUSH_STACK_POS();
    }
    break;

  case IR_CALL:
    {
      IR *precall = ir->call.precall;
      int reg_args = ir->call.reg_arg_count;
      push_caller_save_regs(
          precall->precall.living_pregs,
          reg_args * WORD_SIZE + precall->precall.stack_args_size + precall->precall.stack_aligned);

      static const char *kArgReg64s[] = {RDI, RSI, RDX, RCX, R8, R9};
#ifndef __NO_FLONUM
      static const char *kArgFReg64s[] = {XMM0, XMM1, XMM2, XMM3, XMM4, XMM5, XMM6, XMM7};
      int freg = 0;
#endif

      // Pop register arguments.
      int ireg = 0;
      int total_arg_count = ir->call.total_arg_count;
      for (int i = 0; i < total_arg_count; ++i) {
        if (ir->call.arg_vtypes[i]->flag & VRTF_NON_REG)
          continue;
#ifndef __NO_FLONUM
        if (ir->call.arg_vtypes[i]->flag & VRTF_FLONUM) {
          if (freg < MAX_FREG_ARGS) {
            switch (ir->call.arg_vtypes[i]->size) {
            case SZ_FLOAT: MOVSS(INDIRECT(RSP, NULL, 1), kArgFReg64s[freg]); break;
            case SZ_DOUBLE: MOVSD(INDIRECT(RSP, NULL, 1), kArgFReg64s[freg]); break;
            default: assert(false); break;
            }
            ++freg;
            ADD(IM(WORD_SIZE), RSP);
            POP_STACK_POS();
          }
          continue;
        }
#endif
        if (ireg < MAX_REG_ARGS) {
          POP(kArgReg64s[ireg++]);
          POP_STACK_POS();
        }
      }

#ifndef __NO_FLONUM
      if (ir->call.vaargs) {
        if (freg > 0)
          MOV(IM(freg), AL);
        else
          XOR(AL, AL);
      }
#endif
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

      int align_stack = precall->precall.stack_aligned + precall->precall.stack_args_size;
      if (align_stack != 0) {
        ADD(IM(align_stack), RSP);
        stackpos -= precall->precall.stack_aligned;
      }

      // Resore caller save registers.
      pop_caller_save_regs(precall->precall.living_pregs);

#ifndef __NO_FLONUM
      if (ir->dst->vtype->flag & VRTF_FLONUM) {
        switch (ir->size) {
        case SZ_FLOAT: MOVSS(XMM0, kFReg64s[ir->dst->phys]); break;
        case SZ_DOUBLE: MOVSD(XMM0, kFReg64s[ir->dst->phys]); break;
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
      case SZ_FLOAT: MOVSS(kFReg64s[ir->opr1->phys], XMM0); break;
      case SZ_DOUBLE: MOVSD(kFReg64s[ir->opr1->phys], XMM0); break;
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
        MOV(IM(ir->opr1->fixnum), kRegATable[pow]);
      else
        MOV(regs[ir->opr1->phys], kRegATable[pow]);
    }
    break;

  case IR_SUBSP:
    if (ir->opr1->flag & VRF_CONST) {
      if (ir->opr1->fixnum > 0)
        SUB(IM(ir->opr1->fixnum), RSP);
      else if (ir->opr1->fixnum < 0)
        ADD(IM(-ir->opr1->fixnum), RSP);
      // stackpos += ir->opr1->fixnum;
    } else {
      SUB(kReg64s[ir->opr1->phys], RSP);
    }
    if (ir->dst != NULL)
      MOV(RSP, kReg64s[ir->dst->phys]);
    break;

  case IR_CAST: assert((ir->opr1->flag & VRF_CONST) == 0);
#ifndef __NO_FLONUM
    if (ir->dst->vtype->flag & VRTF_FLONUM) {
      if (ir->opr1->vtype->flag & VRTF_FLONUM) {
        // flonum->flonum
        // Assume flonum are just two types.
        switch (ir->size) {
        case SZ_FLOAT: CVTSD2SS(kFReg64s[ir->opr1->phys], kFReg64s[ir->dst->phys]); break;
        case SZ_DOUBLE: CVTSS2SD(kFReg64s[ir->opr1->phys], kFReg64s[ir->dst->phys]); break;
        default: assert(false); break;
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
        case SZ_FLOAT:
          CVTSI2SS(kRegSizeTable[pows][ir->opr1->phys], kFReg64s[ir->dst->phys]);
          break;
        case SZ_DOUBLE:
          CVTSI2SD(kRegSizeTable[pows][ir->opr1->phys], kFReg64s[ir->dst->phys]);
          break;
        default: assert(false); break;
        }
      }
      break;
    } else if (ir->opr1->vtype->flag & VRTF_FLONUM) {
      // flonum->fix
      int powd = kPow2Table[ir->dst->vtype->size];
      switch (ir->opr1->vtype->size) {
      case SZ_FLOAT: CVTTSS2SI(kFReg64s[ir->opr1->phys], kRegSizeTable[powd][ir->dst->phys]); break;
      case SZ_DOUBLE:
        CVTTSD2SI(kFReg64s[ir->opr1->phys], kRegSizeTable[powd][ir->dst->phys]);
        break;
      default: assert(false); break;
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
          case SZ_FLOAT: MOVSS(kFReg64s[ir->opr1->phys], kFReg64s[ir->dst->phys]); break;
          case SZ_DOUBLE: MOVSD(kFReg64s[ir->opr1->phys], kFReg64s[ir->dst->phys]); break;
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
        MOV(IM(ir->opr1->fixnum), regs[ir->dst->phys]);
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
    if (ir->dst != NULL) {
      assert(0 <= ir->size && ir->size < kPow2TableSize);
      assert(!(ir->dst->flag & VRF_CONST));
      int pow = kPow2Table[ir->size];
      assert(0 <= pow && pow < 4);
      const char **regs = kRegSizeTable[pow];
      MOV(kRegATable[pow], regs[ir->dst->phys]);
    }
    break;

  case IR_LOAD_SPILLED:
#ifndef __NO_FLONUM
    if (ir->spill.flag & VRTF_FLONUM) {
      const char **regs = kFReg64s;
      switch (ir->size) {
      case SZ_FLOAT: MOVSS(OFFSET_INDIRECT(ir->value, RBP, NULL, 1), regs[WORK_REG_NO]); break;
      case SZ_DOUBLE: MOVSD(OFFSET_INDIRECT(ir->value, RBP, NULL, 1), regs[WORK_REG_NO]); break;
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
      case SZ_FLOAT: MOVSS(regs[WORK_REG_NO], OFFSET_INDIRECT(ir->value, RBP, NULL, 1)); break;
      case SZ_DOUBLE: MOVSD(regs[WORK_REG_NO], OFFSET_INDIRECT(ir->value, RBP, NULL, 1)); break;
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

  default: assert(false); break;
  }
}

//

static IR *is_last_jmp(BB *bb) {
  int len;
  IR *ir;
  if ((len = bb->irs->len) > 0 && (ir = bb->irs->data[len - 1])->kind == IR_JMP)
    return ir;
  return NULL;
}

static IR *is_last_any_jmp(BB *bb) {
  IR *ir = is_last_jmp(bb);
  return ir != NULL && ir->jmp.cond == COND_ANY ? ir : NULL;
}

static IR *is_last_jtable(BB *bb) {
  int len;
  IR *ir;
  if ((len = bb->irs->len) > 0 && (ir = bb->irs->data[len - 1])->kind == IR_TJMP)
    return ir;
  return NULL;
}

static void replace_jmp_destination(BBContainer *bbcon, BB *src, BB *dst) {
  Vector *bbs = bbcon->bbs;
  for (int i = 0; i < bbs->len; ++i) {
    BB *bb = bbs->data[i];
    if (bb == src)
      continue;

    IR *ir = is_last_jmp(bb);
    if (ir != NULL && ir->jmp.bb == src)
      ir->jmp.bb = dst;

    IR *tjmp = is_last_jtable(bb);
    if (tjmp != NULL) {
      BB **bbs = tjmp->tjmp.bbs;
      for (size_t j = 0, len = tjmp->tjmp.len; j < len; ++j) {
        if (bbs[j] == src)
          bbs[j] = dst;
      }
    }
  }
}

void remove_unnecessary_bb(BBContainer *bbcon) {
  Vector *bbs = bbcon->bbs;
  Table keeptbl;
  for (;;) {
    table_init(&keeptbl);
    assert(bbs->len > 0);
    BB *bb0 = bbs->data[0];
    table_put(&keeptbl, bb0->label, bb0);

    for (int i = 0; i < bbs->len - 1; ++i) {
      BB *bb = bbs->data[i];
      bool remove = false;
      IR *ir_jmp = is_last_jmp(bb);
      if (bb->irs->len == 0) {  // Empty BB.
        replace_jmp_destination(bbcon, bb, bb->next);
        remove = true;
      } else if (bb->irs->len == 1 && ir_jmp != NULL && ir_jmp->jmp.cond == COND_ANY &&
                 !equal_name(bb->label, ir_jmp->jmp.bb->label)) {  // jmp only.
        replace_jmp_destination(bbcon, bb, ir_jmp->jmp.bb);
        if (i > 0) {
          BB *pbb = bbs->data[i - 1];
          IR *ir0 = is_last_jmp(pbb);
          if (ir0 != NULL && ir0->jmp.cond != COND_ANY) {  // Fallthrough pass exists.
            if (ir0->jmp.bb == bb->next) {                 // Skip jmp: Fix bb connection.
              // Invert prev jmp condition and change jmp destination.
              ir0->jmp.cond = invert_cond(ir0->jmp.cond);
              ir0->jmp.bb = ir_jmp->jmp.bb;
              remove = true;
            }
          }
        }
      }

      if (ir_jmp != NULL)
        table_put(&keeptbl, ir_jmp->jmp.bb->label, bb);
      if (ir_jmp == NULL || ir_jmp->jmp.cond != COND_ANY)
        table_put(&keeptbl, bb->next->label, bb);

      IR *tjmp = is_last_jtable(bb);
      if (tjmp != NULL) {
        BB **bbs = tjmp->tjmp.bbs;
        for (size_t j = 0, len = tjmp->tjmp.len; j < len; ++j)
          table_put(&keeptbl, bbs[j]->label, bb);
      }

      if (remove)
        table_delete(&keeptbl, bb->label);
    }

    bool again = false;
    for (int i = 0; i < bbs->len - 1; ++i) {  // Make last one keeps alive.
      BB *bb = bbs->data[i];
      if (!table_try_get(&keeptbl, bb->label, NULL)) {
        if (i > 0) {
          BB *pbb = bbs->data[i - 1];
          pbb->next = bb->next;
        }

        vec_remove_at(bbs, i);
        --i;
        again = true;
      }
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

int push_callee_save_regs(unsigned short used) {
  int count = 0;
  for (int i = 0; i < CALLEE_SAVE_REG_COUNT; ++i) {
    int ireg = kCalleeSaveRegs[i];
    if (used & (1 << ireg)) {
      PUSH(kReg64s[ireg]);
      PUSH_STACK_POS();
      ++count;
    }
  }
  return count;
}

void pop_callee_save_regs(unsigned short used) {
  for (int i = CALLEE_SAVE_REG_COUNT; --i >= 0;) {
    int ireg = kCalleeSaveRegs[i];
    if (used & (1 << ireg)) {
      POP(kReg64s[ireg]);
      POP_STACK_POS();
    }
  }
}

static void push_caller_save_regs(unsigned short living, int base) {
#ifndef __NO_FLONUM
  {
    for (int i = CALLER_SAVE_FREG_COUNT; i > 0;) {
      int ireg = kCallerSaveFRegs[--i];
      if (living & (1U << (ireg + PHYSICAL_REG_MAX))) {
        // TODO: Detect register size.
        MOVSD(kFReg64s[ireg], OFFSET_INDIRECT(base, RSP, NULL, 1));
        base += WORD_SIZE;
      }
    }
  }
#endif

  for (int i = CALLER_SAVE_REG_COUNT; i > 0;) {
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
    for (int i = CALLER_SAVE_FREG_COUNT; i > 0;) {
      int ireg = kCallerSaveFRegs[--i];
      if (living & (1U << (ireg + PHYSICAL_REG_MAX))) {
        // TODO: Detect register size.
        MOVSD(OFFSET_INDIRECT(count * WORD_SIZE, RSP, NULL, 1), kFReg64s[ireg]);
        ++count;
      }
    }
    if (count > 0) {
      ADD(IM(WORD_SIZE * count), RSP);
      stackpos -= WORD_SIZE * count;
    }
  }
#endif

  for (int i = CALLER_SAVE_REG_COUNT; --i >= 0;) {
    int ireg = kCallerSaveRegs[i];
    if (living & (1 << ireg)) {
      POP(kReg64s[ireg]);
      POP_STACK_POS();
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

    default: break;
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
