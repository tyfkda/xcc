#include "../config.h"
#include "ir.h"

#include <assert.h>
#include <stdlib.h>  // malloc
#include <string.h>

#include "emit_code.h"
#include "regalloc.h"
#include "table.h"
#include "util.h"
#include "x64.h"

static void push_caller_save_regs(unsigned long living, int base);
static void pop_caller_save_regs(unsigned long living);

int stackpos = 8;

// Register allocator

static const char *kRegSizeTable[][PHYSICAL_REG_MAX] = {
  { BL, R10B, R11B, R12B, R13B, R14B, R15B},
  { BX, R10W, R11W, R12W, R13W, R14W, R15W},
  {EBX, R10D, R11D, R12D, R13D, R14D, R15D},
  {RBX, R10,  R11,  R12,  R13,  R14,  R15},
};

#define kReg8s   (kRegSizeTable[0])
#define kReg32s  (kRegSizeTable[2])
#define kReg64s  (kRegSizeTable[3])

static const char *kRegATable[] = {AL, AX, EAX, RAX};
static const char *kRegDTable[] = {DL, DX, EDX, RDX};

#ifndef __NO_FLONUM
#define SZ_FLOAT   (4)
#define SZ_DOUBLE  (8)
static const char *kFReg64s[PHYSICAL_FREG_MAX] = {XMM8, XMM9, XMM10, XMM11, XMM12, XMM13, XMM14, XMM15};
#endif

#define CALLEE_SAVE_REG_COUNT  ((int)(sizeof(kCalleeSaveRegs) / sizeof(*kCalleeSaveRegs)))
static const int kCalleeSaveRegs[] = {
  0,  // RBX
  3,  // R12
  4,  // R13
  5,  // R14
  6,  // R15
};

#define CALLER_SAVE_REG_COUNT  ((int)(sizeof(kCallerSaveRegs) / sizeof(*kCallerSaveRegs)))
static const int kCallerSaveRegs[] = {
  1,  // R10
  2,  // R11
};

#ifndef __NO_FLONUM
#define CALLER_SAVE_FREG_COUNT  ((int)(sizeof(kCallerSaveFRegs) / sizeof(*kCallerSaveFRegs)))
static const int kCallerSaveFRegs[] = {0, 1, 2, 3, 4, 5, 6, 7};
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

static bool is_got(const Name *name) {
#ifdef __APPLE__
  // TODO: How to detect the label is GOT?
  return name->bytes >= 5 && strncmp(name->chars, "__std", 5) == 0;  // __stdinp, etc.
#else
  UNUSED(name);
  return false;
#endif
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
      char *label = fmt_name(ir->iofs.label);
      if (ir->iofs.global)
        label = MANGLE(label);
      if (!is_got(ir->iofs.label)) {
        LEA(LABEL_INDIRECT(quote_label(label), RIP), kReg64s[ir->dst->phys]);
      } else {
        LEA(LABEL_INDIRECT(GOTPCREL(quote_label(label)), RIP), kReg64s[ir->dst->phys]);
      }
    }
    break;

  case IR_SOFS:
    assert(ir->opr1->flag & VRF_CONST);
    LEA(OFFSET_INDIRECT(ir->opr1->fixnum, RSP, NULL, 1), kReg64s[ir->dst->phys]);
    break;

  case IR_LOAD:
    assert(!(ir->opr1->flag & VRF_CONST));
#ifndef __NO_FLONUM
    if (ir->dst->vtype->flag & VRTF_FLONUM) {
      switch (ir->dst->vtype->size) {
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
      assert(0 <= ir->dst->vtype->size && ir->dst->vtype->size < kPow2TableSize);
      int pow = kPow2Table[ir->dst->vtype->size];
      assert(0 <= pow && pow < 4);
      const char **regs = kRegSizeTable[pow];
      MOV(INDIRECT(kReg64s[ir->opr1->phys], NULL, 1), regs[ir->dst->phys]);
    }
    break;

  case IR_STORE:
#ifndef __NO_FLONUM
    if (ir->opr1->vtype->flag & VRTF_FLONUM) {
      switch (ir->opr1->vtype->size) {
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
      assert(0 <= ir->opr1->vtype->size && ir->opr1->vtype->size < kPow2TableSize);
      int pow = kPow2Table[ir->opr1->vtype->size];
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
        switch (ir->dst->vtype->size) {
        case SZ_FLOAT: ADDSS(regs[ir->opr2->phys], regs[ir->dst->phys]); break;
        case SZ_DOUBLE: ADDSD(regs[ir->opr2->phys], regs[ir->dst->phys]); break;
        default: assert(false); break;
        }
        break;
      }
#endif
      assert(!(ir->opr1->flag & VRF_CONST));
      assert(0 <= ir->dst->vtype->size && ir->dst->vtype->size < kPow2TableSize);
      int pow = kPow2Table[ir->dst->vtype->size];
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
        switch (ir->dst->vtype->size) {
        case SZ_FLOAT: SUBSS(regs[ir->opr2->phys], regs[ir->dst->phys]); break;
        case SZ_DOUBLE: SUBSD(regs[ir->opr2->phys], regs[ir->dst->phys]); break;
        default: assert(false); break;
        }
        break;
      }
#endif
      assert(!(ir->opr1->flag & VRF_CONST));
      assert(0 <= ir->dst->vtype->size && ir->dst->vtype->size < kPow2TableSize);
      int pow = kPow2Table[ir->dst->vtype->size];
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
      assert(!(ir->opr1->flag & VRF_CONST) && !(ir->opr2->flag & VRF_CONST));
#ifndef __NO_FLONUM
      if (ir->dst->vtype->flag & VRTF_FLONUM) {
        assert(ir->dst->phys == ir->opr1->phys);
        const char **regs = kFReg64s;
        switch (ir->dst->vtype->size) {
        case SZ_FLOAT: MULSS(regs[ir->opr2->phys], regs[ir->dst->phys]); break;
        case SZ_DOUBLE: MULSD(regs[ir->opr2->phys], regs[ir->dst->phys]); break;
        default: assert(false); break;
        }
        break;
      }
#endif
      assert(0 <= ir->dst->vtype->size && ir->dst->vtype->size < kPow2TableSize);
      int pow = kPow2Table[ir->dst->vtype->size];
      assert(0 <= pow && pow < 4);
      const char **regs = kRegSizeTable[pow];
      const char *a = kRegATable[pow];
      MOV(regs[ir->opr1->phys], a);
      MUL(regs[ir->opr2->phys]);
      MOV(a, regs[ir->dst->phys]);
    }
    break;

  case IR_DIV:
    assert(!(ir->opr1->flag & VRF_CONST) && !(ir->opr2->flag & VRF_CONST));
#ifndef __NO_FLONUM
    if (ir->dst->vtype->flag & VRTF_FLONUM) {
      assert(ir->dst->phys == ir->opr1->phys);
      const char **regs = kFReg64s;
      switch (ir->dst->vtype->size) {
      case SZ_FLOAT: DIVSS(regs[ir->opr2->phys], regs[ir->dst->phys]); break;
      case SZ_DOUBLE: DIVSD(regs[ir->opr2->phys], regs[ir->dst->phys]); break;
      default: assert(false); break;
      }
      break;
    }
#endif
    if (ir->dst->vtype->size == 1) {
      if (!(ir->dst->vtype->flag & VRTF_UNSIGNED)) {
        MOVSX(kReg8s[ir->opr1->phys], AX);
        IDIV(kReg8s[ir->opr2->phys]);
      } else {
        MOVZX(kReg8s[ir->opr1->phys], AX);
        DIV(kReg8s[ir->opr2->phys]);
      }
      MOV(AL, kReg8s[ir->dst->phys]);
    } else {
      assert(0 <= ir->dst->vtype->size && ir->dst->vtype->size < kPow2TableSize);
      int pow = kPow2Table[ir->dst->vtype->size];
      assert(0 <= pow && pow < 4);
      const char **regs = kRegSizeTable[pow];
      const char *a = kRegATable[pow];
      MOV(regs[ir->opr1->phys], a);
      if (!(ir->dst->vtype->flag & VRTF_UNSIGNED)) {
        switch (pow) {
        case 1: CWTL(); break;
        case 2: CLTD(); break;
        case 3: CQTO(); break;
        default: assert(false); break;
        }
        IDIV(regs[ir->opr2->phys]);
      } else {
        switch (pow) {
        case 1: XOR(DX, DX); break;
        case 2: XOR(EDX, EDX); break;
        case 3: XOR(EDX, EDX); break;  // Clear 64bit register.
        default: assert(false); break;
        }
        DIV(regs[ir->opr2->phys]);
      }
      MOV(a, regs[ir->dst->phys]);
    }
    break;

  case IR_MOD:
    assert(!(ir->opr1->flag & VRF_CONST) && !(ir->opr2->flag & VRF_CONST));
    if (ir->dst->vtype->size == 1) {
      if (!(ir->dst->vtype->flag & VRTF_UNSIGNED)) {
        MOVSX(kReg8s[ir->opr1->phys], AX);
        IDIV(kReg8s[ir->opr2->phys]);
      } else {
        MOVZX(kReg8s[ir->opr1->phys], AX);
        DIV(kReg8s[ir->opr2->phys]);
      }
      MOV(AH, kReg8s[ir->dst->phys]);
    } else {
      assert(0 <= ir->dst->vtype->size && ir->dst->vtype->size < kPow2TableSize);
      int pow = kPow2Table[ir->dst->vtype->size];
      assert(0 <= pow && pow < 4);
      const char **regs = kRegSizeTable[pow];
      const char *a = kRegATable[pow];
      const char *d = kRegDTable[pow];
      MOV(regs[ir->opr1->phys], a);
      if (!(ir->dst->vtype->flag & VRTF_UNSIGNED)) {
        switch (pow) {
        case 1: CWTL(); break;
        case 2: CLTD(); break;
        case 3: CQTO(); break;
        default: assert(false); break;
        }
        IDIV(regs[ir->opr2->phys]);
      } else {
        switch (pow) {
        case 1: XOR(DX, DX); break;
        case 2: XOR(EDX, EDX); break;
        case 3: XOR(EDX, EDX); break;  // Clear 64bit register.
        default: assert(false); break;
        }
        DIV(regs[ir->opr2->phys]);
      }
      MOV(d, regs[ir->dst->phys]);
    }
    break;

  case IR_BITAND:
    {
      assert(ir->dst->phys == ir->opr1->phys);
      assert(!(ir->opr1->flag & VRF_CONST));
      assert(0 <= ir->dst->vtype->size && ir->dst->vtype->size < kPow2TableSize);
      int pow = kPow2Table[ir->dst->vtype->size];
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
      assert(0 <= ir->dst->vtype->size && ir->dst->vtype->size < kPow2TableSize);
      int pow = kPow2Table[ir->dst->vtype->size];
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
      assert(0 <= ir->dst->vtype->size && ir->dst->vtype->size < kPow2TableSize);
      int pow = kPow2Table[ir->dst->vtype->size];
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
      assert(0 <= ir->dst->vtype->size && ir->dst->vtype->size < kPow2TableSize);
      int pow = kPow2Table[ir->dst->vtype->size];
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
      assert(0 <= ir->dst->vtype->size && ir->dst->vtype->size < kPow2TableSize);
      int pow = kPow2Table[ir->dst->vtype->size];
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
        switch (ir->opr1->vtype->size) {
        case SZ_FLOAT: UCOMISS(kFReg64s[ir->opr2->phys], kFReg64s[ir->opr1->phys]); break;
        case SZ_DOUBLE: UCOMISD(kFReg64s[ir->opr2->phys], kFReg64s[ir->opr1->phys]); break;
        default: assert(false); break;
        }
        break;
      }
#endif
      assert(!(ir->opr1->flag & VRF_CONST));
      assert(0 <= ir->opr1->vtype->size && ir->opr1->vtype->size < kPow2TableSize);
      int pow = kPow2Table[ir->opr1->vtype->size];
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
      assert(0 <= ir->dst->vtype->size && ir->dst->vtype->size < kPow2TableSize);
      int pow = kPow2Table[ir->dst->vtype->size];
      assert(0 <= pow && pow < 4);
      const char **regs = kRegSizeTable[pow];
      NEG(regs[ir->dst->phys]);
    }
    break;

  case IR_BITNOT:
    {
      assert(ir->dst->phys == ir->opr1->phys);
      assert(!(ir->dst->flag & VRF_CONST));
      assert(0 <= ir->dst->vtype->size && ir->dst->vtype->size < kPow2TableSize);
      int pow = kPow2Table[ir->dst->vtype->size];
      assert(0 <= pow && pow < 4);
      const char **regs = kRegSizeTable[pow];
      NOT(regs[ir->dst->phys]);
    }
    break;

  case IR_COND:
    {
      assert(!(ir->dst->flag & VRF_CONST));
      const char *dst = kReg8s[ir->dst->phys];
      int kind = ir->cond.kind;
#ifndef __NO_FLONUM
      // On x64, flag for comparing flonum is same as unsigned.
      if (kind & COND_FLONUM)
        kind ^= COND_FLONUM | COND_UNSIGNED;  // Turn off flonum, and turn on unsigned
#endif
      switch (kind) {
      case COND_EQ | COND_UNSIGNED:  // Fallthrough
      case COND_EQ:  SETE(dst); break;

      case COND_NE | COND_UNSIGNED:   // Fallthrough
      case COND_NE:  SETNE(dst); break;

      case COND_LT:  SETL(dst); break;
      case COND_GT:  SETG(dst); break;
      case COND_LE:  SETLE(dst); break;
      case COND_GE:  SETGE(dst); break;

      case COND_LT | COND_UNSIGNED:  SETB(dst); break;
      case COND_GT | COND_UNSIGNED:  SETA(dst); break;
      case COND_LE | COND_UNSIGNED:  SETBE(dst); break;
      case COND_GE | COND_UNSIGNED:  SETAE(dst); break;
      default: assert(false); break;
      }
      MOVSX(dst, kReg32s[ir->dst->phys]);  // Assume bool is 4 byte.
    }
    break;

  case IR_JMP:
    {
      const char *label = fmt_name(ir->jmp.bb->label);
      int kind = ir->jmp.cond;
#ifndef __NO_FLONUM
      // On x64, flag for comparing flonum is same as unsigned.
      if (kind & COND_FLONUM)
        kind ^= COND_FLONUM | COND_UNSIGNED;  // Turn off flonum, and turn on unsigned
#endif
      switch (kind) {
      case COND_ANY: JMP(label); break;

      case COND_EQ | COND_UNSIGNED:  // Fallthrough
      case COND_EQ:  JE(label); break;

      case COND_NE | COND_UNSIGNED:  // Fallthrough
      case COND_NE:  JNE(label); break;

      case COND_LT:  JL(label); break;
      case COND_GT:  JG(label); break;
      case COND_LE:  JLE(label); break;
      case COND_GE:  JGE(label); break;

      case COND_LT | COND_UNSIGNED:  JB(label); break;
      case COND_GT | COND_UNSIGNED:  JA(label); break;
      case COND_LE | COND_UNSIGNED:  JBE(label); break;
      case COND_GE | COND_UNSIGNED:  JAE(label); break;
      default: assert(false); break;
      }
    }
    break;

  case IR_TJMP:
    {
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

      const Name *table_label = alloc_label();
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
      unsigned long living_pregs = ir->precall.living_pregs;
      for (int i = 0; i < CALLER_SAVE_REG_COUNT; ++i) {
        int ireg = kCallerSaveRegs[i];
        if (living_pregs & (1UL << ireg))
          add += WORD_SIZE;
      }
#ifndef __NO_FLONUM
      for (int i = 0; i < CALLER_SAVE_FREG_COUNT; ++i) {
        int freg = kCallerSaveFRegs[i];
        if (living_pregs & (1UL << (freg + PHYSICAL_REG_MAX)))
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
#if defined(VAARG_ON_STACK)
        if (ir->call.vaarg_start >= 0 && i >= ir->call.vaarg_start)
          break;
#endif
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
      if (ir->call.vaarg_start >= 0) {
        if (freg > 0)
          MOV(IM(freg), AL);
        else
          XOR(AL, AL);
      }
#endif
      if (ir->call.label != NULL) {
        char *label = fmt_name(ir->call.label);
        if (ir->call.global)
          label = MANGLE(label);
        CALL(quote_label(label));
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

      if (ir->dst != NULL) {
        assert(0 < ir->dst->vtype->size && ir->dst->vtype->size < kPow2TableSize);
#ifndef __NO_FLONUM
        if (ir->dst->vtype->flag & VRTF_FLONUM) {
          switch (ir->dst->vtype->size) {
          case SZ_FLOAT: MOVSS(XMM0, kFReg64s[ir->dst->phys]); break;
          case SZ_DOUBLE: MOVSD(XMM0, kFReg64s[ir->dst->phys]); break;
          default: assert(false); break;
          }
        } else
#endif
        {
          int pow = kPow2Table[ir->dst->vtype->size];
          assert(0 <= pow && pow < 4);
          const char **regs = kRegSizeTable[pow];
          MOV(kRegATable[pow], regs[ir->dst->phys]);
        }
      }
    }
    break;

  case IR_RESULT:
#ifndef __NO_FLONUM
    if (ir->opr1->vtype->flag & VRTF_FLONUM) {
      switch (ir->opr1->vtype->size) {
      case SZ_FLOAT: MOVSS(kFReg64s[ir->opr1->phys], XMM0); break;
      case SZ_DOUBLE: MOVSD(kFReg64s[ir->opr1->phys], XMM0); break;
      default: assert(false); break;
      }
      break;
    }
#endif
    {
      assert(0 <= ir->opr1->vtype->size && ir->opr1->vtype->size < kPow2TableSize);
      int pow = kPow2Table[ir->opr1->vtype->size];
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

  case IR_CAST:
    assert((ir->opr1->flag & VRF_CONST) == 0);
#ifndef __NO_FLONUM
    if (ir->dst->vtype->flag & VRTF_FLONUM) {
      if (ir->opr1->vtype->flag & VRTF_FLONUM) {
        // flonum->flonum
        // Assume flonum are just two types.
        switch (ir->dst->vtype->size) {
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
        const char *s = kRegSizeTable[pows][ir->opr1->phys];
        const char *d = kFReg64s[ir->dst->phys];
        if (!(ir->opr1->vtype->flag & VRTF_UNSIGNED)) {
          switch (ir->dst->vtype->size) {
          case SZ_FLOAT:   CVTSI2SS(s, d); break;
          case SZ_DOUBLE:  CVTSI2SD(s, d); break;
          default: assert(false); break;
          }
        } else if (pows < 3) {
          const char *s64 = kReg64s[ir->opr1->phys];
          switch (ir->dst->vtype->size) {
          case SZ_FLOAT:   CVTSI2SS(s64, d); break;
          case SZ_DOUBLE:  CVTSI2SD(s64, d); break;
          default: assert(false); break;
          }
        } else {
          // x64 support signed 64bit-signed-int to double only, so pass half value
          // (precision is lost anyway).
          // Break %rax
          const Name *neglabel = alloc_label();
          const Name *skiplabel = alloc_label();
          TEST(s, s);
          JS(fmt_name(neglabel));
          switch (ir->dst->vtype->size) {
          case SZ_FLOAT:   CVTSI2SS(s, d); break;
          case SZ_DOUBLE:  CVTSI2SD(s, d); break;
          default: assert(false); break;
          }
          JMP(fmt_name(skiplabel));
          EMIT_LABEL(fmt_name(neglabel));
          MOV(s, RAX);
          SHR(IM(1), RAX);
          switch (ir->dst->vtype->size) {
          case SZ_FLOAT:   CVTSI2SS(RAX, d); ADDSS(d, d); break;
          case SZ_DOUBLE:  CVTSI2SD(RAX, d); ADDSD(d, d); break;
          default: assert(false); break;
          }
          EMIT_LABEL(fmt_name(skiplabel));
        }
      }
      break;
    } else if (ir->opr1->vtype->flag & VRTF_FLONUM) {
      // flonum->fix
      int powd = kPow2Table[ir->dst->vtype->size];
      if (powd < 2)
        powd = 2;
      switch (ir->opr1->vtype->size) {
      case SZ_FLOAT:   CVTTSS2SI(kFReg64s[ir->opr1->phys], kRegSizeTable[powd][ir->dst->phys]); break;
      case SZ_DOUBLE:  CVTTSD2SI(kFReg64s[ir->opr1->phys], kRegSizeTable[powd][ir->dst->phys]); break;
      default: assert(false); break;
      }
      break;
    }
#endif
    if (ir->dst->vtype->size <= ir->opr1->vtype->size) {
      if (ir->dst->phys != ir->opr1->phys) {
        assert(0 <= ir->dst->vtype->size && ir->dst->vtype->size < kPow2TableSize);
        int pow = kPow2Table[ir->dst->vtype->size];
        assert(0 <= pow && pow < 4);
        const char **regs = kRegSizeTable[pow];
        MOV(regs[ir->opr1->phys], regs[ir->dst->phys]);
      }
    } else {
      assert(0 <= ir->opr1->vtype->size && ir->opr1->vtype->size < kPow2TableSize);
      int pows = kPow2Table[ir->opr1->vtype->size];
      assert(0 <= ir->dst->vtype->size && ir->dst->vtype->size < kPow2TableSize);
      int powd = kPow2Table[ir->dst->vtype->size];
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
          switch (ir->dst->vtype->size) {
          case SZ_FLOAT: MOVSS(kFReg64s[ir->opr1->phys], kFReg64s[ir->dst->phys]); break;
          case SZ_DOUBLE: MOVSD(kFReg64s[ir->opr1->phys], kFReg64s[ir->dst->phys]); break;
          default: assert(false); break;
          }
          break;
        }
      }
#endif
      assert(0 <= ir->dst->vtype->size && ir->dst->vtype->size < kPow2TableSize);
      assert(!(ir->dst->flag & VRF_CONST));
      int pow = kPow2Table[ir->dst->vtype->size];
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
    ir_memcpy(ir->opr2->phys, ir->opr1->phys, ir->memcpy.size);
    break;

  case IR_CLEAR:
    {
      assert(!(ir->opr1->flag & VRF_CONST));
      const Name *label = alloc_label();
      MOV(kReg64s[ir->opr1->phys], RSI);
      MOV(IM(ir->clear.size), EDI);
      XOR(AL, AL);
      EMIT_LABEL(fmt_name(label));
      MOV(AL, INDIRECT(RSI, NULL, 1));
      INC(RSI);
      DEC(EDI);
      JNE(fmt_name(label));
    }
    break;

  case IR_ASM:
    EMIT_ASM(ir->asm_.str);
    if (ir->dst != NULL) {
      assert(0 <= ir->dst->vtype->size && ir->dst->vtype->size < kPow2TableSize);
      assert(!(ir->dst->flag & VRF_CONST));
      int pow = kPow2Table[ir->dst->vtype->size];
      assert(0 <= pow && pow < 4);
      const char **regs = kRegSizeTable[pow];
      MOV(kRegATable[pow], regs[ir->dst->phys]);
    }
    break;

  case IR_LOAD_SPILLED:
    assert(ir->opr1->flag & VRF_SPILLED);
#ifndef __NO_FLONUM
    if (ir->opr1->vtype->flag & VRTF_FLONUM) {
      const char **regs = kFReg64s;
      switch (ir->dst->vtype->size) {
      case SZ_FLOAT: MOVSS(OFFSET_INDIRECT(ir->opr1->offset, RBP, NULL, 1), regs[ir->dst->phys]); break;
      case SZ_DOUBLE: MOVSD(OFFSET_INDIRECT(ir->opr1->offset, RBP, NULL, 1), regs[ir->dst->phys]); break;
      default: assert(false); break;
      }
      break;
    }
#endif
    {
      assert(0 <= ir->dst->vtype->size && ir->dst->vtype->size < kPow2TableSize);
      int pow = kPow2Table[ir->dst->vtype->size];
      assert(0 <= pow && pow < 4);
      const char **regs = kRegSizeTable[pow];
      MOV(OFFSET_INDIRECT(ir->opr1->offset, RBP, NULL, 1), regs[ir->dst->phys]);
    }
    break;

  case IR_STORE_SPILLED:
    assert(ir->opr2->flag & VRF_SPILLED);
#ifndef __NO_FLONUM
    if (ir->opr2->vtype->flag & VRTF_FLONUM) {
      const char **regs = kFReg64s;
      switch (ir->opr1->vtype->size) {
      case SZ_FLOAT: MOVSS(regs[ir->opr1->phys], OFFSET_INDIRECT(ir->opr2->offset, RBP, NULL, 1)); break;
      case SZ_DOUBLE: MOVSD(regs[ir->opr1->phys], OFFSET_INDIRECT(ir->opr2->offset, RBP, NULL, 1)); break;
      default: assert(false); break;
      }
      break;
    }
#endif
    {
      assert(0 <= ir->opr1->vtype->size && ir->opr1->vtype->size < kPow2TableSize);
      int pow = kPow2Table[ir->opr1->vtype->size];
      assert(0 <= pow && pow < 4);
      const char **regs = kRegSizeTable[pow];
      MOV(regs[ir->opr1->phys], OFFSET_INDIRECT(ir->opr2->offset, RBP, NULL, 1));
    }
    break;

  default: assert(false); break;
  }
}

//

int push_callee_save_regs(unsigned long used, unsigned long fused) {
  // Assume no callee save freg exists.
  UNUSED(fused);

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

void pop_callee_save_regs(unsigned long used, unsigned long fused) {
  // Assume no callee save freg exists.
  UNUSED(fused);

  for (int i = CALLEE_SAVE_REG_COUNT; --i >= 0;) {
    int ireg = kCalleeSaveRegs[i];
    if (used & (1 << ireg)) {
      POP(kReg64s[ireg]);
      POP_STACK_POS();
    }
  }
}

static void push_caller_save_regs(unsigned long living, int base) {
#ifndef __NO_FLONUM
  {
    for (int i = CALLER_SAVE_FREG_COUNT; i > 0;) {
      int ireg = kCallerSaveFRegs[--i];
      if (living & (1UL << (ireg + PHYSICAL_REG_MAX))) {
        // TODO: Detect register size.
        MOVSD(kFReg64s[ireg], OFFSET_INDIRECT(base, RSP, NULL, 1));
        base += WORD_SIZE;
      }
    }
  }
#endif

  for (int i = CALLER_SAVE_REG_COUNT; i > 0;) {
    int ireg = kCallerSaveRegs[--i];
    if (living & (1UL << ireg)) {
      MOV(kReg64s[ireg], OFFSET_INDIRECT(base, RSP, NULL, 1));
      base += WORD_SIZE;
    }
  }
}

static void pop_caller_save_regs(unsigned long living) {
#ifndef __NO_FLONUM
  {
    int count = 0;
    for (int i = CALLER_SAVE_FREG_COUNT; i > 0;) {
      int ireg = kCallerSaveFRegs[--i];
      if (living & (1UL << (ireg + PHYSICAL_REG_MAX))) {
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
    if (living & (1UL << ireg)) {
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
static void convert_3to2(BBContainer *bbcon) {
  for (int i = 0; i < bbcon->bbs->len; ++i) {
    BB *bb = bbcon->bbs->data[i];
    Vector *irs = bb->irs;
    for (int i = 0; i < irs->len; ++i) {
      IR *ir = irs->data[i];
      switch (ir->kind) {
      case IR_ADD:  // binops
      case IR_SUB:
      case IR_MUL:
      case IR_DIV:
      case IR_MOD:
      case IR_BITAND:
      case IR_BITOR:
      case IR_BITXOR:
      case IR_LSHIFT:
      case IR_RSHIFT:
      case IR_NEG:  // unary ops
      case IR_BITNOT:
        {
          assert(!(ir->dst->flag & VRF_CONST));
          IR *mov = new_ir_mov(ir->dst, ir->opr1);
          vec_insert(irs, i++, mov);
          ir->opr1 = ir->dst;
        }
        break;

      default: break;
      }
    }
  }
}

static void insert_const_mov(VReg **pvreg, RegAlloc *ra, Vector *irs, int i) {
  VReg *c = *pvreg;
  VReg *tmp = reg_alloc_spawn(ra, c->vtype, 0);
  IR *mov = new_ir_mov(tmp, c);
  vec_insert(irs, i, mov);
  *pvreg = tmp;
}

void tweak_irs(FuncBackend *fnbe) {
  convert_3to2(fnbe->bbcon);

  BBContainer *bbcon = fnbe->bbcon;
  RegAlloc *ra = fnbe->ra;
  for (int i = 0; i < bbcon->bbs->len; ++i) {
    BB *bb = bbcon->bbs->data[i];
    Vector *irs = bb->irs;
    for (int i = 0; i < irs->len; ++i) {
      IR *ir = irs->data[i];
      switch (ir->kind) {
      case IR_MUL:
      case IR_DIV:
      case IR_MOD:
        assert(!(ir->opr1->flag & VRF_CONST));
        if (ir->opr2->flag & VRF_CONST)
          insert_const_mov(&ir->opr2, ra, irs, i++);
        break;

      default: break;
      }
    }
  }
}
