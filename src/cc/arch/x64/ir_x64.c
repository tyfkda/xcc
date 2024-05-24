#include "../../../config.h"
#include "./arch_config.h"
#include "ir.h"

#include <assert.h>
#include <stdlib.h>  // malloc
#include <string.h>

#include "ast.h"
#include "emit_code.h"
#include "regalloc.h"
#include "table.h"
#include "util.h"
#include "x64.h"

static Vector *push_caller_save_regs(uint64_t living);
static void pop_caller_save_regs(Vector *saves);

int stackpos = 8;

// Register allocator

const char *kRegSizeTable[][PHYSICAL_REG_MAX] = {
  { AL, DIL, SIL,  DL,  CL, R8B, R9B,  BL, R12B, R13B, R14B, R15B, BPL, R10B, R11B},
  { AX,  DI,  SI,  DX,  CX, R8W, R9W,  BX, R12W, R13W, R14W, R15W,  BP, R10W, R11W},
  {EAX, EDI, ESI, EDX, ECX, R8D, R9D, EBX, R12D, R13D, R14D, R15D, EBP, R10D, R11D},
  {RAX, RDI, RSI, RDX, RCX,  R8,  R9, RBX,  R12,  R13,  R14,  R15, RBP,  R10,  R11},
};

// Return index of %rcx register.
// Detect the index using the fact that %rcx is 4th parameter on calling convention.
#define GET_AREG_INDEX()  0
#define GET_CREG_INDEX()  4  // ArchRegParamMapping[3]
#define GET_DREG_INDEX()  3  // ArchRegParamMapping[2]
#define GET_BPREG_INDEX() 12

#define CALLEE_SAVE_REG_COUNT  ((int)ARRAY_SIZE(kCalleeSaveRegs))
static const int kCalleeSaveRegs[] = {7, 8, 9, 10, 11, 12};

#define CALLER_SAVE_REG_COUNT  ((int)ARRAY_SIZE(kCallerSaveRegs))
static const int kCallerSaveRegs[] = {13, 14};

const int ArchRegParamMapping[] = {1, 2, 3, 4, 5, 6};

#define kReg8s   (kRegSizeTable[0])
#define kReg32s  (kRegSizeTable[2])
#define kReg64s  (kRegSizeTable[3])

#define SZ_FLOAT   VRegSize4
#define SZ_DOUBLE  VRegSize8
const char *kFReg64s[PHYSICAL_FREG_MAX] = {
  XMM0, XMM1, XMM2, XMM3, XMM4, XMM5, XMM6, XMM7,
  XMM8, XMM9, XMM10, XMM11, XMM12, XMM13, XMM14, XMM15};

#define GET_XMM0_INDEX()   0

#define CALLER_SAVE_FREG_COUNT  ((int)ARRAY_SIZE(kCallerSaveFRegs))
static const int kCallerSaveFRegs[] = {8, 9, 10, 11, 12, 13, 14, 15};

static uint64_t detect_extra_occupied(RegAlloc *ra, IR *ir) {
  uint64_t ioccupy = 0;
  switch (ir->kind) {
  case IR_MUL: case IR_DIV: case IR_MOD:
    if (!(ir->dst->flag & VRF_FLONUM))
      ioccupy = (1ULL << GET_DREG_INDEX()) | (1ULL << GET_AREG_INDEX());
    break;
  case IR_LSHIFT: case IR_RSHIFT:
    if (!(ir->opr2->flag & VRF_CONST))
      ioccupy = 1ULL << GET_CREG_INDEX();
    break;
  case IR_CALL:
    if (ir->call.vaarg_start >= 0) {
      // Break %al if function is vaarg.
      ioccupy = 1ULL << GET_AREG_INDEX();
    }
    break;
  default: break;
  }
  if (ra->flag & RAF_STACK_FRAME)
    ioccupy |= 1ULL << GET_BPREG_INDEX();
  return ioccupy;
}

const RegAllocSettings kArchRegAllocSettings = {
  .detect_extra_occupied = detect_extra_occupied,
  .reg_param_mapping = ArchRegParamMapping,
  .phys_max = PHYSICAL_REG_MAX,
  .phys_temporary_count = PHYSICAL_REG_TEMPORARY,
#ifndef __NO_FLONUM
  .fphys_max = PHYSICAL_FREG_MAX,
  .fphys_temporary_count = PHYSICAL_FREG_TEMPORARY,
#endif
};

//

static bool is_got(const Name *name) {
#if XCC_TARGET_PLATFORM == XCC_PLATFORM_APPLE
  // TODO: How to detect the label is GOT?
  return name->bytes >= 5 && strncmp(name->chars, "__std", 5) == 0;  // __stdinp, etc.
#else
  UNUSED(name);
  return false;
#endif
}

static void ei_bofs(IR *ir) {
  int64_t offset = ir->bofs.frameinfo->offset + ir->bofs.offset;
  LEA(OFFSET_INDIRECT(offset, RBP, NULL, 1), kReg64s[ir->dst->phys]);
}

static void ei_iofs(IR *ir) {
  char *label = fmt_name(ir->iofs.label);
  if (ir->iofs.global)
    label = MANGLE(label);
  label = quote_label(label);
  const char *dst = kReg64s[ir->dst->phys];
  if (!is_got(ir->iofs.label))
    LEA(LABEL_INDIRECT(label, ir->iofs.offset, RIP), dst);
  else
    MOV(LABEL_INDIRECT(GOTPCREL(label), ir->iofs.offset, RIP), dst);
}

static void ei_sofs(IR *ir) {
  assert(ir->opr1->flag & VRF_CONST);
  LEA(OFFSET_INDIRECT(ir->opr1->fixnum, RSP, NULL, 1), kReg64s[ir->dst->phys]);
}

#define ei_load_s  ei_load
static void ei_load(IR *ir) {
  const char *src;
  if (ir->kind == IR_LOAD) {
    if (ir->opr1->flag & VRF_CONST) {
      src = fmt("0x%x", ir->opr1->fixnum);
    } else {
      assert(!(ir->opr1->flag & VRF_SPILLED));
      src = INDIRECT(kReg64s[ir->opr1->phys], NULL, 1);
    }
  } else {
    assert(!(ir->opr1->flag & VRF_CONST));
    assert(ir->opr1->flag & VRF_SPILLED);
    src = OFFSET_INDIRECT(ir->opr1->frame.offset, RBP, NULL, 1);
  }

  if (ir->dst->flag & VRF_FLONUM) {
    switch (ir->dst->vsize) {
    case SZ_FLOAT:  MOVSS(src, kFReg64s[ir->dst->phys]); break;
    case SZ_DOUBLE: MOVSD(src, kFReg64s[ir->dst->phys]); break;
    default: assert(false); break;
    }
  } else {
    int pow = ir->dst->vsize;
    assert(0 <= pow && pow < 4);
    const char **regs = kRegSizeTable[pow];
    MOV(src, regs[ir->dst->phys]);
  }
}

#define ei_store_s  ei_store
static void ei_store(IR *ir) {
  const char *target;
  if (ir->kind == IR_STORE) {
    if (ir->opr2->flag & VRF_CONST) {
      target = fmt("0x%x", ir->opr2->fixnum);
    } else {
      assert(!(ir->opr2->flag & VRF_SPILLED));
      target = INDIRECT(kReg64s[ir->opr2->phys], NULL, 1);
    }
  } else {
    assert(!(ir->opr2->flag & VRF_CONST));
    assert(ir->opr2->flag & VRF_SPILLED);
    target = OFFSET_INDIRECT(ir->opr2->frame.offset, RBP, NULL, 1);
  }

  if (ir->opr1->flag & VRF_FLONUM) {
    switch (ir->opr1->vsize) {
    case SZ_FLOAT:  MOVSS(kFReg64s[ir->opr1->phys], target); break;
    case SZ_DOUBLE: MOVSD(kFReg64s[ir->opr1->phys], target); break;
    default: assert(false); break;
    }
  } else {
    int pow = ir->opr1->vsize;
    assert(0 <= pow && pow < 4);
    if (ir->opr1->flag & VRF_CONST) {
      switch (pow) {
      case 0: MOVB(IM(ir->opr1->fixnum), target); break;
      case 1: MOVW(IM(ir->opr1->fixnum), target); break;
      case 2: MOVL(IM(ir->opr1->fixnum), target); break;
      case 3: MOVQ(IM(ir->opr1->fixnum), target); break;
      default: assert(false); break;
      }
    } else {
      const char **regs = kRegSizeTable[pow];
      MOV(regs[ir->opr1->phys], target);
    }
  }
}

static void ei_add(IR *ir) {
  assert(ir->dst->phys == ir->opr1->phys);
  if (ir->dst->flag & VRF_FLONUM) {
    const char **regs = kFReg64s;
    switch (ir->dst->vsize) {
    case SZ_FLOAT: ADDSS(regs[ir->opr2->phys], regs[ir->dst->phys]); break;
    case SZ_DOUBLE: ADDSD(regs[ir->opr2->phys], regs[ir->dst->phys]); break;
    default: assert(false); break;
    }
  } else {
    assert(!(ir->opr1->flag & VRF_CONST));
    int pow = ir->dst->vsize;
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
}

static void ei_sub(IR *ir) {
  assert(ir->dst->phys == ir->opr1->phys);
  if (ir->dst->flag & VRF_FLONUM) {
    const char **regs = kFReg64s;
    switch (ir->dst->vsize) {
    case SZ_FLOAT: SUBSS(regs[ir->opr2->phys], regs[ir->dst->phys]); break;
    case SZ_DOUBLE: SUBSD(regs[ir->opr2->phys], regs[ir->dst->phys]); break;
    default: assert(false); break;
    }
  } else {
    assert(!(ir->opr1->flag & VRF_CONST));
    int pow = ir->dst->vsize;
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
}

static void ei_mul(IR *ir) {
  assert(!(ir->opr1->flag & VRF_CONST) && !(ir->opr2->flag & VRF_CONST));
  if (ir->dst->flag & VRF_FLONUM) {
    assert(ir->dst->phys == ir->opr1->phys);
    const char **regs = kFReg64s;
    switch (ir->dst->vsize) {
    case SZ_FLOAT: MULSS(regs[ir->opr2->phys], regs[ir->dst->phys]); break;
    case SZ_DOUBLE: MULSD(regs[ir->opr2->phys], regs[ir->dst->phys]); break;
    default: assert(false); break;
    }
  } else {
    // Break %rax, %rdx
    assert(ir->dst->phys == ir->opr1->phys);
    assert(ir->opr2->phys != GET_AREG_INDEX());
    int pow = ir->dst->vsize;
    assert(0 <= pow && pow < 4);
    const char **regs = kRegSizeTable[pow];
    const char *a = regs[GET_AREG_INDEX()];
    if (ir->opr1->phys != GET_AREG_INDEX())
      MOV(regs[ir->opr1->phys], a);
    MUL(regs[ir->opr2->phys]);
    if (ir->dst->phys != GET_AREG_INDEX())
      MOV(a, regs[ir->dst->phys]);
  }
}

static void ei_div(IR *ir) {
  assert(!(ir->opr1->flag & VRF_CONST) && !(ir->opr2->flag & VRF_CONST));
  if (ir->dst->flag & VRF_FLONUM) {
    assert(ir->dst->phys == ir->opr1->phys);
    const char **regs = kFReg64s;
    switch (ir->dst->vsize) {
    case SZ_FLOAT: DIVSS(regs[ir->opr2->phys], regs[ir->dst->phys]); break;
    case SZ_DOUBLE: DIVSD(regs[ir->opr2->phys], regs[ir->dst->phys]); break;
    default: assert(false); break;
    }
  } else if (ir->dst->vsize == VRegSize1) {
    assert(ir->dst->phys == ir->opr1->phys);
    assert(ir->opr2->phys != GET_AREG_INDEX());
    // Break %ax
    if (!(ir->flag & IRF_UNSIGNED)) {
      if (ir->opr1->phys != GET_AREG_INDEX())
        MOVSX(kReg8s[ir->opr1->phys], AX);
      IDIV(kReg8s[ir->opr2->phys]);
    } else {
      if (ir->opr1->phys != GET_AREG_INDEX())
        MOVZX(kReg8s[ir->opr1->phys], AX);
      DIV(kReg8s[ir->opr2->phys]);
    }
    if (ir->dst->phys != GET_AREG_INDEX())
      MOV(AL, kReg8s[ir->dst->phys]);
  } else {
    assert(ir->dst->phys == ir->opr1->phys);
    assert(ir->opr2->phys != GET_AREG_INDEX());
    // Break %rax, %rdx
    int pow = ir->dst->vsize;
    assert(0 <= pow && pow < 4);
    const char **regs = kRegSizeTable[pow];
    const char *a = regs[GET_AREG_INDEX()];
    if (ir->opr1->phys != GET_AREG_INDEX())
      MOV(regs[ir->opr1->phys], a);
    if (!(ir->flag & IRF_UNSIGNED)) {
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
    if (ir->dst->phys != GET_AREG_INDEX())
      MOV(a, regs[ir->dst->phys]);
  }
}

static void ei_mod(IR *ir) {
  assert(!(ir->opr1->flag & VRF_CONST) && !(ir->opr2->flag & VRF_CONST));
  if (ir->dst->vsize == 1) {
    assert(ir->dst->phys == ir->opr1->phys);
    assert(ir->opr2->phys != GET_AREG_INDEX());
    // Break %ax
    if (!(ir->flag & IRF_UNSIGNED)) {
      if (ir->opr1->phys != GET_AREG_INDEX())
        MOVSX(kReg8s[ir->opr1->phys], AX);
      IDIV(kReg8s[ir->opr2->phys]);
    } else {
      if (ir->opr1->phys != GET_AREG_INDEX())
        MOVZX(kReg8s[ir->opr1->phys], AX);
      DIV(kReg8s[ir->opr2->phys]);
    }
    // Cannot `mov` directly from %ah to %r8b
    // MOV(AH, kReg8s[ir->dst->phys]);
    MOV(AH, AL);
    if (ir->dst->phys != GET_AREG_INDEX())
      MOV(AL, kReg8s[ir->dst->phys]);
  } else {
    assert(ir->dst->phys == ir->opr1->phys);
    assert(ir->opr2->phys != GET_AREG_INDEX());
    // Break %rax, %rdx
    int pow = ir->dst->vsize;
    assert(0 <= pow && pow < 4);
    const char **regs = kRegSizeTable[pow];
    const char *a = regs[GET_AREG_INDEX()];
    if (ir->opr1->phys != GET_AREG_INDEX())
      MOV(regs[ir->opr1->phys], a);
    if (!(ir->flag & IRF_UNSIGNED)) {
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
    const int dreg = GET_DREG_INDEX();
    if (ir->dst->phys != dreg)
      MOV(regs[dreg], regs[ir->dst->phys]);
  }
}

static void ei_bitand(IR *ir) {
  assert(ir->dst->phys == ir->opr1->phys);
  assert(!(ir->opr1->flag & VRF_CONST));
  int pow = ir->dst->vsize;
  assert(0 <= pow && pow < 4);
  const char **regs = kRegSizeTable[pow];
  if (ir->opr2->flag & VRF_CONST)
    AND(IM(ir->opr2->fixnum), regs[ir->dst->phys]);
  else
    AND(regs[ir->opr2->phys], regs[ir->dst->phys]);
}

static void ei_bitor(IR *ir) {
  assert(ir->dst->phys == ir->opr1->phys);
  assert(!(ir->opr1->flag & VRF_CONST));
  int pow = ir->dst->vsize;
  assert(0 <= pow && pow < 4);
  const char **regs = kRegSizeTable[pow];
  if (ir->opr2->flag & VRF_CONST)
    OR(IM(ir->opr2->fixnum), regs[ir->dst->phys]);
  else
    OR(regs[ir->opr2->phys], regs[ir->dst->phys]);
}

static void ei_bitxor(IR *ir) {
  assert(ir->dst->phys == ir->opr1->phys);
  assert(!(ir->opr1->flag & VRF_CONST));
  int pow = ir->dst->vsize;
  assert(0 <= pow && pow < 4);
  const char **regs = kRegSizeTable[pow];
  if (ir->opr2->flag & VRF_CONST)
    XOR(IM(ir->opr2->fixnum), regs[ir->dst->phys]);
  else
    XOR(regs[ir->opr2->phys], regs[ir->dst->phys]);
}

static void ei_lshift(IR *ir) {
  assert(ir->dst->phys == ir->opr1->phys);
  assert(!(ir->opr1->flag & VRF_CONST));
  int pow = ir->dst->vsize;
  assert(0 <= pow && pow < 4);
  const char **regs = kRegSizeTable[pow];
  const char *dst = regs[ir->dst->phys];
  if (ir->opr2->flag & VRF_CONST) {
    SHL(IM(ir->opr2->fixnum), dst);
  } else {
    assert(ir->opr2->phys != GET_CREG_INDEX());
    assert(ir->dst->phys != GET_CREG_INDEX());
    MOV(kReg8s[ir->opr2->phys], CL);
    SHL(CL, dst);
  }
}

static void ei_rshift(IR *ir) {
#define RSHIFT_INST(n, x)  do  { if (ir->flag & IRF_UNSIGNED) SHR(n, x); else SAR(n, x); } while (0)
  assert(ir->dst->phys == ir->opr1->phys);
  assert(!(ir->opr1->flag & VRF_CONST));
  int pow = ir->dst->vsize;
  assert(0 <= pow && pow < 4);
  const char **regs = kRegSizeTable[pow];
  const char *dst = regs[ir->dst->phys];
  if (ir->opr2->flag & VRF_CONST) {
    RSHIFT_INST(IM(ir->opr2->fixnum), dst);
  } else {
    assert(ir->opr2->phys != GET_CREG_INDEX());
    assert(ir->dst->phys != GET_CREG_INDEX());
    MOV(kReg8s[ir->opr2->phys], CL);
    RSHIFT_INST(CL, dst);
  }
#undef RSHIFT_INST
}

static void cmp_vregs(VReg *opr1, VReg *opr2) {
  if (opr1->flag & VRF_FLONUM) {
    assert(opr2->flag & VRF_FLONUM);
    switch (opr1->vsize) {
    case SZ_FLOAT: UCOMISS(kFReg64s[opr2->phys], kFReg64s[opr1->phys]); break;
    case SZ_DOUBLE: UCOMISD(kFReg64s[opr2->phys], kFReg64s[opr1->phys]); break;
    default: assert(false); break;
    }
  } else {
    assert(!(opr1->flag & VRF_CONST));
    int pow = opr1->vsize;
    assert(0 <= pow && pow < 4);
    const char **regs = kRegSizeTable[pow];
    const char *o1;
    o1 = regs[opr1->phys];
    if ((opr2->flag & VRF_CONST) && opr2->fixnum == 0) {
      TEST(o1, o1);
    } else {
      const char *o2 = (opr2->flag & VRF_CONST) ? IM(opr2->fixnum) : regs[opr2->phys];
      CMP(o2, o1);
    }
  }
}

static void ei_neg(IR *ir) {
  assert(!(ir->dst->flag & VRF_CONST));
  if (ir->opr1->flag & VRF_FLONUM) {
    assert(ir->dst->phys != ir->opr1->phys);
    VReg *dst = ir->dst, *opr1 = ir->opr1;
    switch (opr1->vsize) {
    case SZ_FLOAT:
    case SZ_DOUBLE:
      {
        bool single = opr1->vsize == SZ_FLOAT;
        const Name *signbit_label = alloc_label();
        const char *dreg = kFReg64s[dst->phys];
        // TODO: MOVSD(LABEL_INDIRECT(fmt_name(signbit_label), 0, RIP), dreg);
        PUSH(RAX);
        LEA(LABEL_INDIRECT(fmt_name(signbit_label), 0, RIP), RAX);
        if (single)
          MOVSS(INDIRECT(RAX, NULL, 1), dreg);
        else
          MOVSD(INDIRECT(RAX, NULL, 1), dreg);
        POP(RAX);
        if (single)
          XORPS(kFReg64s[opr1->phys], dreg);
        else
          XORPD(kFReg64s[opr1->phys], dreg);

        _RODATA();  // gcc warns, should be put into .data section?
        EMIT_ALIGN(8);
        EMIT_LABEL(fmt_name(signbit_label));
        if (single)
          _LONG(hexnum(1U << 31));
        else
          _QUAD(hexnum(1ULL << 63));
        _TEXT();
      }
      break;
    default: assert(false); break;
    }
  } else {
    assert(ir->dst->phys == ir->opr1->phys);
    int pow = ir->dst->vsize;
    assert(0 <= pow && pow < 4);
    const char **regs = kRegSizeTable[pow];
    NEG(regs[ir->dst->phys]);
  }
}

static void ei_bitnot(IR *ir) {
  assert(ir->dst->phys == ir->opr1->phys);
  assert(!(ir->dst->flag & VRF_CONST));
  int pow = ir->dst->vsize;
  assert(0 <= pow && pow < 4);
  const char **regs = kRegSizeTable[pow];
  NOT(regs[ir->dst->phys]);
}

static void ei_cond(IR *ir) {
  cmp_vregs(ir->opr1, ir->opr2);

  assert(!(ir->dst->flag & VRF_CONST));
  const char *dst = kReg8s[ir->dst->phys];
  int cond = ir->cond.kind;
  // On x64, flag for comparing flonum is same as unsigned.
  if (cond & COND_FLONUM)
    cond ^= COND_FLONUM | COND_UNSIGNED;  // Turn off flonum, and turn on unsigned
  switch (cond) {
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

static void ei_jmp(IR *ir) {
  const char *label = fmt_name(ir->jmp.bb->label);
  int cond = ir->jmp.cond;
  // On x64, flag for comparing flonum is same as unsigned.
  if (cond & COND_FLONUM)
    cond ^= COND_FLONUM | COND_UNSIGNED;  // Turn off flonum, and turn on unsigned

  if (cond == COND_ANY) {
    JMP(label);
    return;
  }

  cmp_vregs(ir->opr1, ir->opr2);

  switch (cond) {
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

static void ei_tjmp(IR *ir) {
  int phys = ir->opr1->phys;
  const int powd = 3;
  int pows = ir->opr1->vsize;
  assert(0 <= pows && pows < 4);
  if (pows < powd) {
    if (pows == 2) {
      // MOVZX %64bit, %32bit doesn't exist!
      MOV(kRegSizeTable[pows][phys], kRegSizeTable[pows][phys]);
    } else {
      MOVZX(kRegSizeTable[pows][phys], kRegSizeTable[powd][phys]);
    }
  }

  assert(ir->opr2 != NULL);
  const char *opr2 = kReg64s[ir->opr2->phys];

  const Name *table_label = alloc_label();
  LEA(LABEL_INDIRECT(fmt_name(table_label), 0, RIP), opr2);
  JMP(fmt("*%s", OFFSET_INDIRECT(0, opr2, kReg64s[phys], 8)));

  _RODATA();  // gcc warns, should be put into .data section?
  EMIT_ALIGN(8);
  EMIT_LABEL(fmt_name(table_label));
  for (size_t i = 0, len = ir->tjmp.len; i < len; ++i) {
    BB *bb = ir->tjmp.bbs[i];
    _QUAD(fmt("%.*s", NAMES(bb->label)));
  }
  _TEXT();
}

static void ei_precall(IR *ir) {
  // Living registers are not modified between preparing function arguments,
  // so safely saved before calculating argument values.
  ir->precall.caller_saves = push_caller_save_regs(ir->precall.living_pregs);

  int align_stack = (16 - (stackpos + ir->precall.stack_args_size)) & 15;
  ir->precall.stack_aligned = align_stack;

  if (align_stack > 0) {
    SUB(IM(align_stack), RSP);
    stackpos += align_stack;
  }
}

static void ei_pusharg(IR *ir) {
  if (ir->opr1->flag & VRF_FLONUM) {
    // Assume parameter registers are arranged from index 0.
    if (ir->pusharg.index != ir->opr1->phys) {
      switch (ir->opr1->vsize) {
      case SZ_FLOAT: MOVSS(kFReg64s[ir->opr1->phys], kFReg64s[ir->pusharg.index]); break;
      case SZ_DOUBLE: MOVSD(kFReg64s[ir->opr1->phys], kFReg64s[ir->pusharg.index]); break;
      default: assert(false); break;
      }
    }
  } else {
    const int PARAM_REG_START_INDEX = 1;  // Parameter registers are arranged from this index.
    int pow = ir->opr1->vsize;
    assert(0 <= pow && pow < 4);
    const char *dst = kRegSizeTable[pow][ir->pusharg.index + PARAM_REG_START_INDEX];
    if (ir->opr1->flag & VRF_CONST)
      MOV(IM(ir->opr1->fixnum), dst);
    else if (ir->pusharg.index + PARAM_REG_START_INDEX != ir->opr1->phys)
      MOV(kRegSizeTable[pow][ir->opr1->phys], dst);
  }
}

static void ei_call(IR *ir) {
  if (ir->call.vaarg_start >= 0) {
    int total_arg_count = ir->call.total_arg_count;
    int freg = 0;
    for (int i = 0; i < total_arg_count; ++i) {
      if (ir->call.args[i]->flag & VRF_FLONUM) {
        ++freg;
        if (freg >= MAX_FREG_ARGS)
          break;
      }
    }

    // Break %al
    if (freg > 0)
      MOV(IM(freg), AL);
    else
      XOR(AL, AL);
  }
  if (ir->call.label != NULL) {
    char *label = fmt_name(ir->call.label);
    if (ir->call.global)
      label = MANGLE(label);
    CALL(quote_label(label));
  } else {
    assert(!(ir->opr1->flag & VRF_CONST));
    CALL(fmt("*%s", kReg64s[ir->opr1->phys]));
  }

  IR *precall = ir->call.precall;
  int align_stack = precall->precall.stack_aligned + precall->precall.stack_args_size;
  if (align_stack != 0) {
    ADD(IM(align_stack), RSP);
    stackpos -= precall->precall.stack_aligned;
  }

  // Resore caller save registers.
  pop_caller_save_regs(precall->precall.caller_saves);

  if (ir->dst != NULL) {
    if (ir->dst->flag & VRF_FLONUM) {
      if (ir->dst->phys != GET_XMM0_INDEX()) {
        switch (ir->dst->vsize) {
        case SZ_FLOAT: MOVSS(XMM0, kFReg64s[ir->dst->phys]); break;
        case SZ_DOUBLE: MOVSD(XMM0, kFReg64s[ir->dst->phys]); break;
        default: assert(false); break;
        }
      }
    } else {
      if (ir->dst->phys != GET_AREG_INDEX()) {
        int pow = ir->dst->vsize;
        assert(0 <= pow && pow < 4);
        const char **regs = kRegSizeTable[pow];
        MOV(regs[GET_AREG_INDEX()], regs[ir->dst->phys]);
      }
    }
  }
}

static void ei_result(IR *ir) {
  if (ir->opr1->flag & VRF_FLONUM) {
    int dstphys = ir->dst != NULL ? ir->dst->phys : GET_XMM0_INDEX();
    if (ir->opr1->phys != dstphys) {
      const char *dst = kFReg64s[dstphys];
      switch (ir->opr1->vsize) {
      case SZ_FLOAT: MOVSS(kFReg64s[ir->opr1->phys], dst); break;
      case SZ_DOUBLE: MOVSD(kFReg64s[ir->opr1->phys], dst); break;
      default: assert(false); break;
      }
    }
  } else {
    int pow = ir->opr1->vsize;
    assert(0 <= pow && pow < 4);
    const char **regs = kRegSizeTable[pow];
    int dstphys = ir->dst != NULL ? ir->dst->phys : GET_AREG_INDEX();
    const char *dst = regs[dstphys];
    if (ir->opr1->flag & VRF_CONST)
      MOV(IM(ir->opr1->fixnum), dst);
    else if (ir->opr1->phys != dstphys)
      MOV(regs[ir->opr1->phys], dst);
  }
}

static void ei_subsp(IR *ir) {
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
}

static void ei_cast(IR *ir) {
  assert((ir->opr1->flag & VRF_CONST) == 0);
  if (ir->dst->flag & VRF_FLONUM) {
    if (ir->opr1->flag & VRF_FLONUM) {
      // flonum->flonum
      assert(ir->dst->vsize != ir->opr1->vsize);
      // Assume flonum are just two types.
      switch (ir->dst->vsize) {
      case SZ_FLOAT: CVTSD2SS(kFReg64s[ir->opr1->phys], kFReg64s[ir->dst->phys]); break;
      case SZ_DOUBLE: CVTSS2SD(kFReg64s[ir->opr1->phys], kFReg64s[ir->dst->phys]); break;
      default: assert(false); break;
      }
    } else {
      // fix->flonum
      int pows = ir->opr1->vsize;
      if (pows < 2) {
        if (ir->flag & IRF_UNSIGNED)
          MOVZX(kRegSizeTable[pows][ir->opr1->phys], kRegSizeTable[2][ir->opr1->phys]);
        else
          MOVSX(kRegSizeTable[pows][ir->opr1->phys], kRegSizeTable[2][ir->opr1->phys]);
        pows = 2;
      }
      const char *s = kRegSizeTable[pows][ir->opr1->phys];
      const char *d = kFReg64s[ir->dst->phys];
      if (!(ir->flag & IRF_UNSIGNED)) {
        switch (ir->dst->vsize) {
        case SZ_FLOAT:   CVTSI2SS(s, d); break;
        case SZ_DOUBLE:  CVTSI2SD(s, d); break;
        default: assert(false); break;
        }
      } else if (pows < 3) {
        const char *s64 = kReg64s[ir->opr1->phys];
        switch (ir->dst->vsize) {
        case SZ_FLOAT:   CVTSI2SS(s64, d); break;
        case SZ_DOUBLE:  CVTSI2SD(s64, d); break;
        default: assert(false); break;
        }
      } else {
        // x64 support signed 64bit-signed-int to double only, so pass half value
        // (precision is lost anyway).
        const Name *neglabel = alloc_label();
        const Name *skiplabel = alloc_label();
        TEST(s, s);
        JS(fmt_name(neglabel));
        switch (ir->dst->vsize) {
        case SZ_FLOAT:   CVTSI2SS(s, d); break;
        case SZ_DOUBLE:  CVTSI2SD(s, d); break;
        default: assert(false); break;
        }
        JMP(fmt_name(skiplabel));
        EMIT_LABEL(fmt_name(neglabel));
        PUSH(RAX);  // Push %rax to avoid Break
        MOV(s, RAX);
        SHR(IM(1), RAX);
        switch (ir->dst->vsize) {
        case SZ_FLOAT:   CVTSI2SS(RAX, d); ADDSS(d, d); break;
        case SZ_DOUBLE:  CVTSI2SD(RAX, d); ADDSD(d, d); break;
        default: assert(false); break;
        }
        POP(RAX);  // Pop %rax
        EMIT_LABEL(fmt_name(skiplabel));
      }
    }
  } else if (ir->opr1->flag & VRF_FLONUM) {
    // flonum->fix
    int powd = ir->dst->vsize;
    if (powd < 2)
      powd = 2;
    switch (ir->opr1->vsize) {
    case SZ_FLOAT:   CVTTSS2SI(kFReg64s[ir->opr1->phys], kRegSizeTable[powd][ir->dst->phys]); break;
    case SZ_DOUBLE:  CVTTSD2SI(kFReg64s[ir->opr1->phys], kRegSizeTable[powd][ir->dst->phys]); break;
    default: assert(false); break;
    }
  } else {
    // fix->fix
    assert(ir->dst->vsize != ir->opr1->vsize);
    if (ir->dst->vsize < ir->opr1->vsize) {
      if (ir->dst->phys != ir->opr1->phys) {
        int pow = ir->dst->vsize;
        assert(0 <= pow && pow < 4);
        const char **regs = kRegSizeTable[pow];
        MOV(regs[ir->opr1->phys], regs[ir->dst->phys]);
      }
    } else {
      int pows = ir->opr1->vsize;
      int powd = ir->dst->vsize;
      assert(0 <= pows && pows < 4);
      assert(0 <= powd && powd < 4);
      if (ir->flag & IRF_UNSIGNED) {
        if (pows == 2 && powd == 3) {
          // MOVZX %64bit, %32bit doesn't exist!
          if (ir->dst->phys != ir->opr1->phys)
            MOV(kRegSizeTable[pows][ir->opr1->phys], kRegSizeTable[pows][ir->dst->phys]);
        } else {
          MOVZX(kRegSizeTable[pows][ir->opr1->phys], kRegSizeTable[powd][ir->dst->phys]);
        }
      } else {
        MOVSX(kRegSizeTable[pows][ir->opr1->phys], kRegSizeTable[powd][ir->dst->phys]);
      }
    }
  }
}

static void ei_mov(IR *ir) {
  if (ir->dst->flag & VRF_FLONUM) {
    if (ir->opr1->phys != ir->dst->phys) {
      switch (ir->dst->vsize) {
      case SZ_FLOAT: MOVSS(kFReg64s[ir->opr1->phys], kFReg64s[ir->dst->phys]); break;
      case SZ_DOUBLE: MOVSD(kFReg64s[ir->opr1->phys], kFReg64s[ir->dst->phys]); break;
      default: assert(false); break;
      }
    }
  } else {
    assert(!(ir->dst->flag & VRF_CONST));
    int pow = ir->dst->vsize;
    assert(0 <= pow && pow < 4);
    const char **regs = kRegSizeTable[pow];
    if (ir->opr1->flag & VRF_CONST) {
      MOV(IM(ir->opr1->fixnum), regs[ir->dst->phys]);
    } else {
      if (ir->opr1->phys != ir->dst->phys)
        MOV(regs[ir->opr1->phys], regs[ir->dst->phys]);
    }
  }
}

static void ei_keep(IR *ir) {
  UNUSED(ir);
}

static void ei_asm(IR *ir) {
  EMIT_ASM(ir->asm_.str);
  if (ir->dst != NULL) {
    assert(!(ir->dst->flag & VRF_CONST));
    int pow = ir->dst->vsize;
    assert(0 <= pow && pow < 4);
    const char **regs = kRegSizeTable[pow];
    if (ir->dst->phys != GET_AREG_INDEX())
      MOV(regs[GET_AREG_INDEX()], regs[ir->dst->phys]);
  }
}

//

static int count_callee_save_regs(uint64_t used, uint64_t fused) {
  // Assume no callee save freg exists.
  UNUSED(fused);

  int count = 0;
  for (int i = 0; i < CALLEE_SAVE_REG_COUNT; ++i) {
    int ireg = kCalleeSaveRegs[i];
    if (used & (1 << ireg))
      ++count;
  }
  return count;
}

int push_callee_save_regs(uint64_t used, uint64_t fused) {
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

void pop_callee_save_regs(uint64_t used, uint64_t fused) {
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

int calculate_func_param_bottom(Function *func) {
  FuncBackend *fnbe = func->extra;
  uint64_t used = fnbe->ra->used_reg_bits, fused = fnbe->ra->used_freg_bits;
  int callee_save_count = count_callee_save_regs(used, fused);

  return (callee_save_count * POINTER_SIZE) + (POINTER_SIZE * 2);  // Return address, saved base pointer.
}

static Vector *push_caller_save_regs(uint64_t living) {
  Vector *saves = new_vector();

  for (int i = 0; i < CALLER_SAVE_REG_COUNT; ++i) {
    int ireg = kCallerSaveRegs[i];
    if (living & (1ULL << ireg)) {
      const char *reg = kReg64s[ireg];
      PUSH(reg);
      PUSH_STACK_POS();
      vec_push(saves, reg);
    }
  }

  {
    int fstart = saves->len;
    for (int i = 0; i < CALLER_SAVE_FREG_COUNT; ++i) {
      int ireg = kCallerSaveFRegs[i];
      if (living & (1ULL << (ireg + PHYSICAL_REG_MAX))) {
        // TODO: Detect register size.
        vec_push(saves, kFReg64s[ireg]);
      }
    }
    int n = saves->len - fstart;
    if (n > 0) {
      int ofs = n * POINTER_SIZE;
      SUB(IM(ofs), RSP);
      stackpos += ofs;
      for (int i = 0; i < n; ++i) {
        ofs -= POINTER_SIZE;
        MOVSD(saves->data[i + fstart], OFFSET_INDIRECT(ofs, RSP, NULL, 1));
      }
    }
  }

  return saves;
}

static void pop_caller_save_regs(Vector *saves) {
  int i = saves->len;
  int ofs = 0;
  while (--i >= 0) {
    const char *reg = saves->data[i];
    if (strncmp(reg, "%xmm", 4) != 0)
      break;
    MOVSD(OFFSET_INDIRECT(ofs, RSP, NULL, 1), reg);
    ofs += POINTER_SIZE;
  }
  if (ofs > 0) {
    ADD(IM(ofs), RSP);
    stackpos -= ofs;
  }
  ++i;

  while (--i >= 0) {
    const char *reg = saves->data[i];
    POP(reg);
    POP_STACK_POS();
  }
}

void emit_bb_irs(BBContainer *bbcon) {
  typedef void (*EmitIrFunc)(IR *);
  static const EmitIrFunc table[] = {
    [IR_BOFS] = ei_bofs, [IR_IOFS] = ei_iofs, [IR_SOFS] = ei_sofs,
    [IR_LOAD] = ei_load, [IR_LOAD_S] = ei_load_s, [IR_STORE] = ei_store, [IR_STORE_S] = ei_store_s,
    [IR_ADD] = ei_add, [IR_SUB] = ei_sub, [IR_MUL] = ei_mul, [IR_DIV] = ei_div,
    [IR_MOD] = ei_mod, [IR_BITAND] = ei_bitand, [IR_BITOR] = ei_bitor,
    [IR_BITXOR] = ei_bitxor, [IR_LSHIFT] = ei_lshift, [IR_RSHIFT] = ei_rshift,
    [IR_NEG] = ei_neg, [IR_BITNOT] = ei_bitnot,
    [IR_COND] = ei_cond, [IR_JMP] = ei_jmp, [IR_TJMP] = ei_tjmp,
    [IR_PRECALL] = ei_precall, [IR_PUSHARG] = ei_pusharg, [IR_CALL] = ei_call,
    [IR_RESULT] = ei_result, [IR_SUBSP] = ei_subsp, [IR_CAST] = ei_cast,
    [IR_MOV] = ei_mov, [IR_KEEP] = ei_keep, [IR_ASM] = ei_asm,
  };

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
      assert(ir->kind < (int)ARRAY_SIZE(table));
      assert(table[ir->kind] != NULL);
      (*table[ir->kind])(ir);
    }
  }
}

static void insert_const_mov(VReg **pvreg, RegAlloc *ra, Vector *irs, int i) {
  VReg *c = *pvreg;
  VReg *tmp = reg_alloc_spawn(ra, c->vsize, c->flag & VRF_MASK);
  IR *mov = new_ir_mov(tmp, c, ((IR*)irs->data[i])->flag);
  vec_insert(irs, i, mov);
  *pvreg = tmp;
}

#define insert_tmp_mov  insert_const_mov

// Rewrite `A = B op C` to `A = B; A = A op C`.
static void convert_3to2(FuncBackend *fnbe) {
  BBContainer *bbcon = fnbe->bbcon;
  for (int i = 0; i < bbcon->bbs->len; ++i) {
    BB *bb = bbcon->bbs->data[i];
    Vector *irs = bb->irs;
    for (int j = 0; j < irs->len; ++j) {
      IR *ir = irs->data[j];
      switch (ir->kind) {
      case IR_NEG:  // unary ops
        if (ir->dst->flag & VRF_FLONUM) {
          // To use two xmm registers, keep opr1 and assign dst and opr1 in different register.
          assert(ir->dst->virt != ir->opr1->virt);
          insert_tmp_mov(&ir->opr1, fnbe->ra, irs, j++);

          IR *keep = new_ir_keep(NULL, ir->opr1, NULL);
          vec_insert(irs, ++j, keep);
          break;
        }
        // Fallthrough
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
      case IR_BITNOT:
        {
          assert(!(ir->dst->flag & VRF_CONST));
          IR *mov = new_ir_mov(ir->dst, ir->opr1, ir->flag);
          vec_insert(irs, j++, mov);
          ir->opr1 = ir->dst;
        }
        break;

      default: break;
      }
    }
  }
}

void tweak_irs(FuncBackend *fnbe) {
  convert_3to2(fnbe);

  BBContainer *bbcon = fnbe->bbcon;
  RegAlloc *ra = fnbe->ra;
  for (int i = 0; i < bbcon->bbs->len; ++i) {
    BB *bb = bbcon->bbs->data[i];
    Vector *irs = bb->irs;
    for (int j = 0; j < irs->len; ++j) {
      IR *ir = irs->data[j];

      if (ir->kind != IR_MOV) {
        VReg **vregs[] = {&ir->opr1, &ir->opr2};
        for (int k = 0; k < 2; ++k) {
          VReg **pp = vregs[k], *vreg = *pp;
          if (vreg != NULL && vreg->flag & VRF_CONST && !is_im32(vreg->fixnum))
            insert_const_mov(pp, ra, irs, j++);
        }
      }

      switch (ir->kind) {
      case IR_MUL:
      case IR_DIV:
      case IR_MOD:
        assert(!(ir->opr1->flag & VRF_CONST));
        if (ir->opr2->flag & VRF_CONST)
          insert_const_mov(&ir->opr2, ra, irs, j++);
        break;

      case IR_TJMP:
        {
          // Allocate temporary register to use calculation.
          VReg *tmp = reg_alloc_spawn(ra, VRegSize8, 0);
          IR *keep = new_ir_keep(tmp, NULL, NULL);  // Notify the register begins to be used.
          vec_insert(irs, j++, keep);

          // Store to opr2.
          assert(ir->opr2 == NULL);
          ir->opr2 = tmp;
        }
        break;
      case IR_CALL:
        if (ir->opr1 != NULL && (ir->opr1->flag & VRF_CONST)) {
          insert_const_mov(&ir->opr1, ra, irs, j++);
        }
        break;

      default: break;
      }
    }
  }
}
