#include "../../../config.h"
#include "./arch_config.h"
#include "ir.h"

#include <assert.h>
#include <stdlib.h>  // malloc
#include <string.h>

#include "ast.h"
#include "emit_code.h"
#include "regalloc.h"
#include "riscv64.h"
#include "table.h"
#include "util.h"

static Vector *push_caller_save_regs(unsigned long living);
static void pop_caller_save_regs(Vector *saves);

// Register allocator

// AArch64: Calling Convention
//   X8(XR):              Indirect return value address.
//   X16(IP0), X17(IP1):  Intra-Procedure-call scratch registers.
//   X18(PR):             Platform register. Used for some operating-system-specific special purpose or an additional caller-saved register.
//   X29(FP):             Frame pointer (Callee save)

// static const char *kReg32s[PHYSICAL_REG_MAX] = {
//   W0, W1, W2, W3, W4, W5, W6, W7, W8, W9, W16,            // Temporary
//   W19, W20, W21, W22, W23, W24, W25, W26, W27, W28, W29,  // Callee save
//   W10, W11, W12, W13, W14, W15, W18};                     // Caller save
// static const char *kReg64s[PHYSICAL_REG_MAX] = {
//   X0, X1, X2, X3, X4, X5, X6, X7, X8, X9, X16,            // Temporary
//   X19, X20, X21, X22, X23, X24, X25, X26, X27, X28, X29,  // Callee save
//   X10, X11, X12, X13, X14, X15, X18};                     // Caller save
const char *kReg64s[PHYSICAL_REG_MAX] = {
  A0, A1, A2, A3, A4, A5, A6, A7,                         // Temporary
  S2, S3, S4, S5, S6, S7, S8, S9, S10, S11, FP,           // Callee save
  T0, T1, T2};                                            // Caller save

#define GET_A0_INDEX()   0
// #define GET_X16_INDEX()  10

// #define CALLEE_SAVE_REG_COUNT  ((int)(sizeof(kCalleeSaveRegs) / sizeof(*kCalleeSaveRegs)))
// static const int kCalleeSaveRegs[] = {11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21};

// #define CALLER_SAVE_REG_COUNT  ((int)(sizeof(kCallerSaveRegs) / sizeof(*kCallerSaveRegs)))
// static const int kCallerSaveRegs[] = {22, 23, 24, 25, 26, 27, 28};

const int ArchRegParamMapping[] = {0, 1, 2, 3, 4, 5, 6, 7};

// const char **kRegSizeTable[] = {kReg32s, kReg32s, kReg32s, kReg64s};
// static const char *kZeroRegTable[] = {WZR, WZR, WZR, XZR};

// Break s1 in store, mod and tjmp
static const char *kTmpReg = S1;

// #define SZ_FLOAT   VRegSize4
// #define SZ_DOUBLE  VRegSize8
// const char *kFReg32s[PHYSICAL_FREG_MAX] = {
//    S0,  S1,  S2,  S3,  S4,  S5,  S6,  S7,
//    S8,  S9, S10, S11, S12, S13, S14, S15,
//   S16, S17, S18, S19, S20, S21, S22, S23,
//   S24, S25, S26, S27, S28, S29, S30, S31,
// };
// const char *kFReg64s[PHYSICAL_FREG_MAX] = {
//    D0,  D1,  D2,  D3,  D4,  D5,  D6,  D7,
//    D8,  D9, D10, D11, D12, D13, D14, D15,
//   D16, D17, D18, D19, D20, D21, D22, D23,
//   D24, D25, D26, D27, D28, D29, D30, D31,
// };

// #define GET_D0_INDEX()   0

// #define CALLEE_SAVE_FREG_COUNT  ((int)(sizeof(kCalleeSaveFRegs) / sizeof(*kCalleeSaveFRegs)))
// static const int kCalleeSaveFRegs[] = {8, 9, 10, 11, 12, 13, 14, 15};

// #define CALLER_SAVE_FREG_COUNT  ((int)(sizeof(kCallerSaveFRegs) / sizeof(*kCallerSaveFRegs)))
// static const int kCallerSaveFRegs[] = {16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31};

static unsigned long detect_extra_occupied(RegAlloc *ra, IR *ir) {
  UNUSED(ir);
  unsigned long ioccupy = 0;
  // switch (ir->kind) {
  // case IR_JMP: case IR_TJMP: case IR_CALL:
  //   ioccupy = 1UL << GET_X16_INDEX();
  //   break;
  // default: break;
  // }
  if (ra->flag & RAF_STACK_FRAME)
    ioccupy |= 1UL << GET_FPREG_INDEX();
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

bool is_im12(intptr_t x) {
  return x <= ((1L << 11) - 1) && x >= -(1L << 11);
}

void mov_immediate(const char *dst, int64_t value, bool is_unsigned) {
  UNUSED(is_unsigned);
  LI(dst, IM(value));
}

static void ei_bofs(IR *ir) {
  const char *dst = kReg64s[ir->dst->phys];
  int ofs = ir->bofs.frameinfo->offset;
  // if (ofs < 4096 && ofs > -4096) {
    ADDI(dst, FP, IM(ofs));
  // } else {
  //   mov_immediate(dst, ofs, true, false);
  //   ADD(dst, dst, FP);
  // }
}

static void ei_iofs(IR *ir) {
  char *label = fmt_name(ir->iofs.label);
  if (ir->iofs.global)
    label = MANGLE(label);
  label = quote_label(label);
  const char *dst = kReg64s[ir->dst->phys];
  // if (!is_got(ir->iofs.label)) {
    LUI(dst, LABEL_OFFSET_HI(label));
    ADDI(dst, dst, LABEL_OFFSET_LO(label));
  // } else {
  //   ADRP(dst, LABEL_AT_GOTPAGE(label));
  //   LDR(dst, fmt("[%s,#%s]", dst, LABEL_AT_GOTPAGEOFF(label)));
  // }
}

static void ei_sofs(IR *ir) {
  assert(ir->opr1->flag & VRF_CONST);
  const char *dst = kReg64s[ir->dst->phys];
  // int ofs = ir->opr1->frame.offset;
  // if (ofs < 4096 && ofs > -4096) {
    ADDI(dst, SP, IM(ir->opr1->fixnum));
  // } else {
  //   mov_immediate(dst, ofs, true, false);
  //   ADD(dst, dst, SP);
  // }
}

#define ei_load_s  ei_load
static void ei_load(IR *ir) {
  assert(!(ir->opr1->flag & VRF_CONST));
  const char *src;
  if (ir->kind == IR_LOAD) {
    assert(!(ir->opr1->flag & VRF_SPILLED));
    src = IMMEDIATE_OFFSET0(kReg64s[ir->opr1->phys]);
  } else {
    assert(ir->opr1->flag & VRF_SPILLED);
    if (ir->opr1->frame.offset >= -4096 && ir->opr1->frame.offset <= 4096) {
      src = IMMEDIATE_OFFSET(ir->opr1->frame.offset, FP);
    } else {
      mov_immediate(kTmpReg, ir->opr1->frame.offset, false);
      ADD(kTmpReg, kTmpReg, FP);
      src = IMMEDIATE_OFFSET0(kTmpReg);
    }
  }

  const char *dst;
  if (ir->dst->flag & VRF_FLONUM) {
    assert(false);
  } else {
    int pow = ir->dst->vsize;
    assert(0 <= pow && pow < 4);
    dst = kReg64s[ir->dst->phys];
    switch (pow) {
    case 0:
      if (ir->flag & IRF_UNSIGNED) LBU(dst, src);
      else                         LB(dst, src);
      break;
    case 1:
      if (ir->flag & IRF_UNSIGNED) LHU(dst, src);
      else                         LH(dst, src);
      break;
    case 2:
      if (ir->flag & IRF_UNSIGNED) LWU(dst, src);
      else                         LW(dst, src);
      break;
    case 3:
      LD(dst, src);
      break;
    default: assert(false); break;
    }
  }
}

#define ei_store_s  ei_store
static void ei_store(IR *ir) {
  assert(!(ir->opr2->flag & VRF_CONST));
  const char *target;
  if (ir->kind == IR_STORE) {
    assert(!(ir->opr2->flag & VRF_SPILLED));
    target = IMMEDIATE_OFFSET0(kReg64s[ir->opr2->phys]);
  } else {
    assert(ir->opr2->flag & VRF_SPILLED);
    if (ir->opr2->frame.offset >= -4096 && ir->opr2->frame.offset <= 4096) {
      target = IMMEDIATE_OFFSET(ir->opr2->frame.offset, FP);
    } else {
      mov_immediate(kTmpReg, ir->opr2->frame.offset, false);
      ADD(kTmpReg, kTmpReg, FP);
      target = IMMEDIATE_OFFSET0(kTmpReg);
    }
  }
  const char *src;
  if (ir->opr1->flag & VRF_FLONUM) {
    assert(false);
  } else if (ir->opr1->flag & VRF_CONST) {
    if (ir->opr1->fixnum == 0)
      src = ZERO;
    else
      mov_immediate(src = kTmpReg, ir->opr1->fixnum, ir->flag & IRF_UNSIGNED);
  } else {
    src = kReg64s[ir->opr1->phys];
  }
  switch (ir->opr1->vsize) {
  case 0:  SB(src, target); break;
  case 1:  SH(src, target); break;
  case 2:  SW(src, target); break;
  case 3:  SD(src, target); break;
  default: assert(false); break;
  }
}

static void ei_add(IR *ir) {
  if (ir->dst->flag & VRF_FLONUM) {
    assert(false);
  } else {
    assert(!(ir->opr1->flag & VRF_CONST));
    const char *dst = kReg64s[ir->dst->phys];
    if (ir->dst->vsize <= 2 && !(ir->flag & IRF_UNSIGNED)) {
      if (ir->opr2->flag & VRF_CONST) {
        ADDIW(dst, kReg64s[ir->opr1->phys], IM(ir->opr2->fixnum));
      } else {
        ADDW(dst, kReg64s[ir->opr1->phys], kReg64s[ir->opr2->phys]);
      }
    } else {
      if (ir->opr2->flag & VRF_CONST) {
        ADDI(dst, kReg64s[ir->opr1->phys], IM(ir->opr2->fixnum));
      } else {
        ADD(dst, kReg64s[ir->opr1->phys], kReg64s[ir->opr2->phys]);
      }
    }
  }
}

static void ei_sub(IR *ir) {
  if (ir->dst->flag & VRF_FLONUM) {
    assert(false);
  } else {
    assert(!(ir->opr1->flag & VRF_CONST));
    const char *dst = kReg64s[ir->dst->phys];
    if (ir->dst->vsize <= 2 && !(ir->flag & IRF_UNSIGNED)) {
      if (ir->opr2->flag & VRF_CONST) {
        ADDIW(dst, kReg64s[ir->opr1->phys], IM(-ir->opr2->fixnum));
      } else {
        SUBW(dst, kReg64s[ir->opr1->phys], kReg64s[ir->opr2->phys]);
      }
    } else {
      if (ir->opr2->flag & VRF_CONST) {
        ADDI(dst, kReg64s[ir->opr1->phys], IM(-ir->opr2->fixnum));
      } else {
        SUB(dst, kReg64s[ir->opr1->phys], kReg64s[ir->opr2->phys]);
      }
    }
  }
}

static void ei_mul(IR *ir) {
  if (ir->dst->flag & VRF_FLONUM) {
    assert(false);
  } else {
    assert(!(ir->opr1->flag & VRF_CONST) && !(ir->opr2->flag & VRF_CONST));
    if (ir->dst->vsize <= 2 && !(ir->flag & IRF_UNSIGNED)) {
      MULW(kReg64s[ir->dst->phys], kReg64s[ir->opr1->phys], kReg64s[ir->opr2->phys]);
    } else {
      MUL(kReg64s[ir->dst->phys], kReg64s[ir->opr1->phys], kReg64s[ir->opr2->phys]);
    }
  }
}

static void ei_div(IR *ir) {
  if (ir->dst->flag & VRF_FLONUM) {
    assert(false);
  } else {
    assert(!(ir->opr1->flag & VRF_CONST) && !(ir->opr2->flag & VRF_CONST));
    if (ir->dst->vsize <= 2) {
      if (!(ir->flag & IRF_UNSIGNED))
        DIVW(kReg64s[ir->dst->phys], kReg64s[ir->opr1->phys], kReg64s[ir->opr2->phys]);
      else
        DIVUW(kReg64s[ir->dst->phys], kReg64s[ir->opr1->phys], kReg64s[ir->opr2->phys]);
    } else {
      if (!(ir->flag & IRF_UNSIGNED))
        DIV(kReg64s[ir->dst->phys], kReg64s[ir->opr1->phys], kReg64s[ir->opr2->phys]);
      else
        DIVU(kReg64s[ir->dst->phys], kReg64s[ir->opr1->phys], kReg64s[ir->opr2->phys]);
    }
  }
}

static void ei_mod(IR *ir) {
  assert(!(ir->dst->flag & VRF_FLONUM));
  assert(!(ir->opr1->flag & VRF_CONST) && !(ir->opr2->flag & VRF_CONST));
  if (ir->dst->vsize <= 2) {
    if (!(ir->flag & IRF_UNSIGNED))
      REMW(kReg64s[ir->dst->phys], kReg64s[ir->opr1->phys], kReg64s[ir->opr2->phys]);
    else
      REMUW(kReg64s[ir->dst->phys], kReg64s[ir->opr1->phys], kReg64s[ir->opr2->phys]);
  } else {
    if (!(ir->flag & IRF_UNSIGNED))
      REM(kReg64s[ir->dst->phys], kReg64s[ir->opr1->phys], kReg64s[ir->opr2->phys]);
    else
      REMU(kReg64s[ir->dst->phys], kReg64s[ir->opr1->phys], kReg64s[ir->opr2->phys]);
  }
}

static void ei_bitand(IR *ir) {
  assert(!(ir->opr1->flag & VRF_CONST));
  if (ir->opr2->flag & VRF_CONST)
    ANDI(kReg64s[ir->dst->phys], kReg64s[ir->opr1->phys], IM(ir->opr2->fixnum));
  else
    AND(kReg64s[ir->dst->phys], kReg64s[ir->opr1->phys], kReg64s[ir->opr2->phys]);
}

static void ei_bitor(IR *ir) {
  assert(!(ir->opr1->flag & VRF_CONST));
  if (ir->opr2->flag & VRF_CONST)
    ORI(kReg64s[ir->dst->phys], kReg64s[ir->opr1->phys], IM(ir->opr2->fixnum));
  else
    OR(kReg64s[ir->dst->phys], kReg64s[ir->opr1->phys], kReg64s[ir->opr2->phys]);
}

static void ei_bitxor(IR *ir) {
  assert(!(ir->opr1->flag & VRF_CONST));
  if (ir->opr2->flag & VRF_CONST)
    XORI(kReg64s[ir->dst->phys], kReg64s[ir->opr1->phys], IM(ir->opr2->fixnum));
  else
    XOR(kReg64s[ir->dst->phys], kReg64s[ir->opr1->phys], kReg64s[ir->opr2->phys]);
}

static void ei_lshift(IR *ir) {
  assert(!(ir->opr1->flag & VRF_CONST));
  if (ir->opr2->flag & VRF_CONST)
    SLLI(kReg64s[ir->dst->phys], kReg64s[ir->opr1->phys], IM(ir->opr2->fixnum));
  else
    SLL(kReg64s[ir->dst->phys], kReg64s[ir->opr1->phys], kReg64s[ir->opr2->phys]);
}

static void ei_rshift(IR *ir) {
  assert(!(ir->opr1->flag & VRF_CONST));
  const char *dst = kReg64s[ir->dst->phys];
  const char *opr1 = kReg64s[ir->opr1->phys];
  if (ir->opr2->flag & VRF_CONST) {
    const char *opr2 = IM(ir->opr2->fixnum);
    if (ir->flag & IRF_UNSIGNED) SRLI(dst, opr1, opr2);
    else                         SRAI(dst, opr1, opr2);
  } else {
    const char *opr2 = kReg64s[ir->opr2->phys];
    if (ir->flag & IRF_UNSIGNED) SRL(dst, opr1, opr2);
    else                         SRA(dst, opr1, opr2);
  }
}

static void ei_result(IR *ir) {
  if (ir->opr1->flag & VRF_FLONUM) {
    assert(false);
  } else {
    int dstphys = ir->dst != NULL ? ir->dst->phys : GET_A0_INDEX();
    const char *dst = kReg64s[dstphys];
    if (ir->opr1->flag & VRF_CONST) {
      mov_immediate(dst, ir->opr1->fixnum, ir->flag & IRF_UNSIGNED);
    } else if (ir->opr1->phys != dstphys) {  // Source is not return register.
      MV(dst, kReg64s[ir->opr1->phys]);
    }
  }
}

static void ei_subsp(IR *ir) {
  if (ir->opr1->flag & VRF_CONST) {
    // assert(ir->opr1->fixnum % 16 == 0);
    if (ir->opr1->fixnum > 0)
      ADDI(SP, SP, IM(-ir->opr1->fixnum));
    else if (ir->opr1->fixnum < 0)
      ADDI(SP, SP, IM(-ir->opr1->fixnum));
  } else {
    SUB(SP, SP, kReg64s[ir->opr1->phys]);
  }
  if (ir->dst != NULL)
    MV(kReg64s[ir->dst->phys], SP);
}

static void ei_mov(IR *ir) {
  if (ir->dst->flag & VRF_FLONUM) {
    assert(false);
  } else {
    assert(!(ir->dst->flag & VRF_CONST));
    const char *dst = kReg64s[ir->dst->phys];
    if (ir->opr1->flag & VRF_CONST) {
      mov_immediate(dst, ir->opr1->fixnum, ir->flag & IRF_UNSIGNED);
    } else {
      if (ir->opr1->phys != ir->dst->phys) {
        MV(dst, kReg64s[ir->opr1->phys]);
      }
    }
  }
}

static void ei_neg(IR *ir) {
  assert(!(ir->opr1->flag & VRF_CONST));
  NEG(kReg64s[ir->dst->phys], kReg64s[ir->opr1->phys]);
}

static void ei_bitnot(IR *ir) {
  assert(!(ir->opr1->flag & VRF_CONST));
  NOT(kReg64s[ir->dst->phys], kReg64s[ir->opr1->phys]);
}

static void ei_cond(IR *ir) {
  assert(ir->opr1 != NULL);
  assert(ir->opr2 != NULL);
  const char *dst = kReg64s[ir->dst->phys];
  assert(!(ir->opr1->flag & VRF_CONST));
  const char *opr1 = kReg64s[ir->opr1->phys];

  int cond = ir->cond.kind & (COND_MASK | COND_UNSIGNED);
  switch (cond) {
  case COND_EQ: case COND_EQ | COND_UNSIGNED:
  case COND_NE: case COND_NE | COND_UNSIGNED:
    assert((ir->opr2->flag & VRF_CONST) && ir->opr2->fixnum == 0);
    if ((cond & COND_MASK) == COND_EQ)
      SEQZ(dst, opr1);
    else
      SNEZ(dst, opr1);
    break;

  case COND_LT: case COND_LT | COND_UNSIGNED:
  case COND_GT: case COND_GT | COND_UNSIGNED:
    {
      VReg *opr1 = ir->opr1, *opr2 = ir->opr2;
      if ((cond & COND_MASK) == COND_GT) {
        opr1 = ir->opr2;
        opr2 = ir->opr1;
      }
      assert(!(opr1->flag & VRF_CONST));
      const char *o1 = kReg64s[opr1->phys];
      if (!(cond & COND_UNSIGNED)) {
        if (opr2->flag & VRF_CONST)
          SLTI(dst, o1, IM(opr2->fixnum));
        else
          SLT(dst, o1, kReg64s[opr2->phys]);
      } else {
        if (opr2->flag & VRF_CONST)
          SLTIU(dst, o1, IM(opr2->fixnum));
        else
          SLTU(dst, o1, kReg64s[opr2->phys]);
      }
    }
    break;
  case COND_LE: case COND_LE | COND_UNSIGNED:
  case COND_GE: case COND_GE | COND_UNSIGNED:
    {
      VReg *opr1 = ir->opr1, *opr2 = ir->opr2;
      if ((cond & COND_MASK) == COND_GE) {
        opr1 = ir->opr2;
        opr2 = ir->opr1;
      }
      assert(!(opr2->flag & VRF_CONST));
      // lhs <= rhs <=> !(rhs < lhs) <=> 1 - (rhs < lhs)
      const char *o2 = kReg64s[opr2->phys];
      if (!(cond & COND_UNSIGNED)) {
        if (opr1->flag & VRF_CONST)
          SLTI(dst, o2, IM(opr1->fixnum));
        else
          SLT(dst, o2, kReg64s[opr1->phys]);
      } else {
        if (opr1->flag & VRF_CONST)
          SLTIU(dst, o2, IM(opr1->fixnum));
        else
          SLTU(dst, o2, kReg64s[opr1->phys]);
      }
      NEG(dst, dst);
      ADDI(dst, dst, IM(1));
    }
    break;
  default: assert(false); break;
  }
}

static void ei_jmp(IR *ir) {
  const char *label = fmt_name(ir->jmp.bb->label);
  switch (ir->jmp.cond & (COND_MASK | COND_UNSIGNED)) {
  case COND_ANY: J(label); return;
  case COND_NONE: return;
  default: break;
  }

  assert(!(ir->opr1->flag & VRF_CONST));
  assert(!(ir->opr2->flag & VRF_CONST) || ir->opr2->fixnum == 0);

  const char *opr1 = kReg64s[ir->opr1->phys];
  const char *opr2 = !(ir->opr2->flag & VRF_CONST) ? kReg64s[ir->opr2->phys] : ZERO;

  // On aarch64, flag for comparing flonum is signed.
  switch (ir->jmp.cond & (COND_MASK | COND_UNSIGNED)) {
  case COND_EQ | COND_UNSIGNED:  // Fallthrough
  case COND_EQ:  Bcc(CEQ, opr1, opr2, label); break;

  case COND_NE | COND_UNSIGNED:  // Fallthrough
  case COND_NE:  Bcc(CNE, opr1, opr2, label); break;

  case COND_LT:  Bcc(CLT, opr1, opr2, label); break;
  case COND_GT:  Bcc(CLT, opr2, opr1, label); break;
  case COND_LE:  Bcc(CGE, opr2, opr1, label); break;
  case COND_GE:  Bcc(CGE, opr1, opr2, label); break;

  case COND_LT | COND_UNSIGNED:  Bcc(CLTU, opr1, opr2, label); break;
  case COND_GT | COND_UNSIGNED:  Bcc(CLTU, opr2, opr1, label); break;
  case COND_LE | COND_UNSIGNED:  Bcc(CGEU, opr2, opr1, label); break;
  case COND_GE | COND_UNSIGNED:  Bcc(CGEU, opr1, opr2, label); break;
  default: assert(false); break;
  }
}

static void ei_tjmp(IR *ir) {
  const char *dst = kTmpReg;
  const Name *table_label = alloc_label();
  char *label = fmt_name(table_label);
  LUI(dst, LABEL_OFFSET_HI(label));
  ADDI(dst, dst, LABEL_OFFSET_LO(label));
  // dst = label + (opr1 << 3)
  assert(!(ir->opr1->flag & VRF_CONST));
  const char *opr1 = kReg64s[ir->opr1->phys];
  SLLI(opr1, opr1, IM(3));
  ADD(dst, dst, opr1);
  LD(dst, IMMEDIATE_OFFSET0(dst));
  JR(dst);

  _RODATA();
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

  int align_stack = (16 - (ir->precall.stack_args_size)) & 15;
  ir->precall.stack_aligned = align_stack;

  if (align_stack > 0) {
    SUB(SP, SP, IM(align_stack));
  }
}

static void ei_pusharg(IR *ir) {
  assert(!(ir->opr1->flag & VRF_CONST));
  if (ir->opr1->flag & VRF_FLONUM) {
    assert(false);
  } else {
    // Assume parameter registers are arranged from index 0.
    if (ir->pusharg.index != ir->opr1->phys)
      MV(kReg64s[ir->pusharg.index], kReg64s[ir->opr1->phys]);
  }
}

static void ei_call(IR *ir) {
  if (ir->call.label != NULL) {
    char *label = fmt_name(ir->call.label);
    if (ir->call.global)
      label = MANGLE(label);
    CALL(quote_label(label));
  } else {
    assert(!(ir->opr1->flag & VRF_CONST));
    JALR(kReg64s[ir->opr1->phys]);
  }

  IR *precall = ir->call.precall;
  int align_stack = precall->precall.stack_aligned + precall->precall.stack_args_size;
  if (align_stack != 0) {
    ADD(SP, SP, IM(align_stack));
  }

  // Resore caller save registers.
  pop_caller_save_regs(precall->precall.caller_saves);

  if (ir->dst != NULL) {
    if (ir->dst->flag & VRF_FLONUM) {
      assert(false);
    } else {
      if (ir->dst->phys != GET_A0_INDEX()) {
        MV(kReg64s[ir->dst->phys], kReg64s[GET_A0_INDEX()]);
      }
    }
  }
}

static void ei_cast(IR *ir) {
  assert((ir->opr1->flag & VRF_CONST) == 0);
  if (ir->dst->flag & VRF_FLONUM) {
    assert(false);
  } else if (ir->opr1->flag & VRF_FLONUM) {
    assert(false);
  } else {
    // fix->fix
    assert(ir->dst->vsize != ir->opr1->vsize);
    int pows = ir->opr1->vsize;
    int powd = ir->dst->vsize;
    assert(0 <= pows && pows < 4);
    assert(0 <= powd && powd < 4);
    int pow = MIN(powd, pows);
    const char *dst = kReg64s[ir->dst->phys], *src = kReg64s[ir->opr1->phys];

    if (ir->flag & IRF_UNSIGNED) {
      const char *shift = IM((8 - (1 << pow)) * TARGET_CHAR_BIT);
      SLLI(dst, src, shift);
      SRLI(dst, dst, shift);
    } else {
      if (pow < 2) {
        const char *shift = IM((4 - (1 << pows)) * TARGET_CHAR_BIT);
        SLLIW(dst, src, shift);
        SRAI(dst, dst, shift);
      } else {
        SEXTW(dst, src);
      }
    }
  }
}

static void ei_asm(IR *ir) {
  EMIT_ASM(ir->asm_.str);
  // if (ir->dst != NULL) {
  //   assert(!(ir->dst->flag & VRF_CONST));
  //   int pow = ir->dst->vsize;
  //   assert(0 <= pow && pow < 4);
  //   const char **regs = kRegSizeTable[pow];
  //   MOV(regs[ir->dst->phys], regs[GET_X0_INDEX()]);
  // }
}

//

int push_callee_save_regs(unsigned long used, unsigned long fused) {
  UNUSED(used);
  UNUSED(fused);
  return 0;
}

void pop_callee_save_regs(unsigned long used, unsigned long fused) {
  UNUSED(used);
  UNUSED(fused);
}

int calculate_func_param_bottom(Function *func) {
  UNUSED(func);
  return (POINTER_SIZE * 2);  // Return address, saved base pointer.
}
#undef N

static Vector *push_caller_save_regs(unsigned long living) {
  UNUSED(living);
  Vector *saves = new_vector();
  return saves;
}

static void pop_caller_save_regs(Vector *saves) {
  UNUSED(saves);
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
    [IR_MOV] = ei_mov, [IR_ASM] = ei_asm,
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
      assert(ir->kind < (int)(sizeof(table) / sizeof(*table)));
      assert(table[ir->kind] != NULL);
      (*table[ir->kind])(ir);
    }
  }
}

//

static void swap_opr12(IR *ir) {
  VReg *tmp = ir->opr1;
  ir->opr1 = ir->opr2;
  ir->opr2 = tmp;
}

static void insert_const_mov(VReg **pvreg, RegAlloc *ra, Vector *irs, int i) {
  VReg *c = *pvreg;
  VReg *tmp = reg_alloc_spawn(ra, c->vsize, 0);
  IR *mov = new_ir_mov(tmp, c, ((IR*)irs->data[i])->flag);
  vec_insert(irs, i, mov);
  *pvreg = tmp;
}

#define insert_tmp_mov  insert_const_mov

void tweak_irs(FuncBackend *fnbe) {
  UNUSED(fnbe);

  BBContainer *bbcon = fnbe->bbcon;
  RegAlloc *ra = fnbe->ra;
  for (int i = 0; i < bbcon->bbs->len; ++i) {
    BB *bb = bbcon->bbs->data[i];
    Vector *irs = bb->irs;
    for (int j = 0; j < irs->len; ++j) {
      IR *ir = irs->data[j];
      switch (ir->kind) {
      case IR_LOAD:
        if (ir->opr1->flag & VRF_CONST) {
          insert_const_mov(&ir->opr1, ra, irs, j++);
        }
        break;
      case IR_STORE:
        if (ir->opr2->flag & VRF_CONST) {
          insert_const_mov(&ir->opr2, ra, irs, j++);
        }
        break;
      case IR_ADD:
        assert(!(ir->opr1->flag & VRF_CONST) || !(ir->opr2->flag & VRF_CONST));
        if (ir->opr1->flag & VRF_CONST)
          swap_opr12(ir);
        if (ir->opr2->flag & VRF_CONST) {
          // if (ir->opr2->fixnum < 0) {
          //   ir->kind = IR_SUB;
          //   VReg *old = ir->opr2;
          //   ir->opr2 = reg_alloc_spawn_const(ra, -old->fixnum, old->vsize);
          //   ir->opr2->flag = old->flag;
          // }
          if (ir->opr2->fixnum > 0x0fff || ir->opr2->fixnum < -0x0fff)
            insert_const_mov(&ir->opr2, ra, irs, j++);
        }
        break;
      case IR_SUB:
        assert(!(ir->opr1->flag & VRF_CONST) || !(ir->opr2->flag & VRF_CONST));
        if (ir->opr1->flag & VRF_CONST) {
          if (ir->opr1->fixnum == 0) {
            ir->kind = IR_NEG;
            ir->opr1 = ir->opr2;
            ir->opr2 = NULL;
            break;
          }
          insert_const_mov(&ir->opr1, ra, irs, j++);
        }
        if (ir->opr2->flag & VRF_CONST) {
          // if (ir->opr2->fixnum < 0) {
          //   ir->kind = IR_ADD;
          //   VReg *old = ir->opr2;
          //   ir->opr2 = reg_alloc_spawn_const(ra, -old->fixnum, old->vsize);
          //   ir->opr2->flag = old->flag;
          // }
          if (ir->opr2->fixnum > 0x0fff || ir->opr2->fixnum < -0x0fff)
            insert_const_mov(&ir->opr2, ra, irs, j++);
        }
        break;
      case IR_MUL:
      case IR_DIV:
      case IR_MOD:
        assert(!(ir->opr1->flag & VRF_CONST) || !(ir->opr2->flag & VRF_CONST));
        if (ir->opr1->flag & VRF_CONST)
          insert_const_mov(&ir->opr1, ra, irs, j++);
        if (ir->opr2->flag & VRF_CONST)
          insert_const_mov(&ir->opr2, ra, irs, j++);
        break;
      case IR_BITAND:
      case IR_BITOR:
      case IR_BITXOR:
        assert(!(ir->opr1->flag & VRF_CONST) || !(ir->opr2->flag & VRF_CONST));
        if (ir->opr1->flag & VRF_CONST)
          insert_const_mov(&ir->opr1, ra, irs, j++);
        if ((ir->opr2->flag & VRF_CONST) && !is_im12(ir->opr2->fixnum))
          insert_const_mov(&ir->opr2, ra, irs, j++);
        break;
      case IR_LSHIFT:
      case IR_RSHIFT:
        assert(!(ir->opr1->flag & VRF_CONST) || !(ir->opr2->flag & VRF_CONST));
        if (ir->opr1->flag & VRF_CONST)
          insert_const_mov(&ir->opr1, ra, irs, j++);
        break;
      case IR_COND:
        {
          assert(ir->opr1 != NULL);
          assert(ir->opr2 != NULL);
          int cond = ir->cond.kind & COND_MASK;
          switch (cond) {
          case COND_EQ: case COND_NE:
            assert(!(ir->opr1->flag & VRF_CONST));
            if (!(ir->opr2->flag & VRF_CONST) || ir->opr2->fixnum != 0) {
              IR *sub = new_ir_bop_raw(IR_SUB, ir->dst, ir->opr1, ir->opr2, ir->flag);
              vec_insert(irs, j++, sub);

              ir->opr1 = ir->dst;
              ir->opr2 = reg_alloc_spawn_const(ra, 0, ir->dst->vsize);
            }
            break;
          case COND_LE: case COND_GT:
            if (ir->opr2->flag & VRF_CONST)
              insert_const_mov(&ir->opr2, ra, irs, j++);
            break;
          case COND_LT: case COND_GE:
            if ((ir->opr2->flag & VRF_CONST) &&
                (ir->opr2->fixnum < -4096 || ir->opr2->fixnum > 4096))
              insert_const_mov(&ir->opr2, ra, irs, j++);
            break;
          default:
            break;
          }
        }
        break;
      case IR_JMP:
        if (ir->opr2 != NULL &&
            (ir->opr2->flag & VRF_CONST) &&
            ir->opr2->fixnum != 0)
          insert_const_mov(&ir->opr2, ra, irs, j++);
        break;
      case IR_TJMP:
        // Make sure opr1 can be broken.
        insert_tmp_mov(&ir->opr1, ra, irs, j++);
        break;
      case IR_PUSHARG:
        if (ir->opr1->flag & VRF_CONST)
          insert_const_mov(&ir->opr1, ra, irs, j++);
        break;

      default: break;
      }
    }
  }
}
