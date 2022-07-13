#include "../config.h"
#include "ir.h"

#include <assert.h>
#include <stdlib.h>  // malloc

#include "aarch64.h"
#include "regalloc.h"
#include "table.h"
#include "util.h"

#define WORK_REG_NO  (PHYSICAL_REG_MAX)

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
  {W20, W21, W22, W23, W24, W25, W26},
  {W20, W21, W22, W23, W24, W25, W26},
  {W20, W21, W22, W23, W24, W25, W26},
  {X20, X21, X22, X23, X24, X25, X26},
};

#define kReg32s  (kRegSizeTable[2])
#define kReg64s  (kRegSizeTable[3])

const char *kRetRegTable[] = {W0, W0, W0, X0};

const char *kTmpRegTable[] = {W9, W9, W9, X9};
const char *kTmpRegTable2[] = {W10, W10, W10, X10};

static const int kPow2Table[] = {-1, 0, 1, -1, 2, -1, -1, -1, 3};
#define kPow2TableSize ((int)(sizeof(kPow2Table) / sizeof(*kPow2Table)))

//

static void mov_immediate(const char *dst, intptr_t value, bool b64) {
  if (is_im16(value)) {
    MOV(dst, IM(value));
  } else if (!b64 || is_im32(value)) {
    int32_t v = value;
    MOV(dst, IM(v & 0xffff));
    MOVK(dst, IM((v >> 16) & 0xffff), _LSL(16));
  } else {
    MOV(dst, IM(value & 0xffff));
    MOVK(dst, IM((value >> 16) & 0xffff), _LSL(16));
    MOVK(dst, IM((value >> 32) & 0xffff), _LSL(32));
    MOVK(dst, IM((value >> 48) & 0xffff), _LSL(48));
  }
}

static void ir_out(IR *ir) {
  switch (ir->kind) {
  case IR_BOFS:
    if (ir->opr1->flag & VRF_CONST)
      mov_immediate(kReg64s[ir->dst->phys], ir->opr1->fixnum, true);
    else
      ADD(kReg64s[ir->dst->phys], FP, IM(ir->opr1->offset));
    break;

  case IR_IOFS:
    {
      char *label = fmt_name(ir->iofs.label);
      if (ir->iofs.global)
        label = MANGLE(label);
      label = quote_label(label);
      const char *dst = kReg64s[ir->dst->phys];
      ADRP(dst, LABEL_AT_PAGE(label));
      ADD(dst, dst, LABEL_AT_PAGEOFF(label));
    }
    break;

  case IR_ADD:
    {
      assert(!(ir->opr1->flag & VRF_CONST) || !(ir->opr2->flag & VRF_CONST));
      assert(0 <= ir->size && ir->size < kPow2TableSize);
      int pow = kPow2Table[ir->size];
      assert(0 <= pow && pow < 4);
      const char **regs = kRegSizeTable[pow];
      if (ir->opr1->flag & VRF_CONST)
        ADD(regs[ir->dst->phys], regs[ir->opr2->phys], IM(ir->opr1->fixnum));
      else if (ir->opr2->flag & VRF_CONST)
        ADD(regs[ir->dst->phys], regs[ir->opr1->phys], IM(ir->opr2->fixnum));
      else
        ADD(regs[ir->dst->phys], regs[ir->opr1->phys], regs[ir->opr2->phys]);
    }
    break;

  case IR_SUB:
    {
      assert(!(ir->opr1->flag & VRF_CONST) || !(ir->opr2->flag & VRF_CONST));
      assert(0 <= ir->size && ir->size < kPow2TableSize);
      int pow = kPow2Table[ir->size];
      assert(0 <= pow && pow < 4);
      const char **regs = kRegSizeTable[pow];
      if (ir->opr1->flag & VRF_CONST) {
        const char *tmp = kTmpRegTable[pow];
        mov_immediate(tmp, ir->opr1->fixnum, pow >= 3);
        SUB(regs[ir->dst->phys], tmp, regs[ir->opr2->phys]);
      } else if (ir->opr2->flag & VRF_CONST)
        SUB(regs[ir->dst->phys], regs[ir->opr1->phys], IM(ir->opr2->fixnum));
      else
        SUB(regs[ir->dst->phys], regs[ir->opr1->phys], regs[ir->opr2->phys]);
    }
    break;

  case IR_MUL:
    {
      assert(!(ir->opr1->flag & VRF_CONST) || !(ir->opr2->flag & VRF_CONST));
      assert(0 <= ir->size && ir->size < kPow2TableSize);
      int pow = kPow2Table[ir->size];
      assert(0 <= pow && pow < 4);
      const char **regs = kRegSizeTable[pow];
      if (ir->opr1->flag & VRF_CONST) {
        const char *tmp = kTmpRegTable[pow];
        mov_immediate(tmp, ir->opr1->fixnum, pow >= 3);
        MUL(regs[ir->dst->phys], tmp, regs[ir->opr2->phys]);
      } else if (ir->opr2->flag & VRF_CONST) {
        const char *tmp = kTmpRegTable[pow];
        mov_immediate(tmp, ir->opr2->fixnum, pow >= 3);
        MUL(regs[ir->dst->phys], regs[ir->opr1->phys], tmp);
      } else {
        MUL(regs[ir->dst->phys], regs[ir->opr1->phys], regs[ir->opr2->phys]);
      }
    }
    break;

  case IR_DIV:
    {
      assert(!(ir->opr1->flag & VRF_CONST) || !(ir->opr2->flag & VRF_CONST));
      assert(0 <= ir->size && ir->size < kPow2TableSize);
      int pow = kPow2Table[ir->size];
      assert(0 <= pow && pow < 4);
      const char **regs = kRegSizeTable[pow];
      if (ir->opr1->flag & VRF_CONST) {
        const char *tmp = kTmpRegTable[pow];
        mov_immediate(tmp, ir->opr1->fixnum, pow >= 3);
        if (!(ir->dst->vtype->flag & VRTF_UNSIGNED))
          SDIV(regs[ir->dst->phys], tmp, regs[ir->opr2->phys]);
        else
          UDIV(regs[ir->dst->phys], tmp, regs[ir->opr2->phys]);
      } else if (ir->opr2->flag & VRF_CONST) {
        const char *tmp = kTmpRegTable[pow];
        mov_immediate(tmp, ir->opr2->fixnum, pow >= 3);
        if (!(ir->dst->vtype->flag & VRTF_UNSIGNED))
          SDIV(regs[ir->dst->phys], regs[ir->opr1->phys], tmp);
        else
          UDIV(regs[ir->dst->phys], regs[ir->opr1->phys], tmp);
      } else {
        if (!(ir->dst->vtype->flag & VRTF_UNSIGNED))
          SDIV(regs[ir->dst->phys], regs[ir->opr1->phys], regs[ir->opr2->phys]);
        else
          UDIV(regs[ir->dst->phys], regs[ir->opr1->phys], regs[ir->opr2->phys]);
      }
    }
    break;

  case IR_MOD:
    {
      assert(!(ir->opr1->flag & VRF_CONST) || !(ir->opr2->flag & VRF_CONST));
      assert(0 <= ir->size && ir->size < kPow2TableSize);
      int pow = kPow2Table[ir->size];
      assert(0 <= pow && pow < 4);
      const char **regs = kRegSizeTable[pow];
      const char *num, *div;
      if (ir->opr1->flag & VRF_CONST) {
        num = kTmpRegTable2[pow];
        mov_immediate(num, ir->opr1->fixnum, pow >= 3);
        div = regs[ir->opr2->phys];
      } else if (ir->opr2->flag & VRF_CONST) {
        div = kTmpRegTable2[pow];
        mov_immediate(div, ir->opr2->fixnum, pow >= 3);
        num = regs[ir->opr1->phys];
      } else {
        num = regs[ir->opr1->phys];
        div = regs[ir->opr2->phys];
      }
      const char *tmp = kTmpRegTable[pow];
      if (!(ir->dst->vtype->flag & VRTF_UNSIGNED))
        SDIV(tmp, num, div);
      else
        UDIV(tmp, num, div);
      const char *dst = regs[ir->dst->phys];
      MSUB(dst, tmp, div, num);
    }
    break;

  case IR_RESULT:
    {
      assert(0 <= ir->size && ir->size < kPow2TableSize);
      int pow = kPow2Table[ir->size];
      assert(0 <= pow && pow < 4);
      if (ir->opr1->flag & VRF_CONST) {
        mov_immediate(kRetRegTable[pow], ir->opr1->fixnum, pow >= 3);
      } else {
        MOV(kRetRegTable[pow], kRegSizeTable[pow][ir->opr1->phys]);
      }
    }
    break;

  case IR_MOV:
    {
      assert(0 <= ir->size && ir->size < kPow2TableSize);
      assert(!(ir->dst->flag & VRF_CONST));
      int pow = kPow2Table[ir->size];
      assert(0 <= pow && pow < 4);
      const char **regs = kRegSizeTable[pow];
      if (ir->opr1->flag & VRF_CONST) {
        mov_immediate(regs[ir->dst->phys], ir->opr1->fixnum, pow >= 3);
      } else {
        if (ir->opr1->phys != ir->dst->phys)
          MOV(regs[ir->dst->phys], regs[ir->opr1->phys]);
      }
    }
    break;

  case IR_CMP:
    {
      assert(!(ir->opr1->flag & VRF_CONST));
      assert(0 <= ir->size && ir->size < kPow2TableSize);
      int pow = kPow2Table[ir->size];
      assert(0 <= pow && pow < 4);
      const char **regs = kRegSizeTable[pow];
      if (ir->opr2->flag & VRF_CONST)
        CMP(regs[ir->opr1->phys], IM(ir->opr2->fixnum));
      else
        CMP(regs[ir->opr1->phys], regs[ir->opr2->phys]);
    }
    break;

  case IR_COND:
    {
      assert(!(ir->dst->flag & VRF_CONST));
      const char *dst = kReg32s[ir->dst->phys];  // Assume bool is 4 byte.
      switch (ir->cond.kind) {
      case COND_EQ:  CSET(dst, CEQ); break;
      case COND_NE:  CSET(dst, CNE); break;
      case COND_LT:  CSET(dst, CLT); break;
      case COND_GT:  CSET(dst, CGT); break;
      case COND_LE:  CSET(dst, CLE); break;
      case COND_GE:  CSET(dst, CGE); break;
      case COND_ULT: CSET(dst, CLO); break;
      case COND_UGT: CSET(dst, CHI); break;
      case COND_ULE: CSET(dst, CLS); break;
      case COND_UGE: CSET(dst, CHS); break;
      default: assert(false); break;
      }
    }
    break;

  case IR_JMP:
    switch (ir->jmp.cond) {
    case COND_ANY: BRANCH(fmt_name(ir->jmp.bb->label)); break;
    case COND_EQ:  Bcc(CEQ, fmt_name(ir->jmp.bb->label)); break;
    case COND_NE:  Bcc(CNE, fmt_name(ir->jmp.bb->label)); break;
    case COND_LT:  Bcc(CLT, fmt_name(ir->jmp.bb->label)); break;
    case COND_GT:  Bcc(CGT, fmt_name(ir->jmp.bb->label)); break;
    case COND_LE:  Bcc(CLE, fmt_name(ir->jmp.bb->label)); break;
    case COND_GE:  Bcc(CGE, fmt_name(ir->jmp.bb->label)); break;
    case COND_ULT: Bcc(CLO, fmt_name(ir->jmp.bb->label)); break;
    case COND_UGT: Bcc(CHI, fmt_name(ir->jmp.bb->label)); break;
    case COND_ULE: Bcc(CLS, fmt_name(ir->jmp.bb->label)); break;
    case COND_UGE: Bcc(CHS, fmt_name(ir->jmp.bb->label)); break;
    default: assert(false); break;
    }
    break;

  case IR_PRECALL:
    break;

  case IR_PUSHARG:
    {
      const char *src;
      if (ir->opr1->flag & VRF_CONST) {
        src = kTmpRegTable[3];
        mov_immediate(src, ir->opr1->fixnum, true);
      } else {
        src = kRegSizeTable[3][ir->opr1->phys];
      }
      STR(src, PRE_INDEX(SP, -16));
    }
    break;

  case IR_CALL:
    {
      static const char *kArgReg64s[] = {X0, X1, X2, X3, X4, X5, X6, X7};

      int ireg = 0;
      int total_arg_count = ir->call.total_arg_count;
      for (int i = 0; i < total_arg_count; ++i) {
        if (ir->call.arg_vtypes[i]->flag & VRTF_NON_REG)
          continue;
        if (ireg < MAX_REG_ARGS) {
          LDR(kArgReg64s[ireg++], POST_INDEX(SP, 16));
        }
      }

      if (ir->call.label != NULL) {
        char *label = fmt_name(ir->call.label);
        if (ir->call.global)
          label = MANGLE(label);
        BL(quote_label(label));
      } else {
        assert(!(ir->opr1->flag & VRF_CONST));
        BLR(kReg64s[ir->opr1->phys]);
      }

      assert(0 <= ir->size && ir->size < kPow2TableSize);
      if (ir->size > 0) {
        int pow = kPow2Table[ir->size];
        assert(0 <= pow && pow < 4);
        const char **regs = kRegSizeTable[pow];
        MOV(regs[ir->dst->phys], kRetRegTable[pow]);
      }
    }
    break;

  case IR_CAST:
    assert((ir->opr1->flag & VRF_CONST) == 0);
    if (ir->size <= ir->opr1->vtype->size) {
      if (ir->dst->phys != ir->opr1->phys) {
        assert(0 <= ir->size && ir->size < kPow2TableSize);
        int pow = kPow2Table[ir->size];
        assert(0 <= pow && pow < 3);
        const char **regs = kRegSizeTable[pow];
        MOV(regs[ir->dst->phys], regs[ir->opr1->phys]);
      }
    } else {
      assert(0 <= ir->opr1->vtype->size && ir->opr1->vtype->size < kPow2TableSize);
      int pows = kPow2Table[ir->opr1->vtype->size];
      assert(0 <= ir->size && ir->size < kPow2TableSize);
      int powd = kPow2Table[ir->size];
      assert(0 <= pows && pows < 4);
      assert(0 <= powd && powd < 4);
      if (ir->opr1->vtype->flag & VRTF_UNSIGNED) {
        switch (pows) {
        case 0:  UXTB(kRegSizeTable[powd][ir->dst->phys], kRegSizeTable[pows][ir->opr1->phys]); break;
        case 1:  UXTH(kRegSizeTable[powd][ir->dst->phys], kRegSizeTable[pows][ir->opr1->phys]); break;
        case 2:  UXTW(kRegSizeTable[powd][ir->dst->phys], kRegSizeTable[pows][ir->opr1->phys]); break;
        default: assert(false); break;
        }
      } else {
        switch (pows) {
        case 0:  SXTB(kRegSizeTable[powd][ir->dst->phys], kRegSizeTable[pows][ir->opr1->phys]); break;
        case 1:  SXTH(kRegSizeTable[powd][ir->dst->phys], kRegSizeTable[pows][ir->opr1->phys]); break;
        case 2:  SXTW(kRegSizeTable[powd][ir->dst->phys], kRegSizeTable[pows][ir->opr1->phys]); break;
        default: assert(false); break;
        }
      }
    }
    break;

  case IR_LOAD_SPILLED:
    {
      assert(0 <= ir->size && ir->size < kPow2TableSize);
      int pow = kPow2Table[ir->size];
      assert(0 <= pow && pow < 4);
      const char **regs = kRegSizeTable[pow];
      LDR(regs[ir->dst->phys], IMMEDIATE_OFFSET(FP, ir->opr1->offset));
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
  // int count = 0;
  // for (int i = 0; i < CALLEE_SAVE_REG_COUNT; ++i) {
  //   int ireg = kCalleeSaveRegs[i];
  //   if (used & (1 << ireg)) {
  //     PUSH(kReg64s[ireg]);
  //     PUSH_STACK_POS();
  //     ++count;
  //   }
  // }
  // return count;
  UNUSED(used);
  return 0;
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

void convert_3to2(BBContainer *bbcon) {
  UNUSED(bbcon);
}
