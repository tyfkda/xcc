#include "../../config.h"
#include "optimize.h"

#include <assert.h>
#include <stdlib.h>  // free

#include "ir.h"
#include "regalloc.h"
#include "ssa.h"
#include "table.h"
#include "util.h"

inline enum ConditionKind invert_cond(enum ConditionKind cond) {
  int c = cond & COND_MASK;
  assert(COND_EQ <= c && c <= COND_GT);
  int ic = c <= COND_NE ? (COND_NE + COND_EQ) - c
                        : (assert((COND_LT & 3) == 0), c ^ 2);  // COND_LT + ((c - COND_LT) ^ 2)
  return ic | (cond & ~COND_MASK);
}

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

static void remove_unnecessary_bb(BBContainer *bbcon) {
  Vector *bbs = bbcon->bbs;
  Table keeptbl;
  for (;;) {
    table_init(&keeptbl);
    assert(bbs->len > 0);

    for (int i = 0; i < bbs->len; ++i) {
      BB *bb = bbs->data[i];
      bool remove = false;
      IR *ir_jmp = is_last_jmp(bb);
      if (bb->irs->len == 0 && bb->next != NULL) {  // Empty BB.
        replace_jmp_destination(bbcon, bb, bb->next);
        remove = true;
      } else if (bb->irs->len == 1 && ir_jmp != NULL && ir_jmp->jmp.cond == COND_ANY &&
                 !equal_name(bb->label, ir_jmp->jmp.bb->label)) {  // jmp only.
        replace_jmp_destination(bbcon, bb, ir_jmp->jmp.bb);
        if (i > 0) {
          BB *pbb = bbs->data[i - 1];
          IR *ir0 = is_last_jmp(pbb);
          if (ir0 != NULL && ir0->jmp.cond != COND_ANY &&  // Fallthrough pass exists.
              ir0->jmp.bb == bb->next &&                   // Skip jmp: Fix bb connection.
              !(ir0->jmp.cond & COND_FLONUM)) {
            // Invert prev jmp condition and change jmp destination.
            ir0->jmp.cond = invert_cond(ir0->jmp.cond);
            ir0->jmp.bb = ir_jmp->jmp.bb;
            remove = true;
          }
        }
      }

      if (ir_jmp != NULL)
        table_put(&keeptbl, ir_jmp->jmp.bb->label, bb);
      if ((ir_jmp == NULL || ir_jmp->jmp.cond != COND_ANY) && bb->next != NULL)
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
    for (int i = 1; i < bbs->len; ++i) {
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

//

static void remove_unused_vregs(RegAlloc *ra, BBContainer *bbcon) {
  int vreg_count = ra->vregs->len;
  unsigned char *vreg_read = malloc_or_die(vreg_count);
  for (;;) {
    for (int i = 0; i < vreg_count; ++i) {
      VReg *vreg = ra->vregs->data[i];
      // Must keep function parameter and `&` taken one.
      vreg_read[i] = vreg != NULL && (vreg->flag & (VRF_PARAM | VRF_REF)) != 0;
    }

    // Check VReg usage.
    for (int i = 0; i < bbcon->bbs->len; ++i) {
      BB *bb = bbcon->bbs->data[i];
      for (int j = 0; j < bb->irs->len; ++j) {
        IR *ir = bb->irs->data[j];
        VReg *operands[] = {ir->opr1, ir->opr2};
        int n = 2;
        if (ir->kind == IR_PHI)
          n += ir->phi.vregs->len;
        for (int k = 0; k < n; ++k) {
          VReg *vreg;
          if (k < 2) {
            vreg = operands[k];
            if (vreg == NULL || (vreg->flag & VRF_CONST))
              continue;
          } else {
            vreg = ir->phi.vregs->data[k - 2];
          }
          vreg_read[vreg->virt] = true;
        }
      }
    }

    // Remove instruction if the destination is unread.
    for (int i = 0; i < bbcon->bbs->len; ++i) {
      BB *bb = bbcon->bbs->data[i];
      for (int j = 0; j < bb->irs->len; ++j) {
        IR *ir = bb->irs->data[j];
        if (ir->dst == NULL || vreg_read[ir->dst->virt])
          continue;
        if (ir->kind == IR_CALL) {
          // Function must be CALLed even if the result is unused.
          ir->dst = NULL;
        } else {
          vec_remove_at(bb->irs, j);
          --j;
        }
      }
    }

    // Mark unused VRegs.
    bool again = false;
    for (int i = 0; i < vreg_count; ++i) {
      VReg *vreg;
      if (!vreg_read[i] && (vreg = ra->vregs->data[i]) != NULL) {
        ra->vregs->data[i] = NULL;
        vreg->flag |= VRF_UNUSED;
        again = true;
      }
    }
    if (!again)
      break;
  }

  free(vreg_read);
}

//

static void peephole(RegAlloc *ra, BB *bb) {
  for (int i = 0; i < bb->irs->len; ++i) {
    IR *ir = bb->irs->data[i];
    switch (ir->kind) {
    case IR_BOFS:
    case IR_IOFS:
      if (i < bb->irs->len - 1) {
        IR *next = bb->irs->data[i + 1];
        if ((next->kind == IR_ADD || next->kind == IR_SUB) &&
            next->opr1 == ir->dst && next->opr2->flag & VRF_CONST) {
          // Overwrite next IR. Current IR should be eliminated because of dst is unused.
          VReg *dst = next->dst;
          int64_t offset = next->opr2->fixnum;
          if (next->kind == IR_SUB)
            offset = -offset;
          *next = *ir;
          next->dst = dst;
          if (ir->kind == IR_BOFS)
            next->bofs.offset += offset;
          else
            next->iofs.offset += offset;
        }
      }
      break;
    case IR_ADD:
      if (ir->opr2->flag & VRF_CONST && i < bb->irs->len - 1) {
        IR *next = bb->irs->data[i + 1];
        if ((next->kind == IR_ADD || next->kind == IR_SUB) &&
            next->opr1 == ir->dst && next->opr2->flag & VRF_CONST) {
          // Overwrite next IR. Current IR should be eliminated because of dst is unused.
          VReg *dst = next->dst;
          VReg *opr2 = ir->opr2;
          int64_t value = next->opr2->fixnum;
          if (next->kind == IR_SUB)
            value = -value;
          value += opr2->fixnum;
          *next = *ir;
          next->dst = dst;
          next->opr2 = reg_alloc_spawn_const(ra, value, opr2->vsize);
        }
      }
      break;
    default:
      break;
    }
  }
}

void optimize(RegAlloc *ra, BBContainer *bbcon) {
#if defined(__GNUC__)
// Suppress unused warnings.
UNUSED(remove_unused_vregs);
UNUSED(remove_unnecessary_bb);
#endif

  // Peephole
  for (int i = 0; i < bbcon->bbs->len; ++i) {
    BB *bb = bbcon->bbs->data[i];
    peephole(ra, bb);
  }

  make_ssa(ra, bbcon);
  remove_unused_vregs(ra, bbcon);
  resolve_phis(bbcon);
  remove_unnecessary_bb(bbcon);
  detect_from_bbs(bbcon);
}
