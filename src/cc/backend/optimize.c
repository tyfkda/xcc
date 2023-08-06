#include "../../config.h"
#include "optimize.h"

#include <assert.h>

#include "ir.h"
#include "table.h"
#include "util.h"

static enum ConditionKind invert_cond(enum ConditionKind cond) {
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
    for (int i = 0; i < bbs->len; ++i) {
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

void optimize(BBContainer *bbcon) {
  remove_unnecessary_bb(bbcon);
}
