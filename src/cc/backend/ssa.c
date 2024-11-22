#include "../../config.h"
#include "ssa.h"

#include <assert.h>
#include <stdlib.h>
#include <string.h>

#include "ir.h"
#include "regalloc.h"
#include "util.h"

#include "table.h"

#define ORIG_VIRT(vreg)  ((vreg)->orig_virt >= 0 ? (vreg)->orig_virt : (vreg)->virt)

inline void assign_new_vregs(RegAlloc *ra, Vector **vreg_table, BB *bb, VReg **vregs) {
  for (int iir = 0; iir < bb->irs->len; ++iir) {
    IR *ir = bb->irs->data[iir];
    if (ir->opr1 != NULL && !(ir->opr1->flag & (VRF_CONST | VRF_REF))) {
      ir->opr1 = vregs[ORIG_VIRT(ir->opr1)];
    }
    if (ir->opr2 != NULL && !(ir->opr2->flag & (VRF_CONST | VRF_REF))) {
      ir->opr2 = vregs[ORIG_VIRT(ir->opr2)];
    }
    if (ir->dst != NULL && !(ir->dst->flag & (VRF_CONST | VRF_REF))) {
      int virt = ORIG_VIRT(ir->dst);
      Vector *vt = vreg_table[virt];
      VReg *dst = ra->vregs->data[virt];
      if (vt->len > 0)
        ir->dst = dst = reg_alloc_with_version(ra, dst, vt->len);
      vec_push(vt, dst);
      vregs[virt] = dst;
    }
  }
}

inline void push_nexts(BB *bb, Vector *unchecked) {
  Vector *irs = bb->irs;
  if (irs->len > 0) {
    IR *ir = irs->data[irs->len - 1];  // JMP must be the last IR.
    switch (ir->kind) {
    case IR_JMP:
      {
        BB *dst = ir->jmp.bb;
        vec_push(unchecked, dst);
        if (ir->jmp.cond == COND_ANY)
          return;  // Next BB is not reachable.
      }
      break;
    case IR_TJMP:
      for (size_t j = 0; j < ir->tjmp.len; ++j) {
        BB *nbb = ir->tjmp.bbs[j];
        vec_push(unchecked, nbb);
      }
      return;
    default: break;
    }
  }
  BB *next = bb->next;
  if (next != NULL)
    vec_push(unchecked, next);
}

static void replace_vreg_set(Vector *v, VReg **vregs) {
  for (int i = 0; i < v->len; ++i) {
    VReg *vreg = v->data[i];
    if (vreg->flag & VRF_REF)
      continue;
    int virt = ORIG_VIRT(vreg);
    v->data[i] = vregs[virt];
  }
}

static Vector **ssa_transform(RegAlloc *ra, BBContainer *bbcon) {
  int vreg_count = ra->vregs->len;
  Vector **vreg_table = malloc_or_die(sizeof(*vreg_table) * vreg_count);
  for (int i = 0; i < vreg_count; ++i) {
    VReg *vreg = ra->vregs->data[i];
    Vector *vt = new_vector();
    if (vreg->flag & (VRF_PARAM | VRF_REF))
      vec_push(vt, vreg);
    vreg_table[i] = vt;
  }

  // Traverse BBs.
  int count = bbcon->len;
  assert(count > 0);

  Table checked;
  table_init(&checked);
  Vector unchecked;
  vec_init(&unchecked);
  BB *bb0 = bbcon->data[0];
  vec_push(&unchecked, bb0);

  VReg **vregs = malloc_or_die(sizeof(*vregs) * vreg_count);

  do {
    BB *bb = vec_pop(&unchecked);
    if ((bb->from_bbs->len == 0 && bb != bb0) || table_try_get(&checked, bb->label, NULL))
      continue;
    table_put(&checked, bb->label, bb);

    memcpy(vregs, ra->vregs->data, sizeof(*vregs) * vreg_count);
    if (bb->from_bbs->len == 1) {
      // Oneway flow: Hand over out_regs of the predecessor.
      BB *from = bb->from_bbs->data[0];
      for (int i = 0; i < from->out_regs->len; ++i) {
        VReg *vreg = from->out_regs->data[i];
        if (vreg->flag & VRF_REF)
          continue;
        int virt = ORIG_VIRT(vreg);
        vregs[virt] = vreg;
      }

      replace_vreg_set(bb->in_regs, vregs);
    } else if (bb->from_bbs->len > 1) {
      // Merge flow: Create new versions, prepare for PHI functions.
      for (int i = 0; i < bb->in_regs->len; ++i) {
        VReg *vreg = bb->in_regs->data[i];
        if (vreg->flag & VRF_REF)
          continue;
        int virt = ORIG_VIRT(vreg);  // `vreg` must be original, though.
        Vector *vt = vreg_table[virt];
        VReg *newver = reg_alloc_with_version(ra, ra->vregs->data[virt], vt->len);
        bb->in_regs->data[i] = vregs[virt] = newver;
        vec_push(vt, newver);
      }
    }
    assign_new_vregs(ra, vreg_table, bb, vregs);
    replace_vreg_set(bb->out_regs, vregs);

    push_nexts(bb, &unchecked);
  } while (unchecked.len > 0);

  free(vregs);

  return vreg_table;
}

void make_ssa(RegAlloc *ra, BBContainer *bbcon) {
  analyze_reg_flow(bbcon);
  ra->original_vreg_count = ra->vregs->len;
  ra->vreg_table = ssa_transform(ra, bbcon);
}
