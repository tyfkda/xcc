#include "../../config.h"
#include "ssa.h"

#include <assert.h>

#include "ir.h"
#include "regalloc.h"
#include "util.h"

static Vector **increment_vreg_versions(RegAlloc *ra, BBContainer *bbcon) {
  int vreg_count = ra->vregs->len;
  Vector **vreg_table = malloc_or_die(sizeof(*vreg_table) * vreg_count);
  for (int i = 0; i < vreg_count; ++i) {
    VReg *vreg = ra->vregs->data[i];
    Vector *v = new_vector();
    if (vreg->flag & (VRF_PARAM | VRF_REF))
      vec_push(v, vreg);
    vreg_table[i] = v;
  }

  for (int ibb = 0; ibb < bbcon->bbs->len; ++ibb) {
    BB *bb = bbcon->bbs->data[ibb];
    if (ibb > 0 && bb->from_bbs->len > 0) {
      for (int i = 0; i < bb->in_regs->len; ++i) {
        VReg *vreg = bb->in_regs->data[i];
        if (vreg->flag & VRF_REF)
          continue;
        Vector *vt = vreg_table[vreg->virt];
        VReg *newver = reg_alloc_with_version(ra, ra->vregs->data[vreg->virt], vt->len);
        vec_push(vt, newver);
      }
    }

    for (int iir = 0; iir < bb->irs->len; ++iir) {
      IR *ir = bb->irs->data[iir];
      if (ir->opr1 != NULL && !(ir->opr1->flag & (VRF_CONST | VRF_REF))) {
        Vector *vt = vreg_table[ir->opr1->virt];
        assert(vt->len > 0);
        ir->opr1 = vt->data[vt->len - 1];
      }
      if (ir->opr2 != NULL && !(ir->opr2->flag & (VRF_CONST | VRF_REF))) {
        Vector *vt = vreg_table[ir->opr2->virt];
        assert(vt->len > 0);
        ir->opr2 = vt->data[vt->len - 1];
      }
      if (ir->dst != NULL && !(ir->dst->flag & (VRF_CONST | VRF_REF))) {
        int virt = ir->dst->virt;
        Vector *vt = vreg_table[virt];
        VReg *dst = ra->vregs->data[virt];
        if (vt->len > 0)
          ir->dst = dst = reg_alloc_with_version(ra, dst, vt->len);
        vec_push(vt, dst);
      }
    }
  }
  return vreg_table;
}

void make_ssa(RegAlloc *ra, BBContainer *bbcon) {
  analyze_reg_flow(bbcon);
  increment_vreg_versions(ra, bbcon);
}
