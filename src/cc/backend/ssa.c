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
        bb->in_regs->data[i] = newver;
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

    // Replace out_regs.
    for (int i = 0; i < bb->out_regs->len; ++i) {
      VReg *vreg = bb->out_regs->data[i];
      if (vreg->flag & VRF_REF)
        continue;
      Vector *vt = vreg_table[vreg->virt];
      if (vt->len <= 0) {
        // This case exists when a variable might be uninitialized (in syntactically).
        continue;
      }
      bb->out_regs->data[i] = vt->data[vt->len - 1];
    }
  }

  // assigned_regs is remained, so incorrect.

  return vreg_table;
}

static void replace_vreg_all(BBContainer *bbcon, VReg *src, VReg *dst) {
  for (int ibb = 0; ibb < bbcon->bbs->len; ++ibb) {
    BB *bb = bbcon->bbs->data[ibb];
    for (int i = 0; i < bb->in_regs->len; ++i) {
      VReg *v = bb->in_regs->data[i];
      if (v == src)
        bb->in_regs->data[i] = dst;
    }
    for (int i = 0; i < bb->out_regs->len; ++i) {
      VReg *v = bb->out_regs->data[i];
      if (v == src)
        bb->out_regs->data[i] = dst;
    }
    for (int i = 0; i < bb->assigned_regs->len; ++i) {
      VReg *v = bb->assigned_regs->data[i];
      if (v == src)
        bb->assigned_regs->data[i] = dst;
    }

    for (int iir = 0; iir < bb->irs->len; ++iir) {
      IR *ir = bb->irs->data[iir];
      if (ir->opr1 == src)
        ir->opr1 = dst;
      if (ir->opr2 == src)
        ir->opr2 = dst;
      if (ir->dst == src)
        ir->dst = dst;
      if (ir->kind == IR_PHI) {
        for (int i = 0; i < ir->phi.vregs->len; ++i) {
          VReg *v = ir->phi.vregs->data[i];
          if (v == src)
            ir->phi.vregs->data[i] = dst;
        }
      }
    }
  }
}

static void insert_phis(BBContainer *bbcon) {
  assert(curbb == NULL);
  for (int ibb = 1; ibb < bbcon->bbs->len; ++ibb) {
    BB *bb = bbcon->bbs->data[ibb];
    if (bb->from_bbs->len == 0)
      continue;
    if (bb->from_bbs->len == 1) {
      BB *from = bb->from_bbs->data[0];
      for (int i = 0; i < bb->in_regs->len; ++i) {
        VReg *vreg = bb->in_regs->data[i];
        VReg *fv = NULL;
        for (int j = 0; j < from->out_regs->len; ++j) {
          VReg *o = from->out_regs->data[j];
          if (o->orig_virt == vreg->orig_virt) {
            fv = o;
            break;
          }
        }
        assert(fv != NULL);
        replace_vreg_all(bbcon, vreg, fv);
      }
    } else {
      for (int i = bb->in_regs->len; i-- > 0; ) {
        VReg *vreg = bb->in_regs->data[i];
        Vector *ins = new_vector();
        for (int ifrom = 0; ifrom < bb->from_bbs->len; ++ifrom) {
          BB *from = bb->from_bbs->data[ifrom];
          VReg *fv = NULL;
          for (int j = 0; j < from->out_regs->len; ++j) {
            VReg *o = from->out_regs->data[j];
            if (o->orig_virt == vreg->orig_virt) {
              fv = o;
              break;
            }
          }
          assert(fv != NULL);
          vec_push(ins, fv);
        }
        bool same_all = true;
        for (int j = 1; j < ins->len; ++j) {
          if (ins->data[j] != ins->data[0]) {
            same_all = false;
            break;
          }
        }
        if (same_all) {
          assert(ins->len > 0);
          replace_vreg_all(bbcon, vreg, ins->data[0]);
        } else {
          vec_insert(bb->irs, 0, new_ir_phi(vreg, ins));
        }
      }
    }
  }
}

void make_ssa(RegAlloc *ra, BBContainer *bbcon) {
  analyze_reg_flow(bbcon);
  increment_vreg_versions(ra, bbcon);
  insert_phis(bbcon);
}

void resolve_phis(BBContainer *bbcon) {
  assert(curbb == NULL);
  for (int ibb = 0; ibb < bbcon->bbs->len; ++ibb) {
    BB *bb = bbcon->bbs->data[ibb];
    int nphi = 0;
    for (nphi = 0; nphi < bb->irs->len; ++nphi) {
      IR *ir = bb->irs->data[nphi];
      if (ir->kind != IR_PHI)
        break;
    }
    if (nphi == 0)
      continue;

    for (int ifrom = 0; ifrom < bb->from_bbs->len; ++ifrom) {
      BB *from = bb->from_bbs->data[ifrom];
      for (int i = 0; i < bb->in_regs->len; ++i) {
        VReg *dst_vreg = bb->in_regs->data[i];
        IR *phi = NULL;
        for (int iphi = 0; iphi < nphi; ++iphi) {
          IR *ir = bb->irs->data[iphi];
          assert(ir->kind == IR_PHI);
          if (ir->dst == dst_vreg) {
            phi = ir;
            break;
          }
        }
        // assert(phi != NULL);
        if (phi == NULL)
          continue;
        VReg *src_vreg = phi->phi.vregs->data[ifrom];
        IR *mov = new_ir_mov(dst_vreg, src_vreg, 0);
        int pos = from->irs->len;
        if (pos > 0) {
          IR *last = from->irs->data[pos - 1];
          if (last->kind == IR_JMP || last->kind == IR_TJMP) {
            --pos;
          }
        }
        vec_insert(from->irs, pos, mov);
      }
    }

    for (int i = 0; i < nphi; ++i)
      vec_remove_at(bb->irs, 0);
  }
}
