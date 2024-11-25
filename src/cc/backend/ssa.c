#include "../../config.h"
#include "ssa.h"

#include <assert.h>
#include <limits.h>
#include <stdlib.h>
#include <string.h>

#include "ir.h"
#include "regalloc.h"
#include "util.h"

#include "table.h"

#define ORIG_VIRT(vreg)  ((vreg)->orig_virt >= 0 ? (vreg)->orig_virt : (vreg)->virt)

static inline void assign_new_vregs(RegAlloc *ra, Vector **vreg_table, BB *bb, VReg **vregs) {
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

static inline void push_nexts(BB *bb, Vector *unchecked) {
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

static void insert_phis(BBContainer *bbcon, int original_vreg_count) {
  assert(curbb == NULL);
  VReg **from_vregs = calloc_or_die(sizeof(*from_vregs) * original_vreg_count);
  for (int ibb = 1; ibb < bbcon->len; ++ibb) {
    BB *bb = bbcon->data[ibb];
    if (bb->from_bbs->len < 2)
      continue;

    int reg_count = bb->in_regs->len;  // Phi target only.
    for (int i = 0; i < reg_count; ++i) {
      VReg *vreg = bb->in_regs->data[i];
      if (vreg->flag & VRF_REF) {
        // Swap with the last one.
        --reg_count;
        bb->in_regs->data[i] = bb->in_regs->data[reg_count];
        bb->in_regs->data[reg_count] = vreg;
        if (i < reg_count)
          --i;  // Recheck the swapped one.
      }
    }

    Vector *phi_params = new_vector();  // <<VReg*>>
    for (int i = 0; i < reg_count; ++i)
      vec_push(phi_params, new_vector());

    for (int ifrom = 0; ifrom < bb->from_bbs->len; ++ifrom) {
      BB *from = bb->from_bbs->data[ifrom];
      for (int j = 0; j < from->out_regs->len; ++j) {
        VReg *oreg = from->out_regs->data[j];
        assert(0 <= oreg->orig_virt && oreg->orig_virt < original_vreg_count);
        from_vregs[oreg->orig_virt] = oreg;
      }

      for (int j = 0; j < reg_count; ++j) {
        VReg *vreg = bb->in_regs->data[j];
        assert(0 <= vreg->orig_virt && vreg->orig_virt < original_vreg_count);
        VReg *fv = from_vregs[vreg->orig_virt];
        vec_push(phi_params->data[j], fv);
      }
    }

    Vector *phis = phi_params;  // Caution, reuse phi_params as phis:<Phi*>
    for (int i = 0; i < reg_count; ++i) {
      VReg *vreg = bb->in_regs->data[i];
      Vector *params = phi_params->data[i];
      Phi *phi = new_phi(vreg, params);
      phis->data[i] = phi;
    }
    bb->phis = phis;
  }
  free(from_vregs);
}

// To make BB merging flow only from unconditional transition, insert BB.
static int insert_unconditional_bb(BBContainer *bbcon, int ito, int ifrom, IR* last) {
  BB *to = bbcon->data[ito];
  BB *from = bbcon->data[ifrom];
  BB *bb = new_bb();
  int at;

  bool fallthrough = false;
  if (ito > 0) {
    BB *prevbb = bbcon->data[ito - 1];
    assert(prevbb->next == to);
    if (prevbb->irs->len > 0) {
      IR *ir = prevbb->irs->data[prevbb->irs->len - 1];
      fallthrough = !((ir->kind == IR_JMP && ir->jmp.cond == COND_ANY) || ir->kind == IR_TJMP);
    } else {
      fallthrough = true;
    }
  }

  if (fallthrough) {
    at = ifrom + 1;
  } else {
    at = ito;
  }
  switch (last->kind) {
  case IR_JMP:
    if (last->jmp.bb == to) {
      assert(last->jmp.cond != COND_ANY);
      if (fallthrough) {
        last->jmp.bb = from->next;
        last->jmp.cond = invert_cond(last->jmp.cond);
      } else {
        last->jmp.bb = bb;
      }
    }
    break;
  case IR_TJMP:
    assert(from->next != NULL);
    for (size_t i = 0; i < last->tjmp.len; ++i) {
      if (last->tjmp.bbs[i] == to)
        last->tjmp.bbs[i] = bb;
    }
    break;
  default: break;
  }

  vec_push(bb->from_bbs, from);

  if (at != ito) {
    IR *jmp = new_ir_jmp(to);
    vec_push(bb->irs, jmp);
  }

  vec_insert(bbcon, at, bb);

  // Maintain BB link list.
  if (at > 0) {
    BB *prevbb = bbcon->data[at - 1];
    assert(prevbb->next == (at < bbcon->len - 1 ? (BB*)bbcon->data[at + 1] : NULL));
    prevbb->next = bb;
  }
  if (at < bbcon->len - 1) {
    BB *nextbb = bbcon->data[at + 1];
    bb->next = nextbb;
  }

  return at;
}

static int make_from_bb_unconditional(BBContainer *bbcon, int ibb, int ifb) {
  BB *bb = bbcon->data[ibb];
  assert(ifb < bb->from_bbs->len);
  BB *from = bb->from_bbs->data[ifb];
  int at = INT_MAX;
  int pos = from->irs->len;
  if (pos > 0) {
    IR *last = from->irs->data[pos - 1];
    if ((last->kind == IR_JMP && last->jmp.cond != COND_ANY) || last->kind == IR_TJMP) {
      int ifrom;
      for (ifrom = 0; ifrom < bbcon->len; ++ifrom) {
        if (bbcon->data[ifrom] == from)
          break;
      }
      assert(ifrom < bbcon->len);
      at = insert_unconditional_bb(bbcon, ibb, ifrom, last);

      // Replace from_bbs with inserted one.
      bb->from_bbs->data[ifb] = bbcon->data[at];
    }
  }
  return at;
}

static Vector *extract_cyclic_dependency(int ifb, Vector *phis) {
  // Check destination register dependency.
  int n = phis->len;
  for (int i = 0; i < n; ++i) {
    Phi *phi = phis->data[i];
    assert(ifb < phi->params->len);
    VReg *src = phi->params->data[ifb];
    // Find that src is one of predecessor destination vreg.
    int j;
    for (j = 0; j < i; ++j) {
      Phi *phi2 = phis->data[j];
      if (phi2->dst == src)
        break;
    }
    if (j >= i)
      continue;

    // Move i before j: shift intermediate elements.
    int d = i - j;
    memmove(&phis->data[j + 1], &phis->data[j], d * sizeof(*phis->data));
    phis->data[j] = phi;
    ++i;
  }

  // Detect cyclic dependency.
  Vector *cyclics = NULL;
  for (int i = 0; i < n; ++i) {
    Phi *phi = phis->data[i];
    VReg *src = phi->params->data[ifb];
    // Find that src is one of predecessor destination vreg.
    int j;
    for (j = 0; j < i; ++j) {
      Phi *phi2 = phis->data[j];
      if (phi2->dst == src)
        break;
    }
    if (j >= i)
      continue;

    // Destination is dependent on predecessor: Pick up cyclic dependency to `cyclic`.
    Vector *cyclic = new_vector();
    for (;;) {
      Phi *phi2 = phis->data[j];
      vec_push(cyclic, phi2);
      phis->data[j] = NULL;
      if (j >= i)
        break;

      VReg *src2 = phi2->params->data[ifb];
      while (++j < i) {
        Phi *phi3 = phis->data[j];
        if (phi3->dst == src2)
          break;
      }
    }
    assert(cyclic->len >= 2);

    if (cyclics == NULL)
      cyclics = new_vector();
    vec_push(cyclics, cyclic);

    // Eliminate them from phis.
    for (int k = 0; k < n; ++k) {
      if (phis->data[k] == NULL) {
        phis->data[k] = phis->data[--n];
        phis->data[n] = NULL;
      }
    }
    for (; n > 0 && phis->data[n - 1] == NULL; --n)
      ;
    phis->len = n;  // Truncate (Recovered in later).
  }
  return cyclics;
}

static void replace_phis(RegAlloc *ra, BB *bb, int ifb, Vector *phis) {
  Vector *cyclics = extract_cyclic_dependency(ifb, phis);

  // Detect insertion point.
  BB *from = bb->from_bbs->data[ifb];
  int pos = from->irs->len;
  if (pos > 0) {
    IR *last = from->irs->data[pos - 1];
    assert(last->kind != IR_JMP || last->jmp.cond == COND_ANY);
    if (last->kind == IR_JMP || last->kind == IR_TJMP)
      --pos;
  }

  // Put non-cyclic phis.
  for (int iphi = 0; iphi < phis->len; ++iphi) {
    Phi *phi = phis->data[iphi];
    VReg *src_vreg = phi->params->data[ifb];
    IR *mov = new_ir_mov(phi->dst, src_vreg, 0);
    vec_insert(from->irs, pos++, mov);
  }

  if (cyclics != NULL) {
    Vector **vreg_table = ra->vreg_table;
    for (int i = 0; i < cyclics->len; ++i) {
      Vector *cyclic = cyclics->data[i];
      assert(cyclic->len > 0);
      Phi *first = cyclic->data[0];

      // Allocate temporary vreg.
      VReg *parent = first->dst;
      assert(parent->orig_virt >= 0);  // phi's destination must not be an original virtual register.
      Vector *vt = vreg_table[parent->orig_virt];
      VReg *tmp = reg_alloc_with_version(ra, parent, vt->len);
      tmp->orig_virt = parent->orig_virt;
      vec_push(vt, tmp);

      for (int j = 0; j < cyclic->len; ++j) {
        Phi *phi = cyclic->data[j];
        VReg *src_vreg = phi->params->data[ifb];
        IR *mov = new_ir_mov(j == 0 ? tmp : phi->dst, src_vreg, 0);
        vec_insert(from->irs, pos++, mov);
        vec_push(phis, phi);  // Restore phi.
      }

      IR *mov = new_ir_mov(first->dst, tmp, 0);
      vec_insert(from->irs, pos++, mov);

      free_vector(cyclic);
    }
    free_vector(cyclics);
  }
}

//

void make_ssa(RegAlloc *ra, BBContainer *bbcon) {
  analyze_reg_flow(bbcon);
  ra->original_vreg_count = ra->vregs->len;
  ra->vreg_table = ssa_transform(ra, bbcon);
  insert_phis(bbcon, ra->original_vreg_count);
}

void resolve_phis(RegAlloc *ra, BBContainer *bbcon) {
  assert(curbb == NULL);
  for (int ibb = 0; ibb < bbcon->len; ++ibb) {
    BB *bb = bbcon->data[ibb];
    Vector *phis = bb->phis;
    if (phis == NULL)
      continue;

    for (int ifb = 0; ifb < bb->from_bbs->len; ++ifb) {
      int at = make_from_bb_unconditional(bbcon, ibb, ifb);
      if (at <= ibb) {  // Inserted before this BB.
        ++ibb;
      }

      replace_phis(ra, bb, ifb, phis);
    }
    vec_clear(phis);
  }
}
