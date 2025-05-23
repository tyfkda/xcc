#include "../../config.h"
#include "optimize.h"

#include <assert.h>
#include <limits.h>
#include <stdlib.h>  // free

#include "ir.h"
#include "regalloc.h"
#include "ssa.h"
#include "table.h"
#include "util.h"

bool keep_phi;
bool apply_ssa;

static IR *is_last_jmp(BB *bb) {
  int len;
  IR *ir;
  if ((len = bb->irs->len) > 0 && (ir = bb->irs->data[len - 1])->kind == IR_JMP)
    return ir;
  return NULL;
}

static IR *is_last_jtable(BB *bb) {
  int len;
  IR *ir;
  if ((len = bb->irs->len) > 0 && (ir = bb->irs->data[len - 1])->kind == IR_TJMP)
    return ir;
  return NULL;
}

static void replace_jmp_destination(BBContainer *bbcon, BB *src, BB *dst) {
  for (int i = 0; i < bbcon->len; ++i) {
    BB *bb = bbcon->data[i];
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
  Table keeptbl;
  for (;;) {
    table_init(&keeptbl);
    assert(bbcon->len > 0);

    for (int i = 0; i < bbcon->len; ++i) {
      BB *bb = bbcon->data[i];
      bool remove = false;
      IR *ir_jmp = is_last_jmp(bb);
      if (ir_jmp != NULL && ir_jmp->jmp.bb == bb->next) {  // Remove jmp to next instruction.
        vec_pop(bb->irs);
        ir_jmp = NULL;
      }
      if (bb->irs->len == 0 && bb->next != NULL) {  // Empty BB.
        replace_jmp_destination(bbcon, bb, bb->next);
        remove = true;
      } else if (bb->irs->len == 1 && ir_jmp != NULL && ir_jmp->jmp.cond == COND_ANY &&
                 bb != ir_jmp->jmp.bb) {  // jmp only.
        replace_jmp_destination(bbcon, bb, ir_jmp->jmp.bb);
        if (i > 0) {
          BB *pbb = bbcon->data[i - 1];
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
    for (int i = 1; i < bbcon->len; ++i) {
      BB *bb = bbcon->data[i];
      if (!table_try_get(&keeptbl, bb->label, NULL)) {
        if (i > 0) {
          BB *pbb = bbcon->data[i - 1];
          pbb->next = bb->next;
        }

        vec_remove_at(bbcon, i);
        --i;
        again = true;
      }
    }
    if (!again)
      break;
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
    for (int i = 0; i < bbcon->len; ++i) {
      BB *bb = bbcon->data[i];
      Vector *phis = bb->phis;
      if (phis != NULL) {
        for (int j = 0; j < phis->len; ++j) {
          Phi *phi = phis->data[j];
          for (int k = 0; k < phi->params->len; ++k) {
            VReg *vreg = phi->params->data[k];
            assert(vreg != NULL);
            if (!(vreg->flag & VRF_CONST))
              vreg_read[vreg->virt] = true;
          }
        }
      }

      for (int j = 0; j < bb->irs->len; ++j) {
        IR *ir = bb->irs->data[j];
        VReg *operands[] = {ir->opr1, ir->opr2};
        for (int k = 0; k < 2; ++k) {
          VReg *vreg = operands[k];
          if (vreg != NULL && !(vreg->flag & VRF_CONST))
            vreg_read[vreg->virt] = true;
        }
      }
    }

    // Remove instruction if the destination is unread.
    for (int i = 0; i < bbcon->len; ++i) {
      BB *bb = bbcon->data[i];
      Vector *phis = bb->phis;
      if (phis != NULL) {
        for (int j = 0; j < phis->len; ++j) {
          Phi *phi = phis->data[j];
          if (vreg_read[phi->dst->virt])
            continue;
          vec_remove_at(phis, j);
          --j;
        }
      }

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
        if (vreg->original != vreg) {
          assert(vreg->original->virt < ra->original_vreg_count);
          Vector *vt = ra->vreg_table[vreg->original->virt];
          int j;
          for (j = 0; j < vt->len; ++j) {
            if (vt->data[j] == vreg)
              break;
          }
          assert(j < vt->len);
          vec_remove_at(vt, j);
        }
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

static int replace_register_in_bb(BB *bb, VReg *target, VReg *alternation, int start, int ip) {
  int first = INT_MAX;
  for (int iir = start; iir < bb->irs->len; ++iir, ++ip) {
    IR *ir = bb->irs->data[iir];
    if (ir->dst == target && ir->opr1 != alternation)
      break;

    if (ir->opr1 == target) {
      ir->opr1 = alternation;
      first = MIN(first, ip);
    }
    if (ir->opr2 == target) {
      ir->opr2 = alternation;
      first = MIN(first, ip);
    }

    if (ir->kind == IR_CALL) {
      int n = ir->call->total_arg_count;
      VReg **operands = ir->call->args;
      for (int i = 0; i < n; ++i) {
        VReg *opr = operands[i];
        if (opr == target) {
          operands[i] = alternation;
          first = MIN(first, ip);
        }
      }
    }
  }
  return first;
}

static bool calc_const_cond(enum ConditionKind cond, VReg *opr1, VReg *opr2) {
  assert(opr1->flag & VRF_CONST);
  assert(opr2->flag & VRF_CONST);

#ifndef __NO_FLONUM
  if (opr1->flag & VRF_FLONUM) {
    double f1 = opr1->flonum.value;
    double f2 = opr2->flonum.value;
    switch ((int)cond) {
    case COND_EQ | COND_FLONUM:  return f1 == f2;
    case COND_NE | COND_FLONUM:  return f1 != f2;
    case COND_LT | COND_FLONUM:  return f1 < f2;
    case COND_GT | COND_FLONUM:  return f1 > f2;
    case COND_LE | COND_FLONUM:  return f1 <= f2;
    case COND_GE | COND_FLONUM:  return f1 >= f2;
    default: assert(false); return false;
    }
  } else
#endif
  {
    bool is_unsigned = (cond & COND_UNSIGNED) != 0;
    int64_t n1 = wrap_value(opr1->fixnum, 1 << opr1->vsize, is_unsigned);
    int64_t n2 = wrap_value(opr2->fixnum, 1 << opr2->vsize, is_unsigned);
    switch ((int)cond) {
    case COND_EQ | COND_UNSIGNED:  // Fallthrough
    case COND_EQ:  return n1 == n2;

    case COND_NE | COND_UNSIGNED:  // Fallthrough
    case COND_NE:  return n1 != n2;

    case COND_LT:  return n1 < n2;
    case COND_GT:  return n1 > n2;
    case COND_LE:  return n1 <= n2;
    case COND_GE:  return n1 >= n2;

    case COND_LT | COND_UNSIGNED:  return (uint64_t)n1 < (uint64_t)n2;
    case COND_GT | COND_UNSIGNED:  return (uint64_t)n1 > (uint64_t)n2;
    case COND_LE | COND_UNSIGNED:  return (uint64_t)n1 <= (uint64_t)n2;
    case COND_GE | COND_UNSIGNED:  return (uint64_t)n1 >= (uint64_t)n2;
    default: assert(false); return false;
    }
  }
}

static bool replace_const_jmp(IR *ir) {
  assert(ir->opr1 != NULL);
  assert(ir->opr2 != NULL);
  assert(ir->jmp.cond != COND_ANY && ir->jmp.cond != COND_NONE);
  if (ir->opr1->flag & VRF_CONST) {
    if (ir->opr2->flag & VRF_CONST) {
      ir->jmp.cond = calc_const_cond(ir->jmp.cond, ir->opr1, ir->opr2) ? COND_ANY : COND_NONE;
      ir->opr1 = ir->opr2 = NULL;
      return true;
    } else {
      ir->jmp.cond = swap_cond(ir->jmp.cond & COND_MASK) | (ir->jmp.cond & ~COND_MASK);
      VReg *tmp = ir->opr1;
      ir->opr1 = ir->opr2;
      ir->opr2 = tmp;
    }
  }
  return false;
}

static bool replace_const_cond(RegAlloc *ra, IR *ir) {
  assert(ir->opr1 != NULL);
  assert(ir->opr2 != NULL);
  assert(ir->cond.kind != COND_ANY && ir->cond.kind != COND_NONE);
  if (ir->opr1->flag & VRF_CONST) {
    if (ir->opr2->flag & VRF_CONST) {
      // Replace COND to MOV.
      bool result = calc_const_cond(ir->cond.kind, ir->opr1, ir->opr2);
      ir->kind = IR_MOV;
      ir->opr1 = reg_alloc_spawn_const(ra, result, ir->dst->vsize);
      ir->opr2 = NULL;
      return true;
    } else {
      ir->cond.kind = swap_cond(ir->cond.kind & COND_MASK) | (ir->cond.kind & ~COND_MASK);
      VReg *tmp = ir->opr1;
      ir->opr1 = ir->opr2;
      ir->opr2 = tmp;
    }
  }
  return false;
}

static int64_t calc_const_expr(IR *ir) {
  assert((ir->opr1->flag & (VRF_FLONUM | VRF_CONST)) == VRF_CONST);
  assert(ir->opr2 == NULL || ir->opr2->flag & VRF_CONST);

#define CALC_CONST(kind) \
  switch (kind) { \
  case IR_ADD: value = opr1 + opr2; break; \
  case IR_SUB: value = opr1 - opr2; break; \
  case IR_MUL: value = opr1 * opr2; break; \
  case IR_DIV: assert(opr2 != 0); value = opr1 / opr2; break; \
  case IR_MOD: assert(opr2 != 0); value = opr1 % opr2; break; \
  case IR_BITAND: value = opr1 & opr2; break; \
  case IR_BITOR: value = opr1 | opr2; break; \
  case IR_BITXOR: value = opr1 ^ opr2; break; \
  case IR_LSHIFT: value = opr1 << opr2; break; \
  case IR_RSHIFT: value = opr1 >> opr2; break; \
  case IR_NEG: value = -opr1; break; \
  case IR_BITNOT: value = ~opr1; break; \
  default: assert(false); break; \
  }

  int64_t value = 0;
  if (ir->flag & IRF_UNSIGNED) {
    uint64_t opr1 = ir->opr1->fixnum;
    uint64_t opr2 = ir->opr2 != NULL ? ir->opr2->fixnum : 0;
    CALC_CONST(ir->kind);
  } else {
    int64_t opr1 = ir->opr1->fixnum;
    int64_t opr2 = ir->opr2 != NULL ? ir->opr2->fixnum : 0;
    CALC_CONST(ir->kind);
  }
#undef CALC

  return value;
}

#ifndef __NO_FLONUM
static double calc_fconst_expr(IR *ir) {
  assert((ir->opr1->flag & (VRF_FLONUM | VRF_CONST)) == (VRF_FLONUM | VRF_CONST));
  assert(ir->opr2 == NULL || ir->opr2->flag & VRF_CONST);

  double value = 0;
  double opr1 = ir->opr1->flonum.value;
  double opr2 = ir->opr2 != NULL ? ir->opr2->flonum.value : 0;
  switch (ir->kind) {
  case IR_ADD: value = opr1 + opr2; break;
  case IR_SUB: value = opr1 - opr2; break;
  case IR_MUL: value = opr1 * opr2; break;
  case IR_DIV: assert(opr2 != 0); value = opr1 / opr2; break;
  case IR_NEG: value = -opr1; break;
  default: assert(false); break;
  }

  return value;
}
#endif

static bool constant_folding(RegAlloc *ra, IR *ir) {
  switch (ir->kind) {
  case IR_DIV:
  case IR_MOD:
    if ((ir->opr2->flag & VRF_CONST) &&
        (   (!(ir->opr2->flag & VRF_FLONUM) && ir->opr2->fixnum == 0)
#ifndef __NO_FLONUM
          || ( (ir->opr2->flag & VRF_FLONUM) && ir->opr2->flonum.value == 0)
#endif
        )) {
      // Stay as it is, DIV or MOD instruction accepts even if both operands are constant.
      // Zero division exception will be thrown on runtime.
      // TODO: warning message?
      break;
    }
    // Fallthrough
  case IR_ADD:
  case IR_SUB:
  case IR_MUL:
  case IR_BITAND:
  case IR_BITOR:
  case IR_BITXOR:
  case IR_LSHIFT:
  case IR_RSHIFT:
  case IR_NEG:
  case IR_BITNOT:
    if ((ir->opr1->flag & VRF_CONST) && (ir->opr2 == NULL || ir->opr2->flag & VRF_CONST)) {
#ifndef __NO_FLONUM
      if (ir->opr1->flag & VRF_FLONUM) {
        double value = calc_fconst_expr(ir);
        // Replace to MOV.
        ir->kind = IR_MOV;
        ir->opr1 = reg_alloc_spawn_fconst(ra, value, ir->dst->vsize);
        ir->opr2 = NULL;
      } else
#endif
      {
        int64_t value = wrap_value(calc_const_expr(ir), 1 << ir->dst->vsize,
                                   ir->flag & IRF_UNSIGNED);
        // Replace to MOV.
        ir->kind = IR_MOV;
        ir->opr1 = reg_alloc_spawn_const(ra, value, ir->dst->vsize);
        ir->opr2 = NULL;
      }
      return true;
    }
    break;
  case IR_COND:
    assert(ir->cond.kind != COND_ANY && ir->cond.kind != COND_NONE);
    return replace_const_cond(ra, ir);
  case IR_CAST:
    if (ir->opr1->flag & VRF_CONST) {
#ifndef __NO_FLONUM
      if (ir->dst->flag & VRF_FLONUM) {
        double value;
        if (!(ir->opr1->flag & VRF_FLONUM)) {
          value = (ir->flag & IRF_UNSIGNED) ? (double)(uint64_t)ir->opr1->fixnum
                                            : (double)ir->opr1->fixnum;
        } else {
          value = ir->opr1->flonum.value;
        }
        // Replace to MOV.
        ir->kind = IR_MOV;
        ir->opr1 = reg_alloc_spawn_fconst(ra, value, ir->dst->vsize);
      } else
#endif
      {
        int64_t value;
#ifndef __NO_FLONUM
        if (ir->opr1->flag & VRF_FLONUM) {
          double d = ir->opr1->flonum.value;
          value = !(ir->flag & IRF_UNSIGNED) ? (int64_t)d : (int64_t)(uint64_t)d;
          value = wrap_value(value, 1 << ir->dst->vsize, ir->flag & IRF_UNSIGNED);
        } else
#endif
        {
          value = ir->opr1->fixnum;
          if (ir->dst->vsize > ir->opr1->vsize)
            value = wrap_value(value, 1 << ir->opr1->vsize, ir->flag & IRF_UNSIGNED);
        }
        // Replace to MOV.
        ir->kind = IR_MOV;
        ir->opr1 = reg_alloc_spawn_const(ra, value, ir->dst->vsize);
      }
      return true;
    }
    break;
  case IR_JMP:
    if (ir->jmp.cond != COND_ANY) {
      assert(ir->jmp.cond != COND_NONE);
      return replace_const_jmp(ir);
    }
    break;

  default: break;
  }
  return false;
}

//

static inline void fold_biofs_addition(BB *bb, int i) {
  IR *ir = bb->irs->data[i];
  if (i < bb->irs->len - 1) {
    IR *next = bb->irs->data[i + 1];
    if ((next->kind == IR_ADD || next->kind == IR_SUB) &&
        next->opr1 == ir->dst && next->opr2->flag & VRF_CONST) {
      assert(!(next->opr2->flag & VRF_FLONUM));
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
}

static inline void fold_addition(RegAlloc *ra, BB *bb, int i) {
  IR *ir = bb->irs->data[i];
  if ((ir->opr2->flag & (VRF_FLONUM | VRF_CONST)) == VRF_CONST && i < bb->irs->len - 1) {
    IR *next = bb->irs->data[i + 1];
    if ((next->kind == IR_ADD || next->kind == IR_SUB) &&
        next->opr1 == ir->dst && next->opr2->flag & VRF_CONST) {
      assert(!(next->opr2->flag & VRF_FLONUM));
      // Overwrite next IR. Current IR should be eliminated because of dst is unused.
      VReg *dst = next->dst;
      VReg *opr2 = ir->opr2;
      int64_t value1 = opr2->fixnum;
      if (ir->kind == IR_SUB)
        value1 = -value1;
      int64_t value2 = next->opr2->fixnum;
      if (next->kind == IR_SUB)
        value2 = -value2;
      *next = *ir;
      next->kind = IR_ADD;
      next->dst = dst;
      next->opr2 = reg_alloc_spawn_const(ra, value1 + value2, opr2->vsize);
    }
  }
}

static inline int muldiv_to_shift(RegAlloc *ra, BB *bb, int i) {
  IR *ir = bb->irs->data[i];
  if ((ir->opr2->flag & (VRF_FLONUM | VRF_CONST)) == VRF_CONST &&
      IS_POWER_OF_2(ir->opr2->fixnum)) {
    int shift = most_significant_bit(ir->opr2->fixnum);
    if (ir->kind == IR_DIV && !(ir->flag & IRF_UNSIGNED)) {
      // Patch for signed right shift:
      enum VRegSize vsize = ir->opr1->vsize;
      int bits = TARGET_CHAR_BIT << vsize;
      VReg *tmp = reg_alloc_spawn(ra, vsize, ir->opr1->flag & VRF_MASK);
      IR *mov = new_ir_mov(tmp, ir->opr1, ir->flag);
      IR *sign_bit = new_ir_bop_raw(IR_RSHIFT, tmp, tmp,
                                    reg_alloc_spawn_const(ra, bits - 1, vsize), ir->flag);
      IR *addend = new_ir_bop_raw(IR_RSHIFT, tmp, tmp,
                                  reg_alloc_spawn_const(ra, bits - shift, vsize), IRF_UNSIGNED);
      IR *add = new_ir_bop_raw(IR_ADD, tmp, tmp, ir->opr1, 0);
      vec_insert(bb->irs, i++, mov);
      vec_insert(bb->irs, i++, sign_bit);
      vec_insert(bb->irs, i++, addend);
      vec_insert(bb->irs, i++, add);
      ir->opr1 = tmp;
    }
    ir->kind = ir->kind + (IR_LSHIFT - IR_MUL);
    ir->opr2 = reg_alloc_spawn_const(ra, shift, ir->opr2->vsize);
  }
  return i;
}

static void peephole(RegAlloc *ra, BB *bb) {
  for (int i = 0; i < bb->irs->len; ++i) {
    IR *ir = bb->irs->data[i];
    switch (ir->kind) {
    case IR_BOFS:
    case IR_IOFS:
      fold_biofs_addition(bb, i);
      break;
    case IR_ADD:
    case IR_SUB:
      fold_addition(ra, bb, i);
      break;
    case IR_MUL:
    case IR_DIV:
      i = muldiv_to_shift(ra, bb, i);
      break;
    case IR_MOV:
      if (!(ir->opr1->flag & VRF_CONST) && i > 0) {
        // t = a @@ b, c = t  =>  c = a + b, t = c
        IR *prev = bb->irs->data[i - 1];
        if (prev->dst == ir->opr1) {
          VReg *vt = prev->dst, *vc = ir->dst;
          prev->dst = vc;
          ir->dst = vt;
          ir->opr1 = vc;
        }
      }
      break;
    default:
      break;
    }
  }
}

// Depends on SSA.

static int replace_register(BBContainer *bbcon, VReg *target, VReg *alternation) {
  if (target->flag & VRF_FORCEMEMORY || alternation->flag & VRF_FORCEMEMORY)
    return INT_MAX;

  int first = INT_MAX;
  int ip = 0;
  for (int ibb = 0; ibb < bbcon->len; ++ibb) {
    BB *bb = bbcon->data[ibb];

    Vector *phis = bb->phis;
    if (phis != NULL) {
      for (int iphi = 0; iphi < phis->len; ++iphi) {
        Phi *phi = phis->data[iphi];
        for (int i = 0; i < phi->params->len; ++i) {
          VReg *opr = phi->params->data[i];
          if (opr == target) {
            phi->params->data[i] = alternation;
            first = MIN(first, ip);
          }
        }
      }
    }

    int at = replace_register_in_bb(bb, target, alternation, 0, ip);
    first = MIN(first, at);
    ip += bb->irs->len;
  }
  return first;
}

static void copy_propagation(RegAlloc *ra, BBContainer *bbcon) {
  bool again;
  do {
    again = false;
    int ip = 0;
    for (int ibb = 0; ibb < bbcon->len; ++ibb) {
      BB *bb = bbcon->data[ibb];
      if (bb->from_bbs->len == 0 && ibb > 0)
        continue;

      Vector *phis = bb->phis;
      if (phis != NULL) {
        for (int iphi = 0; iphi < phis->len; ++iphi) {
          Phi *phi = phis->data[iphi];
          int n = phi->params->len;
          assert(n > 0);
          VReg *dst = phi->dst;
          VReg *value = phi->params->data[0];
          int i;
          for (i = 1; i < n; ++i) {
            VReg *vreg = phi->params->data[i];
            if (vreg != value && vreg != dst)
              break;
          }
          if (i >= n) {  // All values are same.
            IR *ir = new_ir_mov(dst, value, 0);
            vec_insert(bb->irs, 0, ir);
            vec_remove_at(phis, iphi--);
          }
        }
      }

      for (int iir = 0; iir < bb->irs->len; ++iir, ++ip) {
        IR *ir = bb->irs->data[iir];
        if (constant_folding(ra, ir)) {
          if (ir->kind == IR_JMP && ir->jmp.cond == COND_NONE) {
            vec_remove_at(bb->irs, iir);
            --iir;
            --ip;
            continue;
          }
        }

        switch (ir->kind) {
        case IR_RESULT:
          // Inlined function call uses RESULT with `dst`.
          if (ir->dst == NULL)
            break;
          // Fallthrough
        case IR_MOV:
          if (ir->dst->flag & VRF_VOLATILE)
            break;
          if (replace_register(bbcon, ir->dst, ir->opr1) < ip)  // Former instruction is replaced:
            again = true;  // Try again.
          break;
        default: break;
        }
      }
    }
  } while (again);
}

//

void optimize(RegAlloc *ra, BBContainer *bbcon) {
  // Clean up unused IRs.
  for (int i = 1; i < bbcon->len; ++i) {
    BB *bb = bbcon->data[i];
    if (bb->from_bbs->len == 0)
      vec_clear(bb->irs);
  }

  // Peephole
  for (int i = 0; i < bbcon->len; ++i) {
    BB *bb = bbcon->data[i];
    peephole(ra, bb);
  }

  if (apply_ssa) {
    make_ssa(ra, bbcon);
    copy_propagation(ra, bbcon);
    remove_unused_vregs(ra, bbcon);
    if (!keep_phi) {
      resolve_phis(ra, bbcon);
      remove_unnecessary_bb(bbcon);
    }
  } else {
    remove_unused_vregs(ra, bbcon);
    remove_unnecessary_bb(bbcon);
  }
  detect_from_bbs(bbcon);
}
