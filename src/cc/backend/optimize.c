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
      if (bb->irs->len == 0 && bb->next != NULL) {  // Empty BB.
        replace_jmp_destination(bbcon, bb, bb->next);
        remove = true;
      } else if (bb->irs->len == 1 && ir_jmp != NULL && ir_jmp->jmp.cond == COND_ANY &&
                 !equal_name(bb->label, ir_jmp->jmp.bb->label)) {  // jmp only.
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

  // Remove jmp to next instruction.
  for (int i = 0; i < bbcon->len - 1; ++i) {  // Make last one keeps alive.
    BB *bb = bbcon->data[i];
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

// Depends on SSA.

static int replace_register(BBContainer *bbcon, VReg *target, VReg *alternation) {
  if (target->flag & VRF_REF || alternation->flag & VRF_REF)
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

    for (int iir = 0; iir < bb->irs->len; ++iir, ++ip) {
      IR *ir = bb->irs->data[iir];
      if (ir->opr1 == target) {
        // Special case: Keep the original register for floating point number.
        if (ir->kind == IR_CAST && ir->dst->flag & VRF_FLONUM)
          continue;
        ir->opr1 = alternation;
        first = MIN(first, ip);
      }
      if (ir->opr2 == target) {
        ir->opr2 = alternation;
        first = MIN(first, ip);
      }

      if (ir->kind != IR_CALL)
        continue;
      int n = ir->call.total_arg_count;
      VReg **operands = ir->call.args;
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

  int64_t n1 = opr1->fixnum;
  int64_t n2 = opr2->fixnum;
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
  assert(ir->opr1->flag & VRF_CONST);
  assert(ir->opr2 == NULL || ir->opr2->flag & VRF_CONST);

#define CALC_CONST(kind) \
  switch (kind) { \
  case IR_ADD: value = opr1 + opr2; break; \
  case IR_SUB: value = opr1 - opr2; break; \
  case IR_MUL: value = opr1 * opr2; break; \
  case IR_DIV: value = opr1 / opr2; break; \
  case IR_MOD: value = opr1 % opr2; break; \
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
        switch (ir->kind) {
        case IR_RESULT:
          // Inlined function call uses RESULT with `dst`.
          if (ir->dst == NULL)
            break;
          // Fallthrough
        case IR_MOV:
          if (replace_register(bbcon, ir->dst, ir->opr1) < ip)  // Former instruction is replaced:
            again = true;  // Try again.
          break;
        case IR_ADD:
        case IR_SUB:
        case IR_MUL:
        case IR_DIV:
        case IR_MOD:
        case IR_BITAND:
        case IR_BITOR:
        case IR_BITXOR:
        case IR_LSHIFT:
        case IR_RSHIFT:
        case IR_NEG:
        case IR_BITNOT:
          if ((ir->opr1->flag & VRF_CONST) && (ir->opr2 == NULL || ir->opr2->flag & VRF_CONST)) {
            int64_t value = wrap_value(calc_const_expr(ir), 1 << ir->dst->vsize, ir->flag & IRF_UNSIGNED);
            // Replace to MOV.
            ir->kind = IR_MOV;
            ir->opr1 = reg_alloc_spawn_const(ra, value, ir->dst->vsize);
            ir->opr2 = NULL;
            if (replace_register(bbcon, ir->dst, ir->opr1) < ip)
              again = true;
          }
          break;
        case IR_JMP:
          if (ir->jmp.cond != COND_ANY) {
            assert(ir->jmp.cond != COND_NONE);
            if (replace_const_jmp(ir)) {
              if (ir->jmp.cond == COND_NONE) {
                vec_remove_at(bb->irs, iir);
                --iir;
              }
            }
          }
          break;
        case IR_COND:
          assert(ir->cond.kind != COND_ANY && ir->cond.kind != COND_NONE);
          if (replace_const_cond(ra, ir)) {
            assert(ir->kind == IR_MOV);  // Must be replaced with MOV.
            if (replace_register(bbcon, ir->dst, ir->opr1) < ip)  // Propagate const value.
              again = true;
          }
          break;
        case IR_CAST:
          if (ir->opr1->flag & VRF_CONST) {
            assert(!(ir->dst->flag & VRF_FLONUM));
            int64_t value = ir->opr1->fixnum;
            if (ir->dst->vsize > ir->opr1->vsize)
              value = wrap_value(value, 1 << ir->opr1->vsize, ir->flag & IRF_UNSIGNED);
            // Replace to MOV.
            ir->kind = IR_MOV;
            ir->opr1 = reg_alloc_spawn_const(ra, value, ir->dst->vsize);
            if (replace_register(bbcon, ir->dst, ir->opr1) < ip)
              again = true;
          }
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
