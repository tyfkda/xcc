#include "../../config.h"
#include "ir.h"

#include <assert.h>
#include <stdlib.h>  // malloc

#include "regalloc.h"
#include "table.h"
#include "util.h"

static const enum VRegSize vtVoidPtr = VRegSize8;
static const enum VRegSize vtBool    = VRegSize4;

Phi *new_phi(VReg *dst, Vector *params) {
  Phi *phi = malloc_or_die(sizeof(*phi));
  phi->dst = dst;
  phi->params = params;
  return phi;
}

enum ConditionKind swap_cond(enum ConditionKind cond) {
  assert((cond & ~COND_MASK) == 0);
  if (cond >= COND_LT)
    cond = (COND_GT + COND_LT) - cond;
  return cond;
}

enum ConditionKind invert_cond(enum ConditionKind cond) {
  int c = cond & COND_MASK;
  assert(COND_EQ <= c && c <= COND_GT);
  int ic = c <= COND_NE ? (COND_NE + COND_EQ) - c
                        : (assert((COND_LT & 3) == 0), c ^ 2);  // COND_LT + ((c - COND_LT) ^ 2)
  return ic | (cond & ~COND_MASK);
}

// Virtual register

void spill_vreg(VReg *vreg) {
  vreg->phys = -1;  //SPILLED_REG_NO(ra);
  assert(!(vreg->flag & VRF_NO_SPILL));
  vreg->flag |= VRF_SPILLED;
}

//
RegAlloc *curra;

// Intermediate Representation

static IR *new_ir(enum IrKind kind) {
  IR *ir = malloc_or_die(sizeof(*ir));
  ir->kind = kind;
  ir->flag = 0;
  ir->dst = ir->opr1 = ir->opr2 = NULL;
  if (curbb != NULL)
    vec_push(curbb->irs, ir);
  return ir;
}

VReg *new_const_vreg(int64_t value, enum VRegSize vsize) {
  assert(curra != NULL);
  return reg_alloc_spawn_const(curra, value, vsize);
}

IR *new_ir_bop_raw(enum IrKind kind, VReg *dst, VReg *opr1, VReg *opr2, int flag) {
  IR *ir = new_ir(kind);
  ir->flag = flag;
  ir->dst = dst;
  ir->opr1 = opr1;
  ir->opr2 = opr2;
  return ir;
}

VReg *new_ir_bop(enum IrKind kind, VReg *opr1, VReg *opr2, enum VRegSize vsize, int flag) {
  if (opr1->flag & VRF_CONST) {
    if (opr2->flag & VRF_CONST) {
      int64_t value = 0;
      switch (kind) {
      case IR_ADD:     value = opr1->fixnum + opr2->fixnum; break;
      case IR_SUB:     value = opr1->fixnum - opr2->fixnum; break;
      case IR_MUL:     value = opr1->fixnum * opr2->fixnum; break;

      case IR_DIV:
      case IR_MOD:
        if (opr2->fixnum == 0)
          error("Divide by 0");
        switch (kind) {
        case IR_DIV:
          if (flag & IRF_UNSIGNED)
            value = (uint64_t)opr1->fixnum / (uint64_t)opr2->fixnum;
          else
            value = opr1->fixnum / opr2->fixnum;
          break;
        case IR_MOD:
          if (flag & IRF_UNSIGNED)
            value = (uint64_t)opr1->fixnum / (uint64_t)opr2->fixnum;
          else
            value = opr1->fixnum / opr2->fixnum;
          break;
        default: assert(false); break;
        }
        break;

      case IR_BITAND:  value = opr1->fixnum & opr2->fixnum; break;
      case IR_BITOR:   value = opr1->fixnum | opr2->fixnum; break;
      case IR_BITXOR:  value = opr1->fixnum ^ opr2->fixnum; break;
      case IR_LSHIFT:  value = opr1->fixnum << opr2->fixnum; break;
      case IR_RSHIFT:
        //assert(opr1->type->kind == TY_FIXNUM);
        if (flag & IRF_UNSIGNED)
          value = (uint64_t)opr1->fixnum >> opr2->fixnum;
        else
          value = opr1->fixnum >> opr2->fixnum;
        break;
      default: assert(false); break;
      }
      return new_const_vreg(wrap_value(value, 1 << vsize, (flag & IRF_UNSIGNED) != 0), vsize);
    } else {
      switch (kind) {
      case IR_ADD:
        if (opr1->fixnum == 0)
          return opr2;
        break;
      case IR_SUB:
        if (opr1->fixnum == 0)
          return new_ir_unary(IR_NEG, opr2, opr2->vsize, flag);
        break;
      case IR_MUL:
        if (opr1->fixnum == 1)
          return opr2;
        break;
      case IR_DIV:
      case IR_MOD:
        if (opr1->fixnum == 0)
          return opr1;  // TODO: whether opr2 is zero.
        break;
      case IR_BITAND:
        if (opr1->fixnum == 0)
          return opr1;  // 0
        break;
      case IR_BITOR:
      case IR_BITXOR:
        if (opr1->fixnum == 0)
          return opr2;
        break;
      case IR_LSHIFT:
      case IR_RSHIFT:
        if (opr1->fixnum == 0)
          return opr1;  // 0
        break;
      default:
        break;
      }
    }
  } else {
    if (opr2->flag & VRF_CONST) {
      switch (kind) {
      case IR_ADD:
      case IR_SUB:
        if (opr2->fixnum == 0)
          return opr1;
        break;
      case IR_MUL:
      case IR_DIV:
        if (opr2->fixnum == 1)
          return opr1;
        break;
      case IR_BITAND:
        if (opr2->fixnum == 0)
          return opr2;  // 0
        break;
      case IR_BITOR:
      case IR_BITXOR:
        if (opr2->fixnum == 0)
          return opr1;
        break;
      case IR_LSHIFT:
      case IR_RSHIFT:
        if (opr2->fixnum == 0)
          return opr1;
        break;
      default:
        break;
      }
    }
  }

  VReg *dst = reg_alloc_spawn(curra, vsize, opr1->flag & VRF_MASK);
  new_ir_bop_raw(kind, dst, opr1, opr2, flag);
  return dst;
}

VReg *new_ir_unary(enum IrKind kind, VReg *opr, enum VRegSize vsize, int flag) {
  assert(kind != IR_LOAD);
  if (opr->flag & VRF_CONST) {
    int64_t value = 0;
    switch (kind) {
    case IR_NEG:     value = -opr->fixnum; break;
    case IR_BITNOT:  value = ~opr->fixnum; break;
    default: assert(false); break;
    }
    return new_const_vreg(wrap_value(value, 1 << vsize, (flag & IRF_UNSIGNED) != 0), vsize);
  }

  IR *ir = new_ir(kind);
  ir->opr1 = opr;
  return ir->dst = reg_alloc_spawn(curra, vsize, opr->flag & VRF_MASK);
}

VReg *new_ir_load(VReg *opr, enum VRegSize vsize, int vflag, int irflag) {
  IR *ir = new_ir(IR_LOAD);
  ir->opr1 = opr;
  ir->flag = irflag;
  return ir->dst = reg_alloc_spawn(curra, vsize, vflag);
}

VReg *new_ir_bofs(FrameInfo *fi) {
  assert(fi != NULL);
  IR *ir = new_ir(IR_BOFS);
  ir->bofs.frameinfo = fi;
  ir->bofs.offset = 0;
  return ir->dst = reg_alloc_spawn(curra, vtVoidPtr, 0);
}

VReg *new_ir_iofs(const Name *label, bool global) {
  IR *ir = new_ir(IR_IOFS);
  ir->iofs.label = label;
  ir->iofs.global = global;
  ir->iofs.offset = 0;
  return ir->dst = reg_alloc_spawn(curra, vtVoidPtr, 0);
}

VReg *new_ir_sofs(VReg *src) {
  IR *ir = new_ir(IR_SOFS);
  ir->opr1 = src;
  return ir->dst = reg_alloc_spawn(curra, vtVoidPtr, 0);
}

void new_ir_store(VReg *dst, VReg *src, int flag) {
  IR *ir = new_ir(IR_STORE);
  ir->opr1 = src;
  ir->opr2 = dst;  // `dst` is used by indirect, so it is not actually `dst`.
  ir->flag = flag;
}

VReg *new_ir_cond(VReg *opr1, VReg *opr2, enum ConditionKind cond) {
  IR *ir = new_ir(IR_COND);
  ir->opr1 = opr1;
  ir->opr2 = opr2;
  ir->cond.kind = cond;
  return ir->dst = reg_alloc_spawn(curra, vtBool, 0);
}

IR *new_ir_jmp(BB *bb) {
  IR *ir = new_ir(IR_JMP);
  ir->jmp.bb = bb;
  ir->jmp.cond = COND_ANY;
  return ir;
}

void new_ir_cjmp(VReg *opr1, VReg *opr2, enum ConditionKind cond, BB *bb) {
  if ((cond & COND_MASK) == COND_NONE)
    return;
  IR *ir = new_ir(IR_JMP);
  ir->opr1 = opr1;
  ir->opr2 = opr2;
  ir->jmp.bb = bb;
  ir->jmp.cond = cond;
}

void new_ir_tjmp(VReg *val, BB **bbs, size_t len) {
  assert(len >= 1);
  IR *ir = new_ir(IR_TJMP);
  ir->opr1 = val;
  ir->tjmp.bbs = bbs;
  ir->tjmp.len = len;
}

IR *new_ir_pusharg(VReg *vreg, int index) {
  assert(index >= 0);
  IR *ir = new_ir(IR_PUSHARG);
  ir->opr1 = vreg;
  ir->pusharg.index = index;
#if VAARG_FP_AS_GP
  ir->pusharg.fp_as_gp = false;
#endif
  return ir;
}

IR *new_ir_precall(int arg_count, int stack_args_size) {
  IR *ir = new_ir(IR_PRECALL);
  ir->precall.arg_count = arg_count;
  ir->precall.stack_args_size = stack_args_size;
  ir->precall.stack_aligned = 0;
  ir->precall.living_pregs = 0;
  ir->precall.caller_saves = NULL;
  return ir;
}

VReg *new_ir_call(const Name *label, bool global, VReg *freg, int total_arg_count,
                  int reg_arg_count, enum VRegSize result_size, int result_flag, IR *precall,
                  VReg **args, int vaarg_start) {
  IR *ir = new_ir(IR_CALL);
  ir->call.label = label;
  ir->call.global = global;
  ir->opr1 = freg;
  ir->call.precall = precall;
  ir->call.args = args;
  ir->call.total_arg_count = total_arg_count;
  ir->call.reg_arg_count = reg_arg_count;
  ir->call.vaarg_start = vaarg_start;
  return ir->dst = result_size < 0 ? NULL : reg_alloc_spawn(curra, result_size, result_flag);
}

void new_ir_result(VReg *dst, VReg *vreg, int flag) {
  IR *ir = new_ir(IR_RESULT);
  ir->dst = dst;
  ir->opr1 = vreg;
  ir->flag = flag;
}

void new_ir_subsp(VReg *value, VReg *dst) {
  IR *ir = new_ir(IR_SUBSP);
  ir->opr1 = value;
  ir->dst = dst;
}

IR *new_ir_cast(VReg *vreg, enum VRegSize dstsize, int vflag) {
  IR *ir = new_ir(IR_CAST);
  ir->opr1 = vreg;
  ir->dst = reg_alloc_spawn(curra, dstsize, vflag);
  return ir;
}

IR *new_ir_mov(VReg *dst, VReg *src, int flag) {
  IR *ir = new_ir(IR_MOV);
  ir->dst = dst;
  ir->opr1 = src;
  ir->flag = flag;
  return ir;
}

IR *new_ir_keep(VReg *dst, VReg *opr1, VReg *opr2) {
  IR *ir = new_ir(IR_KEEP);
  ir->dst = dst;
  ir->opr1 = opr1;
  ir->opr2 = opr2;
  return ir;
}

void new_ir_asm(const char *asm_, VReg *dst) {
  IR *ir = new_ir(IR_ASM);
  ir->asm_.str = asm_;
  ir->dst = dst;
}

IR *new_ir_load_spilled(VReg *vreg, VReg *src, int flag) {
  IR *ir = new_ir(IR_LOAD_S);
  ir->dst = vreg;
  ir->opr1 = src;
  ir->flag = flag;
  return ir;
}

IR *new_ir_store_spilled(VReg *dst, VReg *vreg) {
  IR *ir = new_ir(IR_STORE_S);
  ir->opr1 = vreg;
  ir->opr2 = dst;  // `dst` is used by indirect, so it is not actually `dst`.
  return ir;
}

// Basic Block

BB *curbb;

BB *new_bb(void) {
  BB *bb = malloc_or_die(sizeof(*bb));
  bb->next = NULL;
  bb->from_bbs = new_vector();
  bb->label = alloc_label();
  bb->irs = new_vector();
  bb->in_regs = new_vector();
  bb->out_regs = new_vector();
  bb->assigned_regs = new_vector();
  bb->phis = NULL;
  return bb;
}

BBContainer *new_func_blocks(void) {
  return new_vector();
}

//

void detect_from_bbs(BBContainer *bbcon) {
  int count = bbcon->len;
  if (count <= 0)
    return;

  // Clear all from_bbs
  for (int i = 0; i < count; ++i) {
    BB *bb = bbcon->data[i];
    vec_clear(bb->from_bbs);
  }

  Table checked;
  table_init(&checked);
  Vector unchecked;
  vec_init(&unchecked);
  vec_push(&unchecked, bbcon->data[0]);

  do {
    BB *bb = vec_pop(&unchecked);
    if (table_try_get(&checked, bb->label, NULL))
      continue;
    table_put(&checked, bb->label, bb);

    Vector *irs = bb->irs;
    if (irs->len > 0) {
      IR *ir = irs->data[irs->len - 1];  // JMP must be the last IR.
      switch (ir->kind) {
      case IR_JMP:
        {
          BB *dst = ir->jmp.bb;
          vec_push(dst->from_bbs, bb);
          vec_push(&unchecked, dst);
          if (ir->jmp.cond == COND_ANY)
            continue;  // Next BB is not reachable.
        }
        break;
      case IR_TJMP:
        for (size_t j = 0; j < ir->tjmp.len; ++j) {
          BB *nbb = ir->tjmp.bbs[j];
          vec_push(nbb->from_bbs, bb);
          vec_push(&unchecked, nbb);
        }
        continue;
      default: break;
      }
    }
    BB *next = bb->next;
    if (next != NULL) {
      vec_push(next->from_bbs, bb);
      vec_push(&unchecked, next);
    }
  } while (unchecked.len > 0);
}

static bool insert_vreg_into_vec(Vector *vregs, VReg *vreg) {
  int lo = -1, hi = vregs->len;
  while (hi - lo > 1) {
    int m = lo + (hi - lo) / 2;
    VReg *mid = vregs->data[m];
    if      (mid->virt < vreg->virt)  lo = m;
    else if (mid->virt > vreg->virt)  hi = m;
    else                              return false;
  }
  assert(0 <= hi && hi <= vregs->len);
  assert(hi == vregs->len || ((VReg*)vregs->data[hi])->virt != vreg->virt);
  vec_insert(vregs, hi, vreg);
  return true;
}

static void propagate_out_regs(VReg *vreg, Vector *froms) {
  for (BB *bb; (bb = vec_pop(froms)) != NULL; ) {
    insert_vreg_into_vec(bb->out_regs, vreg);
    if (vec_contains(bb->assigned_regs, vreg) ||
        !insert_vreg_into_vec(bb->in_regs, vreg))
      continue;
    vec_concat(froms, bb->from_bbs);
  }
}

void analyze_reg_flow(BBContainer *bbcon) {
  // Enumerate in and assigned regsiters for each BB.
  for (int i = 0; i < bbcon->len; ++i) {
    BB *bb = bbcon->data[i];
    Vector *in_regs = bb->in_regs;
    Vector *assigned_regs = bb->assigned_regs;
    vec_clear(in_regs);
    vec_clear(assigned_regs);
    vec_clear(bb->out_regs);

    Vector *phis = bb->phis;
    if (phis != NULL) {
      for (int j = 0; j < phis->len; ++j) {
        Phi *phi = phis->data[j];
        for (int k = 0; k < phi->params->len; ++k) {
          VReg *vreg = phi->params->data[k];
          assert(vreg != NULL);
          if (vreg->flag & VRF_CONST)
            continue;
          assert(!vec_contains(assigned_regs, vreg));
          insert_vreg_into_vec(in_regs, vreg);
        }
        insert_vreg_into_vec(assigned_regs, phi->dst);
      }
    }

    Vector *irs = bb->irs;
    for (int j = 0; j < irs->len; ++j) {
      IR *ir = irs->data[j];
      VReg *vregs[] = {ir->opr1, ir->opr2};
      for (int k = 0; k < 2; ++k) {
        VReg *vreg = vregs[k];
        if (vreg == NULL || vreg->flag & VRF_CONST)
          continue;
        if (!vec_contains(assigned_regs, vreg))
          insert_vreg_into_vec(in_regs, vreg);
      }
      if (ir->dst != NULL)
        insert_vreg_into_vec(assigned_regs, ir->dst);
    }
  }

  // Propagate in_regs to out_regs to from_bbs recursively.
  Vector *dstbbs = new_vector();
  for (int i = bbcon->len; --i >= 0; ) {
    BB *bb = bbcon->data[i];
    Vector *from_bbs = bb->from_bbs;
    Vector *in_regs = bb->in_regs;
    for (int j = 0; j < in_regs->len; ++j) {
      assert(dstbbs->len == 0);
      vec_concat(dstbbs, from_bbs);
      VReg *vreg = in_regs->data[j];
      propagate_out_regs(vreg, dstbbs);
    }
  }
}
