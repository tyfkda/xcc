#include "../../config.h"
#include "ir.h"

#include <assert.h>
#include <stdlib.h>  // malloc

#include "regalloc.h"
#include "table.h"
#include "util.h"

static VRegType vtVoidPtr = {.size = WORD_SIZE, .align = WORD_SIZE, .flag = VRTF_UNSIGNED};
static VRegType vtBool    = {.size = 4, .align = 4, .flag = 0};

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
  ir->dst = ir->opr1 = ir->opr2 = NULL;
  ir->value = 0;
  if (curbb != NULL)
    vec_push(curbb->irs, ir);
  return ir;
}

VReg *new_const_vreg(int64_t value, VRegType vtype) {
  VReg *vreg = reg_alloc_spawn(curra, vtype, VRF_CONST);
  vreg->fixnum = value;
  return vreg;
}

VReg *new_ir_bop(enum IrKind kind, VReg *opr1, VReg *opr2, VRegType vtype) {
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
          if (vtype.flag & VRTF_UNSIGNED)
            value = (uint64_t)opr1->fixnum / opr2->fixnum;
          else
            value = opr1->fixnum / opr2->fixnum;
          break;
        case IR_MOD:
          if (vtype.flag & VRTF_UNSIGNED)
            value = opr1->fixnum / opr2->fixnum;
          else
            value = (uint64_t)opr1->fixnum / opr2->fixnum;
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
        if (opr1->vtype.flag & VRTF_UNSIGNED)
          value = (uint64_t)opr1->fixnum >> opr2->fixnum;
        else
          value = opr1->fixnum >> opr2->fixnum;
        break;
      default: assert(false); break;
      }
      return new_const_vreg(wrap_value(value, vtype.size, (vtype.flag & VRTF_UNSIGNED) != 0), vtype);
    } else {
      switch (kind) {
      case IR_ADD:
        if (opr1->fixnum == 0)
          return opr2;
        break;
      case IR_SUB:
        if (opr1->fixnum == 0)
          return new_ir_unary(IR_NEG, opr2, opr2->vtype);
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

  IR *ir = new_ir(kind);
  ir->opr1 = opr1;
  ir->opr2 = opr2;
  return ir->dst = reg_alloc_spawn(curra, vtype, 0);
}

VReg *new_ir_unary(enum IrKind kind, VReg *opr, VRegType vtype) {
  if (opr->flag & VRF_CONST && kind != IR_LOAD) {
    int64_t value = 0;
    switch (kind) {
    case IR_NEG:     value = -opr->fixnum; break;
    case IR_BITNOT:  value = ~opr->fixnum; break;
    default: assert(false); break;
    }
    return new_const_vreg(wrap_value(value, vtype.size, (vtype.flag & VRTF_UNSIGNED) != 0), vtype);
  }

  IR *ir = new_ir(kind);
  ir->opr1 = opr;
  return ir->dst = reg_alloc_spawn(curra, vtype, 0);
}

VReg *new_ir_bofs(FrameInfo *fi, VReg *src) {
  IR *ir = new_ir(IR_BOFS);
  ir->bofs.frameinfo = fi;
  ir->opr1 = src;  // Just keep the vreg is referred.
  return ir->dst = reg_alloc_spawn(curra, vtVoidPtr, 0);
}

VReg *new_ir_iofs(const Name *label, bool global) {
  IR *ir = new_ir(IR_IOFS);
  ir->iofs.label = label;
  ir->iofs.global = global;
  return ir->dst = reg_alloc_spawn(curra, vtVoidPtr, 0);
}

VReg *new_ir_sofs(VReg *src) {
  IR *ir = new_ir(IR_SOFS);
  ir->opr1 = src;
  return ir->dst = reg_alloc_spawn(curra, vtVoidPtr, 0);
}

void new_ir_store(VReg *dst, VReg *src) {
  IR *ir = new_ir(IR_STORE);
  ir->opr1 = src;
  ir->opr2 = dst;  // `dst` is used by indirect, so it is not actually `dst`.
}

void new_ir_cmp(VReg *opr1, VReg *opr2) {
  IR *ir = new_ir(IR_CMP);
  ir->opr1 = opr1;
  ir->opr2 = opr2;
}

VReg *new_ir_cond(enum ConditionKind cond) {
  IR *ir = new_ir(IR_COND);
  ir->cond.kind = cond;
  return ir->dst = reg_alloc_spawn(curra, vtBool, 0);
}

void new_ir_jmp(enum ConditionKind cond, BB *bb) {
  if ((cond & COND_MASK) == COND_NONE)
    return;
  IR *ir = new_ir(IR_JMP);
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

void new_ir_pusharg(VReg *vreg, int index) {
  assert(index >= 0);
  assert(index < ((vreg->vtype.flag & VRTF_FLONUM) ? MAX_FREG_ARGS : MAX_REG_ARGS));
  IR *ir = new_ir(IR_PUSHARG);
  ir->opr1 = vreg;
  ir->pusharg.index = index;
}

IR *new_ir_precall(int arg_count, int stack_args_size) {
  IR *ir = new_ir(IR_PRECALL);
  ir->precall.arg_count = arg_count;
  ir->precall.stack_args_size = stack_args_size;
  ir->precall.stack_aligned = false;
  ir->precall.living_pregs = 0;
  ir->precall.caller_saves = NULL;
  return ir;
}

VReg *new_ir_call(const Name *label, bool global, VReg *freg, int total_arg_count, int reg_arg_count,
                  const VRegType *result_type, IR *precall, VReg **args, int vaarg_start) {
  IR *ir = new_ir(IR_CALL);
  ir->call.label = label;
  ir->call.global = global;
  ir->opr1 = freg;
  ir->call.precall = precall;
  ir->call.args = args;
  ir->call.total_arg_count = total_arg_count;
  ir->call.reg_arg_count = reg_arg_count;
  ir->call.vaarg_start = vaarg_start;
  return ir->dst = result_type == NULL ? NULL : reg_alloc_spawn(curra, *result_type, 0);
}

void new_ir_result(VReg *vreg) {
  IR *ir = new_ir(IR_RESULT);
  ir->opr1 = vreg;
}

void new_ir_subsp(VReg *value, VReg *dst) {
  IR *ir = new_ir(IR_SUBSP);
  ir->opr1 = value;
  ir->dst = dst;
}

VReg *new_ir_cast(VReg *vreg, VRegType dsttype) {
  IR *ir = new_ir(IR_CAST);
  ir->opr1 = vreg;
  return ir->dst = reg_alloc_spawn(curra, dsttype, 0);
}

IR *new_ir_mov(VReg *dst, VReg *src) {
  IR *ir = new_ir(IR_MOV);
  ir->dst = dst;
  ir->opr1 = src;
  return ir;
}

void new_ir_asm(const char *asm_, VReg *dst) {
  IR *ir = new_ir(IR_ASM);
  ir->asm_.str = asm_;
  ir->dst = dst;
}

IR *new_ir_load_spilled(VReg *vreg, VReg *src) {
  IR *ir = new_ir(IR_LOAD_S);
  ir->dst = vreg;
  ir->opr1 = src;
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
  bb->in_regs = NULL;
  bb->out_regs = NULL;
  bb->assigned_regs = NULL;
  return bb;
}

BBContainer *new_func_blocks(void) {
  BBContainer *bbcon = malloc_or_die(sizeof(*bbcon));
  bbcon->bbs = new_vector();
  return bbcon;
}

//

static void detect_from_bbs(BBContainer *bbcon) {
  // Clear all from_bbs
  for (int i = 0; i < bbcon->bbs->len; ++i) {
    BB *bb = bbcon->bbs->data[i];
    vec_clear(bb->from_bbs);
  }

  for (int i = 0; i < bbcon->bbs->len; ++i) {
    BB *bb = bbcon->bbs->data[i];
    Vector *irs = bb->irs;
    if (irs->len == 0)
      continue;
    IR *ir = irs->data[irs->len - 1];
    switch (ir->kind) {
    case IR_JMP:
      vec_push(ir->jmp.bb->from_bbs, bb);
      if (ir->jmp.cond == COND_ANY)
        continue;
      break;
    case IR_TJMP:
      for (size_t j = 0; j < ir->tjmp.len; ++j) {
        BB *nbb = ir->tjmp.bbs[j];
        vec_push(nbb->from_bbs, bb);
      }
      continue;
    default: break;
    }
    if (bb->next != NULL)
      vec_push(bb->next->from_bbs, bb);
  }
}

static void propagate_out_regs(VReg *vreg, Vector *froms) {
  for (BB *bb; (bb = vec_pop(froms)) != NULL; ) {
    if (!vec_contains(bb->out_regs, vreg))
      vec_push(bb->out_regs, vreg);
    if (!vec_contains(bb->in_regs, vreg) &&
        !vec_contains(bb->assigned_regs, vreg)) {
      vec_push(bb->in_regs, vreg);
      for (int i = 0; i < bb->from_bbs->len; ++i)
        vec_push(froms, bb->from_bbs->data[i]);
    }
  }
}

void analyze_reg_flow(BBContainer *bbcon) {
  detect_from_bbs(bbcon);

  // Enumerate in and assigned regsiters for each BB.
  for (int i = 0; i < bbcon->bbs->len; ++i) {
    BB *bb = bbcon->bbs->data[i];
    Vector *in_regs = new_vector();
    Vector *assigned_regs = new_vector();
    Vector *irs = bb->irs;
    for (int j = 0; j < irs->len; ++j) {
      IR *ir = irs->data[j];
      VReg *vregs[] = {ir->opr1, ir->opr2};
      for (int k = 0; k < 2; ++k) {
        VReg *vreg = vregs[k];
        if (vreg == NULL || vreg->flag & VRF_CONST)
          continue;
        if (!vec_contains(in_regs, vreg) &&
            !vec_contains(assigned_regs, vreg))
          vec_push(in_regs, vreg);
      }
      if (ir->dst != NULL && !vec_contains(assigned_regs, ir->dst))
        vec_push(assigned_regs, ir->dst);
    }

    bb->in_regs = in_regs;
    bb->out_regs = new_vector();
    bb->assigned_regs = assigned_regs;
  }

  // Propagate in_regs to out_regs to from_bbs recursively.
  Vector *dstbbs = new_vector();
  for (int i = bbcon->bbs->len; --i >= 0; ) {
    BB *bb = bbcon->bbs->data[i];
    Vector *from_bbs = bb->from_bbs;
    Vector *in_regs = bb->in_regs;
    for (int j = 0; j < in_regs->len; ++j) {
      assert(dstbbs->len == 0);
      for (int i = 0; i < from_bbs->len; ++i)
        vec_push(dstbbs, from_bbs->data[i]);
      VReg *vreg = in_regs->data[j];
      propagate_out_regs(vreg, dstbbs);
    }
  }
}
