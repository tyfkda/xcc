#include "../config.h"
#include "ir.h"

#include <assert.h>
#include <stdlib.h>  // malloc

#include "regalloc.h"
#include "table.h"
#include "util.h"

static VRegType vtVoidPtr = {.size = WORD_SIZE, .align = WORD_SIZE, .flag = 0};
static VRegType vtBool    = {.size = 4, .align = 4, .flag = 0};

// Virtual register

VReg *new_vreg(int vreg_no, const VRegType *vtype, int flag) {
  VReg *vreg = malloc(sizeof(*vreg));
  vreg->virt = vreg_no;
  vreg->phys = -1;
  vreg->fixnum = 0;
  vreg->vtype = vtype;
  vreg->flag = flag;
  vreg->param_index = -1;
  vreg->offset = 0;
  return vreg;
}

void spill_vreg(VReg *vreg) {
  vreg->phys = -1;  //SPILLED_REG_NO(ra);
  assert(!(vreg->flag & VRF_NO_SPILL));
  vreg->flag |= VRF_SPILLED;
}

//
RegAlloc *curra;

// Intermediate Representation

static IR *new_ir(enum IrKind kind) {
  IR *ir = malloc(sizeof(*ir));
  ir->kind = kind;
  ir->dst = ir->opr1 = ir->opr2 = NULL;
  ir->value = 0;
  if (curbb != NULL)
    vec_push(curbb->irs, ir);
  return ir;
}

VReg *new_const_vreg(intptr_t value, const VRegType *vtype) {
  VReg *vreg = reg_alloc_spawn(curra, vtype, VRF_CONST);
  vreg->fixnum = value;
  return vreg;
}

static intptr_t clamp_value(intptr_t value, const VRegType *vtype) {
  if (vtype->flag & VRTF_UNSIGNED) {
    switch (vtype->size) {
    case 1:  value = (unsigned char)value; break;
    case 2:  value = (unsigned short)value; break;
    case 4:  value = (unsigned int)value; break;
    default:  break;
    }
  } else {
    switch (vtype->size) {
    case 1:  value = (char)value; break;
    case 2:  value = (short)value; break;
    case 4:  value = (int)value; break;
    default:  break;
    }
  }
  return value;
}

VReg *new_ir_bop(enum IrKind kind, VReg *opr1, VReg *opr2, const VRegType *vtype) {
  if (opr1->flag & VRF_CONST) {
    if (opr2->flag & VRF_CONST) {
      intptr_t value = 0;
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
          if (vtype->flag & VRTF_UNSIGNED)
            value = (uintptr_t)opr1->fixnum / opr2->fixnum;
          else
            value = opr1->fixnum / opr2->fixnum;
          break;
        case IR_MOD:
          if (vtype->flag & VRTF_UNSIGNED)
            value = opr1->fixnum / opr2->fixnum;
          else
            value = (uintptr_t)opr1->fixnum / opr2->fixnum;
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
        if (opr1->vtype->flag & VRTF_UNSIGNED)
          value = (uintptr_t)opr1->fixnum >> opr2->fixnum;
        else
          value = opr1->fixnum >> opr2->fixnum;
        break;
      default: assert(false); break;
      }
      return new_const_vreg(clamp_value(value, vtype), vtype);
    } else {
      switch (kind) {
      case IR_ADD:
      case IR_SUB:
        if (opr1->fixnum == 0)
          return opr2;
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
          return opr1;
        break;
      case IR_BITOR:
        if (opr1->fixnum == 0)
          return opr2;
        break;
      case IR_BITXOR:
        if (opr1->fixnum == 0)
          return opr2;
        break;
      case IR_LSHIFT:
      case IR_RSHIFT:
        if (opr1->fixnum == 0)
          return opr1;
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
        if (opr2->fixnum == 0)
          error("Divide by 0");
        if (opr2->fixnum == 1)
          return opr1;
        break;
      case IR_BITAND:
        if (opr2->fixnum == 0)
          return opr2;
        break;
      case IR_BITOR:
        if (opr2->fixnum == 0)
          return opr1;
        break;
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

VReg *new_ir_unary(enum IrKind kind, VReg *opr, const VRegType *vtype) {
  if (opr->flag & VRF_CONST && kind != IR_LOAD) {
    intptr_t value = 0;
    switch (kind) {
    case IR_NEG:     value = -opr->fixnum; break;
    case IR_BITNOT:  value = ~opr->fixnum; break;
    default: assert(false); break;
    }
    return new_const_vreg(clamp_value(value, vtype), vtype);
  }

  IR *ir = new_ir(kind);
  ir->opr1 = opr;
  return ir->dst = reg_alloc_spawn(curra, vtype, 0);
}

VReg *new_ir_bofs(VReg *src) {
  IR *ir = new_ir(IR_BOFS);
  ir->opr1 = src;
  return ir->dst = reg_alloc_spawn(curra, &vtVoidPtr, 0);
}

VReg *new_ir_iofs(const Name *label, bool global) {
  IR *ir = new_ir(IR_IOFS);
  ir->iofs.label = label;
  ir->iofs.global = global;
  return ir->dst = reg_alloc_spawn(curra, &vtVoidPtr, 0);
}

VReg *new_ir_sofs(VReg *src) {
  IR *ir = new_ir(IR_SOFS);
  ir->opr1 = src;
  return ir->dst = reg_alloc_spawn(curra, &vtVoidPtr, 0);
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
  return ir->dst = reg_alloc_spawn(curra, &vtBool, 0);
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

void new_ir_pusharg(VReg *vreg) {
  IR *ir = new_ir(IR_PUSHARG);
  ir->opr1 = vreg;
}

IR *new_ir_precall(int arg_count, int stack_args_size) {
  IR *ir = new_ir(IR_PRECALL);
  ir->precall.arg_count = arg_count;
  ir->precall.stack_args_size = stack_args_size;
  ir->precall.stack_aligned = false;
  ir->precall.living_pregs = 0;
  return ir;
}

VReg *new_ir_call(const Name *label, bool global, VReg *freg, int total_arg_count, int reg_arg_count,
                  const VRegType *result_type, IR *precall, VRegType **arg_vtypes, int vaarg_start) {
  IR *ir = new_ir(IR_CALL);
  ir->call.label = label;
  ir->call.global = global;
  ir->opr1 = freg;
  ir->call.precall = precall;
  ir->call.arg_vtypes = arg_vtypes;
  ir->call.total_arg_count = total_arg_count;
  ir->call.reg_arg_count = reg_arg_count;
  ir->call.vaarg_start = vaarg_start;
  return ir->dst = result_type == NULL ? NULL : reg_alloc_spawn(curra, result_type, 0);
}

void new_ir_result(VReg *reg) {
  IR *ir = new_ir(IR_RESULT);
  ir->opr1 = reg;
}

void new_ir_subsp(VReg *value, VReg *dst) {
  IR *ir = new_ir(IR_SUBSP);
  ir->opr1 = value;
  ir->dst = dst;
}

VReg *new_ir_cast(VReg *vreg, const VRegType *dsttype) {
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

void new_ir_memcpy(VReg *dst, VReg *src, size_t size) {
  if (size > 0) {
    IR *ir = new_ir(IR_MEMCPY);
    ir->opr1 = src;
    ir->opr2 = dst;
    ir->memcpy.size = size;
  }
}

void new_ir_clear(VReg *reg, size_t size) {
  IR *ir = new_ir(IR_CLEAR);
  ir->opr1 = reg;
  ir->clear.size = size;
}

void new_ir_asm(const char *asm_, VReg *dst) {
  IR *ir = new_ir(IR_ASM);
  ir->asm_.str = asm_;
  ir->dst = dst;
}

IR *new_ir_load_spilled(VReg *reg, VReg *src) {
  IR *ir = new_ir(IR_LOAD_SPILLED);
  ir->dst = reg;
  ir->opr1 = src;
  return ir;
}

IR *new_ir_store_spilled(VReg *dst, VReg *reg) {
  IR *ir = new_ir(IR_STORE_SPILLED);
  ir->opr1 = reg;
  ir->opr2 = dst;  // `dst` is used by indirect, so it is not actually `dst`.
  return ir;
}

// Basic Block

BB *curbb;

BB *new_bb(void) {
  BB *bb = malloc(sizeof(*bb));
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
  BBContainer *bbcon = malloc(sizeof(*bbcon));
  bbcon->bbs = new_vector();
  return bbcon;
}

//

static enum ConditionKind invert_cond(enum ConditionKind cond) {
  int c = cond & COND_MASK;
  assert(COND_EQ <= c && c <= COND_GT);
  int ic = c <= COND_NE ? (COND_NE + COND_EQ) - c
                        : (assert((COND_LT & 3) == 0), COND_LT ^ 2);  // COND_LT + ((c - COND_LT) ^ 2)
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

void remove_unnecessary_bb(BBContainer *bbcon) {
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

void detect_from_bbs(BBContainer *bbcon) {
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

static void propagate_out_regs(VReg *reg, Vector *froms) {
  for (BB *bb; (bb = vec_pop(froms)) != NULL; ) {
    if (!vec_contains(bb->out_regs, reg))
      vec_push(bb->out_regs, reg);
    if (!vec_contains(bb->in_regs, reg) &&
        !vec_contains(bb->assigned_regs, reg)) {
      vec_push(bb->in_regs, reg);
      for (int i = 0; i < bb->from_bbs->len; ++i)
        vec_push(froms, bb->from_bbs->data[i]);
    }
  }
}

void analyze_reg_flow(BBContainer *bbcon) {
  // Enumerate in and assigned regsiters for each BB.
  for (int i = 0; i < bbcon->bbs->len; ++i) {
    BB *bb = bbcon->bbs->data[i];
    Vector *in_regs = new_vector();
    Vector *assigned_regs = new_vector();
    Vector *irs = bb->irs;
    for (int j = 0; j < irs->len; ++j) {
      IR *ir = irs->data[j];
      VReg *regs[] = {ir->opr1, ir->opr2};
      for (int k = 0; k < 2; ++k) {
        VReg *reg = regs[k];
        if (reg == NULL || reg->flag & VRF_CONST)
          continue;
        if (!vec_contains(in_regs, reg) &&
            !vec_contains(assigned_regs, reg))
          vec_push(in_regs, reg);
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
