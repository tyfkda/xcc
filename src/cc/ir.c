#include "ir.h"

#include <assert.h>
#include <stdlib.h>  // malloc

#include "regalloc.h"
#include "table.h"
#include "util.h"

#define WORK_REG_NO  (PHYSICAL_REG_MAX)

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

//
RegAlloc *curra;

// Intermediate Representation

static IR *new_ir(enum IrKind kind) {
  IR *ir = malloc(sizeof(*ir));
  ir->kind = kind;
  ir->dst = ir->opr1 = ir->opr2 = NULL;
  ir->size = -1;
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
      case IR_DIVU:
      case IR_MOD:
      case IR_MODU:
        if (opr2->fixnum == 0)
          error("Divide by 0");
        switch (kind) {
        case IR_DIV:  value = opr1->fixnum / opr2->fixnum; break;
        case IR_DIVU: value = (uintptr_t)opr1->fixnum / opr2->fixnum; break;
        case IR_MOD:  value = opr1->fixnum / opr2->fixnum; break;
        case IR_MODU: value = (uintptr_t)opr1->fixnum / opr2->fixnum; break;
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
      case IR_DIVU:
      case IR_MOD:
      case IR_MODU:
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
      case IR_DIVU:
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
  ir->size = vtype->size;
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
  ir->size = vtype->size;
  return ir->dst = reg_alloc_spawn(curra, vtype, 0);
}

VReg *new_ir_bofs(VReg *src) {
  IR *ir = new_ir(IR_BOFS);
  ir->opr1 = src;
  ir->size = WORD_SIZE;
  return ir->dst = reg_alloc_spawn(curra, &vtVoidPtr, 0);
}

VReg *new_ir_iofs(const Name *label, bool global) {
  IR *ir = new_ir(IR_IOFS);
  ir->iofs.label = label;
  ir->iofs.global = global;
  ir->size = WORD_SIZE;
  return ir->dst = reg_alloc_spawn(curra, &vtVoidPtr, 0);
}

VReg *new_ir_sofs(VReg *src) {
  IR *ir = new_ir(IR_SOFS);
  ir->opr1 = src;
  ir->size = WORD_SIZE;
  return ir->dst = reg_alloc_spawn(curra, &vtVoidPtr, 0);
}

void new_ir_store(VReg *dst, VReg *src) {
  IR *ir = new_ir(IR_STORE);
  ir->opr1 = src;
  ir->size = src->vtype->size;
  ir->opr2 = dst;  // `dst` is used by indirect, so it is not actually `dst`.
}

void new_ir_cmp(VReg *opr1, VReg *opr2) {
  IR *ir = new_ir(IR_CMP);
  ir->opr1 = opr1;
  ir->opr2 = opr2;
  ir->size = opr1->vtype->size;
}

VReg *new_ir_cond(enum ConditionKind cond) {
  IR *ir = new_ir(IR_COND);
  ir->cond.kind = cond;
  return ir->dst = reg_alloc_spawn(curra, &vtBool, 0);
}

void new_ir_jmp(enum ConditionKind cond, BB *bb) {
  if (cond == COND_NONE)
    return;
  IR *ir = new_ir(IR_JMP);
  ir->jmp.bb = bb;
  ir->jmp.cond = cond;
}

void new_ir_pusharg(VReg *vreg, const VRegType *vtype) {
  IR *ir = new_ir(IR_PUSHARG);
  ir->opr1 = vreg;
  ir->size = vtype->size;
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
                  const VRegType *result_type, IR *precall, VRegType **arg_vtypes, bool vaargs) {
  IR *ir = new_ir(IR_CALL);
  ir->call.label = label;
  ir->call.global = global;
  ir->opr1 = freg;
  ir->call.precall = precall;
  ir->call.arg_vtypes = arg_vtypes;
  ir->call.total_arg_count = total_arg_count;
  ir->call.reg_arg_count = reg_arg_count;
  ir->call.vaargs = vaargs;
  ir->size = result_type->size;
  return ir->dst = reg_alloc_spawn(curra, result_type, 0);
}

void new_ir_result(VReg *reg) {
  IR *ir = new_ir(IR_RESULT);
  ir->opr1 = reg;
  ir->size = reg->vtype->size;
}

void new_ir_subsp(VReg *value, VReg *dst) {
  IR *ir = new_ir(IR_SUBSP);
  ir->opr1 = value;
  ir->dst = dst;
  ir->size = WORD_SIZE;
}

VReg *new_ir_cast(VReg *vreg, const VRegType *dsttype) {
  IR *ir = new_ir(IR_CAST);
  ir->opr1 = vreg;
  ir->size = dsttype->size;
  return ir->dst = reg_alloc_spawn(curra, dsttype, 0);
}

void new_ir_mov(VReg *dst, VReg *src) {
  IR *ir = new_ir(IR_MOV);
  ir->dst = dst;
  ir->opr1 = src;
  ir->size = dst->vtype->size;
}

void new_ir_memcpy(VReg *dst, VReg *src, int size) {
  if (size > 0) {
    IR *ir = new_ir(IR_MEMCPY);
    ir->opr1 = src;
    ir->opr2 = dst;
    ir->size = size;
  }
}

void new_ir_clear(VReg *reg, size_t size) {
  IR *ir = new_ir(IR_CLEAR);
  ir->size = size;
  ir->opr1 = reg;
}

void new_ir_asm(const char *asm_) {
  IR *ir = new_ir(IR_ASM);
  ir->asm_.str = asm_;
}

IR *new_ir_load_spilled(VReg *reg, int offset, int size, int flag) {
  IR *ir = new_ir(IR_LOAD_SPILLED);
  ir->value = offset;
  ir->size = size;
  ir->dst = reg;
  ir->spill.flag = flag;
  return ir;
}

IR *new_ir_store_spilled(VReg *reg, int offset, int size, int flag) {
  IR *ir = new_ir(IR_STORE_SPILLED);
  ir->value = offset;
  ir->size = size;
  ir->opr1 = reg;
  ir->spill.flag = flag;
  return ir;
}

// Basic Block

BB *curbb;

BB *new_bb(void) {
  BB *bb = malloc(sizeof(*bb));
  bb->next = NULL;
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
