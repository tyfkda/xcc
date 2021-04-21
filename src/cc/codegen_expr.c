#include "codegen.h"

#include <assert.h>
#include <limits.h>  // CHAR_BIT
#include <stdlib.h>  // malloc

#include "ast.h"
#include "ir.h"
#include "lexer.h"
#include "regalloc.h"
#include "type.h"
#include "util.h"
#include "var.h"

#include "parser.h"  // curfunc

VRegType *to_vtype(const Type *type) {
  VRegType *vtype = malloc(sizeof(*vtype));
  vtype->size = type_size(type);
  vtype->align = align_size(type);

  int flag = 0;
  bool is_unsigned = is_fixnum(type->kind) ? type->fixnum.is_unsigned : true;
#ifndef __NO_FLONUM
  if (is_flonum(type)) {
    flag |= VRTF_FLONUM;
    is_unsigned = false;
  }
#endif
  if (is_unsigned)
    flag |= VRTF_UNSIGNED;
  vtype->flag = flag;

  return vtype;
}

VReg *add_new_reg(const Type *type, int flag) {
  return reg_alloc_spawn(curfunc->ra, to_vtype(type), flag);
}

static enum ConditionKind swap_cond(enum ConditionKind cond) {
  assert(COND_EQ <= cond && cond <= COND_GT);
  if (cond >= COND_LT)
    cond = COND_GT - (cond - COND_LT);
  return cond;
}

static enum ConditionKind gen_compare_expr(enum ExprKind kind, Expr *lhs, Expr *rhs) {
  assert(lhs->type->kind == rhs->type->kind);

  enum ConditionKind cond = kind + (COND_EQ - EX_EQ);
  assert(cond >= COND_EQ && cond < COND_ULT);
  if (is_const(lhs)) {
    assert(!is_const(rhs));
    Expr *tmp = lhs;
    lhs = rhs;
    rhs = tmp;
    cond = swap_cond(cond);
  }

  if (cond > COND_NE &&
      ((is_fixnum(lhs->type->kind) && lhs->type->fixnum.is_unsigned) ||
#ifndef __NO_FLONUM
        is_flonum(lhs->type) ||
#endif
       lhs->type->kind == TY_PTR)) {
    // unsigned
    cond += COND_ULT - COND_LT;
  }

  VReg *lhs_reg = gen_expr(lhs);
  if (rhs->kind == EX_FIXNUM && rhs->fixnum == 0 &&
      (cond == COND_EQ || cond == COND_NE)) {
    new_ir_test(lhs_reg);
  } else if (rhs->kind == EX_FIXNUM &&
             ((is_fixnum(lhs->type->kind) && lhs->type->fixnum.kind < FX_LONG) ||
               is_im32(rhs->fixnum))) {
    VReg *num = new_const_vreg(rhs->fixnum, to_vtype(rhs->type));
    new_ir_cmp(lhs_reg, num);
  } else {
    switch (lhs->type->kind) {
    case TY_FIXNUM: case TY_PTR:
#ifndef __NO_FLONUM
    case TY_FLONUM:
#endif
      break;
    default: assert(false); break;
    }

    VReg *rhs_reg = gen_expr(rhs);
    // Allocate new register to avoid comparing spilled registers.
    VReg *tmp = add_new_reg(lhs->type, 0);
    new_ir_mov(tmp, lhs_reg);
    new_ir_cmp(tmp, rhs_reg);
  }

  return cond;
}

void gen_cond_jmp(Expr *cond, bool tf, BB *bb) {
  enum ExprKind ck = cond->kind;
  switch (ck) {
  case EX_FIXNUM:
    if (cond->fixnum == 0)
      tf = !tf;
    if (tf)
      new_ir_jmp(COND_ANY, bb);
    return;
  case EX_EQ:
  case EX_NE:
  case EX_LT:
  case EX_LE:
  case EX_GE:
  case EX_GT:
    if (!tf) {
      if (ck <= EX_NE)
        ck = (EX_EQ + EX_NE) - ck;
      else
        ck = EX_LT + ((ck - EX_LT) ^ 2);
    }
    new_ir_jmp(gen_compare_expr(ck, cond->bop.lhs, cond->bop.rhs), bb);
    return;
  case EX_LOGAND:
    if (!tf) {
      BB *bb1 = new_bb();
      BB *bb2 = new_bb();
      gen_cond_jmp(cond->bop.lhs, false, bb);
      set_curbb(bb1);
      gen_cond_jmp(cond->bop.rhs, false, bb);
      set_curbb(bb2);
    } else {
      BB *bb1 = new_bb();
      BB *bb2 = new_bb();
      gen_cond_jmp(cond->bop.lhs, false, bb2);
      set_curbb(bb1);
      gen_cond_jmp(cond->bop.rhs, true, bb);
      set_curbb(bb2);
    }
    return;
  case EX_LOGIOR:
    if (tf) {
      BB *bb1 = new_bb();
      BB *bb2 = new_bb();
      gen_cond_jmp(cond->bop.lhs, true, bb);
      set_curbb(bb1);
      gen_cond_jmp(cond->bop.rhs, true, bb);
      set_curbb(bb2);
    } else {
      BB *bb1 = new_bb();
      BB *bb2 = new_bb();
      gen_cond_jmp(cond->bop.lhs, true, bb2);
      set_curbb(bb1);
      gen_cond_jmp(cond->bop.rhs, false, bb);
      set_curbb(bb2);
    }
    return;
  default:
    assert(false);
    break;
  }
}

static VReg *gen_cast(VReg *reg, const Type *dst_type) {
  if (dst_type->kind == TY_VOID)
    return NULL;  // Assume void value is not used.

  if (reg->flag & VRF_CONST) {
#ifndef __NO_FLONUM
    assert(!(reg->vtype->flag & VRTF_FLONUM));  // No const vreg for flonum.
#endif
    Fixnum value = reg->fixnum;
    size_t dst_size = type_size(dst_type);
    if (dst_size < (size_t)reg->vtype->size && dst_size < sizeof(Fixnum)) {
      // Assume that integer is represented in Two's complement
      size_t bit = dst_size * CHAR_BIT;
      UFixnum mask = (-1UL) << bit;
      if (dst_type->kind == TY_FIXNUM && !dst_type->fixnum.is_unsigned &&  // signed
          (value & (1 << (bit - 1))))  // negative
        value |= mask;
      else
        value &= ~mask;
    }

    VRegType *vtype = to_vtype(dst_type);
    return new_const_vreg(value, vtype);
  }

  int dst_size = type_size(dst_type);
  bool lu = dst_type->kind == TY_FIXNUM ? dst_type->fixnum.is_unsigned : dst_type->kind == TY_PTR;
  bool ru = (reg->vtype->flag & VRTF_UNSIGNED) ? true : false;
  if (dst_size == reg->vtype->size && lu == ru
#ifndef __NO_FLONUM
      && is_flonum(dst_type) == ((reg->vtype->flag & VRTF_FLONUM) != 0)
#endif
  )
    return reg;

  return new_ir_cast(reg, to_vtype(dst_type));
}

static VReg *gen_lval(Expr *expr) {
  switch (expr->kind) {
  case EX_VAR:
    {
      Scope *scope;
      const VarInfo *varinfo = scope_find(expr->var.scope, expr->var.name, &scope);
      assert(varinfo != NULL && scope == expr->var.scope);
      if (is_global_scope(scope)) {
        return new_ir_iofs(expr->var.name, (varinfo->storage & VS_STATIC) == 0);
      } else {
        if (varinfo->storage & VS_STATIC)
          return new_ir_iofs(varinfo->static_.gvar->name, false);
        else if (varinfo->storage & VS_EXTERN)
          return new_ir_iofs(expr->var.name, true);
        else
          return new_ir_bofs(varinfo->local.reg);
      }
    }
  case EX_DEREF:
    return gen_expr(expr->unary.sub);
  case EX_MEMBER:
    {
      const Type *type = expr->member.target->type;
      if (ptr_or_array(type))
        type = type->pa.ptrof;
      assert(type->kind == TY_STRUCT);
      const Vector *members = type->struct_.info->members;
      const VarInfo *member = members->data[expr->member.index];

      VReg *reg = gen_expr(expr->member.target);
      if (member->struct_member.offset == 0)
        return reg;
      VRegType *vtype = to_vtype(&tySize);
      VReg *imm = new_const_vreg(member->struct_member.offset, vtype);
      VReg *result = new_ir_bop(IR_ADD, reg, imm, vtype);
      return result;
    }
  case EX_COMPLIT:
    {
      Expr *var = expr->complit.var;
      assert(var->var.scope != NULL);
      const VarInfo *varinfo = scope_find(var->var.scope, var->var.name, NULL);
      assert(varinfo != NULL);
      assert(varinfo->local.reg != NULL);
      varinfo->local.reg->flag |= VRF_REF;

      gen_stmts(expr->complit.inits);
      return gen_lval(expr->complit.var);
    }
  default:
    assert(false);
    break;
  }
  return NULL;
}

static VReg *gen_variable(Expr *expr) {
  switch (expr->type->kind) {
  case TY_FIXNUM:
  case TY_PTR:
#ifndef __NO_FLONUM
  case TY_FLONUM:
#endif
    {
      Scope *scope;
      const VarInfo *varinfo = scope_find(expr->var.scope, expr->var.name, &scope);
      assert(varinfo != NULL && scope == expr->var.scope);
      if (!is_global_scope(scope) && !(varinfo->storage & (VS_STATIC | VS_EXTERN))) {
        assert(varinfo->local.reg != NULL);
        return varinfo->local.reg;
      }

      VReg *reg = gen_lval(expr);
      VReg *result = new_ir_unary(IR_LOAD, reg, to_vtype(expr->type));
      return result;
    }
  default:
    assert(false);
    // Fallthrough to suppress compile error.
  case TY_ARRAY:   // Use variable address as a pointer.
  case TY_STRUCT:  // struct value is handled as a pointer.
  case TY_FUNC:
    return gen_lval(expr);
  }
}

static VReg *gen_ternary(Expr *expr) {
  BB *tbb = new_bb();
  BB *fbb = new_bb();
  BB *nbb = new_bb();
  bool no_value = expr->type->kind == TY_VOID;

  VReg *result = add_new_reg(expr->type, 0);
  gen_cond_jmp(expr->ternary.cond, false, fbb);

  set_curbb(tbb);
  VReg *tval = gen_expr(expr->ternary.tval);
  if (!no_value)
    new_ir_mov(result, tval);
  new_ir_jmp(COND_ANY, nbb);

  set_curbb(fbb);
  VReg *fval = gen_expr(expr->ternary.fval);
  if (!no_value)
    new_ir_mov(result, fval);

  set_curbb(nbb);
  return result;
}

bool is_stack_param(const Type *type) {
  return type->kind == TY_STRUCT;
}

typedef struct {
  int reg_index;
  int offset;
  int size;
  bool stack_arg;
#ifndef __NO_FLONUM
  bool is_flonum;
#endif
} ArgInfo;

static VReg *gen_funcall(Expr *expr) {
  Expr *func = expr->funcall.func;
  Vector *args = expr->funcall.args;
  int arg_count = args != NULL ? args->len : 0;

  int offset = 0;

  VReg *retvar_reg = NULL;  // Return value is on the stack.
  if (is_stack_param(expr->type)) {
    const Token *ident = alloc_ident(alloc_label(), NULL, NULL);
    VarInfo *ret_varinfo = scope_add(curscope, ident, expr->type, 0);
    ret_varinfo->local.reg = retvar_reg = add_new_reg(expr->type, VRF_LOCAL);
  }

  VRegType **arg_vtypes = (retvar_reg == NULL && arg_count <= 0) ? NULL :
    calloc(arg_count + (retvar_reg != NULL ? 1 : 0), sizeof(*arg_vtypes));

  ArgInfo *arg_infos = NULL;
  int stack_arg_count = 0;
  if (args != NULL) {
    bool vaargs = false;
    if (func->kind == EX_VAR && is_global_scope(func->var.scope)) {
      vaargs = func->type->func.vaargs;
    } else {
      // TODO:
    }

    int arg_start = retvar_reg != NULL ? 1 : 0;
    int ireg_index = arg_start;
#ifndef __NO_FLONUM
    int freg_index = 0;
#endif

    // Check stack arguments.
    arg_infos = malloc(sizeof(*arg_infos) * arg_count);
    for (int i = 0; i < arg_count; ++i) {
      ArgInfo *p = &arg_infos[i];
      p->reg_index = -1;
      p->offset = -1;
      Expr *arg = args->data[i];
      assert(arg->type->kind != TY_ARRAY);
      p->size = type_size(arg->type);
#ifndef __NO_FLONUM
      p->is_flonum = is_flonum(arg->type);
#endif
      p->stack_arg = is_stack_param(arg->type);
      bool reg_arg = !p->stack_arg;
      if (reg_arg) {
#ifndef __NO_FLONUM
        if (p->is_flonum)
          reg_arg = freg_index < MAX_FREG_ARGS;
        else
#endif
          reg_arg = ireg_index < MAX_REG_ARGS;
      }
      if (!reg_arg) {
        if (ireg_index >= MAX_REG_ARGS && vaargs) {
          parse_error(((Expr*)args->data[ireg_index])->token,
                      "Param count exceeds %d", MAX_REG_ARGS);
        }

        offset = ALIGN(offset, align_size(arg->type));
        p->offset = offset;
        offset += ALIGN(p->size, WORD_SIZE);
        ++stack_arg_count;
      } else {
#ifndef __NO_FLONUM
        if (p->is_flonum)
          p->reg_index = freg_index++;
        else
#endif
          p->reg_index = ireg_index++;
      }
    }

    for (int i = 0; i < arg_count; ++i) {
      Expr *arg = args->data[i];
      arg_vtypes[i + arg_start] = to_vtype(arg->type);
    }
  }
  offset = ALIGN(offset, 8);

  IR *precall = new_ir_precall(arg_count - stack_arg_count, offset);

  int reg_arg_count = 0;
  if (offset > 0)
    new_ir_addsp(-offset);
  if (args != NULL) {
    // Register arguments.
    for (int i = arg_count; --i >= 0; ) {
      Expr *arg = args->data[i];
      VReg *reg = gen_expr(arg);
      const ArgInfo *p = &arg_infos[i];
      if (p->offset < 0) {
        new_ir_pusharg(reg, to_vtype(arg->type));
        ++reg_arg_count;
      } else {
        VRegType offset_type = {.size = 4, .align = 4, .flag = 0};  // TODO:
        VReg *dst = new_ir_sofs(new_const_vreg(p->offset + reg_arg_count * WORD_SIZE,
                                               &offset_type));
        if (p->stack_arg) {
          new_ir_memcpy(dst, reg, type_size(arg->type));
        } else {
          if (reg->flag & VRF_CONST) {
            // Allocate new register to avoid constant register.
            VReg *tmp = add_new_reg(arg->type, 0);
            new_ir_mov(tmp, reg);
            reg = tmp;
          }
          new_ir_store(dst, reg);
        }
      }
    }
  }
  if (retvar_reg != NULL) {
    // gen_lval(retvar)
    VReg *dst = new_ir_bofs(retvar_reg);
    VRegType *vtype = to_vtype(ptrof(expr->type));
    new_ir_pusharg(dst, vtype);
    arg_vtypes[0] = vtype;
    ++reg_arg_count;
  }

  bool label_call = false;
  bool global = false;
  if (func->kind == EX_VAR) {
    const VarInfo *varinfo = scope_find(func->var.scope, func->var.name, NULL);
    assert(varinfo != NULL);
    label_call = varinfo->type->kind == TY_FUNC;
    global = !(varinfo->storage & VS_STATIC);
  }

  VReg *result_reg = NULL;
  {
    const Type *type = expr->type;
    if (retvar_reg != NULL)
      type = ptrof(type);
    VRegType *ret_vtype = to_vtype(type);
    if (label_call) {
      result_reg = new_ir_call(func->var.name, global, NULL, reg_arg_count, ret_vtype,
                               precall, arg_vtypes);
    } else {
      VReg *freg = gen_expr(func);
      result_reg = new_ir_call(NULL, false, freg, reg_arg_count, ret_vtype, precall, arg_vtypes);
    }
  }

  free(arg_infos);

  return result_reg;
}

VReg *gen_arith(enum ExprKind kind, const Type *type, VReg *lhs, VReg *rhs) {
  switch (kind) {
  case EX_ADD:
  case EX_SUB:
  case EX_MUL:
#ifndef __NO_FLONUM
    if (is_flonum(type)) {
      return new_ir_bop(kind + (IR_ADD - EX_ADD), lhs, rhs, to_vtype(type));
    }
#endif
    return new_ir_bop(kind + (IR_ADD - EX_ADD), lhs, rhs, to_vtype(type));
  case EX_BITAND:
  case EX_BITOR:
  case EX_BITXOR:
  case EX_LSHIFT:
  case EX_RSHIFT:
    return new_ir_bop(kind + (IR_ADD - EX_ADD), lhs, rhs, to_vtype(type));

  case EX_DIV:
  case EX_MOD:
    assert(is_number(type));
#ifndef __NO_FLONUM
    if (is_flonum(type)) {
      return new_ir_bop(kind + (IR_DIV - EX_DIV), lhs, rhs, to_vtype(type));
    }
#endif
    return new_ir_bop(kind + ((type->fixnum.is_unsigned ? IR_DIVU : IR_DIV) - EX_DIV), lhs, rhs, to_vtype(type));

  default:
    assert(false);
    return NULL;
  }
}

#ifndef __NO_FLONUM
VReg *gen_const_flonum(Expr *expr) {
  assert(expr->type->kind == TY_FLONUM);
  Initializer *init = malloc(sizeof(*init));
  init->kind = IK_SINGLE;
  init->single = expr;
  init->token = expr->token;

  assert(curscope != NULL);
  const Type *type = qualified_type(expr->type, TQ_CONST);
  const Token *ident = alloc_ident(alloc_label(), NULL, NULL);
  VarInfo *varinfo = scope_add(curscope, ident, type, VS_STATIC);
  VarInfo *gvarinfo = is_global_scope(curscope) ? varinfo : varinfo->static_.gvar;
  gvarinfo->global.init = init;

  VReg *src = new_ir_iofs(gvarinfo->name, false);
  return new_ir_unary(IR_LOAD, src, to_vtype(type));
}
#endif

VReg *gen_expr(Expr *expr) {
  switch (expr->kind) {
  case EX_FIXNUM:
    {
      assert(expr->type->kind == TY_FIXNUM);
      VReg *reg = new_const_vreg(expr->fixnum, to_vtype(expr->type));
      if (!is_im32(expr->fixnum)) {
        // Large constant value is not allowed in x86,
        // so use mov instruction.
        VReg *tmp = add_new_reg(expr->type, 0);
        new_ir_mov(tmp, reg);
        reg = tmp;
      }
      return reg;
    }
#ifndef __NO_FLONUM
  case EX_FLONUM:
    return gen_const_flonum(expr);
#endif

  case EX_STR:
    assert(!"should be handled in parser");

  case EX_VAR:
    return gen_variable(expr);

  case EX_REF:
    return gen_lval(expr->unary.sub);

  case EX_DEREF:
    {
      VReg *reg = gen_expr(expr->unary.sub);
      VReg *result;
      switch (expr->type->kind) {
      case TY_FIXNUM:
      case TY_PTR:
#ifndef __NO_FLONUM
      case TY_FLONUM:
#endif
        result = new_ir_unary(IR_LOAD, reg, to_vtype(expr->type));
        return result;

      default:
        assert(false);
        // Fallthrough to suppress compile error.
      case TY_ARRAY:
      case TY_STRUCT:
      case TY_FUNC:
        // array, struct and func values are handled as a pointer.
        return reg;
      }
    }

  case EX_MEMBER:
    {
      VReg *reg = gen_lval(expr);
      VReg *result;
      switch (expr->type->kind) {
      case TY_FIXNUM:
      case TY_PTR:
#ifndef __NO_FLONUM
      case TY_FLONUM:
#endif
        result = new_ir_unary(IR_LOAD, reg, to_vtype(expr->type));
        break;
      default:
        assert(false);
        // Fallthrough to suppress compile error.
      case TY_ARRAY:
      case TY_STRUCT:
        result = reg;
        break;
      }
      return result;
    }

  case EX_COMMA:
    gen_expr(expr->bop.lhs);
    return gen_expr(expr->bop.rhs);

  case EX_TERNARY:
    return gen_ternary(expr);

  case EX_CAST:
    return gen_cast(gen_expr(expr->unary.sub), expr->type);

  case EX_ASSIGN:
    {
      VReg *src = gen_expr(expr->bop.rhs);
      if (expr->bop.lhs->kind == EX_VAR) {
        Expr *lhs = expr->bop.lhs;
        switch (lhs->type->kind) {
        case TY_FIXNUM:
        case TY_PTR:
#ifndef __NO_FLONUM
        case TY_FLONUM:
#endif
          {
            Scope *scope;
            const VarInfo *varinfo = scope_find(lhs->var.scope, lhs->var.name, &scope);
            assert(varinfo != NULL);
            if (!is_global_scope(scope) && !(varinfo->storage & (VS_STATIC | VS_EXTERN))) {
              assert(varinfo->local.reg != NULL);
              new_ir_mov(varinfo->local.reg, src);
              return src;
            }
          }
          break;
        default:
          break;
        }
      }

      VReg *dst = gen_lval(expr->bop.lhs);

      switch (expr->type->kind) {
      default:
        assert(false);
        // Fallthrough to suppress compiler error.
      case TY_FIXNUM:
      case TY_PTR:
#ifndef __NO_FLONUM
      case TY_FLONUM:
#endif
#if 0
        new_ir_store(dst, src);
#else
        // To avoid both spilled registers, add temporary register.
        {
          VReg *tmp = add_new_reg(expr->type, 0);
          new_ir_mov(tmp, src);
          new_ir_store(dst, tmp);
        }
#endif
        break;
      case TY_STRUCT:
        if (expr->type->struct_.info->size > 0) {
          VReg *tmp = add_new_reg(&tyVoidPtr, 0);
          new_ir_mov(tmp, src);
          new_ir_memcpy(dst, tmp, expr->type->struct_.info->size);
        }
        break;
      }
      return src;
    }

  case EX_MODIFY:
    {
      Expr *sub = expr->unary.sub;
      if (sub->bop.lhs->kind == EX_VAR && !is_global_scope(sub->bop.lhs->var.scope)) {
        VReg *lhs = gen_expr(sub->bop.lhs);
        VReg *rhs = gen_expr(sub->bop.rhs);
        VReg *result = gen_arith(sub->kind, sub->type, lhs, rhs);
        new_ir_mov(lhs, result);
        return result;
      } else {
        VReg *lval = gen_lval(sub->bop.lhs);
        VReg *rhs = gen_expr(sub->bop.rhs);
        VReg *lhs = new_ir_unary(IR_LOAD, lval, to_vtype(sub->bop.lhs->type));
        VReg *result = gen_arith(sub->kind, sub->type, lhs, rhs);
        VReg *cast = gen_cast(result, expr->type);
        new_ir_store(lval, cast);
        return result;
      }
    }

  case EX_PREINC:
  case EX_PREDEC:
    {
      size_t value = 1;
      if (expr->type->kind == TY_PTR)
        value = type_size(expr->type->pa.ptrof);

      VRegType *vtype = to_vtype(expr->type);
      Expr *sub = expr->unary.sub;
      if (sub->kind == EX_VAR && !is_global_scope(sub->var.scope)) {
        const VarInfo *varinfo = scope_find(sub->var.scope, sub->var.name, NULL);
        assert(varinfo != NULL);
        if (!(varinfo->storage & (VS_STATIC | VS_EXTERN))) {
#ifndef __NO_FLONUM
          if (is_flonum(sub->type)) {
            VReg *one = gen_const_flonum(new_expr_flolit(sub->type, NULL, 1));
            VReg *result = new_ir_bop(expr->kind == EX_PREINC ? IR_ADD : IR_SUB,
                                      varinfo->local.reg, one, vtype);
            new_ir_mov(varinfo->local.reg, result);
            return result;
          }
#endif
          VReg *num = new_const_vreg(value, vtype);
          VReg *result = new_ir_bop(expr->kind == EX_PREINC ? IR_ADD : IR_SUB,
                                    varinfo->local.reg, num, vtype);
          new_ir_mov(varinfo->local.reg, result);
          return result;
        }
      }

      VReg *lval = gen_lval(sub);
#ifndef __NO_FLONUM
      if (is_flonum(sub->type)) {
        VReg *val = new_ir_unary(IR_LOAD, lval, vtype);
        VReg *one = gen_const_flonum(new_expr_flolit(sub->type, NULL, value));
        VReg *result = new_ir_bop(expr->kind == EX_PREINC ? IR_ADD : IR_SUB,
                                  val, one, vtype);
        new_ir_store(lval, result);
        return result;
      }
#endif
      new_ir_incdec(expr->kind == EX_PREINC ? IR_INC : IR_DEC,
                    lval, type_size(expr->type), value);
      VReg *result = new_ir_unary(IR_LOAD, lval, vtype);
      return result;
    }

  case EX_POSTINC:
  case EX_POSTDEC:
    {
      size_t value = 1;
      if (expr->type->kind == TY_PTR)
        value = type_size(expr->type->pa.ptrof);

      VRegType *vtype = to_vtype(expr->type);
      Expr *sub = expr->unary.sub;
      if (sub->kind == EX_VAR && !is_global_scope(sub->var.scope)) {
        const VarInfo *varinfo = scope_find(sub->var.scope, sub->var.name, NULL);
        assert(varinfo != NULL);
        if (!(varinfo->storage & (VS_STATIC | VS_EXTERN))) {
#ifndef __NO_FLONUM
          if (is_flonum(sub->type)) {
            VReg *org_val = add_new_reg(sub->type, 0);
            new_ir_mov(org_val, varinfo->local.reg);
            VReg *one = gen_const_flonum(new_expr_flolit(sub->type, NULL, 1));
            VReg *result = new_ir_bop(expr->kind == EX_POSTINC ? IR_ADD : IR_SUB,
                                      varinfo->local.reg, one, vtype);
            new_ir_mov(varinfo->local.reg, result);
            return org_val;
          }
#endif
          VReg *org_val = add_new_reg(sub->type, 0);
          new_ir_mov(org_val, varinfo->local.reg);
          VReg *num = new_const_vreg(value, vtype);
          VReg *result = new_ir_bop(expr->kind == EX_POSTINC ? IR_ADD : IR_SUB,
                                    varinfo->local.reg, num, vtype);
          new_ir_mov(varinfo->local.reg, result);
          return org_val;
        }
      }

      VReg *lval = gen_lval(expr->unary.sub);
#ifndef __NO_FLONUM
      if (is_flonum(sub->type)) {
        VReg *val = new_ir_unary(IR_LOAD, lval, vtype);
        VReg *one = gen_const_flonum(new_expr_flolit(sub->type, NULL, value));
        VReg *result = new_ir_bop(expr->kind == EX_POSTINC ? IR_ADD : IR_SUB,
                                  val, one, vtype);
        new_ir_store(lval, result);
        return val;
      }
#endif
      VReg *result = new_ir_unary(IR_LOAD, lval, vtype);
      new_ir_incdec(expr->kind == EX_POSTINC ? IR_INC : IR_DEC,
                    lval, type_size(expr->type), value);
      return result;
    }

  case EX_FUNCALL:
    return gen_funcall(expr);

  case EX_POS:
    return gen_expr(expr->unary.sub);

  case EX_NEG:
    {
      VReg *reg = gen_expr(expr->unary.sub);
#ifndef __NO_FLONUM
      if (is_flonum(expr->type)) {
        VReg *zero = gen_expr(new_expr_flolit(expr->type, NULL, 0.0));
        return gen_arith(EX_SUB, expr->type, zero, reg);
      }
#endif
      VReg *result = new_ir_unary(IR_NEG, reg, to_vtype(expr->type));
      return result;
    }

  case EX_BITNOT:
    {
      VReg *reg = gen_expr(expr->unary.sub);
      VReg *result = new_ir_unary(IR_BITNOT, reg, to_vtype(expr->type));
      return result;
    }

  case EX_EQ:
  case EX_NE:
  case EX_LT:
  case EX_GT:
  case EX_LE:
  case EX_GE:
    {
      enum ConditionKind cond = gen_compare_expr(expr->kind, expr->bop.lhs, expr->bop.rhs);
      switch (cond) {
      case COND_NONE:
      case COND_ANY:
        return new_const_vreg(cond == COND_ANY, to_vtype(&tyBool));
      default:
        return new_ir_cond(cond);
      }
    }

  case EX_LOGAND:
    {
      BB *bb1 = new_bb();
      BB *bb2 = new_bb();
      BB *false_bb = new_bb();
      BB *next_bb = new_bb();
      gen_cond_jmp(expr->bop.lhs, false, false_bb);
      set_curbb(bb1);
      gen_cond_jmp(expr->bop.rhs, false, false_bb);
      set_curbb(bb2);
      VRegType *vtbool = to_vtype(&tyBool);
      VReg *result = add_new_reg(&tyBool, 0);
      new_ir_mov(result, new_const_vreg(true, vtbool));
      new_ir_jmp(COND_ANY, next_bb);
      set_curbb(false_bb);
      new_ir_mov(result, new_const_vreg(false, vtbool));
      set_curbb(next_bb);
      return result;
    }

  case EX_LOGIOR:
    {
      BB *bb1 = new_bb();
      BB *bb2 = new_bb();
      BB *true_bb = new_bb();
      BB *next_bb = new_bb();
      gen_cond_jmp(expr->bop.lhs, true, true_bb);
      set_curbb(bb1);
      gen_cond_jmp(expr->bop.rhs, true, true_bb);
      set_curbb(bb2);
      VRegType *vtbool = to_vtype(&tyBool);
      VReg *result = add_new_reg(&tyBool, 0);
      new_ir_mov(result, new_const_vreg(false, vtbool));
      new_ir_jmp(COND_ANY, next_bb);
      set_curbb(true_bb);
      new_ir_mov(result, new_const_vreg(true, vtbool));
      set_curbb(next_bb);
      return result;
    }

  case EX_ADD:
  case EX_SUB:
  case EX_MUL:
  case EX_DIV:
  case EX_MOD:
  case EX_LSHIFT:
  case EX_RSHIFT:
  case EX_BITAND:
  case EX_BITOR:
  case EX_BITXOR:
    {
      VReg *lhs = gen_expr(expr->bop.lhs);
      VReg *rhs = gen_expr(expr->bop.rhs);
      return gen_arith(expr->kind, expr->type, lhs, rhs);
    }

  case EX_COMPLIT:
    gen_stmts(expr->complit.inits);
    return gen_expr(expr->complit.var);

  default:
    fprintf(stderr, "Expr kind=%d, ", expr->kind);
    assert(!"Unhandled in gen_expr");
    break;
  }

  return NULL;
}
