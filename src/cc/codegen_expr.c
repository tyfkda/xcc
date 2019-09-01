#include "codegen.h"

#include <assert.h>
#include <stdlib.h>  // malloc

#include "expr.h"
#include "parser.h"  // Initializer
#include "ir.h"
#include "sema.h"
#include "type.h"
#include "util.h"
#include "var.h"
#include "x86_64.h"

static VReg *gen_lval(Expr *expr);

// test %eax, %eax, and so on.
static void gen_test_opcode(const Type *type) {
  int size = type_size(type);
  switch (type->type) {
  case TY_NUM: case TY_PTR:
    break;
  case TY_ARRAY: case TY_FUNC:
    size = WORD_SIZE;
    break;
  default: assert(false); break;
  }

  new_ir_st(IR_PUSH);
  new_ir_imm(0, size);
  new_ir_op(IR_CMP, size);
}

static enum ConditionType flip_cond(enum ConditionType cond) {
  assert(COND_EQ <= cond && cond <= COND_GT);
  if (cond >= COND_LT)
    cond = COND_GT - (cond - COND_LT);
  return cond;
}

static enum ConditionType gen_compare_expr(enum ExprType type, Expr *lhs, Expr *rhs) {
  const Type *ltype = lhs->valType;
  UNUSED(ltype);
  assert(ltype->type == rhs->valType->type);

  enum ConditionType cond = type + (COND_EQ - EX_EQ);
  if (rhs->type != EX_NUM && lhs->type == EX_NUM) {
    Expr *tmp = lhs;
    lhs = rhs;
    rhs = tmp;
    cond = flip_cond(cond);
  }

  gen_expr(lhs);
  if (rhs->type == EX_NUM && rhs->u.num.ival == 0 &&
      (cond == COND_EQ || cond == COND_NE)) {
    gen_test_opcode(lhs->valType);
  } else if (rhs->type == EX_NUM && (lhs->valType->u.num.type != NUM_LONG || is_im32(rhs->u.num.ival))) {
    new_ir_cmpi(rhs->u.num.ival, type_size(lhs->valType));
  } else {
    switch (lhs->valType->type) {
    case TY_NUM: case TY_PTR:
      break;
    default: assert(false); break;
    }

    new_ir_st(IR_PUSH);
    gen_expr(rhs);
    new_ir_op(IR_CMP, type_size(lhs->valType));
  }

  return cond;
}

void gen_cond_jmp(Expr *cond, bool tf, BB *bb) {
  // Local optimization: if `cond` is compare expression, then
  // jump using flags after CMP directly.
  switch (cond->type) {
  case EX_NUM:
    if (cond->u.num.ival == 0)
      tf = !tf;
    if (tf)
      new_ir_jmp(COND_ANY, bb);
    return;

  case EX_EQ:
  case EX_NE:
    {
      enum ConditionType type = gen_compare_expr(cond->type, cond->u.bop.lhs, cond->u.bop.rhs);
      if (type != COND_EQ)
        tf = !tf;
      if (tf)
        new_ir_jmp(COND_EQ, bb);
      else
        new_ir_jmp(COND_NE, bb);
      return;
    }
  case EX_LT:
  case EX_GT:
  case EX_LE:
  case EX_GE:
    {
      enum ConditionType type = gen_compare_expr(cond->type, cond->u.bop.lhs, cond->u.bop.rhs);
      switch (type) {
      case COND_LT:
      case COND_GE:
        if (type != COND_LT)
          tf = !tf;
        if (tf)
          new_ir_jmp(COND_LT, bb);
        else
          new_ir_jmp(COND_GE, bb);
        break;
      case COND_GT:
      case COND_LE:
        if (type != COND_GT)
          tf = !tf;
        if (tf)
          new_ir_jmp(COND_GT, bb);
        else
          new_ir_jmp(COND_LE, bb);
        break;
      default:  assert(false); break;
      }
    }
    return;
  case EX_NOT:
    gen_cond_jmp(cond->u.unary.sub, !tf, bb);
    return;
  case EX_LOGAND:
    if (!tf) {
      BB *bb1 = bb_split(curbb);
      BB *bb2 = bb_split(bb1);
      gen_cond_jmp(cond->u.bop.lhs, false, bb);
      set_curbb(bb1);
      gen_cond_jmp(cond->u.bop.rhs, false, bb);
      set_curbb(bb2);
    } else {
      BB *bb1 = bb_split(curbb);
      BB *bb2 = bb_split(bb1);
      gen_cond_jmp(cond->u.bop.lhs, false, bb2);
      set_curbb(bb1);
      gen_cond_jmp(cond->u.bop.rhs, true, bb);
      set_curbb(bb2);
    }
    return;
  case EX_LOGIOR:
    if (tf) {
      BB *bb1 = bb_split(curbb);
      BB *bb2 = bb_split(bb1);
      gen_cond_jmp(cond->u.bop.lhs, true, bb);
      set_curbb(bb1);
      gen_cond_jmp(cond->u.bop.rhs, true, bb);
      set_curbb(bb2);
    } else {
      BB *bb1 = bb_split(curbb);
      BB *bb2 = bb_split(bb1);
      gen_cond_jmp(cond->u.bop.lhs, true, bb2);
      set_curbb(bb1);
      gen_cond_jmp(cond->u.bop.rhs, false, bb);
      set_curbb(bb2);
    }
    return;
  default:
    break;
  }

  gen_expr(cond);
  gen_test_opcode(cond->valType);
  new_ir_jmp(tf ? COND_NE : COND_EQ, bb);
}

static void gen_cast(const Type *ltypep, const Type *rtypep) {
  size_t dst_size = type_size(ltypep);
  size_t src_size;
  if (rtypep->type == TY_ARRAY)
    src_size = WORD_SIZE;
  else
    src_size = type_size(rtypep);
  new_ir_cast(dst_size, src_size);
}

static VReg *gen_rval(Expr *expr) {
  return gen_expr(expr);  // ?
}

static VReg *gen_ref(Expr *expr) {
  return gen_lval(expr);
}

static VReg *gen_lval(Expr *expr) {
  switch (expr->type) {
  case EX_VARREF:
    if (expr->u.varref.scope == NULL) {
      // TODO: Implement for IR
      new_ir_iofs(expr->u.varref.ident);
    } else {
      Scope *scope = expr->u.varref.scope;
      VarInfo *varinfo = scope_find(&scope, expr->u.varref.ident);
      assert(varinfo != NULL);
      assert(!(varinfo->flag & VF_STATIC));
      int offset = varinfo->offset;
      return new_ir_bofs(offset);
    }
    break;
  case EX_DEREF:
    gen_rval(expr->u.unary.sub);
    break;
  case EX_MEMBER:
    {
      const Type *type = expr->u.member.target->valType;
      if (type->type == TY_PTR || type->type == TY_ARRAY)
        type = type->u.pa.ptrof;
      assert(type->type == TY_STRUCT);
      calc_struct_size(type->u.struct_.info);
      Vector *members = type->u.struct_.info->members;
      VarInfo *varinfo = (VarInfo*)members->data[expr->u.member.index];

      if (expr->u.member.target->valType->type == TY_PTR)
        gen_expr(expr->u.member.target);
      else
        gen_ref(expr->u.member.target);
      if (varinfo->offset != 0) {
        new_ir_st(IR_PUSH);
        new_ir_imm(varinfo->offset, type_size(&tyLong));
        new_ir_op(IR_ADD, type_size(&tySize));
      }
    }
    break;
  default:
    error("No lvalue: %d", expr->type);
    break;
  }
  return NULL;
}

static VReg *gen_varref(Expr *expr) {
  VReg *reg = gen_lval(expr);
  VReg *result;
  switch (expr->valType->type) {
  case TY_NUM:
  case TY_PTR:
    result = new_ir_unary(IR_LOAD, reg, type_size(expr->valType));
    new_ir_unreg(reg);
    break;
  default:
    assert(false);
    // Fallthrough to suppress compile error.
  case TY_ARRAY:   // Use variable address as a pointer.
  case TY_STRUCT:  // struct value is handled as a pointer.
  case TY_FUNC:
    result = reg;
    break;
  }
  return result;
}

static void gen_ternary(Expr *expr) {
  BB *tbb = bb_split(curbb);
  BB *fbb = bb_split(tbb);
  BB *nbb = bb_split(fbb);

  gen_cond_jmp(expr->u.ternary.cond, false, fbb);

  set_curbb(tbb);
  gen_expr(expr->u.ternary.tval);
  new_ir_jmp(COND_ANY, nbb);

  set_curbb(fbb);
  gen_expr(expr->u.ternary.fval);

  set_curbb(nbb);
}

static void gen_funcall(Expr *expr) {
  Expr *func = expr->u.funcall.func;
  Vector *args = expr->u.funcall.args;
  int arg_count = args != NULL ? args->len : 0;

  int stack_args = MAX(arg_count - MAX_REG_ARGS, 0);
  bool align_stack = ((stackpos + stack_args * WORD_SIZE) & 15) != 0;
  if (align_stack)
    new_ir_addsp(-8);

  if (args != NULL) {
    int len = args->len;
    if (len > MAX_REG_ARGS) {
      bool vaargs = false;
      if (func->type == EX_VARREF && func->u.varref.scope == NULL) {
        VarInfo *varinfo = find_global(func->u.varref.ident);
        assert(varinfo != NULL && varinfo->type->type == TY_FUNC);
        vaargs = varinfo->type->u.func.vaargs;
      } else {
        // TODO:
      }

      if (vaargs)
        error("Param count exceeds %d (%d)", MAX_REG_ARGS, len);
    }

    for (int i = len; --i >= 0; ) {
      gen_expr((Expr*)args->data[i]);
      new_ir_st(IR_PUSH);
    }
  }

  if (func->type == EX_VARREF && func->u.varref.scope == NULL) {
    new_ir_call(func->u.varref.ident, arg_count);
  } else {
    // TODO: IR
    gen_expr(func);
    new_ir_call(NULL, arg_count);
  }

  int stack_add = stack_args * 8;
  if (align_stack) {
    stack_add += 8;
  }
  if (stack_add > 0) {
    new_ir_addsp(stack_add);
  }
}

VReg *gen_arith(enum ExprType exprType, const Type *valType, VReg *lhs, VReg *rhs) {
  // lhs=rax, rhs=rdi, result=rax
  switch (exprType) {
  case EX_ADD:
  case EX_SUB:
  case EX_MUL:
  case EX_DIV:
  case EX_MOD:
  case EX_BITAND:
  case EX_BITOR:
  case EX_BITXOR:
  case EX_LSHIFT:
  case EX_RSHIFT:
    {
      VReg *result = new_ir_bop(exprType + (IR_ADD - EX_ADD), lhs, rhs, type_size(valType));
      new_ir_unreg(lhs);
      new_ir_unreg(rhs);
      return result;
    }

  default:
    assert(false);
    return NULL;
  }
}

VReg *gen_expr(Expr *expr) {
  switch (expr->type) {
  case EX_NUM:
    assert(expr->valType->type == TY_NUM);
    return new_ir_imm(expr->u.num.ival, type_size(expr->valType));

  case EX_STR:
    {
      Initializer *init = malloc(sizeof(*init));
      init->type = vSingle;
      init->u.single = expr;

      // Create string and point to it.
      const char * label = alloc_label();
      Type* strtype = arrayof(&tyChar, expr->u.str.size);
      VarInfo *varinfo = define_global(strtype, VF_CONST | VF_STATIC, NULL, label);
      varinfo->u.g.init = init;

      new_ir_iofs(label);
    }
    break;

  case EX_SIZEOF:
    new_ir_imm(type_size(expr->u.sizeof_.type), type_size(expr->valType));
    break;

  case EX_VARREF:
    return gen_varref(expr);

  case EX_REF:
    gen_ref(expr->u.unary.sub);
    break;

  case EX_DEREF:
    {
      VReg *reg = gen_rval(expr->u.unary.sub);
      VReg *result;
      switch (expr->valType->type) {
      case TY_NUM:
      case TY_PTR:
        result = new_ir_unary(IR_LOAD, reg, type_size(expr->valType));
        new_ir_unreg(reg);
        return result;

      default:
        assert(false);
        // Fallthrough to suppress compile error.
      case TY_ARRAY:
      case TY_STRUCT:
        // array and struct values are handled as a pointer.
        return reg;
      }
    }

  case EX_MEMBER:
    {
      VReg *reg = gen_lval(expr);
      VReg *result;
      switch (expr->valType->type) {
      case TY_NUM:
      case TY_PTR:
        result = new_ir_unary(IR_LOAD, reg, type_size(expr->valType));
        new_ir_unreg(reg);
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
    break;

  case EX_COMMA:
    {
      Vector *list = expr->u.comma.list;
      for (int i = 0, len = list->len; i < len; ++i)
        gen_expr(list->data[i]);
    }
    break;

  case EX_TERNARY:
    gen_ternary(expr);
    break;

  case EX_CAST:
    if (expr->u.unary.sub->type == EX_NUM) {
      assert(expr->u.unary.sub->valType->type == TY_NUM);
      intptr_t value = expr->u.unary.sub->u.num.ival;
      switch (expr->u.unary.sub->valType->u.num.type) {
      case NUM_CHAR:
        value = (int8_t)value;
        break;
      case NUM_SHORT:
        value = (int16_t)value;
        break;
      case NUM_INT: case NUM_ENUM:
        value = (int32_t)value;
        break;
      case NUM_LONG:
        value = (int64_t)value;
        break;
      default:
        assert(false);
        value = -1;
        break;
      }

      new_ir_imm(value, type_size(expr->valType));
    } else {
      gen_expr(expr->u.unary.sub);
      gen_cast(expr->valType, expr->u.unary.sub->valType);
    }
    break;

  case EX_ASSIGN:
    {
      VReg *src = gen_expr(expr->u.bop.rhs);
      VReg *dst = gen_lval(expr->u.bop.lhs);

      switch (expr->valType->type) {
      default:
        assert(false);
        // Fallthrough to suppress compiler error.
      case TY_NUM:
      case TY_PTR:
        new_ir_store(dst, src, type_size(expr->valType));
        break;
      case TY_STRUCT:
        new_ir_memcpy(dst, src, expr->valType->u.struct_.info->size);
        break;
      }
      new_ir_unreg(dst);
      return src;
    }
    break;

#if 0
  case EX_ASSIGN_WITH:
    {
      Expr *sub = expr->u.unary.sub;
      gen_expr(sub->u.bop.rhs);
      new_ir_st(IR_PUSH);
      gen_lval(sub->u.bop.lhs);
      new_ir_st(IR_SAVE_LVAL);
      new_ir_unary(IR_LOAD, type_size(sub->u.bop.lhs->valType));
      gen_arith(sub->type, sub->valType, sub->u.bop.rhs->valType);
      gen_cast(expr->valType, sub->valType);
      new_ir_assign_lval(type_size(expr->valType));
    }
    break;
#endif

  case EX_PREINC:
  case EX_PREDEC:
  case EX_POSTINC:
  case EX_POSTDEC:
    {
      size_t value = 1;
      if (expr->valType->type == TY_PTR)
        value = type_size(expr->valType->u.pa.ptrof);
      gen_lval(expr->u.unary.sub);
      new_ir_incdec(((expr->type - EX_PREINC) & 1) == 0, expr->type < EX_POSTINC,
                    type_size(expr->valType), value);
    }
    break;

  case EX_FUNCALL:
    gen_funcall(expr);
    break;

  case EX_NEG:
    gen_expr(expr->u.unary.sub);
    new_ir_op(IR_NEG, type_size(expr->valType));
    break;

  case EX_NOT:
    gen_expr(expr->u.unary.sub);
    switch (expr->u.unary.sub->valType->type) {
    case TY_NUM: case TY_PTR:
      new_ir_op(IR_NOT, type_size(expr->u.unary.sub->valType));
      break;
    case TY_ARRAY: case TY_FUNC:
      // Array is handled as a pointer.
      new_ir_op(IR_NOT, WORD_SIZE);
      break;
    default:  assert(false); break;
    }
    break;

  case EX_EQ:
  case EX_NE:
  case EX_LT:
  case EX_GT:
  case EX_LE:
  case EX_GE:
    {
      enum ConditionType cond = gen_compare_expr(expr->type, expr->u.bop.lhs, expr->u.bop.rhs);
      new_ir_set(cond);
    }
    break;

  case EX_LOGAND:
    {
      BB *bb1 = bb_split(curbb);
      BB *bb2 = bb_split(bb1);
      BB *false_bb = bb_split(bb2);
      BB *next_bb = bb_split(false_bb);
      gen_cond_jmp(expr->u.bop.lhs, false, false_bb);
      set_curbb(bb1);
      gen_cond_jmp(expr->u.bop.rhs, false, false_bb);
      set_curbb(bb2);
      new_ir_imm(true, type_size(&tyBool));
      new_ir_jmp(COND_ANY, next_bb);
      set_curbb(false_bb);
      new_ir_imm(false, type_size(&tyBool));
      set_curbb(next_bb);
    }
    break;

  case EX_LOGIOR:
    {
      BB *bb1 = bb_split(curbb);
      BB *bb2 = bb_split(bb1);
      BB *true_bb = bb_split(bb2);
      BB *next_bb = bb_split(true_bb);
      gen_cond_jmp(expr->u.bop.lhs, true, true_bb);
      set_curbb(bb1);
      gen_cond_jmp(expr->u.bop.rhs, true, true_bb);
      set_curbb(bb2);
      new_ir_imm(false, type_size(&tyBool));
      new_ir_jmp(COND_ANY, next_bb);
      set_curbb(true_bb);
      new_ir_imm(true, type_size(&tyBool));
      set_curbb(next_bb);
    }
    break;

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
      VReg *lhs = gen_expr(expr->u.bop.lhs);
      VReg *rhs = gen_expr(expr->u.bop.rhs);
      return gen_arith(expr->type, expr->valType, lhs, rhs);
    }
    break;

  default:
    fprintf(stderr, "Expr type=%d, ", expr->type);
    assert(!"Unhandled in gen_expr");
    break;
  }

  return NULL;
}
