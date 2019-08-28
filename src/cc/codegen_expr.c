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

static void gen_lval(Expr *expr);

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
#if 0  // Disable optimization for a while
  } else if (rhs->type == EX_NUM && (numtype != NUM_LONG || is_im32(rhs->u.num.ival))) {
    switch (numtype) {
    case NUM_CHAR: CMP(IM(rhs->u.num.ival), AL); break;
    case NUM_SHORT: CMP(IM(rhs->u.num.ival), AX); break;
    case NUM_INT: case NUM_ENUM:
      CMP(IM(rhs->u.num.ival), EAX);
      break;
    case NUM_LONG:
      CMP(IM(rhs->u.num.ival), RAX);
      break;
    default: assert(false); break;
    }
#endif
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

void gen_cond_jmp(Expr *cond, bool tf, const char *label) {
#if 0  // Disable optimization for a while
  // Local optimization: if `cond` is compare expression, then
  // jump using flags after CMP directly.
  switch (cond->type) {
  case EX_NUM:
    if (cond->u.num.ival == 0)
      tf = !tf;
    if (tf)
      JMP(label);
    return;

  case EX_EQ:
  case EX_NE:
    {
      enum ExprType type = gen_compare_expr(cond->type, cond->u.bop.lhs, cond->u.bop.rhs);
      if (type != EX_EQ)
        tf = !tf;
      if (tf)
        JE(label);
      else
        JNE(label);
      return;
    }
  case EX_LT:
  case EX_GT:
  case EX_LE:
  case EX_GE:
    {
      enum ExprType type = gen_compare_expr(cond->type, cond->u.bop.lhs, cond->u.bop.rhs);
      switch (type) {
      case EX_LT:
      case EX_GE:
        if (type != EX_LT)
          tf = !tf;
        if (tf)
          JL(label);
        else
          JGE(label);
        break;
      case EX_GT:
      case EX_LE:
        if (type != EX_GT)
          tf = !tf;
        if (tf)
          JG(label);
        else
          JLE(label);
        break;
      default:  assert(false); break;
      }
    }
    return;
  case EX_NOT:
    gen_cond_jmp(cond->u.unary.sub, !tf, label);
    return;
  case EX_LOGAND:
    if (!tf) {
      gen_cond_jmp(cond->u.bop.lhs, false, label);
      gen_cond_jmp(cond->u.bop.rhs, false, label);
    } else {
      const char *skip = alloc_label();
      gen_cond_jmp(cond->u.bop.lhs, false, skip);
      gen_cond_jmp(cond->u.bop.rhs, true, label);
      EMIT_LABEL(skip);
    }
    return;
  case EX_LOGIOR:
    if (tf) {
      gen_cond_jmp(cond->u.bop.lhs, true, label);
      gen_cond_jmp(cond->u.bop.rhs, true, label);
    } else {
      const char *skip = alloc_label();
      gen_cond_jmp(cond->u.bop.lhs, true, skip);
      gen_cond_jmp(cond->u.bop.rhs, false, label);
      EMIT_LABEL(skip);
    }
    return;
  default:
    break;
  }
#endif

  gen_expr(cond);
  gen_test_opcode(cond->valType);
  new_ir_jmp(tf ? COND_NE : COND_EQ, label);
}

static void gen_cast(const Type *ltypep, const Type *rtypep) {
  enum eType ltype = ltypep->type;
  enum eType rtype = rtypep->type;

  if (ltype == rtype) {
    if (ltype == TY_NUM) {
      enum NumType lnumtype = ltypep->u.num.type;
      enum NumType rnumtype = rtypep->u.num.type;
      if (lnumtype == rnumtype)
        return;

      switch (lnumtype) {
      case NUM_CHAR:
        switch (rnumtype) {
        case NUM_SHORT: return;
        case NUM_INT:   return;
        case NUM_LONG:  return;
        default: assert(false); break;
        }
        break;
      case NUM_SHORT:
        switch (rnumtype) {
        case NUM_CHAR: MOVSX(AL, AX); return;
        case NUM_INT:  return;
        case NUM_LONG: return;
        default: assert(false); break;
        }
        break;
      case NUM_INT: case NUM_ENUM:
        switch (rnumtype) {
        case NUM_CHAR:  MOVSX(AL, EAX); return;
        case NUM_SHORT: MOVSX(AX, EAX); return;
        case NUM_INT:   return;
        case NUM_LONG:  return;
        case NUM_ENUM:  return;
        default: assert(false); break;
        }
        break;
      case NUM_LONG:
        switch (rnumtype) {
        case NUM_CHAR:  MOVSX(AL, RAX); return;
        case NUM_SHORT: MOVSX(AX, RAX); return;
        case NUM_INT: case NUM_ENUM:
          MOVSX(EAX, RAX);
          return;
        default: assert(false); break;
        }
        break;
      default: assert(false); break;
      }
    }
    return;
  }

  switch (ltype) {
  case TY_VOID:
    return;
  case TY_NUM:
    switch (rtype) {
    case TY_PTR:
    case TY_ARRAY:
      if (ltypep->u.num.type == NUM_LONG)
        return;
      break;
    default: assert(false); break;
    }
    break;
  case TY_PTR:
    switch (rtype) {
    case TY_NUM:
      switch (rtypep->u.num.type) {
      case NUM_INT:   MOVSX(EAX, RAX); return;
      case NUM_LONG:  return;
      default: break;
      }
      break;
    case TY_ARRAY: case TY_FUNC:
      return;
    default: break;
    }
    assert(false);
    break;
  default: assert(false); break;
  }

  fprintf(stderr, "ltype=%d, rtype=%d\n", ltype, rtype);
  assert(!"Cast failed");
}

static void gen_rval(Expr *expr) {
  gen_expr(expr);  // ?
}

static void gen_ref(Expr *expr) {
  gen_lval(expr);
}

static void gen_lval(Expr *expr) {
  switch (expr->type) {
  case EX_VARREF:
    if (expr->u.varref.scope == NULL) {
      new_ir_iofs(expr->u.varref.ident);
    } else {
      Scope *scope = expr->u.varref.scope;
      VarInfo *varinfo = scope_find(&scope, expr->u.varref.ident);
      assert(varinfo != NULL);
      assert(!(varinfo->flag & VF_STATIC));
      int offset = varinfo->offset;
      new_ir_bofs(offset);
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
}

static void gen_varref(Expr *expr) {
  gen_lval(expr);
  switch (expr->valType->type) {
  case TY_NUM:
  case TY_PTR:
    new_ir_load(type_size(expr->valType));
    break;
  case TY_ARRAY: break;  // Use variable address as a pointer.
  case TY_FUNC:  break;
  case TY_STRUCT:
    // struct value is handled as a pointer.
    break;
  default: assert(false); break;
  }
}

static void gen_ternary(Expr *expr) {
  const char *nlabel = alloc_label();
  const char *flabel = alloc_label();
  gen_cond_jmp(expr->u.ternary.cond, false, flabel);
  gen_expr(expr->u.ternary.tval);
  new_ir_jmp(COND_ANY, nlabel);
  new_ir_label(flabel, false);
  gen_expr(expr->u.ternary.fval);
  new_ir_label(nlabel, false);
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
    CALL(fmt("*%s", RAX));
  }

  int stack_add = stack_args * 8;
  if (align_stack) {
    stack_add += 8;
  }
  if (stack_add > 0) {
    new_ir_addsp(stack_add);
  }
}

void gen_arith(enum ExprType exprType, const Type *valType, const Type *rhsType) {
  // lhs=rax, rhs=rdi, result=rax
  UNUSED(rhsType);

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
    new_ir_op(exprType + (IR_ADD - EX_ADD), type_size(valType));
    break;

  default:
    assert(false);
    break;
  }
}

void gen_expr(Expr *expr) {
  switch (expr->type) {
  case EX_NUM:
    assert(expr->valType->type == TY_NUM);
    new_ir_imm(expr->u.num.ival, type_size(expr->valType));
    break;

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
    return;

  case EX_SIZEOF:
    new_ir_imm(type_size(expr->u.sizeof_.type), type_size(expr->valType));
    return;

  case EX_VARREF:
    gen_varref(expr);
    return;

  case EX_REF:
    gen_ref(expr->u.unary.sub);
    return;

  case EX_DEREF:
    gen_rval(expr->u.unary.sub);
    switch (expr->valType->type) {
    case TY_NUM:
      switch (expr->valType->u.num.type) {
      case NUM_CHAR:  MOV(INDIRECT(RAX), AL); break;
      case NUM_SHORT: MOV(INDIRECT(RAX), AX); break;
      case NUM_INT: case NUM_ENUM:
        MOV(INDIRECT(RAX), EAX);
        break;
      case NUM_LONG:  MOV(INDIRECT(RAX), RAX); break;
      default: assert(false); break;
      }
      break;
    case TY_PTR:  MOV(INDIRECT(RAX), RAX); break;
    case TY_ARRAY: break;
    case TY_STRUCT:
      // struct value is handled as a pointer.
      break;
    default: assert(false); break;
    }
    return;

  case EX_MEMBER:
    gen_lval(expr);
    switch (expr->valType->type) {
    case TY_NUM:
      switch (expr->valType->u.num.type) {
      case NUM_CHAR:  MOV(INDIRECT(RAX), AL); break;
      case NUM_SHORT: MOV(INDIRECT(RAX), AX); break;
      case NUM_INT: case NUM_ENUM:
        MOV(INDIRECT(RAX), EAX);
        break;
      case NUM_LONG:  MOV(INDIRECT(RAX), RAX); break;
      default: assert(false); break;
      }
      break;
    case TY_PTR:  MOV(INDIRECT(RAX), RAX); break;
    case TY_ARRAY:
    case TY_STRUCT:
      break;
    default:
      assert(false);
      break;
    }
    return;

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
    gen_lval(expr->u.bop.lhs);
    new_ir_st(IR_PUSH);
    gen_expr(expr->u.bop.rhs);

    switch (expr->valType->type) {
    case TY_NUM:
    case TY_PTR:
      new_ir_store(type_size(expr->valType));
      break;
    case TY_STRUCT:
      new_ir_memcpy(expr->valType->u.struct_.info->size);
      break;
    default: assert(false); break;
    }
    return;

  case EX_ASSIGN_WITH:
    {
      Expr *sub = expr->u.unary.sub;
      gen_expr(sub->u.bop.rhs);
      PUSH(RAX); PUSH_STACK_POS();
      gen_lval(sub->u.bop.lhs);
      MOV(RAX, RSI);  // Save lhs address to %rsi.

      // Move lhs to %?ax
      switch (expr->u.bop.lhs->valType->type) {
      case TY_NUM:
        switch (expr->u.bop.lhs->valType->u.num.type) {
        case NUM_CHAR:  MOV(INDIRECT(RAX), AL); break;
        case NUM_SHORT: MOV(INDIRECT(RAX), AX); break;
        case NUM_INT:   MOV(INDIRECT(RAX), EAX); break;
        case NUM_LONG:  MOV(INDIRECT(RAX), RAX); break;
        default: assert(false); break;
        }
        break;
      case TY_PTR:  MOV(INDIRECT(RAX), RAX); break;
      default: assert(false); break;
      }

      POP(RDI); POP_STACK_POS();  // %rdi=rhs
      gen_arith(sub->type, sub->valType, sub->u.bop.rhs->valType);
      gen_cast(expr->valType, sub->valType);

      switch (expr->valType->type) {
      case TY_NUM:
        switch (expr->valType->u.num.type) {
        case NUM_CHAR:  MOV(AL, INDIRECT(RSI)); break;
        case NUM_SHORT: MOV(AX, INDIRECT(RSI)); break;
        case NUM_INT:   MOV(EAX, INDIRECT(RSI)); break;
        case NUM_LONG:  MOV(RAX, INDIRECT(RSI)); break;
        default: assert(false); break;
        }
        break;
      case TY_PTR:  MOV(RAX, INDIRECT(RSI)); break;
      default: assert(false); break;
      }
    }
    return;

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
    return;

  case EX_FUNCALL:
    gen_funcall(expr);
    return;

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
    return;

  case EX_LOGAND:
    {
      const char *l_false = alloc_label();
      const char *l_next = alloc_label();
      gen_cond_jmp(expr->u.bop.lhs, false, l_false);
      gen_cond_jmp(expr->u.bop.rhs, false, l_false);
      new_ir_imm(true, type_size(&tyBool));
      new_ir_jmp(COND_ANY, l_next);
      new_ir_label(l_false, false);
      new_ir_imm(false, type_size(&tyBool));
      new_ir_label(l_next, false);
    }
    return;

  case EX_LOGIOR:
    {
      const char *l_true = alloc_label();
      const char *l_next = alloc_label();
      gen_cond_jmp(expr->u.bop.lhs, true, l_true);
      gen_cond_jmp(expr->u.bop.rhs, true, l_true);
      new_ir_imm(false, type_size(&tyBool));
      new_ir_jmp(COND_ANY, l_next);
      new_ir_label(l_true, false);
      new_ir_imm(true, type_size(&tyBool));
      new_ir_label(l_next, false);
    }
    return;

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
    gen_expr(expr->u.bop.rhs);
    new_ir_st(IR_PUSH);
    gen_expr(expr->u.bop.lhs);
    gen_arith(expr->type, expr->valType, expr->u.bop.rhs->valType);
    return;

  default:
    fprintf(stderr, "Expr type=%d, ", expr->type);
    assert(!"Unhandled in gen_expr");
    break;
  }
}
