#include "codegen.h"

#include <assert.h>
#include <stdlib.h>  // malloc

#include "expr.h"
#include "type.h"
#include "util.h"
#include "var.h"
#include "x86_64.h"

void gen_cond_jmp(Expr *cond, bool tf, const char *label);
static void gen_lval(Expr *expr);

static void cast(const Type *ltypep, const Type *rtypep) {
  enum eType ltype = ltypep->type;
  enum eType rtype = rtypep->type;

  if (ltype == rtype) {
    if (ltype == TY_NUM) {
      enum NumType lnumtype = ltypep->u.numtype;
      enum NumType rnumtype = rtypep->u.numtype;
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
        case NUM_CHAR: MOVSX_AL_AX(); return;
        case NUM_INT:  return;
        case NUM_LONG: return;
        default: assert(false); break;
        }
        break;
      case NUM_INT: case NUM_ENUM:
        switch (rnumtype) {
        case NUM_CHAR:  MOVSX_AL_EAX(); return;
        case NUM_SHORT: MOVSX_AX_EAX(); return;
        case NUM_INT:   return;
        case NUM_LONG:  return;
        case NUM_ENUM:  return;
        default: assert(false); break;
        }
        break;
      case NUM_LONG:
        switch (rnumtype) {
        case NUM_CHAR:  MOVSX_AL_RAX(); return;
        case NUM_SHORT: MOVSX_AX_RAX(); return;
        case NUM_INT: case NUM_ENUM:
          MOVSX_EAX_RAX();
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
      if (ltypep->u.numtype == NUM_LONG)
        return;
      break;
    default: assert(false); break;
    }
    break;
  case TY_PTR:
    switch (rtype) {
    case TY_NUM:
      switch (rtypep->u.numtype) {
      case NUM_INT:   MOVSX_EAX_RAX(); return;
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
    if (expr->u.varref.global) {
      LEA_LABEL32_RIP_RAX(expr->u.varref.ident);
    } else {
      VarInfo *varinfo = scope_find(curscope, expr->u.varref.ident);
      assert(varinfo != NULL);
      assert(!(varinfo->flag & VF_STATIC));
      int offset = varinfo->offset;
      LEA_OFS32_RBP_RAX(offset);
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
      assert(type->type == TY_STRUCT || type->type == TY_UNION);
      calc_struct_size(type->u.struct_.info, type->type == TY_UNION);
      Vector *members = type->u.struct_.info->members;
      VarInfo *varinfo = (VarInfo*)members->data[expr->u.member.index];

      if (expr->u.member.target->valType->type == TY_PTR)
        gen_expr(expr->u.member.target);
      else
        gen_ref(expr->u.member.target);
      if (varinfo->offset != 0)
        ADD_IM32_RAX(varinfo->offset);
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
    switch (expr->valType->u.numtype) {
    case NUM_CHAR:  MOV_IND_RAX_AL(); break;
    case NUM_SHORT: MOV_IND_RAX_AX(); break;
    case NUM_INT: case NUM_ENUM:
      MOV_IND_RAX_EAX();
      break;
    case NUM_LONG:  MOV_IND_RAX_RAX(); break;
    default: assert(false); break;
    }
    break;
  case TY_PTR: MOV_IND_RAX_RAX(); break;
  case TY_ARRAY: break;  // Use variable address as a pointer.
  case TY_FUNC:  break;
  default: assert(false); break;
  }
}

static void gen_ternary(Expr *expr) {
  const char *nlabel = alloc_label();
  const char *flabel = alloc_label();
  gen_cond_jmp(expr->u.ternary.cond, false, flabel);
  gen_expr(expr->u.ternary.tval);
  JMP32(nlabel);
  ADD_LABEL(flabel);
  gen_expr(expr->u.ternary.fval);
  ADD_LABEL(nlabel);
}

static void gen_funcall(Expr *expr) {
  Vector *args = expr->u.funcall.args;
  if (args != NULL) {
    int len = args->len;
    if (len > 6)
      error("Param count exceeds 6 (%d)", len);

    for (int i = 0; i < len; ++i) {
      gen_expr((Expr*)args->data[i]);
      PUSH_RAX();
    }

    switch (len) {
    case 6:  POP_R9();  // Fallthrough
    case 5:  POP_R8();  // Fallthrough
    case 4:  POP_RCX();  // Fallthrough
    case 3:  POP_RDX();  // Fallthrough
    case 2:  POP_RSI();  // Fallthrough
    case 1:  POP_RDI();  // Fallthrough
    default: break;
    }
  }

  bool align_stack = (stackpos & 15) != 0;
  if (align_stack)
    SUB_IM8_RSP(8);

  Expr *func = expr->u.funcall.func;
  if (func->type == EX_VARREF && func->u.varref.global) {
    CALL(func->u.varref.ident);
  } else {
    gen_expr(func);
    CALL_IND_RAX();
  }

  if (align_stack)
    ADD_IM8_RSP(8);
}

void gen_arith(enum ExprType exprType, const Type *valType, const Type *rhsType) {
  // lhs=rax, rhs=rdi, result=rax

  switch (exprType) {
  case EX_ADD:
    switch (valType->type) {
    case TY_NUM:
      switch (valType->u.numtype) {
      case NUM_CHAR:  ADD_DIL_AL(); break;
      case NUM_SHORT: ADD_DI_AX(); break;
      case NUM_INT:   ADD_EDI_EAX(); break;
      case NUM_LONG:  ADD_RDI_RAX(); break;
      default: assert(false); break;
      }
      break;
    case TY_PTR:  ADD_RDI_RAX(); break;
    default: assert(false); break;
    }
    break;

  case EX_SUB:
    switch (valType->type) {
    case TY_NUM:
      switch (valType->u.numtype) {
      case NUM_CHAR:  SUB_DIL_AL(); break;
      case NUM_SHORT: SUB_DI_AX(); break;
      case NUM_INT:   SUB_EDI_EAX(); break;
      case NUM_LONG:  SUB_RDI_RAX(); break;
      default: assert(false); break;
      }
      break;
    case TY_PTR:  SUB_RDI_RAX(); break;
    default: assert(false); break;
    }
    break;

  case EX_MUL:
    assert(valType->type == TY_NUM);
    switch (valType->u.numtype) {
    case NUM_CHAR:  MUL_DIL(); break;
    case NUM_SHORT: MUL_DI(); break;
    case NUM_INT:   MUL_EDI(); break;
    case NUM_LONG:  MUL_RDI(); break;
    default: assert(false); break;
    }
    break;

  case EX_DIV:
    XOR_EDX_EDX();  // MOV_IM32_RDX(0);
    assert(valType->type == TY_NUM);
    switch (valType->u.numtype) {
    case NUM_CHAR:  DIV_DIL(); break;
    case NUM_SHORT: DIV_DI(); break;
    case NUM_INT:   DIV_EDI(); break;
    case NUM_LONG:  DIV_RDI(); break;
    default: assert(false); break;
    }
    break;

  case EX_MOD:
    XOR_EDX_EDX();  // MOV_IM32_RDX(0);
    assert(valType->type == TY_NUM);
    switch (valType->u.numtype) {
    case NUM_CHAR:  DIV_DIL(); MOV_DL_AL(); break;
    case NUM_SHORT: DIV_DI();  MOV_DX_AX(); break;
    case NUM_INT:   DIV_EDI(); MOV_EDX_EAX(); break;
    case NUM_LONG:  DIV_RDI(); MOV_RDX_RAX(); break;
    default: assert(false); break;
    }
    break;

  case EX_BITAND:
    assert(valType->type == TY_NUM);
    switch (valType->u.numtype) {
    case NUM_CHAR:  AND_DIL_AL(); break;
    case NUM_SHORT: AND_DI_AX(); break;
    case NUM_INT:   AND_EDI_EAX(); break;
    case NUM_LONG:  AND_RDI_RAX(); break;
    default: assert(false); break;
    }
    break;

  case EX_BITOR:
    assert(valType->type == TY_NUM);
    switch (valType->u.numtype) {
    case NUM_CHAR:  OR_DIL_AL(); break;
    case NUM_SHORT: OR_DI_AX(); break;
    case NUM_INT: case NUM_ENUM:
      OR_EDI_EAX();
      break;
    case NUM_LONG:  OR_RDI_RAX(); break;
    default: assert(false); break;
    }
    break;

  case EX_BITXOR:
    assert(valType->type == TY_NUM);
    switch (valType->u.numtype) {
    case NUM_CHAR:  XOR_DIL_AL(); break;
    case NUM_SHORT: XOR_DI_AX(); break;
    case NUM_INT:   XOR_EDI_EAX(); break;
    case NUM_LONG:  XOR_RDI_RAX(); break;
    default: assert(false); break;
    }
    break;

  case EX_LSHIFT:
  case EX_RSHIFT:
    assert(rhsType->type == TY_NUM);
    switch (rhsType->u.numtype) {
    case NUM_CHAR:  MOV_DIL_CL(); break;
    case NUM_SHORT: MOV_DI_CX(); break;
    case NUM_INT:   MOV_EDI_ECX(); break;
    case NUM_LONG:  MOV_RDI_RCX(); break;
    default: assert(false); break;
    }
    assert(valType->type == TY_NUM);
    if (exprType == EX_LSHIFT) {
      switch (valType->u.numtype) {
      case NUM_CHAR:  SHL_CL_AL(); break;
      case NUM_SHORT: SHL_CL_AX(); break;
      case NUM_INT:   SHL_CL_EAX(); break;
      case NUM_LONG:  SHL_CL_RAX(); break;
      default: assert(false); break;
      }
    } else {
      switch (valType->u.numtype) {
      case NUM_CHAR:  SHR_CL_AL(); break;
      case NUM_SHORT: SHR_CL_AX(); break;
      case NUM_INT:   SHR_CL_EAX(); break;
      case NUM_LONG:  SHR_CL_RAX(); break;
      default: assert(false); break;
      }
    }
    break;

  default:
    assert(false);
    break;
  }
}

void gen_expr(Expr *expr) {
  switch (expr->type) {
  case EX_NUM:
    switch (expr->valType->u.numtype) {
    case NUM_CHAR:
      if (expr->u.num.ival == 0)
        XOR_AL_AL();
      else
        MOV_IM8_AL(expr->u.num.ival);
      return;

    case NUM_INT:
    case NUM_ENUM:
      if (expr->u.num.ival == 0)
        XOR_EAX_EAX();
      else
        MOV_IM32_EAX(expr->u.num.ival);
      return;

    case NUM_LONG:
      if (expr->u.num.ival == 0)
        XOR_EAX_EAX();  // upper 32bit is also cleared.
      else if (expr->u.num.ival <= 0x7fffffffL && expr->u.num.ival >= -0x80000000L)
        MOV_IM32_RAX(expr->u.num.ival);
      else
        MOV_IM64_RAX(expr->u.num.ival);
      return;

    default: assert(false); break;
    }
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

      LEA_LABEL32_RIP_RAX(label);
    }
    return;

  case EX_SIZEOF:
    {
      size_t size = type_size(expr->u.sizeof_.type);
      if (size <= 0x7fffffffL)
        MOV_IM32_RAX(size);
      else
        MOV_IM64_RAX(size);
    }
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
      switch (expr->valType->u.numtype) {
      case NUM_CHAR:  MOV_IND_RAX_AL(); break;
      case NUM_SHORT: MOV_IND_RAX_AX(); break;
      case NUM_INT: case NUM_ENUM:
        MOV_IND_RAX_EAX();
        break;
      case NUM_LONG:  MOV_IND_RAX_RAX(); break;
      default: assert(false); break;
      }
      break;
    case TY_PTR:  MOV_IND_RAX_RAX(); break;
    case TY_ARRAY: break;
    default: assert(false); break;
    }
    return;

  case EX_MEMBER:
    gen_lval(expr);
    switch (expr->valType->type) {
    case TY_NUM:
      switch (expr->valType->u.numtype) {
      case NUM_CHAR:  MOV_IND_RAX_AL(); break;
      case NUM_SHORT: MOV_IND_RAX_AX(); break;
      case NUM_INT: case NUM_ENUM:
        MOV_IND_RAX_EAX();
        break;
      case NUM_LONG:  MOV_IND_RAX_RAX(); break;
      default: assert(false); break;
      }
      break;
    case TY_PTR:  MOV_IND_RAX_RAX(); break;
    case TY_ARRAY:
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
    gen_expr(expr->u.cast.sub);
    cast(expr->valType, expr->u.cast.sub->valType);
    break;

  case EX_ASSIGN:
    gen_lval(expr->u.bop.lhs);
    PUSH_RAX(); PUSH_STACK_POS();
    gen_expr(expr->u.bop.rhs);

    POP_RDI(); POP_STACK_POS();
    switch (expr->u.bop.lhs->valType->type) {
    case TY_NUM:
      switch (expr->u.bop.lhs->valType->u.numtype) {
      case NUM_CHAR:  MOV_AL_IND_RDI(); break;
      case NUM_SHORT: MOV_AX_IND_RDI(); break;
      case NUM_INT: case NUM_ENUM:
        MOV_EAX_IND_RDI();
        break;
      case NUM_LONG:  MOV_RAX_IND_RDI(); break;
      default: assert(false); break;
      }
      break;
    case TY_PTR:  MOV_RAX_IND_RDI(); break;
    default: assert(false); break;
    }
    return;

  case EX_ASSIGN_WITH:
    {
      Expr *sub = expr->u.unary.sub;
      gen_expr(sub->u.bop.rhs);
      PUSH_RAX(); PUSH_STACK_POS();
      gen_lval(sub->u.bop.lhs);
      MOV_RAX_RSI();  // Save lhs address to %rsi.

      // Move lhs to %?ax
      switch (expr->u.bop.lhs->valType->type) {
      case TY_NUM:
        switch (expr->u.bop.lhs->valType->u.numtype) {
        case NUM_CHAR:  MOV_IND_RAX_AL(); break;
        case NUM_SHORT: MOV_IND_RAX_AX(); break;
        case NUM_INT:   MOV_IND_RAX_EAX(); break;
        case NUM_LONG:  MOV_IND_RAX_RAX(); break;
        default: assert(false); break;
        }
        break;
      case TY_PTR:  MOV_IND_RAX_RAX(); break;
      default: assert(false); break;
      }

      POP_RDI(); POP_STACK_POS();  // %rdi=rhs
      gen_arith(sub->type, sub->valType, sub->u.bop.rhs->valType);
      cast(expr->valType, sub->valType);

      switch (expr->valType->type) {
      case TY_NUM:
        switch (expr->valType->u.numtype) {
        case NUM_CHAR:  MOV_AL_IND_RSI(); break;
        case NUM_SHORT: MOV_AX_IND_RSI(); break;
        case NUM_INT:   MOV_EAX_IND_RSI(); break;
        case NUM_LONG:  MOV_RAX_IND_RSI(); break;
        default: assert(false); break;
        }
        break;
      case TY_PTR:  MOV_RAX_IND_RSI(); break;
      default: assert(false); break;
      }
    }
    return;

  case EX_PREINC:
  case EX_PREDEC:
    gen_lval(expr->u.unary.sub);
    switch (expr->valType->type) {
    case TY_NUM:
      switch (expr->valType->u.numtype) {
      case NUM_CHAR:
        if (expr->type == EX_PREINC)  INCB_IND_RAX();
        else                          DECB_IND_RAX();
        MOV_IND_RAX_AL();
        break;
      case NUM_SHORT:
        if (expr->type == EX_PREINC)  INCW_IND_RAX();
        else                          DECW_IND_RAX();
        MOV_IND_RAX_AX();
        break;
      case NUM_INT:
        if (expr->type == EX_PREINC)  INCL_IND_RAX();
        else                          DECL_IND_RAX();
        MOV_IND_RAX_EAX();
        break;
      case NUM_LONG:
        if (expr->type == EX_PREINC)  INCQ_IND_RAX();
        else                          DECQ_IND_RAX();
        MOV_IND_RAX_RAX();
        break;
      default: assert(false); break;
      }
      break;
    case TY_PTR:
      {
        MOV_RAX_RDI();
        size_t size = type_size(expr->valType->u.pa.ptrof);
        MOV_IM32_RAX(expr->type == EX_PREINC ? size : -size);
        ADD_IND_RDI_RAX();
        MOV_RAX_IND_RDI();
      }
      break;
    default:
      assert(false);
      break;
    }
    return;

  case EX_POSTINC:
  case EX_POSTDEC:
    gen_lval(expr->u.unary.sub);
    MOV_IND_RAX_RDI();
    switch (expr->valType->type) {
    case TY_NUM:
      switch (expr->valType->u.numtype) {
      case NUM_CHAR:
        if (expr->type == EX_POSTINC)  INCB_IND_RAX();
        else                           DECB_IND_RAX();
        break;
      case NUM_SHORT:
        if (expr->type == EX_POSTINC)  INCW_IND_RAX();
        else                           DECW_IND_RAX();
        break;
      case NUM_INT:
        if (expr->type == EX_POSTINC)  INCL_IND_RAX();
        else                           DECL_IND_RAX();
        break;
      case NUM_LONG:
        if (expr->type == EX_POSTINC)  INCQ_IND_RAX();
        else                           DECQ_IND_RAX();
        break;
      default: assert(false); break;
      }
      break;
    case TY_PTR:
      {
        size_t size = type_size(expr->valType->u.pa.ptrof);
        assert(size < ((size_t)1 << 31));  // TODO:
        if (expr->type == EX_POSTINC) {
          if (size < 256)  ADDQ_IM8_IND_RAX(size);
          else             ADDQ_IM32_IND_RAX(size);
        } else {
          if (size < 256)  SUBQ_IM8_IND_RAX(size);
          else             SUBQ_IM32_IND_RAX(size);
        }
      }
      break;
    default:
      assert(false);
      break;
    }
    MOV_RDI_RAX();
    return;

  case EX_FUNCALL:
    gen_funcall(expr);
    return;

  case EX_NEG:
    gen_expr(expr->u.unary.sub);
    assert(expr->valType->type == TY_NUM);
    switch (expr->valType->u.numtype) {
    case NUM_CHAR: NEG_AL(); break;
    case NUM_INT:  NEG_EAX(); break;
    case NUM_LONG: NEG_RAX(); break;
    default:  assert(false); break;
    }
    break;

  case EX_NOT:
    gen_expr(expr->u.unary.sub);
    switch (expr->valType->type) {
    case TY_NUM:
      switch (expr->valType->u.numtype) {
      case NUM_CHAR: TEST_AL_AL(); break;
      case NUM_INT:  TEST_EAX_EAX(); break;
      default:  assert(false); break;
      }
      break;
    case TY_PTR:  TEST_RAX_RAX(); break;
    default:  assert(false); break;
    }
    SETE_AL();
    MOVZX_AL_EAX();
    break;

  case EX_EQ:
  case EX_NE:
  case EX_LT:
  case EX_GT:
  case EX_LE:
  case EX_GE:
    {
      enum ExprType type = expr->type;
      Expr *lhs = expr->u.bop.lhs;
      Expr *rhs = expr->u.bop.rhs;
      const Type *ltype = lhs->valType;

      assert(ltype->type == rhs->valType->type && (ltype->type != TY_NUM || ltype->u.numtype == rhs->valType->u.numtype));
      if (type == EX_LE || type == EX_GT) {
        Expr *tmp = lhs; lhs = rhs; rhs = tmp;
        type = type == EX_LE ? EX_GE : EX_LT;
      }

      gen_expr(lhs);
      PUSH_RAX(); PUSH_STACK_POS();
      gen_expr(rhs);

      POP_RDI(); POP_STACK_POS();
      switch (ltype->type) {
      case TY_NUM:
        switch (ltype->u.numtype) {
        case NUM_CHAR: CMP_AL_DIL(); break;
        case NUM_INT: case NUM_ENUM:
          CMP_EAX_EDI();
          break;
        case NUM_LONG: CMP_RAX_RDI(); break;
        default: assert(false); break;
        }
        break;
      case TY_PTR:  CMP_RAX_RDI(); break;
      default: assert(false); break;
      }

      switch (type) {
      case EX_EQ:  SETE_AL(); break;
      case EX_NE:  SETNE_AL(); break;
      case EX_LT:  SETS_AL(); break;
      case EX_GE:  SETNS_AL(); break;
      default: assert(false); break;
      }
    }
    MOVZX_AL_EAX();
    return;

  case EX_LOGAND:
    {
      const char * l_false = alloc_label();
      const char * l_true = alloc_label();
      const char * l_next = alloc_label();
      gen_cond_jmp(expr->u.bop.lhs, false, l_false);
      gen_cond_jmp(expr->u.bop.rhs, true, l_true);
      ADD_LABEL(l_false);
      XOR_EAX_EAX();  // 0
      JMP8(l_next);
      ADD_LABEL(l_true);
      MOV_IM32_EAX(1);
      ADD_LABEL(l_next);
    }
    return;

  case EX_LOGIOR:
    {
      const char * l_false = alloc_label();
      const char * l_true = alloc_label();
      const char * l_next = alloc_label();
      gen_cond_jmp(expr->u.bop.lhs, true, l_true);
      gen_cond_jmp(expr->u.bop.rhs, false, l_false);
      ADD_LABEL(l_true);
      MOV_IM32_EAX(1);
      JMP8(l_next);
      ADD_LABEL(l_false);
      XOR_EAX_EAX();  // 0
      ADD_LABEL(l_next);
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
    PUSH_RAX(); PUSH_STACK_POS();
    gen_expr(expr->u.bop.lhs);

    POP_RDI(); POP_STACK_POS();

    gen_arith(expr->type, expr->valType, expr->u.bop.rhs->valType);
    return;

  default:
    fprintf(stderr, "Expr type=%d, ", expr->type);
    assert(!"Unhandled in gen_expr");
    break;
  }
}
