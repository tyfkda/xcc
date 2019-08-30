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
  switch (type->type) {
  case TY_NUM:
    switch (type->u.num.type) {
    case NUM_CHAR:  TEST(AL, AL); break;
    case NUM_SHORT: TEST(AX, AX); break;
    case NUM_INT: case NUM_ENUM:
      TEST(EAX, EAX);
      break;
    case NUM_LONG:  TEST(RAX, RAX); break;
    default: assert(false); break;
    }
    break;
  case TY_PTR: case TY_ARRAY: case TY_FUNC:
    TEST(RAX, RAX);
    break;
  default: assert(false); break;
  }
}

static enum ExprType gen_compare_expr(enum ExprType type, Expr *lhs, Expr *rhs) {
  const Type *ltype = lhs->valType;
  UNUSED(ltype);
  assert(ltype->type == rhs->valType->type);

  if (rhs->type != EX_NUM && lhs->type == EX_NUM) {
    Expr *tmp = lhs;
    lhs = rhs;
    rhs = tmp;
    type = flip_cmp(type);
  }

  enum NumType numtype;
  switch (lhs->valType->type) {
  case TY_NUM:
    numtype = lhs->valType->u.num.type;
    break;
  default:
    assert(false);
    // Fallthrough to avoid compile error.
  case TY_PTR:
    numtype = NUM_LONG;
    break;
  }

  gen_expr(lhs);
  if (rhs->type == EX_NUM && rhs->u.num.ival == 0 &&
      (type == EX_EQ || type == EX_NE)) {
    gen_test_opcode(lhs->valType);
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
  } else {
    PUSH(RAX); PUSH_STACK_POS();
    gen_expr(rhs);
    POP(RDI); POP_STACK_POS();

    switch (numtype) {
    case NUM_CHAR: CMP(AL, DIL); break;
    case NUM_SHORT: CMP(AX, DI); break;
    case NUM_INT: case NUM_ENUM:
      CMP(EAX, EDI);
      break;
    case NUM_LONG: CMP(RAX, RDI); break;
    default: assert(false); break;
    }
  }

  return type;
}

void gen_cond_jmp(Expr *cond, bool tf, const char *label) {
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

  gen_expr(cond);
  gen_test_opcode(cond->valType);

  if (tf)
    JNE(label);
  else
    JE(label);
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
      LEA(LABEL_INDIRECT(expr->u.varref.ident, RIP), RAX);
    } else {
      Scope *scope = expr->u.varref.scope;
      VarInfo *varinfo = scope_find(&scope, expr->u.varref.ident);
      assert(varinfo != NULL);
      assert(!(varinfo->flag & VF_STATIC));
      int offset = varinfo->offset;
      LEA(OFFSET_INDIRECT(offset, RBP), RAX);
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
      if (varinfo->offset != 0)
        ADD(IM(varinfo->offset), RAX);
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
  case TY_PTR: MOV(INDIRECT(RAX), RAX); break;
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
  JMP(nlabel);
  EMIT_LABEL(flabel);
  gen_expr(expr->u.ternary.fval);
  EMIT_LABEL(nlabel);
}

static void gen_funcall(Expr *expr) {
  static const char *kReg64s[] = {RDI, RSI, RDX, RCX, R8, R9};

  Expr *func = expr->u.funcall.func;
  Vector *args = expr->u.funcall.args;
  int arg_count = args != NULL ? args->len : 0;

  int stack_args = MAX(arg_count - MAX_REG_ARGS, 0);
  bool align_stack = ((stackpos + stack_args * WORD_SIZE) & 15) != 0;
  if (align_stack) {
    SUB(IM(8), RSP); PUSH_STACK_POS();
  }

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
      PUSH(RAX); PUSH_STACK_POS();
    }

    int reg_args = MIN(len, MAX_REG_ARGS);
    for (int i = 0; i < reg_args; ++i) {
      POP(kReg64s[i]); POP_STACK_POS();
    }
  }

  if (func->type == EX_VARREF && func->u.varref.scope == NULL) {
    CALL(func->u.varref.ident);
  } else {
    gen_expr(func);
    CALL(fmt("*%s", RAX));
  }

  for (int i = 0; i < stack_args; ++i)
    POP_STACK_POS();

  int stack_add = stack_args * 8;
  if (align_stack) {
    stack_add += 8; POP_STACK_POS();
  }
  if (stack_add > 0)
    ADD(IM(stack_add), RSP);
}

void gen_arith(enum ExprType exprType, const Type *valType, const Type *rhsType) {
  // lhs=rax, rhs=rdi, result=rax

  switch (exprType) {
  case EX_ADD:
    switch (valType->type) {
    case TY_NUM:
      switch (valType->u.num.type) {
      case NUM_CHAR:  ADD(DIL, AL); break;
      case NUM_SHORT: ADD(DI, AX); break;
      case NUM_INT:   ADD(EDI, EAX); break;
      case NUM_LONG:  ADD(RDI, RAX); break;
      default: assert(false); break;
      }
      break;
    case TY_PTR:  ADD(RDI, RAX); break;
    default: assert(false); break;
    }
    break;

  case EX_SUB:
    switch (valType->type) {
    case TY_NUM:
      switch (valType->u.num.type) {
      case NUM_CHAR:  SUB(DIL, AL); break;
      case NUM_SHORT: SUB(DI, AX); break;
      case NUM_INT:   SUB(EDI, EAX); break;
      case NUM_LONG:  SUB(RDI, RAX); break;
      default: assert(false); break;
      }
      break;
    case TY_PTR:  SUB(RDI, RAX); break;
    default: assert(false); break;
    }
    break;

  case EX_MUL:
    assert(valType->type == TY_NUM);
    switch (valType->u.num.type) {
    case NUM_CHAR:  MUL(DIL); break;
    case NUM_SHORT: MUL(DI); break;
    case NUM_INT:   MUL(EDI); break;
    case NUM_LONG:  MUL(RDI); break;
    default: assert(false); break;
    }
    break;

  case EX_DIV:
    XOR(EDX, EDX);  // RDX = 0
    assert(valType->type == TY_NUM);
    switch (valType->u.num.type) {
    case NUM_CHAR:
      MOVSX(DIL, RDI);
      MOVSX(AL, EAX);
      CLTD();
      IDIV(EDI);
      break;
    case NUM_SHORT:
      MOVSX(DI, EDI);
      MOVSX(AX, EAX);
      // Fallthrough
    case NUM_INT:
      CLTD();
      IDIV(EDI);
      break;
    case NUM_LONG:
      CQTO();
      IDIV(RDI);
      break;
    default: assert(false); break;
    }
    break;

  case EX_MOD:
    XOR(EDX, EDX);  // RDX = 0
    assert(valType->type == TY_NUM);
    switch (valType->u.num.type) {
    case NUM_CHAR:  IDIV(DIL); MOV(DL, AL); break;
    case NUM_SHORT: IDIV(DI);  MOV(DX, AX); break;
    case NUM_INT:   IDIV(EDI); MOV(EDX, EAX); break;
    case NUM_LONG:  IDIV(RDI); MOV(RDX, RAX); break;
    default: assert(false); break;
    }
    break;

  case EX_BITAND:
    assert(valType->type == TY_NUM);
    switch (valType->u.num.type) {
    case NUM_CHAR:  AND(DIL, AL); break;
    case NUM_SHORT: AND(DI, AX); break;
    case NUM_INT:   AND(EDI, EAX); break;
    case NUM_LONG:  AND(RDI, RAX); break;
    default: assert(false); break;
    }
    break;

  case EX_BITOR:
    assert(valType->type == TY_NUM);
    switch (valType->u.num.type) {
    case NUM_CHAR:  OR(DIL, AL); break;
    case NUM_SHORT: OR(DI, AX); break;
    case NUM_INT: case NUM_ENUM:
      OR(EDI, EAX);
      break;
    case NUM_LONG:  OR(RDI, RAX); break;
    default: assert(false); break;
    }
    break;

  case EX_BITXOR:
    assert(valType->type == TY_NUM);
    switch (valType->u.num.type) {
    case NUM_CHAR:  XOR(DIL, AL); break;
    case NUM_SHORT: XOR(DI, AX); break;
    case NUM_INT:   XOR(EDI, EAX); break;
    case NUM_LONG:  XOR(RDI, RAX); break;
    default: assert(false); break;
    }
    break;

  case EX_LSHIFT:
  case EX_RSHIFT:
    assert(rhsType->type == TY_NUM);
    switch (rhsType->u.num.type) {
    case NUM_CHAR:  MOV(DIL, CL); break;
    case NUM_SHORT: MOV(DI, CX); break;
    case NUM_INT:   MOV(EDI, ECX); break;
    case NUM_LONG:  MOV(RDI, RCX); break;
    default: assert(false); break;
    }
    assert(valType->type == TY_NUM);
    if (exprType == EX_LSHIFT) {
      switch (valType->u.num.type) {
      case NUM_CHAR:  SHL(CL, AL); break;
      case NUM_SHORT: SHL(CL, AX); break;
      case NUM_INT:   SHL(CL, EAX); break;
      case NUM_LONG:  SHL(CL, RAX); break;
      default: assert(false); break;
      }
    } else {
      switch (valType->u.num.type) {
      case NUM_CHAR:  SHR(CL, AL); break;
      case NUM_SHORT: SHR(CL, AX); break;
      case NUM_INT:   SHR(CL, EAX); break;
      case NUM_LONG:  SHR(CL, RAX); break;
      default: assert(false); break;
      }
    }
    break;

  default:
    assert(false);
    break;
  }
}

static void gen_memcpy(ssize_t size) {
  const char *dst = RDI;
  const char *src = RAX;

  // Break %rcx, %dl
  switch (size) {
  case 1:
    MOV(INDIRECT(src), DL);
    MOV(DL, INDIRECT(dst));
    break;
  case 2:
    MOV(INDIRECT(src), DX);
    MOV(DX, INDIRECT(dst));
    break;
  case 4:
    MOV(INDIRECT(src), EDX);
    MOV(EDX, INDIRECT(dst));
    break;
  case 8:
    MOV(INDIRECT(src), RDX);
    MOV(RDX, INDIRECT(dst));
    break;
  default:
    {
      const char * label = alloc_label();
      PUSH(RAX);
      MOV(IM(size), RCX);
      EMIT_LABEL(label);
      MOV(INDIRECT(src), DL);
      MOV(DL, INDIRECT(dst));
      INC(src);
      INC(dst);
      DEC(RCX);
      JNE(label);
      POP(RAX);
    }
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

      LEA(LABEL_INDIRECT(label, RIP), RAX);
    }
    return;

  case EX_SIZEOF:
    {
      size_t size = type_size(expr->u.sizeof_.type);
      MOV(IM(size), RAX);
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
      enum NumType numtype = expr->u.unary.sub->valType->u.num.type;
      switch (numtype) {
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
    PUSH(RAX); PUSH_STACK_POS();
    gen_expr(expr->u.bop.rhs);

    POP(RDI); POP_STACK_POS();
    switch (expr->u.bop.lhs->valType->type) {
    case TY_NUM:
      switch (expr->u.bop.lhs->valType->u.num.type) {
      case NUM_CHAR:  MOV(AL, INDIRECT(RDI)); break;
      case NUM_SHORT: MOV(AX, INDIRECT(RDI)); break;
      case NUM_INT: case NUM_ENUM:
        MOV(EAX, INDIRECT(RDI));
        break;
      case NUM_LONG:  MOV(RAX, INDIRECT(RDI)); break;
      default: assert(false); break;
      }
      break;
    case TY_PTR:  MOV(RAX, INDIRECT(RDI)); break;
    case TY_STRUCT:
      {
        const StructInfo *sinfo = expr->u.bop.lhs->valType->u.struct_.info;
        ssize_t size = sinfo->size;
        assert(size > 0);
        gen_memcpy(size);
      }
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
    gen_lval(expr->u.unary.sub);
    switch (expr->valType->type) {
    case TY_NUM:
      switch (expr->valType->u.num.type) {
      case NUM_CHAR:
        if (expr->type == EX_PREINC)  INCB(INDIRECT(RAX));
        else                          DECB(INDIRECT(RAX));
        MOV(INDIRECT(RAX), AL);
        break;
      case NUM_SHORT:
        if (expr->type == EX_PREINC)  INCW(INDIRECT(RAX));
        else                          DECW(INDIRECT(RAX));
        MOV(INDIRECT(RAX), AX);
        break;
      case NUM_INT:
        if (expr->type == EX_PREINC)  INCL(INDIRECT(RAX));
        else                          DECL(INDIRECT(RAX));
        MOV(INDIRECT(RAX), EAX);
        break;
      case NUM_LONG:
        if (expr->type == EX_PREINC)  INCQ(INDIRECT(RAX));
        else                          DECQ(INDIRECT(RAX));
        MOV(INDIRECT(RAX), RAX);
        break;
      default: assert(false); break;
      }
      break;
    case TY_PTR:
      {
        MOV(RAX, RDI);
        size_t size = type_size(expr->valType->u.pa.ptrof);
        MOV(IM(expr->type == EX_PREINC ? size : -size), RAX);
        ADD(INDIRECT(RDI), RAX);
        MOV(RAX, INDIRECT(RDI));
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
    switch (expr->valType->type) {
    case TY_NUM:
      switch (expr->valType->u.num.type) {
      case NUM_CHAR:
        MOV(INDIRECT(RAX), DIL);
        if (expr->type == EX_POSTINC)  INCB(INDIRECT(RAX));
        else                           DECB(INDIRECT(RAX));
        MOV(DIL, AL);
        break;
      case NUM_SHORT:
        MOV(INDIRECT(RAX), DI);
        if (expr->type == EX_POSTINC)  INCW(INDIRECT(RAX));
        else                           DECW(INDIRECT(RAX));
        MOV(DI, AX);
        break;
      case NUM_INT:
        MOV(INDIRECT(RAX), EDI);
        if (expr->type == EX_POSTINC)  INCL(INDIRECT(RAX));
        else                           DECL(INDIRECT(RAX));
        MOV(EDI, EAX);
        break;
      case NUM_LONG:
        MOV(INDIRECT(RAX), RDI);
        if (expr->type == EX_POSTINC)  INCQ(INDIRECT(RAX));
        else                           DECQ(INDIRECT(RAX));
        MOV(RDI, RAX);
        break;
      default: assert(false); break;
      }
      break;
    case TY_PTR:
      {
        MOV(INDIRECT(RAX), RDI);
        size_t size = type_size(expr->valType->u.pa.ptrof);
        assert(size < ((size_t)1 << 31));  // TODO:
        if (expr->type == EX_POSTINC) {
          ADDQ(IM(size), INDIRECT(RAX));
        } else {
          SUBQ(IM(size), INDIRECT(RAX));
        }
        MOV(RDI, RAX);
      }
      break;
    default:
      assert(false);
      break;
    }
    return;

  case EX_FUNCALL:
    gen_funcall(expr);
    return;

  case EX_NEG:
    gen_expr(expr->u.unary.sub);
    assert(expr->valType->type == TY_NUM);
    switch (expr->u.unary.sub->valType->u.num.type) {
    case NUM_CHAR:   NEG(AL); break;
    case NUM_SHORT:  NEG(AX); break;
    case NUM_INT:    NEG(EAX); break;
    case NUM_LONG:   NEG(RAX); break;
    default:  assert(false); break;
    }
    break;

  case EX_NOT:
    gen_expr(expr->u.unary.sub);
    switch (expr->u.unary.sub->valType->type) {
    case TY_NUM:
      switch (expr->u.unary.sub->valType->u.num.type) {
      case NUM_CHAR:   TEST(AL, AL); break;
      case NUM_SHORT:  TEST(AX, AX); break;
      case NUM_INT:    TEST(EAX, EAX); break;
      case NUM_LONG:   TEST(RAX, RAX); break;
      default:  assert(false); break;
      }
      break;
    case TY_PTR: case TY_ARRAY: case TY_FUNC:
      TEST(RAX, RAX);
      break;
    default:  assert(false); break;
    }
    SETE(AL);
    MOVSX(AL, EAX);
    break;

  case EX_EQ:
  case EX_NE:
  case EX_LT:
  case EX_GT:
  case EX_LE:
  case EX_GE:
    {
      enum ExprType type = gen_compare_expr(expr->type, expr->u.bop.lhs, expr->u.bop.rhs);
      switch (type) {
      case EX_EQ:  SETE(AL); break;
      case EX_NE:  SETNE(AL); break;
      case EX_LT:  SETL(AL); break;
      case EX_GT:  SETG(AL); break;
      case EX_LE:  SETLE(AL); break;
      case EX_GE:  SETGE(AL); break;
      default: assert(false); break;
      }
    }
    MOVSX(AL, EAX);
    return;

  case EX_LOGAND:
    {
      const char *l_false = alloc_label();
      const char *l_next = alloc_label();
      gen_cond_jmp(expr->u.bop.lhs, false, l_false);
      gen_cond_jmp(expr->u.bop.rhs, false, l_false);
      MOV(IM(1), EAX);
      JMP(l_next);
      EMIT_LABEL(l_false);
      XOR(EAX, EAX);  // 0
      EMIT_LABEL(l_next);
    }
    return;

  case EX_LOGIOR:
    {
      const char *l_true = alloc_label();
      const char *l_next = alloc_label();
      gen_cond_jmp(expr->u.bop.lhs, true, l_true);
      gen_cond_jmp(expr->u.bop.rhs, true, l_true);
      XOR(EAX, EAX);  // 0
      JMP(l_next);
      EMIT_LABEL(l_true);
      MOV(IM(1), EAX);
      EMIT_LABEL(l_next);
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
    PUSH(RAX); PUSH_STACK_POS();
    gen_expr(expr->u.bop.lhs);

    POP(RDI); POP_STACK_POS();

    gen_arith(expr->type, expr->valType, expr->u.bop.rhs->valType);
    return;

  default:
    fprintf(stderr, "Expr type=%d, ", expr->type);
    assert(!"Unhandled in gen_expr");
    break;
  }
}
