#include "sema.h"

#include <assert.h>
#include <stdlib.h>
#include <string.h>

#include "ast.h"
#include "lexer.h"
#include "table.h"
#include "type.h"
#include "util.h"
#include "var.h"

#include "parser.h"  // curscope

Vector *toplevel;

// Returns created global variable info.
VarInfo *str_to_char_array(const Type *type, Initializer *init) {
  assert(type->kind == TY_ARRAY && is_char_type(type->pa.ptrof));
  const Token *ident = alloc_ident(alloc_label(), NULL, NULL);
  VarInfo *varinfo;
  if (curscope != NULL) {
    varinfo = add_cur_scope(ident, type, VF_CONST | VF_STATIC);
  } else {
    varinfo = define_global(type, VF_CONST | VF_STATIC, ident, NULL);

    Vector *decls = new_vector();
    vec_push(decls, new_vardecl(varinfo->type, ident, init, varinfo->flag));
    vec_push(toplevel, new_decl_vardecl(decls));
  }
  varinfo->global.init = init;
  return varinfo;
}

// Call before accessing struct member to ensure that struct is declared.
void ensure_struct(Type *type, const Token *token) {
  assert(type->kind == TY_STRUCT);
  if (type->struct_.info == NULL) {
    StructInfo *sinfo = find_struct(type->struct_.name);
    if (sinfo == NULL)
      parse_error(token, "Accessing unknown struct(%.*s)'s member", type->struct_.name->bytes,
                  type->struct_.name->chars);
    type->struct_.info = sinfo;
  }
}

bool check_cast(const Type *dst, const Type *src, bool zero, bool is_explicit, const Token *token) {
  if (!can_cast(dst, src, zero, is_explicit)) {
    parse_error(token, "Cannot convert value from type %d to %d", src->kind, dst->kind);
    return false;
  }
  if (dst->kind == TY_ARRAY) {
    parse_error(token, "Cannot cast to array type");
    return false;
  }
  return true;
}

Expr *make_cast(const Type *type, const Token *token, Expr *sub, bool is_explicit) {
  if (type->kind == TY_VOID || sub->type->kind == TY_VOID)
    parse_error(NULL, "cannot use `void' as a value");

  if (same_type(type, sub->type))
    return sub;
  //if (is_const(sub)) {
  //  // Casting number types needs its value range info,
  //  // so handlded in codegen.
  //  sub->type = type;
  //  return sub;
  //}

  check_cast(type, sub->type, is_zero(sub), is_explicit, token);

  return new_expr_cast(type, token, sub);
}

const VarInfo *search_from_anonymous(const Type *type, const Name *name, const Token *ident, Vector *stack) {
  assert(type->kind == TY_STRUCT);
  ensure_struct((Type*)type, ident);

  const Vector *members = type->struct_.info->members;
  for (int i = 0, len = members->len; i < len; ++i) {
    const VarInfo *member = members->data[i];
    if (member->name != NULL) {
      if (equal_name(member->name, name)) {
        vec_push(stack, (void*)(long)i);
        return member;
      }
    } else if (member->type->kind == TY_STRUCT) {
      vec_push(stack, (void*)(intptr_t)i);
      const VarInfo *submember = search_from_anonymous(member->type, name, ident, stack);
      if (submember != NULL)
        return submember;
      vec_pop(stack);
    }
  }
  return NULL;
}

// Traverse expr to check semantics and determine value type.
Expr *sema_expr(Expr *expr) {
  if (expr == NULL)
    return NULL;

  assert(expr->type != NULL);
  switch (expr->kind) {
  // Literals
  case EX_NUM:
  case EX_STR:
    break;

  case EX_VARIABLE:
    break;

  // Binary operators
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
  case EX_EQ:
  case EX_NE:
  case EX_LT:
  case EX_GT:
  case EX_LE:
  case EX_GE:
  case EX_LOGAND:
  case EX_LOGIOR:
  case EX_ASSIGN:
  case EX_COMMA:
  case EX_PTRADD:
  case EX_PTRSUB:
    expr->bop.lhs = sema_expr(expr->bop.lhs);
    expr->bop.rhs = sema_expr(expr->bop.rhs);

    switch (expr->kind) {
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
    case EX_EQ:
    case EX_NE:
    case EX_LT:
    case EX_GT:
    case EX_LE:
    case EX_GE:
    case EX_LOGAND:
    case EX_LOGIOR:
      break;

    case EX_ASSIGN:
      break;

    case EX_COMMA:
      break;

    case EX_PTRADD:
    case EX_PTRSUB:
      assert(ptr_or_array(expr->bop.lhs->type));
      assert(is_number(expr->bop.rhs->type->kind));
      break;

    default:
      fprintf(stderr, "expr kind=%d\n", expr->kind);
      assert(!"sema not handled!");
      break;
    }
    break;

  // Unary operators
  case EX_POS:
  case EX_NEG:
  case EX_NOT:
  case EX_BITNOT:
  case EX_PREINC:
  case EX_PREDEC:
  case EX_POSTINC:
  case EX_POSTDEC:
  case EX_REF:
  case EX_DEREF:
  case EX_GROUP:
  case EX_CAST:
  case EX_ASSIGN_WITH:
    expr->unary.sub = sema_expr(expr->unary.sub);

    switch (expr->kind) {
    case EX_POS:
    case EX_NEG:
    case EX_NOT:
    case EX_BITNOT:
      break;

    case EX_PREINC:
    case EX_PREDEC:
    case EX_POSTINC:
    case EX_POSTDEC:
      break;

    case EX_REF:
      break;

    case EX_DEREF:
      break;

    case EX_GROUP:
      break;

    case EX_ASSIGN_WITH:
      break;

    case EX_CAST:
      break;

    default:
      fprintf(stderr, "expr kind=%d\n", expr->kind);
      assert(!"sema not handled!");
      break;
    }
    break;

  case EX_TERNARY:
    expr->ternary.cond = sema_expr(expr->ternary.cond);
    expr->ternary.tval = sema_expr(expr->ternary.tval);
    expr->ternary.fval = sema_expr(expr->ternary.fval);
    break;

  case EX_MEMBER:  // x.member or x->member
    expr->member.target = sema_expr(expr->member.target);
    break;

  case EX_SIZEOF:
    break;

  case EX_FUNCALL:
    {
      expr->funcall.func = sema_expr(expr->funcall.func);
      Vector *args = expr->funcall.args;
      if (args != NULL) {
        for (int i = 0, len = args->len; i < len; ++i)
          args->data[i] = sema_expr(args->data[i]);
      }
    }
    break;

  default:
    fprintf(stderr, "expr kind=%d\n", expr->kind);
    assert(!"sema not handled!");
    break;
  }

  return expr;
}
