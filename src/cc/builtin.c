#include <assert.h>

#include "ast.h"
#include "lexer.h"
#include "parser.h"
#include "table.h"
#include "type.h"
#include "util.h"
#include "var.h"

static Expr *proc_builtin_va_start(const Token *ident) {
  consume(TK_LPAR, "`(' expected");

  Token *token;
  Vector *args = parse_args(&token);
  if (args == NULL || args->len != 2) {
    parse_error(token, "two arguments expected");
    return NULL;
  }

  //#define va_start(ap, param)  (void)(ap = (va_list)&(param))

  const Type *tyvalist = find_typedef(curscope, alloc_name("__builtin_va_list", NULL, false), NULL);
  assert(tyvalist != NULL);

  Expr *ap = args->data[0];
  Expr *param = args->data[1];
  Expr *paramref = make_refer(param->token, param);
  Expr *assign = new_expr_bop(EX_ASSIGN, ap->type, ap->token, ap,
                              make_cast(tyvalist, paramref->token, paramref, true));
  return new_expr_cast(&tyVoid, ident, assign);
}

static Expr *proc_builtin_va_end(const Token *ident) {
  consume(TK_LPAR, "`(' expected");

  Token *token;
  Vector *args = parse_args(&token);
  if (args == NULL || args->len != 1) {
    parse_error(token, "one arguments expected");
    return NULL;
  }

  //#define va_end(ap)           (void)(ap = 0)

  Expr *ap = args->data[0];
  Expr *assign = new_expr_bop(EX_ASSIGN, ap->type, ap->token, ap,
                              new_expr_fixlit(&tyInt, ident, 0));
  return new_expr_cast(&tyVoid, ident, assign);
}

static Expr *proc_builtin_va_arg(const Token *ident) {
  consume(TK_LPAR, "`(' expected");
  Expr *ap = parse_assign();
  consume(TK_COMMA, "`,' expected");
  const Type *type = parse_full_type(NULL, NULL);
  consume(TK_RPAR, "`)' expected");

  //#define va_arg(v,l)     (*(type*)(ap += 1))  // Assume little endian

  Expr *add = new_expr_unary(EX_MODIFY, ap->type, ap->token,
                             new_expr_bop(EX_PTRADD, ap->type, ap->token, ap,
                                          new_expr_fixlit(&tySize, ident, 1)));
  Expr *deref = new_expr_deref(ap->token, make_cast(ptrof(type), ap->token, add, true));
  return deref;
}

static Expr *proc_builtin_va_copy(const Token *ident) {
  consume(TK_LPAR, "`(' expected");

  Token *token;
  Vector *args = parse_args(&token);
  if (args == NULL || args->len != 2) {
    parse_error(token, "two arguments expected");
    return NULL;
  }

  //#define va_start(ap, param)  (void)(ap = (va_list)&(param))

  Expr *dst = args->data[0];
  Expr *src = args->data[1];
  Expr *assign = new_expr_bop(EX_ASSIGN, dst->type, dst->token, dst, src);
  return new_expr_cast(&tyVoid, ident, assign);
}

void install_builtins(void) {
  // __builtin_va_list
  {
    const Type *type = ptrof(&tyVoidPtr);
    const Name *name = alloc_name("__builtin_va_list", NULL, false);
    add_typedef(global_scope, name, type);
  }

  add_builtin_expr_ident("__builtin_va_start", proc_builtin_va_start);
  add_builtin_expr_ident("__builtin_va_end", proc_builtin_va_end);
  add_builtin_expr_ident("__builtin_va_arg", proc_builtin_va_arg);
  add_builtin_expr_ident("__builtin_va_copy", proc_builtin_va_copy);
}
