#include "../config.h"
#include <assert.h>

#include "ast.h"
#include "codegen.h"
#include "ir.h"
#include "lexer.h"
#include "parser.h"
#include "table.h"
#include "type.h"
#include "util.h"
#include "var.h"

static Expr *proc_builtin_type_kind(const Token *ident) {
  consume(TK_LPAR, "`(' expected");
  const Type *type = parse_var_def(NULL, NULL, NULL);
  consume(TK_RPAR, "`)' expected");

  return new_expr_fixlit(&tySize, ident, type->kind);
}

#if defined(VAARG_ON_STACK)
static VReg *gen_builtin_va_start(Expr *expr) {
  assert(expr->kind == EX_FUNCALL);
  Vector *args = expr->funcall.args;
  assert(args->len == 2);
  assert(curfunc != NULL);

  Expr *ap = args->data[0];
  if (ap->kind != EX_VAR || ap->type->kind != TY_PTR) {
    parse_error(PE_NOFATAL, ap->token, "Must be local variable");
    return NULL;
  }

  Scope *scope;
  const VarInfo *varinfo = scope_find(ap->var.scope, ap->var.name, &scope);
  assert(varinfo != NULL);
  if (is_global_scope(scope) || (varinfo->storage & (VS_STATIC | VS_EXTERN))) {
    parse_error(PE_FATAL, ap->token, "Must be local variable");
    return NULL;
  }

  VReg *ptr = new_ir_bofs(new_const_vreg(16, to_vtype(&tyVoidPtr)));  // TODO: Consider stack argument.
  new_ir_mov(varinfo->local.reg, ptr);
  return NULL;
}
#else
static VReg *gen_builtin_va_start(Expr *expr) {
  assert(expr->kind == EX_FUNCALL);
  Vector *args = expr->funcall.args;
  assert(args->len == 2);
  assert(curfunc != NULL);
  Expr *var = strip_cast(args->data[1]);
  if (var->kind == EX_REF)
    var = var->unary.sub;
  int gn = -1, fn = -1;
  if (var->kind == EX_VAR) {
    const Vector *params = curfunc->type->func.params;
    int g = 0, f = 0;
    for (int i = 0; i < params->len; ++i) {
      VarInfo *info = params->data[i];
      const Type *t = info->type;
      if (t->kind != TY_STRUCT) {
#ifndef __NO_FLONUM
        if (is_flonum(t))
          ++f;
        else
#endif
          ++g;
      }

      if (info->name != NULL && equal_name(info->name, var->var.name)) {
        gn = g;
        fn = f;
        break;
      }
    }
  }
  if (gn < 0) {
    parse_error(PE_FATAL, var->token, "Must be function argument");
    return NULL;
  }

  VReg *ap = gen_expr(args->data[0]);
  VReg *gp_offset = ap;
  new_ir_store(gp_offset, new_const_vreg(gn * WORD_SIZE, to_vtype(&tyInt)));

  VReg *fp_offset = new_ir_bop(IR_ADD, ap, new_const_vreg(type_size(&tyInt), to_vtype(&tySize)), ap->vtype);
  new_ir_store(fp_offset, new_const_vreg((MAX_REG_ARGS + fn) * WORD_SIZE, to_vtype(&tySize)));

  {
    const VRegType *vtype = to_vtype(&tyVoidPtr);
    VReg *overflow_arg_area = new_ir_bop(IR_ADD, ap, new_const_vreg(type_size(&tyInt) + type_size(&tyInt), vtype), vtype);
    VReg *offset = new_const_vreg(2 * WORD_SIZE, to_vtype(&tySize));
    VReg *p = new_ir_bofs(offset);
    new_ir_store(overflow_arg_area, p);
  }

  {
    const VRegType *vtype = to_vtype(&tyVoidPtr);
    VReg *reg_save_area = new_ir_bop(IR_ADD, ap, new_const_vreg(type_size(&tyInt) + type_size(&tyInt) + type_size(&tyVoidPtr), vtype), vtype);
    VReg *offset = new_const_vreg(-(MAX_REG_ARGS + MAX_FREG_ARGS) * WORD_SIZE, to_vtype(&tySize));
    VReg *p = new_ir_bofs(offset);
    new_ir_store(reg_save_area, p);
  }
  return NULL;
}
#endif

static VReg *gen_alloca(Expr *expr) {
  const int stack_align = 16;  // TODO
  assert(expr->kind == EX_FUNCALL);
  Vector *args = expr->funcall.args;
  assert(args->len == 1);
  assert(curfunc != NULL);
  Expr *size = args->data[0];
  const Token *token = size->token;
  Expr *aligned_size = new_expr_bop(EX_BITAND, &tySSize, token,
      new_expr_addsub(EX_ADD, token,
                      make_cast(&tySSize, token, size, false),
                      new_expr_fixlit(&tySSize, token, stack_align - 1), false),
      new_expr_fixlit(&tySSize, token, -stack_align));
  VReg *addend = gen_expr(aligned_size);
  VReg *result = add_new_reg(&tyVoidPtr, 0);
  new_ir_subsp(addend, result);
  curfunc->flag |= FUNCF_STACK_MODIFIED;
  return result;
}

void install_builtins(void) {
  static BuiltinExprProc p_reg_class = &proc_builtin_type_kind;
  add_builtin_expr_ident("__builtin_type_kind", &p_reg_class);

  {
#if defined(VAARG_ON_STACK)
    Type *tyVaList = ptrof(&tyVoidPtr);
#else
    Type *tyVaElem = create_struct_type(NULL, alloc_name("__va_elem", NULL, false), 0);
    Type *tyVaList = ptrof(tyVaElem);
#endif
    static BuiltinFunctionProc p_va_start = &gen_builtin_va_start;
    Vector *params = new_vector();
    var_add(params, NULL, tyVaList, 0);
    var_add(params, NULL, &tyVoidPtr, 0);

    Type *rettype = &tyVoid;
    Vector *param_types = extract_varinfo_types(params);
    Type *type = new_func_type(rettype, params, param_types, false);

    add_builtin_function("__builtin_va_start", type, &p_va_start, true);
  }
  {
    static BuiltinFunctionProc p_alloca = &gen_alloca;
    Vector *params = new_vector();
    var_add(params, NULL, &tySize, 0);

    Type *rettype = &tyVoidPtr;
    Vector *param_types = extract_varinfo_types(params);
    Type *type = new_func_type(rettype, params, param_types, false);

    add_builtin_function("alloca", type, &p_alloca, false);
  }
}
