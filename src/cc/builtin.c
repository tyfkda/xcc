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
  const Type *type = parse_full_type(NULL, NULL);
  consume(TK_RPAR, "`)' expected");

  return new_expr_fixlit(&tySize, ident, type->kind);
}

static VReg *gen_builtin_va_start(Expr *expr) {
  assert(expr->kind == EX_FUNCALL);
  Vector *args = expr->funcall.args;
  assert(args->len == 2);
  assert(curfunc != NULL);
  Expr *var = args->data[1];
  if (var->kind == EX_CAST)
    var = var->unary.sub;
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
    parse_error(var->token, "Must be function argument");
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

void install_builtins(void) {
  static BuiltinExprProc p_reg_class = &proc_builtin_type_kind;
  add_builtin_expr_ident("__builtin_type_kind", &p_reg_class);

  Type *tyVaList = create_struct_type(NULL, alloc_name("__va_elem", NULL, false), 0);
  const Type *tyVaListPtr = ptrof(tyVaList);
  {
    static BuiltinFunctionProc p_va_start = &gen_builtin_va_start;
    Vector *params = new_vector();
    var_add(params, NULL, tyVaListPtr, 0);
    var_add(params, NULL, &tyVoidPtr, 0);

    const Type *rettype = &tyVoid;
    Vector *param_types = extract_varinfo_types(params);
    const Type *type = new_func_type(rettype, params, param_types, false);

    add_builtin_function("__builtin_va_start", type, &p_va_start);
  }
}
