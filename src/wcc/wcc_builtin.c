#include "../config.h"
#include "wcc.h"

#include <alloca.h>
#include <assert.h>
#include <stdlib.h>  // strtoull

#ifndef __NO_FLONUM
#include <math.h>
#endif

#include "ast.h"
#include "expr.h"
#include "fe_misc.h"  // curfunc
#include "parser.h"  // parse_args
#include "table.h"
#include "type.h"
#include "util.h"
#include "var.h"
#include "wasm.h"
#include "wasm_obj.h"

extern int cur_depth;

static TagInfo *register_longjmp_tag(void) {
  static const char kTagName[] = "__c_longjmp";
  // Exception type: (jmp_buf *env)
  Vector *params = new_vector();
  vec_push(params, &tyVoidPtr);
  Type *functype = new_func_type(&tyVoid, params, false);
  int typeindex = getsert_func_type_index(functype, true);
  const Name *name = alloc_name(kTagName, NULL, false);
  return getsert_tag(name, typeindex);
}

static void gen_builtin_setjmp(Expr *expr, enum BuiltinFunctionPhase phase) {
  if (phase == BFP_TRAVERSE) {
    FuncExtra *extra = curfunc->extra;
    assert(extra != NULL);
    ++extra->setjmp_count;

    register_longjmp_tag();
    return;
  }

  UNUSED(expr);
  // Handled by traverse.
  assert(!"Unexpected");
}

static void gen_builtin_longjmp(Expr *expr, enum BuiltinFunctionPhase phase) {
  assert(expr->kind == EX_FUNCALL);
  Vector *args = expr->funcall.args;
  assert(args->len == 2);

  if (phase == BFP_TRAVERSE) {
    register_longjmp_tag();

    // Make args no side effect.
    const Token *tok = expr->token;
    for (int i = 0; i < args->len; ++i) {
      Expr *arg = args->data[i];
      if (!is_const(arg) && arg->kind != EX_VAR) {
        // (tmp = arg, tmp)
        Expr *tmp = alloc_tmp_var(curscope, arg->type);
        Expr *assign = new_expr_bop(EX_ASSIGN, &tyVoid, tok, tmp, arg);
        Expr *comma = new_expr_bop(EX_COMMA, &tyVoid, tok, assign, tmp);
        args->data[i] = comma;
      }
    }
  }
  if (phase != BFP_GEN)
    return;

  // env[1] = result == 0 ? 1 : result;
  Expr *env_org = args->data[0];
  gen_expr(env_org, true);

  Expr *result_org = args->data[1];
  if (is_const(result_org)) {
    assert(result_org->kind == EX_FIXNUM);
    if (result_org->fixnum != 0)
      gen_expr(args->data[1], true);
    else
      ADD_CODE(OP_I32_CONST, 1);
  } else {
    Expr *result = result_org->kind == EX_COMMA ? result_org->bop.rhs : result_org;
    gen_expr(result_org, true);
    ADD_CODE(OP_I32_CONST, 1);
    gen_expr(result, true);
    ADD_CODE(OP_SELECT);
  }
  ADD_CODE(OP_I32_STORE, 2, 4);

  Expr *env = env_org->kind == EX_COMMA ? env_org->bop.rhs : env_org;
  gen_expr(env, true);
  TagInfo *ti = register_longjmp_tag();
  ADD_CODE(OP_THROW);
  FuncExtra *extra = curfunc->extra;
  DataStorage *code = extra->code;
  RelocInfo *ri = calloc_or_die(sizeof(*ri));
  ri->type = R_WASM_TAG_INDEX_LEB;
  ri->offset = code->len;
  ri->addend = 0;
  ri->index = ti->symbol_index;
  vec_push(extra->reloc_code, ri);

  ADD_VARUINT32(ti->index);
}

static void gen_builtin_try_catch_longjmp(Expr *expr, enum BuiltinFunctionPhase phase) {
  if (phase != BFP_GEN)
    return;

  assert(expr->kind == EX_FUNCALL);
  Vector *args = expr->funcall.args;
  assert(args->len == 3);

  ADD_CODE(OP_BLOCK, WT_VOID); {
    ADD_CODE(OP_LOOP, WT_VOID); {
      cur_depth += 3;
      ADD_CODE(OP_TRY, WT_VOID); {
        Expr *try_block_expr = args->data[2];
        assert(try_block_expr->kind == EX_BLOCK);
        gen_stmt(try_block_expr->block, false);
        ADD_CODE(OP_BR, 2);
      }
      ADD_CODE(OP_CATCH); {
        TagInfo *ti = register_longjmp_tag();
        FuncExtra *extra = curfunc->extra;
        DataStorage *code = extra->code;
        RelocInfo *ri = calloc_or_die(sizeof(*ri));
        ri->type = R_WASM_TAG_INDEX_LEB;
        ri->offset = code->len;
        ri->addend = 0;
        ri->index = ti->symbol_index;
        vec_push(extra->reloc_code, ri);

        ADD_VARUINT32(ti->index);

        // Assume env has no side effect.
        Expr *env = args->data[0];
        gen_expr(env, true);
        ADD_CODE(OP_I32_NE,
                 OP_IF, WT_VOID,
                 OP_RETHROW, 1,
                 OP_END);
        Expr *var = args->data[1];
        assert(var != NULL && var->kind == EX_VAR);

        const VarInfo *varinfo = scope_find(var->var.scope, var->var.name, NULL);
        if (!(varinfo->storage & VS_REF_TAKEN) && !is_global_datsec_var(varinfo, var->var.scope)) {
          gen_expr(env, true);
          ADD_CODE(OP_I32_LOAD, 2, 4);
          gen_set_to_var(var);
        } else {
          gen_lval(var);
          gen_expr(env, true);
          ADD_CODE(OP_I32_LOAD, 2, 4);
          gen_store(var->type);
        }

        // Restore stack pointer: sp = *env;
        const Token *token = expr->token;
        Expr *spvar = get_sp_var();
        gen_expr(new_expr_bop(EX_ASSIGN, &tyVoid, token, spvar,
                              new_expr_unary(EX_DEREF, spvar->type, token, env)), false);
      } ADD_CODE(OP_END);
      ADD_CODE(OP_BR, 0);  // loop.
      cur_depth -= 3;
    } ADD_CODE(OP_END);
  } ADD_CODE(OP_END);
}

#ifndef __NO_FLONUM
static Expr *proc_builtin_nan(const Token *ident) {
  consume(TK_LPAR, "`(' expected");
  Expr *fmt = parse_assign();
  consume(TK_RPAR, "`)' expected");

  uint64_t significand = 0;
  if (fmt->kind == EX_STR) {
    const char *p = fmt->str.buf;
    int base = 10;
    if (p[0] == '0' && (p[1] == 'x' || p[1] == 'X')) {
      p += 2;
      base = 16;
    }
    significand = strtoull(p, NULL, base);
  } else {
    parse_error(PE_NOFATAL, fmt->token, "String literal expected");
  }

  const uint64_t MASK = ((uint64_t)1 << 52) - 1UL;
  union { double d; uint64_t q; } u;
  u.d = NAN;
  u.q = (u.q & ~MASK) | (significand & MASK);
  return new_expr_flolit(&tyDouble, ident, u.d);
}
#endif

static Expr *proc_builtin_va_start(const Token *ident) {
  if (curfunc == NULL || !curfunc->type->func.vaargs) {
    parse_error(PE_FATAL, ident, "`va_start' can only be used in a variadic function");
    return NULL;
  }

  consume(TK_LPAR, "`(' expected");

  Token *token;
  Vector *args = parse_args(&token);
  if (args->len != 2) {
    parse_error(PE_FATAL, token, "two arguments expected");
    return NULL;
  }

  Type *tyvalist = find_typedef(curscope, alloc_name("__builtin_va_list", NULL, false), NULL);
  assert(tyvalist != NULL);

  Expr *ap = args->data[0];
  Expr *param = args->data[1];
  if (param->kind != EX_VAR)
    parse_error(PE_FATAL, param->token, "variable expected");
  const Vector *funparams = curfunc->params;
  if (funparams == NULL ||
      !equal_name(((VarInfo*)funparams->data[funparams->len - 1])->ident->ident,
                  param->var.name)) {
    parse_error(PE_FATAL, param->token, "must be the last parameter");
    return NULL;
  }

  mark_var_used(ap);
  mark_var_used(param);

  Scope *top_scope = curscope;
  for (Scope *p = curscope; p = p->parent, !is_global_scope(p); )
    top_scope = p;

  // (void)(ap = __va_args__)
  const Name *name = alloc_name(VA_ARGS_NAME, NULL, false);
  Expr *va_args = new_expr_variable(name, tyvalist, param->token, top_scope);
  Expr *assign = new_expr_bop(EX_ASSIGN, ap->type, ap->token, ap, va_args);
  return new_expr_cast(&tyVoid, ident, assign);
}

static Expr *proc_builtin_va_end(const Token *ident) {
  consume(TK_LPAR, "`(' expected");

  Token *token;
  Vector *args = parse_args(&token);
  if (args->len != 1) {
    parse_error(PE_FATAL, token, "one arguments expected");
    return NULL;
  }

  // (void)(ap = 0)
  Expr *ap = args->data[0];
  Expr *assign = new_expr_bop(EX_ASSIGN, ap->type, ap->token, ap,
                              new_expr_fixlit(&tyInt, ident, 0));
  return new_expr_cast(&tyVoid, ident, assign);
}

static Expr *proc_builtin_va_arg(const Token *ident) {
  consume(TK_LPAR, "`(' expected");
  Expr *ap = parse_assign();
  consume(TK_COMMA, "`,' expected");
  Type *type = parse_var_def(NULL, NULL, NULL);
  consume(TK_RPAR, "`)' expected");

  mark_var_used(ap);

  // (ap = ALIGN((size_t)ap, _Alignof(type)) + sizeof(type), *(type*)((size_t)ap - sizeof(type)))
  bool indirect = is_stack_param(type);
  if (indirect)
    type = ptrof(type);
  const Token *tok = ap->token;
  size_t size = type_size(type);
  Expr *size_lit = new_expr_fixlit(&tySize, tok, size);
  if (size <= 0)
    return make_cast(&tyVoid, tok, size_lit, true);
  size_t align = align_size(type);
  assert(align > 0);
  Expr *cap = make_cast(&tySize, tok, ap, true);
  Expr *and = new_expr_bop(EX_BITAND, &tySize, tok,
                           new_expr_bop(EX_ADD, &tySize, tok, cap,
                                        new_expr_fixlit(&tySize, tok, align - 1)),
                           new_expr_fixlit(&tySize, tok, -align));
  Expr *add = new_expr_bop(EX_ASSIGN, &tyVoid, tok, ap,
                           new_expr_bop(EX_ADD, cap->type, tok, and, size_lit));
  Expr *deref = new_expr_deref(
      tok,
      make_cast(ptrof(type), tok,
                new_expr_bop(EX_SUB, cap->type, tok, cap, size_lit),
                true));
  Expr *expr = new_expr_bop(EX_COMMA, type, ident, add, deref);
  if (indirect)
    expr = new_expr_deref(tok, expr);
  return expr;
}

static Expr *proc_builtin_va_copy(const Token *ident) {
  consume(TK_LPAR, "`(' expected");

  Token *token;
  Vector *args = parse_args(&token);
  if (args->len != 2) {
    parse_error(PE_FATAL, token, "two arguments expected");
    return NULL;
  }

  // (void)(dst = src)
  Expr *dst = args->data[0];
  Expr *src = args->data[1];
  Expr *assign = new_expr_bop(EX_ASSIGN, dst->type, dst->token, dst, src);
  return new_expr_cast(&tyVoid, ident, assign);
}

static void gen_alloca(Expr *expr, enum BuiltinFunctionPhase phase) {
  assert(curfunc != NULL);
  FuncInfo *finfo = table_get(&func_info_table, curfunc->ident->ident);

  if (phase != BFP_GEN) {
    finfo->flag |= FF_STACK_MODIFIED;
    return;
  }

  assert(expr->kind == EX_FUNCALL);
  Vector *args = expr->funcall.args;
  assert(args->len == 1);
  assert(curfunc != NULL);
  Expr *size = args->data[0];
  const Token *token = size->token;
  Expr *aligned_size = new_expr_int_bop(
      EX_BITAND, token,
      new_expr_addsub(EX_ADD, token, make_cast(&tySSize, token, size, false),
                      new_expr_fixlit(&tySSize, token, STACK_ALIGN - 1)),
      new_expr_fixlit(&tySSize, token, -STACK_ALIGN));

  assert(finfo != NULL);
  Expr *lspvar = NULL;
  if (finfo->lspname != NULL)
    lspvar = new_expr_variable(finfo->lspname, &tyVoidPtr, NULL, curfunc->scopes->data[0]);

  Expr *gspvar = get_sp_var();
  Expr *modify_sp;
  Expr *updated = new_expr_bop(EX_SUB, &tySize, NULL, gspvar, aligned_size);
  if (lspvar == NULL) {
    modify_sp = new_expr_bop(EX_ASSIGN, &tyVoid, NULL, gspvar, updated);
  } else {
    modify_sp = new_expr_bop(EX_COMMA, &tyVoid, NULL,
                             new_expr_bop(EX_ASSIGN, &tyVoid, NULL, lspvar, updated),
                             new_expr_bop(EX_ASSIGN, &tyVoid, NULL, gspvar, lspvar));
  }
  gen_expr_stmt(modify_sp);

  Expr *result = lspvar != NULL ? lspvar : gspvar;
  size_t stack_work_size = finfo->stack_work_size;
  if (stack_work_size > 0) {
    result = new_expr_bop(EX_ADD, gspvar->type, NULL, result,
                          new_expr_fixlit(&tySize, NULL, stack_work_size));
  }
  gen_expr(result, true);
}

static void gen_builtin_clz(Expr *expr, enum BuiltinFunctionPhase phase) {
  assert(expr->kind == EX_FUNCALL);
  Vector *args = expr->funcall.args;
  assert(args->len == 1);
  Expr *value = args->data[0];

  switch (phase) {
  case BFP_TRAVERSE:
    break;
  case BFP_GEN:
    gen_expr(value, true);
    switch (type_size(value->type)) {
    case 4:  ADD_CODE(OP_I32_CLZ); break;
    case 8:  ADD_CODE(OP_I64_CLZ); break;
    default: assert(false); break;
    }
    break;
  }
}

static void gen_builtin_ctz(Expr *expr, enum BuiltinFunctionPhase phase) {
  assert(expr->kind == EX_FUNCALL);
  Vector *args = expr->funcall.args;
  assert(args->len == 1);
  Expr *value = args->data[0];

  switch (phase) {
  case BFP_TRAVERSE:
    break;
  case BFP_GEN:
    gen_expr(value, true);
    switch (type_size(value->type)) {
    case 4:  ADD_CODE(OP_I32_CTZ); break;
    case 8:  ADD_CODE(OP_I64_CTZ); break;
    default: assert(false); break;
    }
    break;
  }
}

static void gen_builtin_popcount(Expr *expr, enum BuiltinFunctionPhase phase) {
  assert(expr->kind == EX_FUNCALL);
  Vector *args = expr->funcall.args;
  assert(args->len == 1);
  Expr *value = args->data[0];

  switch (phase) {
  case BFP_TRAVERSE:
    break;
  case BFP_GEN:
    gen_expr(value, true);
    switch (type_size(value->type)) {
    case 4:  ADD_CODE(OP_I32_POPCNT); break;
    case 8:  ADD_CODE(OP_I64_POPCNT); break;
    default: assert(false); break;
    }
    break;
  }
}

static void gen_builtin_wasm_memory_size(Expr *expr, enum BuiltinFunctionPhase phase) {
  assert(expr->kind == EX_FUNCALL);
  Vector *args = expr->funcall.args;
  assert(args->len == 1);
  Expr *index = args->data[0];

  switch (phase) {
  case BFP_TRAVERSE:
    if (index->kind != EX_FIXNUM)
      parse_error(PE_NOFATAL, index->token, "Must be constant");
    break;
  case BFP_GEN:
    ADD_CODE(OP_MEMORY_SIZE);
    ADD_ULEB128(index->fixnum);
    break;
  }
}

static void gen_builtin_wasm_memory_grow(Expr *expr, enum BuiltinFunctionPhase phase) {
  assert(expr->kind == EX_FUNCALL);
  Vector *args = expr->funcall.args;
  assert(args->len == 2);
  Expr *index = args->data[0], *npages = args->data[1];

  switch (phase) {
  case BFP_TRAVERSE:
    if (index->kind != EX_FIXNUM)
      parse_error(PE_NOFATAL, index->token, "Must be constant");
    break;
  case BFP_GEN:
    gen_expr(npages, true);
    ADD_CODE(OP_MEMORY_GROW);
    ADD_ULEB128(index->fixnum);
    break;
  }
}

void install_builtins(void) {
  static BuiltinExprProc p_function_name = &proc_builtin_function_name;
  add_builtin_expr_ident("__FUNCTION__", &p_function_name);
  add_builtin_expr_ident("__func__", &p_function_name);

  // __builtin_va_list
  {
    Type *type = ptrof(&tyVoidPtr);
    const Name *name = alloc_name("__builtin_va_list", NULL, false);
    add_typedef(global_scope, name, type);
  }

#ifndef __NO_FLONUM
  static BuiltinExprProc p_nan = &proc_builtin_nan;
  add_builtin_expr_ident("__builtin_nan", &p_nan);
#endif

  static BuiltinExprProc p_va_start = &proc_builtin_va_start;
  static BuiltinExprProc p_va_end = &proc_builtin_va_end;
  static BuiltinExprProc p_va_arg = &proc_builtin_va_arg;
  static BuiltinExprProc p_va_copy = &proc_builtin_va_copy;

  add_builtin_expr_ident("__builtin_va_start", &p_va_start);
  add_builtin_expr_ident("__builtin_va_end", &p_va_end);
  add_builtin_expr_ident("__builtin_va_arg", &p_va_arg);
  add_builtin_expr_ident("__builtin_va_copy", &p_va_copy);

  {
    static BuiltinFunctionProc p_alloca = &gen_alloca;
    Type *rettype = &tyVoidPtr;
    Vector *params = new_vector();
    vec_push(params, &tySize);
    Type *type = new_func_type(rettype, params, false);
    add_builtin_function("alloca", type, &p_alloca, true);
  }

  {
    static BuiltinFunctionProc p_clz = &gen_builtin_clz;
    Vector *params = new_vector();
    vec_push(params, get_fixnum_type(FX_INT, true, 0));
    Type *type = new_func_type(&tyInt, params, false);
    add_builtin_function("__builtin_clz", type, &p_clz, true);

    Vector *paramsl = new_vector();
    vec_push(paramsl, get_fixnum_type(FX_LONG, true, 0));
    Type *typel = new_func_type(get_fixnum_type(FX_LONG, false, 0), paramsl, false);
    add_builtin_function("__builtin_clzl", typel, &p_clz, true);

    Vector *paramsll = new_vector();
    vec_push(paramsll, get_fixnum_type(FX_LLONG, true, 0));
    Type *typell = new_func_type(get_fixnum_type(FX_LLONG, false, 0), paramsll, false);
    add_builtin_function("__builtin_clzll", typell, &p_clz, true);
  }
  {
    static BuiltinFunctionProc p_ctz = &gen_builtin_ctz;
    Vector *params = new_vector();
    vec_push(params, get_fixnum_type(FX_INT, true, 0));
    Type *type = new_func_type(&tyInt, params, false);
    add_builtin_function("__builtin_ctz", type, &p_ctz, true);

    Vector *paramsl = new_vector();
    vec_push(paramsl, get_fixnum_type(FX_LONG, true, 0));
    Type *typel = new_func_type(get_fixnum_type(FX_LONG, false, 0), paramsl, false);
    add_builtin_function("__builtin_ctzl", typel, &p_ctz, true);

    Vector *paramsll = new_vector();
    vec_push(paramsll, get_fixnum_type(FX_LLONG, true, 0));
    Type *typell = new_func_type(get_fixnum_type(FX_LLONG, false, 0), paramsll, false);
    add_builtin_function("__builtin_ctzll", typell, &p_ctz, true);
  }
  {
    static BuiltinFunctionProc p_popcount = &gen_builtin_popcount;
    Vector *params = new_vector();
    vec_push(params, get_fixnum_type(FX_INT, true, 0));
    Type *type = new_func_type(&tyInt, params, false);
    add_builtin_function("__builtin_popcount", type, &p_popcount, true);

    Vector *paramsl = new_vector();
    vec_push(paramsl, get_fixnum_type(FX_LONG, true, 0));
    Type *typel = new_func_type(get_fixnum_type(FX_LONG, false, 0), paramsl, false);
    add_builtin_function("__builtin_popcountl", typel, &p_popcount, true);

    Vector *paramsll = new_vector();
    vec_push(paramsll, get_fixnum_type(FX_LLONG, true, 0));
    Type *typell = new_func_type(get_fixnum_type(FX_LLONG, false, 0), paramsll, false);
    add_builtin_function("__builtin_popcountll", typell, &p_popcount, true);
  }

  {
    static BuiltinFunctionProc p_memory_size = &gen_builtin_wasm_memory_size;
    Type *rettype = &tyInt;
    Vector *params = new_vector();
    vec_push(params, &tyInt);
    Type *type = new_func_type(rettype, params, false);
    add_builtin_function("__builtin_wasm_memory_size", type, &p_memory_size, true);
  }
  {
    static BuiltinFunctionProc p_memory_grow = &gen_builtin_wasm_memory_grow;
    Type *rettype = &tyInt;
    Vector *params = new_vector();
    vec_push(params, &tyInt);
    vec_push(params, &tySize);
    Type *type = new_func_type(rettype, params, false);
    add_builtin_function("__builtin_wasm_memory_grow", type, &p_memory_grow, true);
  }

  {
    static BuiltinFunctionProc p_setjmp = &gen_builtin_setjmp;
    Type *rettype = &tyInt;
    Vector *params = new_vector();
    vec_push(params, &tyVoidPtr);
    Type *type = new_func_type(rettype, params, false);
    add_builtin_function("__builtin_setjmp", type, &p_setjmp, true);
  }
  {
    static BuiltinFunctionProc p_longjmp = &gen_builtin_longjmp;
    Type *rettype = &tyInt;
    Vector *params = new_vector();
    vec_push(params, &tyVoidPtr);
    vec_push(params, &tyInt);
    Type *type = new_func_type(rettype, params, false);
    add_builtin_function("__builtin_longjmp", type, &p_longjmp, true);
  }
  {
    static BuiltinFunctionProc p_try_catch_longjmp = &gen_builtin_try_catch_longjmp;
    Type *rettype = &tyInt;
    Vector *params = new_vector();
    vec_push(params, &tyVoidPtr);  // jmpbuf
    vec_push(params, &tyInt);      // r
    vec_push(params, &tyVoid);     // try_block_expr
    Type *type = new_func_type(rettype, params, false);
    add_builtin_function("__builtin_try_catch_longjmp", type, &p_try_catch_longjmp, true);
  }
}
