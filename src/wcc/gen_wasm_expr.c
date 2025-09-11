#include "../config.h"
#include "wcc.h"

#include <assert.h>

#include "ast.h"
#include "expr.h"
#include "fe_misc.h"  // curfunc, curscope
#include "table.h"
#include "type.h"
#include "util.h"
#include "var.h"
#include "wasm.h"
#include "wasm_obj.h"

DataStorage *curcodeds;

void add_code(const unsigned char *buf, size_t size) {
  data_append(CODE, buf, size);
}

////////////////////////////////////////////////

extern int cur_depth;

unsigned char get_func_ret_wtype(const Type *rettype) {
  return rettype->kind == TY_VOID ? WT_VOID
         : is_prim_type(rettype)  ? to_wtype(rettype)
                                  : WT_I32;  // Pointer.
}

static void gen_load(const Type *type) {
  switch (type->kind) {
  case TY_FIXNUM:
  case TY_PTR:
    {
      bool u = !is_fixnum(type->kind) || type->fixnum.is_unsigned;
      switch (type_size(type)) {
      case 1:  ADD_CODE(u ? OP_I32_LOAD8_U : OP_I32_LOAD8_S, 0, 0); break;
      case 2:  ADD_CODE(u ? OP_I32_LOAD16_U : OP_I32_LOAD16_S, 1, 0); break;
      case 4:  ADD_CODE(OP_I32_LOAD, 2, 0); break;
      case 8:  ADD_CODE(OP_I64_LOAD, 3, 0); break;
      default: assert(false);
      }
    }
    break;
  case TY_FLONUM:
    if (type->flonum.kind < FL_DOUBLE)
      ADD_CODE(OP_F32_LOAD, 2, 0);
    else
      ADD_CODE(OP_F64_LOAD, 3, 0);
    break;
  default: assert(false); break;
  }
}

void gen_store(const Type *type) {
  switch (type->kind) {
  case TY_FIXNUM:
  case TY_PTR:
    switch (type_size(type)) {
    case 1:  ADD_CODE(OP_I32_STORE8, 0, 0); break;
    case 2:  ADD_CODE(OP_I32_STORE16, 1, 0); break;
    case 4:  ADD_CODE(OP_I32_STORE, 2, 0); break;
    case 8:  ADD_CODE(OP_I64_STORE, 3, 0); break;
    default: assert(false); break;
    }
    break;
  case TY_FLONUM:
    if (type->flonum.kind < FL_DOUBLE)
      ADD_CODE(OP_F32_STORE, 2, 0);
    else
      ADD_CODE(OP_F64_STORE, 3, 0);
    break;
  default: assert(false); break;
  }
}

static void gen_arith(enum ExprKind kind, const Type *type) {
  assert(is_number(type) || ptr_or_array(type));
  int index = 0;
  bool is_unsigned = is_fixnum(type->kind) && type->fixnum.is_unsigned;
  if (is_flonum(type)) {
    assert(kind < EX_MOD);
    index = type->flonum.kind >= FL_DOUBLE ? 3 : 2;
  } else {
    index = type_size(type) > I32_SIZE ? 1 : 0;
  }

  static const unsigned char kOpTable[][4][EX_RSHIFT - EX_ADD + 1] = {
    {
      {OP_I32_ADD, OP_I32_SUB, OP_I32_MUL, OP_I32_DIV_S, OP_I32_REM_S, OP_I32_AND, OP_I32_OR, OP_I32_XOR, OP_I32_SHL, OP_I32_SHR_S},
      {OP_I64_ADD, OP_I64_SUB, OP_I64_MUL, OP_I64_DIV_S, OP_I64_REM_S, OP_I64_AND, OP_I64_OR, OP_I64_XOR, OP_I64_SHL, OP_I64_SHR_S},
      {OP_F32_ADD, OP_F32_SUB, OP_F32_MUL, OP_F32_DIV, OP_NOP, OP_NOP, OP_NOP, OP_NOP, OP_NOP, OP_NOP},
      {OP_F64_ADD, OP_F64_SUB, OP_F64_MUL, OP_F64_DIV, OP_NOP, OP_NOP, OP_NOP, OP_NOP, OP_NOP, OP_NOP},
    },
    {
      {OP_I32_ADD, OP_I32_SUB, OP_I32_MUL, OP_I32_DIV_U, OP_I32_REM_U, OP_I32_AND, OP_I32_OR, OP_I32_XOR, OP_I32_SHL, OP_I32_SHR_U},
      {OP_I64_ADD, OP_I64_SUB, OP_I64_MUL, OP_I64_DIV_U, OP_I64_REM_U, OP_I64_AND, OP_I64_OR, OP_I64_XOR, OP_I64_SHL, OP_I64_SHR_U},
      {OP_F32_ADD, OP_F32_SUB, OP_F32_MUL, OP_F32_DIV, OP_NOP, OP_NOP, OP_NOP, OP_NOP, OP_NOP, OP_NOP},
      {OP_F64_ADD, OP_F64_SUB, OP_F64_MUL, OP_F64_DIV, OP_NOP, OP_NOP, OP_NOP, OP_NOP, OP_NOP, OP_NOP},
    },
  };

  assert(EX_ADD <= kind && kind <= EX_RSHIFT);
  assert(kOpTable[is_unsigned][index][kind - EX_ADD] != OP_NOP);
  ADD_CODE(kOpTable[is_unsigned][index][kind - EX_ADD]);
}

static void gen_cast_to(const Type *dst, Type *src) {
  if (dst->kind == TY_VOID) {
    ADD_CODE(OP_DROP);
    return;
  }
  if (src->kind == TY_ARRAY)
    src = array_to_ptr(src);

  switch (dst->kind) {
  case TY_FIXNUM: case TY_PTR:
    switch (src->kind) {
    case TY_FIXNUM: case TY_PTR: case TY_FUNC:
      {
        size_t d = type_size(dst), s = type_size(src);
        bool du = dst->kind != TY_FIXNUM || dst->fixnum.is_unsigned;
        bool su = src->kind != TY_FIXNUM || src->fixnum.is_unsigned;
        enum { I64TO32 = 1, I32TO64 = 2 };
        switch ((d > I32_SIZE ? 2 : 0) + (s > I32_SIZE ? 1 : 0)) {
        case I64TO32: ADD_CODE(OP_I32_WRAP_I64); break;
        case I32TO64: ADD_CODE(su ? OP_I64_EXTEND_I32_U : OP_I64_EXTEND_I32_S); break;
        default: break;
        }
        if (d < s) {
          assert(d <= I32_SIZE);
          if (d < I32_SIZE) {
            if (du) {
              ADD_CODE(OP_I32_CONST);
              ADD_LEB128((1U << (d * TARGET_CHAR_BIT)) - 1);
              ADD_CODE(OP_I32_AND);
            } else {
              ADD_CODE(OP_I32_EXTEND8_S + most_significant_bit(d));
            }
          }
        } else if (du != su) {
          if (du) {  // unsigned <- signed
            if (d < I32_SIZE && s < I32_SIZE) {
              ADD_CODE(OP_I32_CONST);
              ADD_LEB128((1U << (d * TARGET_CHAR_BIT)) - 1);
              ADD_CODE(OP_I32_AND);
            }
          } else {  // signed <- unsigned
            if (d < I32_SIZE)
              ADD_CODE(OP_I32_EXTEND8_S + most_significant_bit(d));
          }
        }
      }
      return;
    case TY_FLONUM:
      {
        static const unsigned char OpTable[][4] = {
          { OP_I32_TRUNC_F32_S, OP_I32_TRUNC_F64_S, OP_I64_TRUNC_F32_S, OP_I64_TRUNC_F64_S },
          { OP_I32_TRUNC_F32_U, OP_I32_TRUNC_F64_U, OP_I64_TRUNC_F32_U, OP_I64_TRUNC_F64_U },
        };
        int d = type_size(dst);
        int index = (d > I32_SIZE ? 2 : 0) + (src->flonum.kind >= FL_DOUBLE ? 1 : 0);
        bool du = !is_fixnum(dst->kind) || dst->fixnum.is_unsigned;
        ADD_CODE(OpTable[du][index]);
      }
      return;
    default: break;
    }
    break;
  case TY_FLONUM:
    switch (src->kind) {
    case TY_FIXNUM:
      {
        static const unsigned char OpTable[][4] = {
          { OP_F32_CONVERT_I32_S, OP_F32_CONVERT_I64_S, OP_F64_CONVERT_I32_S, OP_F64_CONVERT_I64_S },
          { OP_F32_CONVERT_I32_U, OP_F32_CONVERT_I64_U, OP_F64_CONVERT_I32_U, OP_F64_CONVERT_I64_U },
        };
        int s = type_size(src);
        int index = (dst->flonum.kind >= FL_DOUBLE ? 2 : 0) + (s > I32_SIZE ? 1 : 0);
        bool su = !is_fixnum(src->kind) || src->fixnum.is_unsigned;
        ADD_CODE(OpTable[su][index]);
      }
      return;
    case TY_FLONUM:
      {
        size_t ss = type_size(src), ds = type_size(dst);
        if (ss != ds) {
          switch (ds) {
          case 4:  ADD_CODE(OP_F32_DEMOTE_F64); break;
          case 8:  ADD_CODE(OP_F64_PROMOTE_F32); break;
          }
        }
      }
      return;
    default: break;
    }
    break;
  case TY_STRUCT:
    assert(same_type_without_qualifier(dst, src, true));
    return;
  default: break;
  }
  assert(!"Cast not handled");
}

static void gen_ternary(Expr *expr, bool needval) {
  gen_cond(expr->ternary.cond, true, true);
  unsigned char wt = WT_VOID;
  if (needval && expr->type->kind != TY_VOID) {
    Type *type = expr->type;
    wt = to_wtype((is_number(type) || ptr_or_array(type)) ? type : ptrof(type));
  }
  ADD_CODE(OP_IF, wt);
  ++cur_depth;
  gen_expr(expr->ternary.tval, wt != WT_VOID);
  ADD_CODE(OP_ELSE);
  gen_expr(expr->ternary.fval, wt != WT_VOID);
  ADD_CODE(OP_END);
  --cur_depth;
}

typedef struct {
  Expr *lspvar;
  size_t offset;
  size_t vaarg_offset;
  int param_count;
  bool vaargs;
} FuncallWork;

static inline void gen_funarg(Expr *arg, int i, FuncallWork *work) {
  size_t offset = work->offset;
  Expr *lspvar = work->lspvar;
  if (is_stack_param(arg->type)) {
    assert(lspvar != NULL);
    size_t size = type_size(arg->type);
    if (size > 0) {
      offset = ALIGN(offset, align_size(arg->type));
      // _memcpy(global.sp + sarg_offset, &arg, size);
      Expr *src = lspvar;
      if (offset != 0)
        src = new_expr_bop(EX_ADD, &tySize, NULL, lspvar, new_expr_fixlit(&tySize, NULL, offset));
      gen_expr(src, true);
      gen_expr(arg, true);

      ADD_CODE(OP_I32_CONST);
      ADD_LEB128(size);
      ADD_CODE(OP_0xFC, OPFC_MEMORY_COPY, 0, 0);  // src, dst
      offset += size;
    }
  } else if (i < work->param_count) {
    gen_expr(arg, true);
  } else {
    // *(global.sp + offset) = arg
    offset = ALIGN(offset, align_size(arg->type));
    if (offset != 0) {
      gen_expr(new_expr_bop(EX_ADD, &tySize, NULL, lspvar,
                            new_expr_fixlit(&tySize, NULL, offset)),
               true);
    } else {
      gen_expr(lspvar, true);
    }
    const Type *t = arg->type;
    assert(!(t->kind == TY_FIXNUM && t->fixnum.kind < FX_INT));
    offset += type_size(t);

    gen_expr(arg, true);
    gen_store(t);
  }

  if (work->vaargs && i == work->param_count - 1)
    work->vaarg_offset = offset;

  work->offset = offset;
}

static inline void gen_funargs(Expr *expr) {
  Expr *func = expr->funcall.func;
  Vector *args = expr->funcall.args;
  int arg_count = args->len;

  Type *functype = get_callee_type(func->type);
  assert(functype != NULL);
  int param_count = functype->func.params != NULL ? functype->func.params->len : 0;

  size_t work_size = calc_funcall_work_size(expr);

  Expr *lspvar = NULL;
  if (work_size > 0) {
    FuncInfo *finfo = table_get(&func_info_table, curfunc->ident->ident);
    assert(finfo != NULL && finfo->lspname != NULL);
    lspvar = new_expr_variable(finfo->lspname, &tyVoidPtr, NULL, curfunc->scopes->data[0]);
  }

  bool ret_param = functype->func.ret->kind != TY_VOID && !is_prim_type(functype->func.ret);
  if (ret_param) {
    assert(curfunc != NULL);
    FuncExtra *extra = curfunc->extra;
    assert(extra != NULL);
    assert(extra->funcall_results != NULL);
    VarInfo *varinfo = NULL;
    for (int i = 0; i < extra->funcall_results->len; i += 2) {
      if (extra->funcall_results->data[i] == expr) {
        varinfo = extra->funcall_results->data[i + 1];
        break;
      }
    }
    assert(varinfo != NULL);
    // &ret_buf
    Expr *e = new_expr_variable(varinfo->ident->ident, varinfo->type, NULL,
                                curfunc->scopes->data[0]);
    gen_lval(e);
  }

  FuncallWork work;
  work.lspvar = lspvar;
  work.offset = 0;
  work.vaarg_offset = 0;
  work.param_count = param_count;
  work.vaargs = functype->func.vaargs;
  for (int i = 0; i < arg_count; ++i) {
    Expr *arg = args->data[i];
    gen_funarg(arg, i, &work);
  }

  if (functype->func.vaargs) {
    // Top of vaargs.
    if (arg_count > param_count) {
      gen_expr(lspvar, true);
      if (work.vaarg_offset != 0) {
        ADD_CODE(OP_I32_CONST);
        ADD_LEB128(work.vaarg_offset);
        ADD_CODE(OP_I32_ADD);
      }
    } else {
      ADD_CODE(OP_I32_CONST, 0);  // NULL
    }
  }
}

static inline void gen_funcall_by_name(const Name *funcname) {
  FuncInfo *info = table_get(&func_info_table, funcname);
  assert(info != NULL);
  ADD_CODE(OP_CALL);

  FuncExtra *extra = curfunc->extra;
  DataStorage *code = extra->code;
  RelocInfo *ri = calloc_or_die(sizeof(*ri));
  ri->type = R_WASM_FUNCTION_INDEX_LEB;
  ri->offset = code->len;
  ri->addend = 0;
  ri->index = info->index;
  vec_push(extra->reloc_code, ri);
  ADD_VARUINT32(info->index);
}

static inline void gen_funcall_sub(Expr *expr) {
  Expr *func = expr->funcall.func;
  if (func->type->kind == TY_FUNC && func->kind == EX_VAR) {
    gen_funcall_by_name(func->var.name);
    return;
  }

  gen_expr(func, true);
  ADD_CODE(OP_CALL_INDIRECT);

  FuncExtra *extra = curfunc->extra;
  DataStorage *code = extra->code;
  {
    Type *functype = get_callee_type(func->type);
    int index = get_func_type_index(functype);
    assert(index >= 0);
    RelocInfo *ri = calloc_or_die(sizeof(*ri));
    ri->type = R_WASM_TYPE_INDEX_LEB;
    ri->index = index;
    ri->offset = code->len;
    vec_push(extra->reloc_code, ri);
    ADD_VARUINT32(index);
  }
  {
    TableInfo *ti = getsert_indirect_function_table();
    RelocInfo *ri = calloc_or_die(sizeof(*ri));
    ri->type = R_WASM_TABLE_NUMBER_LEB;
    ri->index = ti->symbol_index;
    ri->offset = code->len;
    vec_push(extra->reloc_code, ri);
    ADD_VARUINT32(ti->index);  // table index
  }
}

static void gen_funcall(Expr *expr) {
  Expr *func = expr->funcall.func;
  if (func->kind == EX_VAR && is_global_scope(func->var.scope)) {
    BuiltinFunctionProc *proc = table_get(&builtin_function_table, func->var.name);
    if (proc != NULL) {
      (*proc)(expr, BFP_GEN);
      return;
    }
  }

  gen_funargs(expr);
  gen_funcall_sub(expr);
}

void gen_bpofs(int32_t offset) {
  FuncInfo *finfo = table_get(&func_info_table, curfunc->ident->ident);
  assert(finfo != NULL && finfo->bpname != NULL);
  const Name *bpname = finfo->bpname;
  Scope *scope = curfunc->scopes->data[0];
  const VarInfo *bpvarinfo = scope_find(scope, bpname, &scope);
  assert(bpvarinfo != NULL && !is_global_scope(scope));
  assert(bpvarinfo->local.vreg != NULL);
  ADD_CODE(OP_LOCAL_GET);
  ADD_ULEB128(bpvarinfo->local.vreg->prim.local_index);

  if (offset != 0) {
    ADD_CODE(OP_I32_CONST);
    ADD_LEB128(offset);
    ADD_CODE(OP_I32_ADD);
  }
}

void gen_clear_local_var(const VarInfo *varinfo) {
  if (is_prim_type(varinfo->type))
    return;

  size_t size = type_size(varinfo->type);
  if (size <= 0)
    return;
  VReg *vreg = varinfo->local.vreg;
  gen_bpofs(vreg->non_prim.offset);
  ADD_CODE(OP_I32_CONST, 0, OP_I32_CONST);
  ADD_LEB128(size);
  ADD_CODE(OP_0xFC, OPFC_MEMORY_FILL, 0);
}

static void gen_ref_sub(Expr *expr) {
  switch (expr->kind) {
  case EX_VAR:
    {
      Scope *scope;
      const VarInfo *varinfo = scope_find(expr->var.scope, expr->var.name, &scope);
      assert(varinfo != NULL && scope == expr->var.scope);
      assert(!is_prim_type(expr->type) || varinfo->storage & VS_REF_TAKEN ||
             is_global_datsec_var(varinfo, scope));
      if (is_global_scope(scope) || !is_local_storage(varinfo)) {
        if (varinfo->type->kind == TY_FUNC) {
          ADD_CODE(OP_I32_CONST);
          FuncInfo *info = table_get(&indirect_function_table, expr->var.name);
          assert(info != NULL && info->indirect_index > 0);
          FuncExtra *extra = curfunc->extra;
          DataStorage *code = extra->code;
          RelocInfo *ri = calloc_or_die(sizeof(*ri));
          ri->type = R_WASM_TABLE_INDEX_SLEB;
          ri->offset = code->len;
          ri->index = info->index;  // Assume that symtab index is same as function index.
          vec_push(extra->reloc_code, ri);

          ADD_VARINT32(info->indirect_index);
        } else {
          GVarInfo *info = get_gvar_info(expr);
          assert(info != NULL);
          ADD_CODE(OP_I32_CONST);
          FuncExtra *extra = curfunc->extra;
          DataStorage *code = extra->code;
          RelocInfo *ri = calloc_or_die(sizeof(*ri));
          ri->type = R_WASM_MEMORY_ADDR_SLEB;
          ri->offset = code->len;
          ri->addend = 0;
          ri->index = info->symbol_index;
          vec_push(extra->reloc_code, ri);

          ADD_VARINT32(info->non_prim.address);
        }
      } else {
        VReg *vreg = varinfo->local.vreg;
        gen_bpofs(vreg->non_prim.offset);
      }
    }
    return;
  case EX_DEREF:
    gen_expr(expr->unary.sub, true);
    return;
  case EX_MEMBER:
    {
      gen_expr(expr->member.target, true);
      const MemberInfo *minfo = expr->member.info;
      if (minfo->offset == 0)
        return;
      ADD_CODE(OP_I32_CONST);
      ADD_LEB128(minfo->offset);
      ADD_CODE(OP_I32_ADD);
      return;
    }
  case EX_COMPLIT:
    {
      Expr *var = expr->complit.var;
      VarInfo *varinfo = scope_find(var->var.scope, var->var.name, NULL);
      assert(varinfo != NULL);
      gen_clear_local_var(varinfo);
      gen_stmts(expr->complit.inits, false);
      gen_lval(var);
    }
    return;
  default: assert(false); break;
  }
}

void gen_lval(Expr *expr) {
  gen_ref_sub(reduce_refer(expr));
}

static void gen_var(Expr *expr, bool needval) {
  if (!needval)
    return;

  switch (expr->type->kind) {
  case TY_FIXNUM:
  case TY_PTR:
  case TY_FLONUM:
    {
      Scope *scope;
      const VarInfo *varinfo = scope_find(expr->var.scope, expr->var.name, &scope);
      assert(varinfo != NULL && scope == expr->var.scope);
      if ((varinfo->storage & VS_REF_TAKEN) || is_global_datsec_var(varinfo, scope)) {
        gen_lval(expr);
        gen_load(expr->type);
      } else if (!is_global_scope(scope) && is_local_storage(varinfo)) {
        VReg *vreg = varinfo->local.vreg;
        assert(vreg != NULL);
        ADD_CODE(OP_LOCAL_GET);
        ADD_ULEB128(vreg->prim.local_index);
      } else {
        GVarInfo *info = get_gvar_info(expr);
        assert(info != NULL);
        ADD_CODE(OP_GLOBAL_GET);
        FuncExtra *extra = curfunc->extra;
        DataStorage *code = extra->code;
        RelocInfo *ri = calloc_or_die(sizeof(*ri));
        ri->type = R_WASM_GLOBAL_INDEX_LEB;
        ri->offset = code->len;
        ri->addend = 0;
        ri->index = info->symbol_index;
        vec_push(extra->reloc_code, ri);

        ADD_VARUINT32(info->prim.index);
      }
    }
    break;

  case TY_ARRAY:   // Use variable address as a pointer.
  case TY_STRUCT:  // struct value is handled as a pointer.
  case TY_FUNC:
    gen_lval(expr);
    break;
  case TY_VOID: case TY_AUTO: assert(false); break;
  }
}

static void gen_block_expr(Expr *expr, bool needval) {
  Stmt *stmt = expr->block;
  assert(stmt->kind == ST_BLOCK);

  Scope *bak_curscope = curscope;
  if (stmt->block.scope != NULL)
    curscope = stmt->block.scope;

  Vector *stmts = stmt->block.stmts;
  int len = stmts->len, last = len - 1;
  for (int i = 0; i < len; ++i) {
    Stmt *stmt = stmts->data[i];
    if (stmt == NULL)
      continue;
    if (i == last && stmt->kind == ST_EXPR) {
      gen_expr(stmt->expr, needval);
      break;
    }
    gen_stmt(stmt, false);
  }

  if (stmt->block.scope != NULL)
    curscope = bak_curscope;
}

void gen_set_to_var(Expr *var) {
  assert(var->kind == EX_VAR);
  const VarInfo *varinfo = scope_find(var->var.scope, var->var.name, NULL);
  assert(varinfo != NULL);
  assert(!(varinfo->storage & VS_REF_TAKEN));
  if (!is_global_scope(var->var.scope) && is_local_storage(varinfo)) {
    VReg *vreg = varinfo->local.vreg;
    assert(vreg != NULL);
    ADD_CODE(OP_LOCAL_SET);
    ADD_ULEB128(vreg->prim.local_index);
  } else {
    assert(!is_global_datsec_var(varinfo, var->var.scope));
    GVarInfo *info = get_gvar_info(var);
    assert(info != NULL);
    ADD_CODE(OP_GLOBAL_SET);
    FuncExtra *extra = curfunc->extra;
    DataStorage *code = extra->code;
    RelocInfo *ri = calloc_or_die(sizeof(*ri));
    ri->type = R_WASM_GLOBAL_INDEX_LEB;
    ri->offset = code->len;
    ri->addend = 0;
    ri->index = info->symbol_index;
    vec_push(extra->reloc_code, ri);

    ADD_VARUINT32(info->prim.index);
  }
}

static void gen_fixnum(Expr *expr, bool needval) {
  if (needval) {
    if (type_size(expr->type) <= I32_SIZE) {
      ADD_CODE(OP_I32_CONST);
      ADD_LEB128((int32_t)expr->fixnum);
    } else {
      ADD_CODE(OP_I64_CONST);
      ADD_LEB128(expr->fixnum);
    }
  }
}

static void gen_flonum(Expr *expr, bool needval) {
#ifndef __NO_FLONUM
  if (needval) {
    switch (expr->type->flonum.kind) {
    case FL_FLOAT:
      ADD_CODE(OP_F32_CONST);
      ADD_F32(expr->flonum);
      break;
    case FL_DOUBLE: case FL_LDOUBLE:
      ADD_CODE(OP_F64_CONST);
      ADD_F64(expr->flonum);
      break;
    }
  }
#else
  UNUSED(expr);
  UNUSED(needval);
  assert(false);
#endif
}

static void gen_str(Expr *expr, bool needval) {
  UNUSED(expr);
  UNUSED(needval);
  assert(false);
}

static void gen_bop(Expr *expr, bool needval) {
  gen_expr(expr->bop.lhs, needval);
  gen_expr(expr->bop.rhs, needval);
  if (needval)
    gen_arith(expr->kind, expr->type);
}

static void gen_relation(Expr *expr, bool needval) {
  gen_cond(expr, true, needval);
}

static void gen_pos(Expr *expr, bool needval) {
  gen_expr(expr->unary.sub, needval);
}

static void gen_neg(Expr *expr, bool needval) {
  if (needval) {
    switch (expr->type->kind) {
    case TY_FIXNUM:
      ADD_CODE(type_size(expr->type) <= I32_SIZE ? OP_I32_CONST : OP_I64_CONST);
      ADD_LEB128(0);
      gen_expr(expr->unary.sub, true);
      gen_arith(EX_SUB, expr->type);
      break;
    case TY_FLONUM:
#ifndef __NO_FLONUM
      gen_expr(expr->unary.sub, true);
      switch (expr->type->flonum.kind) {
      case FL_FLOAT:
        ADD_CODE(OP_F32_NEG);
        break;
      case FL_DOUBLE: case FL_LDOUBLE:
        ADD_CODE(OP_F64_NEG);
        break;
      }
#else
      assert(false);
#endif
      break;
    default: assert(false); break;
    }
  } else {
    gen_expr(expr->unary.sub, false);
  }
}

static void gen_bitnot(Expr *expr, bool needval) {
  gen_expr(expr->unary.sub, needval);
  if (needval) {
    unsigned char wt = to_wtype(expr->type);
    switch (wt) {
    case WT_I32:  ADD_CODE(OP_I32_CONST); break;
    case WT_I64:  ADD_CODE(OP_I64_CONST); break;
    default: assert(false); break;
    }
    ADD_LEB128(-1);
    gen_arith(EX_BITXOR, expr->type);
  }
}

static void gen_incdec(Expr *expr, bool needval) {
#define IS_POST(expr)  ((expr)->kind >= EX_POSTINC)
#define IS_DEC(expr)   (((expr)->kind - EX_PREINC) & 1)
  assert(is_prim_type(expr->type));
  Expr *target = expr->unary.sub;
  if (target->kind == EX_COMPLIT) {
    gen_expr(target, true);
    target = target->complit.var;
    assert(target->kind == EX_VAR);
  } else {
    assert(target->kind == EX_VAR);
    gen_expr(target, true);
  }
  if (IS_POST(expr) && needval) {
    gen_expr(target, true);  // Duplicate the result: target is VAR.
    needval = false;
  }

  // gen_incdec(expr->type, IS_DEC(expr));
  // static void gen_incdec(const Type *type, bool dec)
  {
    Type *type = expr->type;
    int addend = type->kind == TY_PTR ? type_size(type->pa.ptrof) : 1;
    unsigned char wtype = to_wtype(type);
    switch (wtype) {
    case WT_I32:
    case WT_I64:
      {
        static unsigned char CONST_OP[] = {OP_I32_CONST, OP_I64_CONST};
        static unsigned char ADDSUB_OP[] = {OP_I32_ADD, OP_I64_ADD, OP_I32_SUB, OP_I64_SUB};
        int i1 = wtype == WT_I32 ? 0 : 1;
        int i2 = IS_DEC(expr) ? 2 : 0;
        ADD_CODE(CONST_OP[i1]);
        ADD_ULEB128(addend);
        ADD_CODE(ADDSUB_OP[i1 | i2]);

        if (type_size(type) < type_size(&tyInt))
          gen_cast_to(target->type, &tyInt);
      }
      break;
#ifndef __NO_FLONUM
    case WT_F32:
    case WT_F64:
      {
        static unsigned char ADDSUB_OP[] = {OP_F32_ADD, OP_F64_ADD, OP_F32_SUB, OP_F64_SUB};
        int i2 = IS_DEC(expr) ? 2 : 0;
        if (wtype == WT_F32) {
          ADD_CODE(OP_F32_CONST);
          ADD_F32(addend);
        } else {
          ADD_CODE(OP_F64_CONST);
          ADD_F64(addend);
          i2 += 1;
        }
        ADD_CODE(ADDSUB_OP[i2]);
      }
      break;
#endif
    default: assert(false); break;
    }
  }

  Scope *scope;
  const VarInfo *varinfo = scope_find(target->var.scope, target->var.name, &scope);
  assert(varinfo != NULL);
  if (!is_global_scope(scope) && is_local_storage(varinfo)) {
    VReg *vreg = varinfo->local.vreg;
    assert(vreg != NULL);
    ADD_CODE(needval ? OP_LOCAL_TEE : OP_LOCAL_SET);
    ADD_ULEB128(vreg->prim.local_index);
  } else {
    assert(!is_global_datsec_var(varinfo, scope));
    GVarInfo *info = get_gvar_info(target);
    assert(info != NULL);
    ADD_CODE(OP_GLOBAL_SET);
    ADD_ULEB128(info->prim.index);
    if (needval) {
      ADD_CODE(OP_GLOBAL_GET);
      ADD_ULEB128(info->prim.index);
    }
  }
#undef IS_POST
#undef IS_DEC
}

static void gen_assign_sub(Expr *lhs, Expr *rhs) {
  switch (lhs->type->kind) {
  case TY_FIXNUM:
  case TY_PTR:
  case TY_FLONUM:
    if (lhs->kind == EX_VAR) {
      const VarInfo *varinfo = scope_find(lhs->var.scope, lhs->var.name, NULL);
      assert(varinfo != NULL);
      if (!(varinfo->storage & VS_REF_TAKEN) && !is_global_datsec_var(varinfo, lhs->var.scope)) {
        gen_expr(rhs, true);
        gen_set_to_var(lhs);
        break;
      }

      if ((varinfo->storage & (VS_STATIC | VS_USED)) == VS_STATIC) {
        // Assignment can be omitted.
        gen_expr(rhs, false);
        return;
      }
    }
    gen_lval(lhs);
    gen_expr(rhs, true);
    gen_store(lhs->type);
    break;
  case TY_STRUCT:
    {
      size_t size = type_size(lhs->type);
      if (size > 0) {
        gen_lval(lhs);
        gen_expr(rhs, true);
        ADD_CODE(OP_I32_CONST);
        ADD_LEB128(size);
        ADD_CODE(OP_0xFC, OPFC_MEMORY_COPY, 0, 0);  // src, dst
      }
    }
    break;
  case TY_ARRAY: case TY_FUNC: case TY_VOID: case TY_AUTO: assert(false); break;
  }
}

static void gen_assign(Expr *expr, bool needval) {
  UNUSED(needval);
  gen_assign_sub(expr->bop.lhs, expr->bop.rhs);
}

static void gen_comma(Expr *expr, bool needval) {
  gen_expr(expr->bop.lhs, false);
  gen_expr(expr->bop.rhs, needval);
}

static void gen_cast(Expr *expr, bool needval) {
  Expr *src = expr->unary.sub;
  Type *dst_type = expr->type;
  if (is_bool(dst_type)) {
    gen_expr(make_cond(src), needval);
    return;
  }

  gen_expr(src, needval);
  if (needval)
    gen_cast_to(dst_type, src->type);
}

static void gen_ref(Expr *expr, bool needval) {
  Expr *sub = expr->unary.sub;
  if (needval)
    gen_ref_sub(sub);
  else
    gen_expr(sub, false);
}

static void gen_deref(Expr *expr, bool needval) {
  gen_expr(expr->unary.sub, needval);
  if (needval) {
    switch (expr->type->kind) {
    case TY_FIXNUM:
    case TY_PTR:
    case TY_FLONUM:
      gen_load(expr->type);
      break;

    case TY_ARRAY:
    case TY_STRUCT:
    case TY_FUNC:
      // array, struct and func values are handled as a pointer.
      break;

    case TY_VOID: case TY_AUTO: assert(false); break;
    }
  }
}

static void gen_member(Expr *expr, bool needval) {
  if (needval) {
#ifndef __NO_BITFIELD
    const MemberInfo *minfo = expr->member.info;
    if (minfo->bitfield.width > 0) {
      Type *type = get_fixnum_type(minfo->bitfield.base_kind, minfo->type->fixnum.is_unsigned, 0);
      Expr *ptr = make_cast(ptrof(type), expr->token, make_refer(expr->token, expr), true);
      Expr *load = new_expr_deref(NULL, ptr);
      Expr *e = extract_bitfield_value(load, minfo);
      gen_expr(e, needval);
      return;
    }
#endif

    gen_lval(expr);
    if (is_prim_type(expr->type))
      gen_load(expr->type);
  } else {
    gen_expr(expr->member.target, false);
  }
}

static void gen_funcall_expr(Expr *expr, bool needval) {
  gen_funcall(expr);
  if (!needval && expr->type->kind != TY_VOID)
    ADD_CODE(OP_DROP);
}

static void gen_complit(Expr *expr, bool needval) {
  Expr *var = expr->complit.var;
  VarInfo *varinfo = scope_find(var->var.scope, var->var.name, NULL);
  assert(varinfo != NULL);
  gen_clear_local_var(varinfo);
  gen_stmts(expr->complit.inits, false);
  gen_expr(var, needval);
}

static void gen_inlined(Expr *expr, bool needval) {
  // Nested inline funcall is transformed so its scope relation is modified.
  // ex. foo(bar(123)) => ({foo(({bar(123)}))}) => tmp=({bar(123)}), ({foo(tmp)})
  Scope *bak_curscope = curscope;
  Stmt *embedded = expr->inlined.embedded;
  assert(embedded->kind == ST_BLOCK);
  Scope *top_scope = embedded->block.scope;
  assert(top_scope != NULL);
  Vector *top_scope_vars = top_scope->vars;
  curscope = top_scope; {
    Vector *args = expr->inlined.args;
    assert(args->len <= top_scope_vars->len);
    for (int i = 0; i < args->len; ++i) {
      Expr *arg = args->data[i];
      VarInfo *varinfo = top_scope_vars->data[i];
      assert(!(varinfo->storage & VS_PARAM));
      Expr *lhs = new_expr_variable(varinfo->ident->ident, varinfo->type, NULL, top_scope);
      gen_assign_sub(lhs, arg);
    }
  } curscope = bak_curscope;

  assert(curfunc != NULL);
  FuncInfo *finfo = table_get(&func_info_table, curfunc->ident->ident);
  int bak_flag = finfo->flag;
  finfo->flag |= FF_INLINING;

  int bak_depth = cur_depth;
  cur_depth = 1;

  const Type *rettype = expr->type;
  unsigned char wt = get_func_ret_wtype(rettype);
  ADD_CODE(OP_BLOCK, wt); {
    gen_stmt(embedded, true);

    assert(embedded->kind == ST_BLOCK);
    Vector *stmts = embedded->block.stmts;
    if (stmts->len > 0) {
      Stmt *last = stmts->data[stmts->len - 1];
      if (last->kind != ST_ASM && rettype->kind != TY_VOID && !check_funcend_return(embedded)) {
        assert(embedded->reach & REACH_STOP);
        ADD_CODE(OP_UNREACHABLE);
      }
    }
  } ADD_CODE(OP_END);

  if (wt != WT_VOID && !needval)
    ADD_CODE(OP_DROP);

  cur_depth = bak_depth;
  finfo->flag = bak_flag;
  curscope = bak_curscope;
}

void gen_expr(Expr *expr, bool needval) {
  typedef void (*GenExprFunc)(Expr *, bool);
  static const GenExprFunc table[] = {
    [EX_FIXNUM] = gen_fixnum, [EX_FLONUM] = gen_flonum, [EX_STR] = gen_str,
    [EX_VAR] = gen_var,
    [EX_ADD] = gen_bop, [EX_SUB] = gen_bop, [EX_MUL] = gen_bop,
    [EX_DIV] = gen_bop, [EX_MOD] = gen_bop, [EX_BITAND] = gen_bop,
    [EX_BITOR] = gen_bop, [EX_BITXOR] = gen_bop,
    [EX_LSHIFT] = gen_bop, [EX_RSHIFT] = gen_bop,
    [EX_EQ] = gen_relation, [EX_NE] = gen_relation, [EX_LT] = gen_relation,
    [EX_LE] = gen_relation, [EX_GE] = gen_relation, [EX_GT] = gen_relation,
    [EX_LOGAND] = gen_relation, [EX_LOGIOR] = gen_relation,
    [EX_ASSIGN] = gen_assign, [EX_COMMA] = gen_comma,
    [EX_POS] = gen_pos, [EX_NEG] = gen_neg, [EX_BITNOT] = gen_bitnot,
    [EX_PREINC] = gen_incdec, [EX_PREDEC] = gen_incdec,
    [EX_POSTINC] = gen_incdec, [EX_POSTDEC] = gen_incdec,
    [EX_REF] = gen_ref, [EX_DEREF] = gen_deref, [EX_CAST] = gen_cast, [EX_TERNARY] = gen_ternary,
    [EX_MEMBER] = gen_member, [EX_FUNCALL] = gen_funcall_expr, [EX_INLINED] = gen_inlined,
    [EX_COMPLIT] = gen_complit, [EX_BLOCK] = gen_block_expr,
  };

  assert(expr->kind < (int)ARRAY_SIZE(table));
  assert(table[expr->kind] != NULL);
  (*table[expr->kind])(expr, needval);
}
