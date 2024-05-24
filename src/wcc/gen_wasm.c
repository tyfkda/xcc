#include "../config.h"
#include "wcc.h"

#include <alloca.h>
#include <assert.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>  // free, strtoull
#include <string.h>

#ifndef __NO_FLONUM
#include <math.h>
#endif

#include "ast.h"
#include "fe_misc.h"  // curfunc
#include "parser.h"
#include "table.h"
#include "type.h"
#include "util.h"
#include "var.h"
#include "wasm.h"
#include "wasm_obj.h"

#define CODE  (((FuncExtra*)curfunc->extra)->code)

#define ADD_LEB128(x)  data_leb128(CODE, -1, x)
#define ADD_ULEB128(x) data_uleb128(CODE, -1, x)
#define ADD_VARUINT32(x)  data_varuint32(CODE, -1, x)

// TODO: Endian.
#define ADD_F32(x)     do { float f = (x); add_code((unsigned char*)&f, sizeof(f)); } while (0)
#define ADD_F64(x)     do { double d = (x); add_code((unsigned char*)&d, sizeof(d)); } while (0)

static void gen_lval(Expr *expr);

void add_code(const unsigned char *buf, size_t size) {
  data_append(CODE, buf, size);
}

////////////////////////////////////////////////

static void gen_cond(Expr *cond, bool tf, bool needval);

// Local variable information for WASM
struct VReg {
  int32_t param_index;
  union {
    struct {  // Primitive(i32, i64, f32, f64)
      uint32_t local_index;
    } prim;
    struct {  // Non-primitive
      int32_t offset;  // for base pointer
    } non_prim;
  };
};

static void gen_stmt(Stmt *stmt, bool is_last);
static void gen_stmts(Vector *stmts, bool is_last);

static int cur_depth;
static int break_depth;
static int continue_depth;

static unsigned char get_func_ret_wtype(const Type *rettype) {
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

static void gen_store(const Type *type) {
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
        int d = type_size(dst);
        int s = type_size(src);
        bool du = dst->kind != TY_FIXNUM || dst->fixnum.is_unsigned;
        bool su = src->kind != TY_FIXNUM || src->fixnum.is_unsigned;
        switch ((d > I32_SIZE ? 2 : 0) + (s > I32_SIZE ? 1 : 0)) {
        case 1: ADD_CODE(OP_I32_WRAP_I64); break;
        case 2: ADD_CODE(su ? OP_I64_EXTEND_I32_U : OP_I64_EXTEND_I32_S); break;
        }
        if (d < s) {
          assert(d <= I32_SIZE);
          if (d < I32_SIZE) {
            if (du) {
              ADD_CODE(OP_I32_CONST);
              ADD_LEB128((1 << (d * TARGET_CHAR_BIT)) - 1);
              ADD_CODE(OP_I32_AND);
            } else {
              int shift = (I32_SIZE - d) * TARGET_CHAR_BIT;
              ADD_CODE(OP_I32_CONST);
              ADD_LEB128(shift);
              ADD_CODE(OP_I32_SHL);
              ADD_CODE(OP_I32_CONST);
              ADD_LEB128(shift);
              ADD_CODE(OP_I32_SHR_S);
            }
          }
        } else if (du != su) {
          if (du) {  // unsigned <- signed
            if (d < I32_SIZE && s < I32_SIZE) {
              ADD_CODE(OP_I32_CONST);
              ADD_LEB128((1 << (d * TARGET_CHAR_BIT)) - 1);
              ADD_CODE(OP_I32_AND);
            }
          } else {  // signed <- unsigned
            // Should be handled in traverse.
            assert(d != s || d >= I32_SIZE);
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

static void gen_funcall_by_name(const Name *funcname) {
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

static void gen_funcall(Expr *expr) {
  Expr *func = expr->funcall.func;
  if (func->kind == EX_VAR && is_global_scope(func->var.scope)) {
    void *proc = table_get(&builtin_function_table, func->var.name);
    if (proc != NULL) {
      (*(BuiltinFunctionProc*)proc)(expr, BFP_GEN);
      return;
    }
  }

  Vector *args = expr->funcall.args;
  int arg_count = args->len;

  Type *functype = get_callee_type(func->type);
  assert(functype != NULL);
  int param_count = functype->func.params != NULL ? functype->func.params->len : 0;

  int sarg_siz = 0;
  for (int i = 0; i < param_count; ++i) {
    Expr *arg = args->data[i];
    if (is_stack_param(arg->type))
      sarg_siz += ALIGN(type_size(arg->type), 4);
  }

  int vaarg_bufsiz = 0;
  if (functype->func.vaargs) {
    int d = arg_count - param_count;
    if (d > 0) {
      for (int i = 0; i < d; ++i) {
        Expr *arg = args->data[i + param_count];
        const Type *t = arg->type;
        assert(!(t->kind == TY_FIXNUM && t->fixnum.kind < FX_INT));
        // vaargs are promoted to int, so alignment is not needed.
        vaarg_bufsiz += type_size(t);
      }
    }
  }

  Expr *spvar = NULL;
  if (sarg_siz > 0 || vaarg_bufsiz > 0) {
    spvar = get_sp_var();
    // global.sp -= ALIGN(sarg_siz + vaarg_bufsiz, 8);
    gen_expr_stmt(new_expr_bop(
        EX_ASSIGN, &tyVoid, NULL, spvar,
        new_expr_bop(EX_SUB, &tySize, NULL, spvar,
                     new_expr_fixlit(&tySize, NULL, ALIGN(sarg_siz + vaarg_bufsiz, 8)))));
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
    Expr *e = new_expr_variable(varinfo->name, varinfo->type, NULL, curfunc->scopes->data[0]);
    gen_lval(e);
  }

  int sarg_offset = 0;
  int vaarg_offset = 0;
  for (int i = 0; i < arg_count; ++i) {
    Expr *arg = args->data[i];
    if (i < param_count) {
      if (!is_stack_param(arg->type)) {
        gen_expr(arg, true);
      } else {
        assert(spvar != NULL);
        size_t size = type_size(arg->type);
        if (size > 0) {
          sarg_offset = ALIGN(sarg_offset, align_size(arg->type));
          // _memcpy(global.sp + sarg_offset, &arg, size);
          if (sarg_offset != 0) {
            gen_expr(new_expr_bop(EX_ADD, &tySize, NULL, spvar,
                                  new_expr_fixlit(&tySize, NULL, sarg_offset)),
                     true);
          } else {
            gen_expr(spvar, true);
          }
          gen_expr(arg, true);

          ADD_CODE(OP_I32_CONST);
          ADD_LEB128(size);
          ADD_CODE(OP_EXTENSION, OPEX_MEMORY_COPY, 0, 0);  // src, dst
        }
        sarg_offset += size;
      }
    } else {
      assert(!is_stack_param(arg->type));
      // *(global.sp + sarg_siz + vaarg_offset) = arg
      int offset = sarg_siz + vaarg_offset;
      if (offset != 0) {
        gen_expr(new_expr_bop(EX_ADD, &tySize, NULL, spvar,
                              new_expr_fixlit(&tySize, NULL, offset)),
                 true);
      } else {
        gen_expr(spvar, true);
      }
      const Type *t = arg->type;
      assert(!(t->kind == TY_FIXNUM && t->fixnum.kind < FX_INT));
      vaarg_offset += type_size(t);

      gen_expr(arg, true);
      gen_store(t);
    }
  }
  if (functype->func.vaargs) {
    // Top of vaargs.
    if (vaarg_bufsiz > 0) {
      gen_expr(spvar, true);
    } else {
      ADD_CODE(OP_I32_CONST, 0);  // NULL
    }
  }

  if (func->type->kind == TY_FUNC && func->kind == EX_VAR) {
    gen_funcall_by_name(func->var.name);
  } else {
    gen_expr(func, true);
    int index = get_func_type_index(functype);
    assert(index >= 0);
    ADD_CODE(OP_CALL_INDIRECT);
    FuncExtra *extra = curfunc->extra;
    DataStorage *code = extra->code;
    RelocInfo *ri = calloc_or_die(sizeof(*ri));
    ri->type = R_WASM_TYPE_INDEX_LEB;
    ri->index = index;
    ri->offset = code->len;
    vec_push(extra->reloc_code, ri);

    ADD_VARUINT32(index);
    ADD_ULEB128(0);      // table index
  }

  if (sarg_siz > 0 || vaarg_bufsiz > 0) {
    // global.sp += ALIGN(sarg_siz + vaarg_bufsiz, 8);
    gen_expr_stmt(new_expr_bop(
        EX_ASSIGN, &tyVoid, NULL, spvar,
        new_expr_bop(EX_ADD, &tySize, NULL, spvar,
                     new_expr_fixlit(&tySize, NULL, ALIGN(sarg_siz + vaarg_bufsiz, 8)))));
  }
}

static void gen_bpofs(int32_t offset) {
  FuncInfo *finfo = table_get(&func_info_table, curfunc->name);
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

static void gen_clear_local_var(const VarInfo *varinfo) {
  if (is_prim_type(varinfo->type))
    return;

  size_t size = type_size(varinfo->type);
  if (size <= 0)
    return;
  VReg *vreg = varinfo->local.vreg;
  gen_bpofs(vreg->non_prim.offset);
  ADD_CODE(OP_I32_CONST, 0, OP_I32_CONST);
  ADD_LEB128(size);
  ADD_CODE(OP_EXTENSION, OPEX_MEMORY_FILL, 0);
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

          ADD_VARUINT32(info->indirect_index);
        } else {
          GVarInfo *info = get_gvar_info(expr);
          ADD_CODE(OP_I32_CONST);
          FuncExtra *extra = curfunc->extra;
          DataStorage *code = extra->code;
          RelocInfo *ri = calloc_or_die(sizeof(*ri));
          ri->type = R_WASM_MEMORY_ADDR_LEB;
          ri->offset = code->len;
          ri->addend = 0;
          ri->index = info->symbol_index;
          vec_push(extra->reloc_code, ri);

          ADD_VARUINT32(info->non_prim.address);
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

static void gen_lval(Expr *expr) {
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
  case TY_VOID: assert(false); break;
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

static void gen_set_to_var(Expr *var) {
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
        ADD_CODE(OP_EXTENSION, OPEX_MEMORY_COPY, 0, 0);  // src, dst
      }
    }
    break;
  case TY_ARRAY: case TY_FUNC: case TY_VOID: assert(false); break;
  }
}

static void gen_assign(Expr *expr, bool needval) {
  UNUSED(needval);
  assert(expr->type->kind == TY_VOID);
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

    case TY_VOID: assert(false); break;
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
  if (!needval && is_prim_type(expr->type))
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
      Expr *lhs = new_expr_variable(varinfo->name, varinfo->type, NULL, top_scope);
      gen_assign_sub(lhs, arg);
    }
  } curscope = bak_curscope;

  assert(curfunc != NULL);
  FuncInfo *finfo = table_get(&func_info_table, curfunc->name);
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

static void gen_compare_expr(enum ExprKind kind, Expr *lhs, Expr *rhs, bool needval) {
  assert(lhs->type->kind == rhs->type->kind || !needval);
  assert(is_prim_type(lhs->type) || !needval);

  gen_expr(lhs, needval);
  if (needval && is_const(rhs) && is_fixnum(rhs->type->kind) && rhs->fixnum == 0 && kind == EX_EQ) {
    ADD_CODE(type_size(lhs->type) <= I32_SIZE ? OP_I32_EQZ : OP_I64_EQZ);
    return;
  }
  gen_expr(rhs, needval);
  if (!needval)
    return;

  int index = 0;
  if (is_flonum(lhs->type)) {
    index = lhs->type->flonum.kind >= FL_DOUBLE ? 5 : 4;
  } else {
    index = (!is_fixnum(lhs->type->kind) || lhs->type->fixnum.is_unsigned ? 2 : 0) +
            (type_size(lhs->type) > I32_SIZE ? 1 : 0);
  }

  static const unsigned char OpTable[][6] = {
    {OP_I32_EQ, OP_I32_NE, OP_I32_LT_S, OP_I32_LE_S, OP_I32_GE_S, OP_I32_GT_S},
    {OP_I64_EQ, OP_I64_NE, OP_I64_LT_S, OP_I64_LE_S, OP_I64_GE_S, OP_I64_GT_S},
    {OP_I32_EQ, OP_I32_NE, OP_I32_LT_U, OP_I32_LE_U, OP_I32_GE_U, OP_I32_GT_U},
    {OP_I64_EQ, OP_I64_NE, OP_I64_LT_U, OP_I64_LE_U, OP_I64_GE_U, OP_I64_GT_U},
    {OP_F32_EQ, OP_F32_NE, OP_F32_LT, OP_F32_LE, OP_F32_GE, OP_F32_GT},
    {OP_F64_EQ, OP_F64_NE, OP_F64_LT, OP_F64_LE, OP_F64_GE, OP_F64_GT},
  };

  ADD_CODE(OpTable[index][kind - EX_EQ]);
}

static void gen_cond(Expr *cond, bool tf, bool needval) {
  enum ExprKind ck = cond->kind;
  switch (ck) {
  case EX_FIXNUM:
    {
      Expr *zero = new_expr_fixlit(&tyInt, NULL, 0);
      gen_compare_expr(tf ? EX_NE : EX_EQ, cond, zero, needval);
    }
    break;
  case EX_EQ:
  case EX_NE:
  case EX_LT:
  case EX_GT:
  case EX_LE:
  case EX_GE:
    if (!tf) {
      if (ck <= EX_NE)
        ck = (EX_EQ + EX_NE) - ck;
      else
        ck = EX_LT + ((ck - EX_LT) ^ 2);
    }
    gen_compare_expr(ck, cond->bop.lhs, cond->bop.rhs, needval);
    break;
  case EX_LOGAND:
  case EX_LOGIOR:
    {
      bool logand = ck == EX_LOGAND;
      gen_cond(cond->bop.lhs, logand, true);
      ADD_CODE(OP_IF, needval ? WT_I32 : WT_VOID);
      ++cur_depth;
      gen_cond(cond->bop.rhs, tf, needval);
      if (needval) {
        ADD_CODE(OP_ELSE);
        assert((tf & ~1) == 0);  // tf == true || tf == false
        ADD_CODE(OP_I32_CONST, tf ^ logand);
      }
      ADD_CODE(OP_END);
      --cur_depth;
    }
    break;
  case EX_COMMA:
    gen_expr(cond->bop.lhs, false);
    gen_cond(cond->bop.rhs, tf, needval);
    break;
  default: assert(false); break;
  }
}

static void gen_cond_jmp(Expr *cond, bool tf, uint32_t depth) {
  gen_cond(cond, tf, true);
  ADD_CODE(OP_BR_IF);
  ADD_ULEB128(depth);
}

static void gen_switch_table_jump(Stmt *stmt, Expr *value, Fixnum min, Fixnum max,
                                  int default_index) {
  Vector *cases = stmt->switch_.cases;
  int case_count = cases->len;

  unsigned int vrange = max - min + 1;
  bool use_alloca = vrange <= 64;
  int *table = use_alloca ? alloca(sizeof(*table) * vrange)
                          : malloc_or_die(sizeof(*table) * vrange);
  for (unsigned int i = 0; i < vrange; ++i)
    table[i] = default_index;
  for (int i = 0; i < case_count; ++i) {
    Stmt *c = cases->data[i];
    if (c->case_.value != NULL)
      table[c->case_.value->fixnum - min] = i;
  }

  gen_expr(value, true);
  bool is_i64 = type_size(value->type) > I32_SIZE;
  if (min != 0) {
    ADD_CODE(is_i64 ? OP_I64_CONST : OP_I32_CONST);
    ADD_LEB128(min);
    ADD_CODE(is_i64 ? OP_I64_SUB : OP_I32_SUB);
  }
  if (is_i64)
    ADD_CODE(OP_I32_WRAP_I64);
  ADD_CODE(OP_BR_TABLE);
  ADD_ULEB128(vrange);
  for (unsigned int i = 0; i < vrange; ++i)
    ADD_ULEB128(table[i]);
  ADD_ULEB128(default_index);

  if (!use_alloca)
    free(table);
}

static void gen_switch(Stmt *stmt) {
  int save_depth = break_depth;
  break_depth = cur_depth;

  ADD_CODE(OP_BLOCK, WT_VOID);
  Vector *cases = stmt->switch_.cases;
  int case_count = cases->len;
  for (int i = 0; i < case_count; ++i)
    ADD_CODE(OP_BLOCK, WT_VOID);
  cur_depth += case_count + 1;

  Expr *value = stmt->switch_.value;
  if (value->kind == EX_COMMA) {
    gen_expr(value, false);
    value = value->bop.rhs;
  }
  // Must be simple expression, because this is evaluated multiple times.
  assert(is_const(value) || value->kind == EX_VAR);
  assert(is_fixnum(value->type->kind));

  int default_index = case_count;
  Fixnum min = INTPTR_MAX;
  Fixnum max = INTPTR_MIN;
  for (int i = 0; i < case_count; ++i) {
    Stmt *c = cases->data[i];
    if (c->case_.value == NULL) {
      default_index = i;
      continue;
    }
    Fixnum v = c->case_.value->fixnum;
    if (v < min)
      min = v;
    if (v > max)
      max = v;
  }

  if (case_count >= 4 && (max - min) / 2 <= case_count) {
    gen_switch_table_jump(stmt, value, min, max, default_index);
  } else {
    bool is_i64 = type_size(value->type) > I32_SIZE;
    unsigned char op_const = is_i64 ? OP_I64_CONST : OP_I32_CONST;
    unsigned char op_eq = is_i64 ? OP_I64_EQ : OP_I32_EQ;
    for (int i = 0; i < case_count; ++i) {
      Stmt *c = cases->data[i];
      if (c->case_.value == NULL) {  // default.
        default_index = i;
        continue;
      }
      gen_expr(value, true);
      ADD_CODE(op_const);
      ADD_LEB128(c->case_.value->fixnum);
      ADD_CODE(op_eq, OP_BR_IF);
      ADD_ULEB128(i);
    }
    // Jump to default.
    ADD_CODE(OP_BR);
    ADD_ULEB128(default_index);
  }

  // Body.
  gen_stmt(stmt->switch_.body, false);

  ADD_CODE(OP_END);
  --cur_depth;
  assert(cur_depth == break_depth);
  break_depth = save_depth;
}

static void gen_case(Stmt *stmt) {
  UNUSED(stmt);
  ADD_CODE(OP_END);
  --cur_depth;
  assert(cur_depth >= 0);
}

static void gen_while(Stmt *stmt) {
  bool infinite_loop = false;
  Expr *cond = stmt->while_.cond;
  if (is_const(cond)) {
    if (!is_const_truthy(cond))
      return;
    infinite_loop = true;
  }

  int save_break = break_depth;
  int save_continue = continue_depth;
  break_depth = cur_depth;
  continue_depth = cur_depth + 1;

  ADD_CODE(OP_BLOCK, WT_VOID);
  ADD_CODE(OP_LOOP, WT_VOID);
  cur_depth += 2;
  if (!infinite_loop)
    gen_cond_jmp(cond, false, 1);
  gen_stmt(stmt->while_.body, false);
  ADD_CODE(OP_BR, 0);
  ADD_CODE(OP_END);
  ADD_CODE(OP_END);
  cur_depth -= 2;
  break_depth = save_break;
  continue_depth = save_continue;
}

static void gen_do_while(Stmt *stmt) {
  bool infinite_loop = false, no_loop = false;
  Expr *cond = stmt->while_.cond;
  if (is_const(cond)) {
    if (is_const_truthy(cond))
      infinite_loop = true;
    else
      no_loop = true;
  }

  int save_break = break_depth;
  int save_continue = continue_depth;
  break_depth = cur_depth;
  continue_depth = cur_depth + 2;

  ADD_CODE(OP_BLOCK, WT_VOID);
  ADD_CODE(OP_LOOP, WT_VOID);
  ADD_CODE(OP_BLOCK, WT_VOID);
  cur_depth += 3;
  gen_stmt(stmt->while_.body, false);
  ADD_CODE(OP_END);
  --cur_depth;
  if (no_loop)
    ADD_CODE(OP_BR, 1);
  else if (infinite_loop)
    ADD_CODE(OP_BR, 0);
  else
    gen_cond_jmp(cond, true, 0);
  ADD_CODE(OP_END);
  ADD_CODE(OP_END);
  cur_depth -= 2;
  break_depth = save_break;
  continue_depth = save_continue;
}

static void gen_for(Stmt *stmt) {
  if (stmt->for_.pre != NULL)
    gen_expr_stmt(stmt->for_.pre);

  bool infinite_loop = false;
  Expr *cond = stmt->for_.cond;
  if (cond == NULL) {
    infinite_loop = true;
  } else if (is_const(cond)) {
    if (!is_const_truthy(cond))
      return;
    infinite_loop = true;
  }

  int save_break = break_depth;
  int save_continue = continue_depth;
  break_depth = cur_depth;
  continue_depth = cur_depth + 2;

  ADD_CODE(OP_BLOCK, WT_VOID);
  ADD_CODE(OP_LOOP, WT_VOID);
  ADD_CODE(OP_BLOCK, WT_VOID);
  cur_depth += 3;
  if (!infinite_loop)
    gen_cond_jmp(cond, false, 2);
  gen_stmt(stmt->for_.body, false);
  ADD_CODE(OP_END);
  --cur_depth;
  if (stmt->for_.post != NULL)
    gen_expr_stmt(stmt->for_.post);
  ADD_CODE(OP_BR, 0);
  ADD_CODE(OP_END);
  ADD_CODE(OP_END);
  cur_depth -= 2;
  break_depth = save_break;
  continue_depth = save_continue;
}

static void gen_break(void) {
  assert(cur_depth > break_depth);
  ADD_CODE(OP_BR, cur_depth - break_depth - 1);
}

static void gen_continue(void) {
  assert(cur_depth > continue_depth);
  ADD_CODE(OP_BR, cur_depth - continue_depth - 1);
}

static void gen_block(Stmt *stmt, bool is_last) {
  assert(stmt->kind == ST_BLOCK);
  // AST may moved, so code generation traversal may differ from lexical scope chain.
  Scope *bak_curscope = curscope;
  if (stmt->block.scope != NULL)
    curscope = stmt->block.scope;
  gen_stmts(stmt->block.stmts, is_last);
  if (stmt->block.scope != NULL)
    curscope = bak_curscope;
}

static void gen_return(Stmt *stmt, bool is_last) {
  assert(curfunc != NULL);
  if (stmt->return_.val != NULL) {
    Expr *val = stmt->return_.val;
    const Type *rettype = val->type;
    assert(rettype->kind != TY_VOID);
    if (is_prim_type(rettype)) {
      gen_expr(val, true);
    } else {
      FuncInfo *finfo = table_get(&func_info_table, curfunc->name);
      assert(finfo != NULL);
      if (!(finfo->flag & FF_INLINING)) {
        // Local #0 is the pointer for result.
        ADD_CODE(OP_LOCAL_GET, 0);
        gen_expr(val, true);
        ADD_CODE(OP_I32_CONST);
        ADD_LEB128(type_size(rettype));
        ADD_CODE(OP_EXTENSION, OPEX_MEMORY_COPY, 0, 0);  // src, dst
        // Result.
        ADD_CODE(OP_LOCAL_GET, 0);
      } else {
        // Inlining a function which returns struct:
        // Put value pointer on top of the stack.
        gen_expr(val, true);
      }
    }
  }

  FuncInfo *finfo = table_get(&func_info_table, curfunc->name);
  assert(finfo != NULL);
  if (!is_last) {
    if (finfo->bpname != NULL || finfo->flag & FF_INLINING) {
      assert(cur_depth > 0);
      ADD_CODE(OP_BR);
      ADD_ULEB128(cur_depth - 1);
    } else {
      ADD_CODE(OP_RETURN);
    }
  }
}

static void gen_if(Stmt *stmt, bool is_last) {
  if (is_const(stmt->if_.cond)) {
    if (is_const_truthy(stmt->if_.cond)) {
      gen_stmt(stmt->if_.tblock, is_last);
    } else if (stmt->if_.fblock != NULL) {
      gen_stmt(stmt->if_.fblock, is_last);
    }
    return;
  }

  unsigned char wt = WT_VOID;
  if (is_last && check_funcend_return(stmt))
    wt = get_func_ret_wtype(curfunc->type->func.ret);

  gen_cond(stmt->if_.cond, true, true);
  ADD_CODE(OP_IF, wt);
  ++cur_depth;
  gen_stmt(stmt->if_.tblock, is_last);
  if (stmt->if_.fblock != NULL) {
    ADD_CODE(OP_ELSE);
    gen_stmt(stmt->if_.fblock, is_last);
  }
  ADD_CODE(OP_END);
  --cur_depth;
}

static void gen_vardecl(Vector *decls) {
  assert(curfunc != NULL);
  for (int i = 0; i < decls->len; ++i) {
    VarDecl *decl = decls->data[i];
    if (decl->init_stmt != NULL) {
      if (decl->ident != NULL) {
        VarInfo *varinfo = scope_find(curscope, decl->ident, NULL);
        gen_clear_local_var(varinfo);
      }
      gen_stmt(decl->init_stmt, false);
    }
  }
}

void gen_expr_stmt(Expr *expr) {
  gen_expr(expr, false);
}

static void gen_asm(Stmt *stmt) {
  assert(stmt->asm_.str->kind == EX_STR);

  // Assume non-digit character is at the end.
  for (const char *p = skip_whitespaces(stmt->asm_.str->str.buf);;) {
    char *next;
    long op = strtol(p, &next, 10);
    if (next == p)
      break;
    ADD_CODE(op);

    p = skip_whitespaces(next);
    if (*p != ',')
      break;
    p = skip_whitespaces(p + 1);
  }
}

static void gen_stmt(Stmt *stmt, bool is_last) {
  if (stmt == NULL)
    return;

  switch (stmt->kind) {
  case ST_EMPTY: break;
  case ST_EXPR:  gen_expr_stmt(stmt->expr); break;
  case ST_RETURN:  gen_return(stmt, is_last); break;
  case ST_BLOCK:  gen_block(stmt, is_last); break;
  case ST_IF:  gen_if(stmt, is_last); break;
  case ST_SWITCH:  gen_switch(stmt); break;
  case ST_CASE:  gen_case(stmt); break;
  case ST_WHILE:  gen_while(stmt); break;
  case ST_DO_WHILE:  gen_do_while(stmt); break;
  case ST_FOR:  gen_for(stmt); break;
  case ST_BREAK:  gen_break(); break;
  case ST_CONTINUE:  gen_continue(); break;
  case ST_LABEL: gen_stmt(stmt->label.stmt, is_last); break;
  case ST_VARDECL:  gen_vardecl(stmt->vardecl.decls); break;
  case ST_ASM:  gen_asm(stmt); break;
  case ST_GOTO: assert(false); break;
  }
}

static void gen_stmts(Vector *stmts, bool is_last) {
  assert(stmts != NULL);
  for (int i = 0, len = stmts->len; i < len; ++i) {
    Stmt *stmt = stmts->data[i];
    if (stmt == NULL)
      continue;
    gen_stmt(stmt, is_last && i == len - 1);
  }
}

static uint32_t allocate_local_variables(Function *func, DataStorage *data) {
  const Type *functype = func->type;
  unsigned int ret_param = functype->func.ret->kind != TY_VOID && !is_prim_type(functype->func.ret) ? 1 : 0;
  unsigned int param_count = functype->func.params != NULL ? functype->func.params->len : 0;
  unsigned int pparam_count = 0;  // Primitive parameter count

  uint32_t frame_size = 0;
  unsigned int local_counts[4];  // I32, I64, F32, F64
  memset(local_counts, 0, sizeof(local_counts));

  for (int i = 0; i < func->scopes->len; ++i) {
    Scope *scope = func->scopes->data[i];
    if (scope->vars == NULL)
      continue;

    for (int j = 0; j < scope->vars->len; ++j) {
      VarInfo *varinfo = scope->vars->data[j];
      if (!is_local_storage(varinfo)) {
        // Static entity is allocated in global, not on stack.
        // Extern doesn't have its entity.
        // Enum members are replaced to constant value.
        continue;
      }

      int param_index = -1;
      if (i == 0 && param_count > 0) {
        int k = get_funparam_index(func, varinfo->name);
        if (k >= 0) {
          param_index = k;
          if (!is_stack_param(varinfo->type))
            ++pparam_count;
        }
      }
      if ((varinfo->storage & VS_REF_TAKEN) || (is_stack_param(varinfo->type) && param_index < 0)) {
        size_t size = type_size(varinfo->type);
        if (size < 1)
          size = 1;
        frame_size = ALIGN(frame_size, align_size(varinfo->type)) + size;
      } else if (!is_stack_param(varinfo->type)) {
        if (param_index < 0) {
          unsigned char wt = to_wtype(varinfo->type);
          assert(WT_F64 <= wt && wt <= WT_I32);
          int index = WT_I32 - wt;
          local_counts[index] += 1;
        }
      }
    }
  }
  if (frame_size > 0 || param_count != pparam_count) {
    frame_size = ALIGN(frame_size, 8);  // TODO:

    // Allocate a variable for base pointer in function top scope.
    const Name *bpname = alloc_label();
    FuncInfo *finfo = table_get(&func_info_table, func->name);
    assert(finfo != NULL);
    finfo->bpname = bpname;

    scope_add(func->scopes->data[0], bpname, &tySize, 0);
    local_counts[WT_I32 - WT_I32] += 1;
  }

  unsigned int local_index_count = 0;
  for (int i = 0; i < 4; ++i) {
    if (local_counts[i] > 0)
      ++local_index_count;
  }
  data_uleb128(data, -1, local_index_count);
  int variadic = func->type->func.vaargs;
  unsigned int local_indices[4];
  for (int i = 0; i < 4; ++i) {
    unsigned int count = local_counts[i];
    if (count > 0) {
      data_uleb128(data, -1, count);
      data_push(data, WT_I32 - i);
    }
    local_indices[i] = i == 0 ? ret_param + variadic + pparam_count
                              : local_indices[i - 1] + local_counts[i - 1];
  }

  uint32_t frame_offset = 0;
  unsigned int param_no = ret_param;
  uint32_t sparam_offset = 0;
  for (int i = 0; i < func->scopes->len; ++i) {
    Scope *scope = func->scopes->data[i];
    if (scope->vars == NULL)
      continue;

    for (int j = 0; j < scope->vars->len; ++j) {
      VarInfo *varinfo = scope->vars->data[j];
      if (!is_local_storage(varinfo))
        continue;

      VReg *vreg = calloc_or_die(sizeof(*vreg));
      varinfo->local.vreg = vreg;
      int param_index = -1;
      if (i == 0 && param_count > 0) {
        int k = get_funparam_index(func, varinfo->name);
        if (k >= 0)
          param_index = k;
      }
      vreg->param_index = ret_param + param_index;
      bool stack_param = is_stack_param(varinfo->type);
      if ((!stack_param && varinfo->storage & VS_REF_TAKEN) ||  // `&` taken wasm local var.
          (stack_param && param_index < 0)) {                   // non-prim variable (not function parameter)
        frame_offset = ALIGN(frame_offset, align_size(varinfo->type));
        vreg->non_prim.offset = frame_offset - frame_size;
        size_t size = type_size(varinfo->type);
        if (size < 1)
          size = 1;
        frame_offset += size;
      } else if (!stack_param) {  // Not `&` taken, wasm local var.
        if (param_index < 0) {
          unsigned char wt = to_wtype(varinfo->type);
          int index = WT_I32 - wt;
          vreg->prim.local_index = local_indices[index]++;
        } else {
          vreg->prim.local_index = param_no;
        }
      } else {  // Non primitive parameter, passed through stack.
        sparam_offset = ALIGN(sparam_offset, align_size(varinfo->type));
        vreg->non_prim.offset = sparam_offset;
        sparam_offset += type_size(varinfo->type);
      }
      if (param_index >= 0 && !stack_param)
        ++param_no;
    }
  }

  return frame_size;
}

static void gen_defun(Function *func) {
  if (func->scopes == NULL)  // Prototype definition
    return;

  curfunc = func;

  DataStorage *code = malloc_or_die(sizeof(*code));
  data_init(code);
  FuncExtra *extra = func->extra;
  assert(extra != NULL);
  extra->code = code;
  func->extra = extra;
  uint32_t frame_size = allocate_local_variables(func, code);

  // Prologue

  const Type *functype = func->type;
  if (functype->func.vaargs) {
    const Name *va_args = alloc_name(VA_ARGS_NAME, NULL, false);
    const VarInfo *varinfo = scope_find(func->scopes->data[0], va_args, NULL);
    assert(varinfo != NULL);
    VReg *vreg = varinfo->local.vreg;
    assert(vreg != NULL);
    ADD_CODE(OP_LOCAL_GET);
    ADD_ULEB128(functype->func.params->len);
    ADD_CODE(OP_LOCAL_SET);
    ADD_ULEB128(vreg->prim.local_index);
  }

  // Set up base pointer.
  FuncInfo *finfo = table_get(&func_info_table, func->name);
  assert(finfo != NULL);
  const Name *bpname = finfo->bpname;
  Expr *bpvar = NULL, *spvar = NULL;
  if (bpname != NULL) {
    bpvar = new_expr_variable(bpname, &tySize, NULL, func->scopes->data[0]);
    spvar = get_sp_var();
    // local.bp = global.sp;
    gen_expr_stmt(new_expr_bop(EX_ASSIGN, &tyVoid, NULL, bpvar, spvar));
    // global.sp = local.bp - frame_size;
    if (frame_size > 0) {
      gen_expr_stmt(new_expr_bop(
          EX_ASSIGN, &tyVoid, NULL, spvar,
          new_expr_bop(EX_SUB, &tySize, NULL, bpvar, new_expr_fixlit(&tySize, NULL, frame_size))));
    }
  }
  // Store ref-taken parameters to stack frame.
  if (func->params != NULL) {
    const Vector *params = func->params;
    for (int i = 0, param_count = params->len; i < param_count; ++i) {
      VarInfo *varinfo = params->data[i];
      if (!(varinfo->storage & VS_REF_TAKEN) || is_stack_param(varinfo->type))
        continue;
      VReg *vreg = varinfo->local.vreg;
      gen_bpofs(vreg->non_prim.offset);
      ADD_CODE(OP_LOCAL_GET);
      ADD_ULEB128(vreg->param_index);
      gen_store(varinfo->type);
    }
  }

  // Statements
  if (bpname != NULL) {
    unsigned char wt = get_func_ret_wtype(functype->func.ret);
    ADD_CODE(OP_BLOCK, wt);
    cur_depth += 1;
  }
  gen_stmt(func->body_block, true);

  {
    Vector *stmts = func->body_block->block.stmts;
    if (stmts->len > 0) {
      Stmt *last = stmts->data[stmts->len - 1];
      if (last->kind != ST_ASM && functype->func.ret->kind != TY_VOID && !check_funcend_return(func->body_block)) {
        assert(func->body_block->reach & REACH_STOP);
        ADD_CODE(OP_UNREACHABLE);
      }
    }
  }

  if (bpname != NULL) {
    ADD_CODE(OP_END);
    cur_depth -= 1;

    // Epilogue

    // Restore stack pointer.
    // global.sp = bp;
    gen_expr_stmt(new_expr_bop(EX_ASSIGN, &tyVoid, NULL, spvar, bpvar));
  }

  ADD_CODE(OP_END);

  size_t before = code->len;
  data_uleb128(code, 0, code->len);  // Insert code size at the top.
  extra->offset = code->len - before;

  curfunc = NULL;
  assert(cur_depth == 0);
}

static void gen_decl(Declaration *decl) {
  if (decl == NULL)
    return;

  switch (decl->kind) {
  case DCL_DEFUN:
    gen_defun(decl->defun.func);
    break;
  case DCL_VARDECL:
    break;
  case DCL_ASM:
    assert(false);
    break;
  }
}

void gen(Vector *decls) {
  if (decls == NULL)
    return;

  for (int i = 0, len = decls->len; i < len; ++i) {
    Declaration *decl = decls->data[i];
    if (decl == NULL)
      continue;
    gen_decl(decl);
  }
}

////////////////////////////////////////////////

static TagInfo *register_longjmp_tag(void) {
  static const char kTagName[] = "__c_longjmp";
  // Exception type: (jmp_buf* env)
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
  if (phase == BFP_TRAVERSE)
    register_longjmp_tag();
  if (phase != BFP_GEN)
    return;

  assert(expr->kind == EX_FUNCALL);
  Vector *args = expr->funcall.args;
  assert(args->len == 2);

  Expr *env = args->data[0];  // TODO: Assume no side effect.
  // env[1] = result == 0 ? 1 : result;
  gen_expr(env, true);
  // result == 0 ? 1 : result
  Expr *result = args->data[1];
  if (is_const(result)) {
    assert(result->kind == EX_FIXNUM);
    if (result->fixnum != 0)
      gen_expr(args->data[1], true);
    else
      ADD_CODE(OP_I32_CONST, 1);
  } else {
    gen_expr(args->data[1], true);
    ADD_CODE(OP_I32_CONST, 1);
    gen_expr(args->data[1], true);  // Assume result has no side effect.
    ADD_CODE(OP_SELECT);
  }
  ADD_CODE(OP_I32_STORE, 2, 4);

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
      } ADD_CODE(OP_CATCH); {
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
        if (var != NULL) {
          // var = env[1];
          gen_expr(env, true);
          ADD_CODE(OP_I32_LOAD, 2, 4);
          gen_set_to_var(var);
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
  Expr *fmt = parse_expr();
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

  const uint64_t MASK = (1ULL << 52) - 1ULL;
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
      !equal_name(((VarInfo*)funparams->data[funparams->len - 1])->name, param->var.name)) {
    parse_error(PE_FATAL, param->token, "must be the last parameter");
    return NULL;
  }

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

  // (ap = (char*)ap + sizeof(type), *(type*)((char*)ap - sizeof(type)))
  size_t size = type_size(type);
  Expr *size_lit = new_expr_fixlit(&tySize, ap->token, size);
  Expr *cap = make_cast(ptrof(&tyUnsignedChar), ap->token, ap, true);
  Expr *add = new_expr_bop(EX_ASSIGN, &tyVoid, ap->token, ap,
                           new_expr_bop(EX_ADD, cap->type, cap->token, cap, size_lit));
  Expr *deref = new_expr_deref(
      ap->token,
      make_cast(ptrof(type), ap->token,
                new_expr_bop(EX_SUB, cap->type, cap->token, cap, size_lit),
                true));
  return new_expr_bop(EX_COMMA, type, ident, add, deref);
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
  if (phase != BFP_GEN)
    return;

  const int stack_align = 8;  // TODO
  assert(expr->kind == EX_FUNCALL);
  Vector *args = expr->funcall.args;
  assert(args->len == 1);
  assert(curfunc != NULL);
  Expr *size = args->data[0];
  const Token *token = size->token;
  Expr *aligned_size = new_expr_int_bop(
      EX_BITAND, token,
      new_expr_addsub(EX_ADD, token, make_cast(&tySSize, token, size, false),
                      new_expr_fixlit(&tySSize, token, stack_align - 1)),
      new_expr_fixlit(&tySSize, token, -stack_align));

  Expr *spvar = get_sp_var();
  gen_expr_stmt(new_expr_bop(EX_ASSIGN, &tyVoid, NULL, spvar,
                             new_expr_bop(EX_SUB, &tySize, NULL, spvar, aligned_size)));
  gen_expr(spvar, true);
}

static void gen_builtin_memory_size(Expr *expr, enum BuiltinFunctionPhase phase) {
  if (phase != BFP_GEN)
    return;

  assert(expr->kind == EX_FUNCALL);
  ADD_CODE(OP_MEMORY_SIZE, 0x00);
}

static void gen_builtin_memory_grow(Expr *expr, enum BuiltinFunctionPhase phase) {
  if (phase != BFP_GEN)
    return;

  assert(expr->kind == EX_FUNCALL);
  Vector *args = expr->funcall.args;
  assert(args->len == 1);
  gen_expr(args->data[0], true);
  ADD_CODE(OP_MEMORY_GROW, 0x00);
}

void install_builtins(void) {
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
    static BuiltinFunctionProc p_memory_size = &gen_builtin_memory_size;
    Type *rettype = &tyInt;
    Vector *params = new_vector();
    Type *type = new_func_type(rettype, params, false);

    add_builtin_function("__builtin_memory_size", type, &p_memory_size, true);
  }
  {
    static BuiltinFunctionProc p_memory_grow = &gen_builtin_memory_grow;
    Type *rettype = &tyInt;
    Vector *params = new_vector();
    vec_push(params, &tySize);
    Type *type = new_func_type(rettype, params, false);

    add_builtin_function("__builtin_memory_grow", type, &p_memory_grow, true);
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
