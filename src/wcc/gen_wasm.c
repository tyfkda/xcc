#include "wcc.h"

#include <alloca.h>
#include <assert.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>  // malloc
#include <string.h>

#include "ast.h"
#include "lexer.h"  // parse_error
#include "parser.h"  // curfunc
#include "table.h"
#include "type.h"
#include "util.h"
#include "var.h"
#include "wasm_util.h"

#define CODE  (((FuncExtra*)curfunc->extra)->code)

#define ADD_LEB128(x)  emit_leb128(CODE, -1, x)
#define ADD_ULEB128(x) emit_uleb128(CODE, -1, x)

// TODO: Endian.
#define ADD_F32(x)     do { float f = (x); add_code((unsigned char*)&f, sizeof(f)); } while (0)
#define ADD_F64(x)     do { double d = (x); add_code((unsigned char*)&d, sizeof(d)); } while (0)

static void gen_lval(Expr *expr, bool needval);

void add_code(const unsigned char* buf, size_t size) {
  data_append(CODE, buf, size);
}

void emit_leb128(DataStorage *data, ssize_t pos, int64_t val) {
  unsigned char buf[5], *p = buf;
  const int64_t MAX = 1 << 6;
  for (;;) {
    if (val < MAX && val >= -MAX) {
      *p++ = val & 0x7f;
      data_insert(data, pos, buf, p - buf);
      return;
    }
    *p++ = (val & 0x7f) | 0x80;
    val >>= 7;
  }
}

void emit_uleb128(DataStorage *data, ssize_t pos, uint64_t val) {
  unsigned char buf[5], *p = buf;
  const uint64_t MAX = 1 << 7;
  for (;;) {
    if (val < MAX) {
      *p++ = val & 0x7f;
      data_insert(data, pos, buf, p - buf);
      return;
    }
    *p++ = (val & 0x7f) | 0x80;
    val >>= 7;
  }
}

////////////////////////////////////////////////

static void gen_cond(Expr *cond, bool tf, bool needval);

Expr *get_sp_var(void) {
  static Expr *spvar;
  if (spvar == NULL) {
    const Name *spname = alloc_name(SP_NAME, NULL, false);
    GVarInfo *info = get_gvar_info_from_name(spname);
    assert(info != NULL);
    spvar = new_expr_variable(spname, info->varinfo->type, NULL, global_scope);
  }
  return spvar;
}

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

static void gen_stmt(Stmt *stmt);
static void gen_stmts(Vector *stmts);

static int cur_depth;
static int break_depth;
static int continue_depth;

unsigned char to_wtype(const Type *type) {
  switch (type->kind) {
  case TY_FIXNUM: return type_size(type) <= I32_SIZE ? WT_I32 : WT_I64;
#ifndef __NO_FLONUM
  case TY_FLONUM: return type->flonum.kind == FL_FLOAT ? WT_F32 : WT_F64;
#endif
  case TY_PTR:
  case TY_ARRAY:
    // Pointer and array is handled as an index of linear memroy.
    return WT_I32;
  default: assert(!"Illegal"); break;
  }
  return WT_I32;
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
#ifndef __NO_FLONUM
  case TY_FLONUM:
    if (type->flonum.kind < FL_DOUBLE)
      ADD_CODE(OP_F32_LOAD, 2, 0);
    else
      ADD_CODE(OP_F64_LOAD, 3, 0);
    break;
#endif
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
#ifndef __NO_FLONUM
  case TY_FLONUM:
    if (type->flonum.kind < FL_DOUBLE)
      ADD_CODE(OP_F32_STORE, 2, 0);
    else
      ADD_CODE(OP_F64_STORE, 3, 0);
    break;
#endif
  default: assert(false); break;
  }
}

static void gen_arith(enum ExprKind kind, const Type *type) {
  assert(is_number(type) || ptr_or_array(type));
  int index = 0;
  bool is_unsigned = is_fixnum(type->kind) && type->fixnum.is_unsigned;
#ifndef __NO_FLONUM
  if (is_flonum(type)) {
    assert(kind < EX_MOD);
    index = type->flonum.kind != FL_FLOAT ? 3 : 2;
  } else
#endif
  {
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

static void gen_cast(const Type *dst, Type *src) {
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
              ADD_LEB128((1 << (d * CHAR_BIT)) - 1);
              ADD_CODE(OP_I32_AND);
            } else {
              int shift = (s - d) * CHAR_BIT;
              ADD_CODE(OP_I32_CONST);
              ADD_LEB128(shift);
              ADD_CODE(OP_I32_SHL);
              ADD_CODE(OP_I32_CONST);
              ADD_LEB128(shift);
              ADD_CODE(OP_I32_SHR_S);
            }
          }
        }
      }
      return;
#ifndef __NO_FLONUM
    case TY_FLONUM:
      {
        static const unsigned char OpTable[][4] = {
          { OP_I32_TRUNC_F32_S, OP_I32_TRUNC_F64_S, OP_I64_TRUNC_F32_S, OP_I64_TRUNC_F64_S },
          { OP_I32_TRUNC_F32_U, OP_I32_TRUNC_F64_U, OP_I64_TRUNC_F32_U, OP_I64_TRUNC_F64_U },
        };
        int d = type_size(dst);
        int index = (d > I32_SIZE ? 2 : 0) + (src->flonum.kind != FL_FLOAT ? 1 : 0);
        bool du = !is_fixnum(dst->kind) || dst->fixnum.is_unsigned;
        ADD_CODE(OpTable[du][index]);
      }
      return;
#endif
    default: break;
    }
    break;
#ifndef __NO_FLONUM
  case TY_FLONUM:
    switch (src->kind) {
    case TY_FIXNUM:
      {
        static const unsigned char OpTable[][4] = {
          { OP_F32_CONVERT_I32_S, OP_F32_CONVERT_I64_S, OP_F64_CONVERT_I32_S, OP_F64_CONVERT_I64_S },
          { OP_F32_CONVERT_I32_U, OP_F32_CONVERT_I64_U, OP_F64_CONVERT_I32_U, OP_F64_CONVERT_I64_U },
        };
        int s = type_size(src);
        int index = (dst->flonum.kind != FL_FLOAT ? 2 : 0) + (s > I32_SIZE ? 1 : 0);
        bool su = !is_fixnum(src->kind) || src->fixnum.is_unsigned;
        ADD_CODE(OpTable[su][index]);
      }
      return;
    case TY_FLONUM:
      {
        assert(dst->flonum.kind != src->flonum.kind);
        switch (dst->flonum.kind) {
        case FL_FLOAT:  ADD_CODE(OP_F32_DEMOTE_F64); break;
        case FL_DOUBLE: ADD_CODE(OP_F64_PROMOTE_F32); break;
        }
      }
      return;
    default: break;
    }
    break;
#endif
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
  ADD_ULEB128(info->index);
}

static void gen_funcall(Expr *expr) {
  Expr *func = expr->funcall.func;
  if (func->kind == EX_VAR && is_global_scope(func->var.scope)) {
    void *proc = table_get(&builtin_function_table, func->var.name);
    if (proc != NULL) {
      (*(BuiltinFunctionProc*)proc)(expr);
      return;
    }
  }

  Vector *args = expr->funcall.args;
  int arg_count = args != NULL ? args->len : 0;

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
        if (t->kind == TY_FIXNUM && t->fixnum.kind < FX_INT)
          t = &tyInt;
        //vaarg_bufsiz = ALIGN(vaarg_bufsiz, align_size(t));
        vaarg_bufsiz += type_size(t);
      }
    }
  }

  Expr *spvar = NULL;
  if (sarg_siz > 0 || vaarg_bufsiz > 0) {
    spvar = get_sp_var();
    // global.sp -= ALIGN(sarg_siz + vaarg_bufsiz, 8);
    gen_expr_stmt(
        new_expr_unary(EX_MODIFY, &tyVoid, NULL,
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
    gen_lval(e, true);
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
          // _memcpy(global.sp + sarg_offset, &arg, size);
          gen_expr(new_expr_bop(EX_ADD, &tySize, NULL, spvar,
                                new_expr_fixlit(&tySize, NULL, sarg_offset)),
                   true);
          gen_expr(arg, true);

          ADD_CODE(type_size(&tySize) <= I32_SIZE ? OP_I32_CONST : OP_I64_CONST);
          ADD_LEB128(size);
          gen_funcall_by_name(alloc_name(MEMCPY_NAME, NULL, false));
        }
        sarg_offset += ALIGN(size, 4);
      }
    } else {
      assert(!is_stack_param(arg->type));
      // *(global.sp + sarg_siz + vaarg_offset) = arg
      gen_expr(new_expr_bop(EX_ADD, &tySize, NULL, spvar,
                            new_expr_fixlit(&tySize, NULL, sarg_siz + vaarg_offset)),
               true);
      const Type *t = arg->type;
      if (t->kind == TY_FIXNUM && t->fixnum.kind < FX_INT)
        t = &tyInt;
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
    ADD_ULEB128(index);  // signature index
    ADD_ULEB128(0);     // table index
  }

  if (sarg_siz > 0 || vaarg_bufsiz > 0) {
    // global.sp += ALIGN(sarg_siz + vaarg_bufsiz, 8);
    gen_expr_stmt(
        new_expr_unary(EX_MODIFY, &tyVoid, NULL,
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
  assert(bpvarinfo->local.reg != NULL);
  ADD_CODE(OP_LOCAL_GET);
  ADD_ULEB128(bpvarinfo->local.reg->prim.local_index);

  if (offset != 0) {
    ADD_CODE(OP_I32_CONST);
    ADD_LEB128(offset);
    ADD_CODE(OP_I32_ADD);
  }
}

static void gen_clear_local_var(const VarInfo *varinfo) {
  // Fill with zeros regardless of variable type.
  size_t size = type_size(varinfo->type);
  if (size <= 0)
    return;
  // VReg *reg = varinfo->local.reg;
  // new_ir_clear(reg, size);

  if (is_prim_type(varinfo->type) && !(varinfo->storage & VS_REF_TAKEN)) {
#ifndef __NO_FLONUM
    if (is_flonum(varinfo->type)) {
      switch (varinfo->type->flonum.kind) {
      case FL_FLOAT:  ADD_CODE(OP_F32_CONST); ADD_F32(0); break;
      case FL_DOUBLE: ADD_CODE(OP_F64_CONST); ADD_F64(0); break;
      default: assert(false); break;
      }
      return;
    }
#endif
    ADD_CODE(size <= I32_SIZE ? OP_I32_CONST : OP_I64_CONST);
    ADD_LEB128(0);
    return;
  }

  // gen_lval(expr->bop.lhs);
  VReg *vreg = varinfo->local.reg;
  gen_bpofs(vreg->non_prim.offset);
  ADD_CODE(OP_I32_CONST);
  ADD_LEB128(0);
  ADD_CODE(type_size(&tySize) <= I32_SIZE ? OP_I32_CONST : OP_I64_CONST);
  ADD_LEB128(size);
  gen_funcall_by_name(alloc_name(MEMSET_NAME, NULL, false));
}

static void gen_lval(Expr *expr, bool needval) {
  switch (expr->kind) {
  case EX_VAR:
    if (needval) {
      Scope *scope;
      const VarInfo *varinfo = scope_find(expr->var.scope, expr->var.name, &scope);
      assert(varinfo != NULL && scope == expr->var.scope);
      assert(!is_prim_type(expr->type) || varinfo->storage & VS_REF_TAKEN);
      if (is_global_scope(scope) || (varinfo->storage & (VS_STATIC | VS_EXTERN))) {
        if (varinfo->type->kind == TY_FUNC) {
          uint32_t indirect_func = get_indirect_function_index(expr->var.name);
          ADD_CODE(OP_I32_CONST);
          ADD_ULEB128(indirect_func);
        } else {
          GVarInfo *info = get_gvar_info(expr);
          ADD_CODE(OP_I32_CONST);
          ADD_LEB128(info->non_prim.address);
        }
      } else {
        VReg *vreg = varinfo->local.reg;
        gen_bpofs(vreg->non_prim.offset);
      }
    }
    return;
  case EX_DEREF:
    gen_expr(expr->unary.sub, needval);
    return;
  case EX_MEMBER:
    {
      const Type *type = expr->member.target->type;
      if (ptr_or_array(type))
        type = type->pa.ptrof;
      assert(type->kind == TY_STRUCT);
      const Vector *members = type->struct_.info->members;
      const MemberInfo *member = members->data[expr->member.index];

      gen_expr(expr->member.target, needval);
      if (needval) {
        if (member->offset == 0)
          return;
        ADD_CODE(OP_I32_CONST);
        ADD_LEB128(member->offset);
        ADD_CODE(OP_I32_ADD);
      }
      return;
    }
  case EX_COMPLIT:
    {
      Expr *var = expr->complit.var;
      VarInfo *varinfo = scope_find(var->var.scope, var->var.name, NULL);
      assert(varinfo != NULL);
      gen_clear_local_var(varinfo);
      gen_stmts(expr->complit.inits);
      gen_lval(var, needval);
    }
    return;
  default: assert(false); break;
  }
}

static void gen_var(Expr *expr) {
  switch (expr->type->kind) {
  case TY_FIXNUM:
  case TY_PTR:
#ifndef __NO_FLONUM
  case TY_FLONUM:
#endif
    {
      Scope *scope;
      const VarInfo *varinfo = scope_find(expr->var.scope, expr->var.name, &scope);
      assert(varinfo != NULL && scope == expr->var.scope);
      if (varinfo->storage & VS_REF_TAKEN) {
        gen_lval(expr, true);
        gen_load(expr->type);
      } else if (!is_global_scope(scope) && !(varinfo->storage & (VS_STATIC | VS_EXTERN))) {
        VReg *vreg = varinfo->local.reg;
        assert(vreg != NULL);
        ADD_CODE(OP_LOCAL_GET);
        ADD_ULEB128(vreg->prim.local_index);
      } else {
        GVarInfo *info = get_gvar_info(expr);
        ADD_CODE(OP_GLOBAL_GET);
        ADD_ULEB128(info->non_prim.address);
      }
    }
    break;

  default:
    assert(false);
    // Fallthrough to suppress compile error.
  case TY_ARRAY:   // Use variable address as a pointer.
  case TY_STRUCT:  // struct value is handled as a pointer.
  case TY_FUNC:
    gen_lval(expr, true);
    return;
  }
}

static void gen_block_expr(Stmt *stmt, bool needval) {
  assert(stmt->kind == ST_BLOCK);

  if (stmt->block.scope != NULL) {
    assert(curscope == stmt->block.scope->parent);
    curscope = stmt->block.scope;
  }

  Vector *stmts = stmt->block.stmts;
  int last = stmts->len - 1;
  assert(last >= 0);
  for (int i = 0; i < last; ++i) {
    Stmt *stmt = stmts->data[i];
    if (stmt == NULL)
      continue;
    gen_stmt(stmt);
  }
  Stmt *last_stmt = stmts->data[last];
  assert(last_stmt->kind == ST_EXPR);
  gen_expr(last_stmt->expr, needval);

  if (stmt->block.scope != NULL)
    curscope = curscope->parent;
}

static void gen_incdec(const Type *type, bool dec) {
  int addend = type->kind == TY_PTR ? type_size(type->pa.ptrof) : 1;
  unsigned char wtype = to_wtype(type);
  switch (wtype) {
  case WT_I32:
  case WT_I64:
    {
      static unsigned char CONST_OP[] = {OP_I32_CONST, OP_I64_CONST};
      static unsigned char ADDSUB_OP[] = {OP_I32_ADD, OP_I64_ADD, OP_I32_SUB, OP_I64_SUB};
      int i1 = wtype == WT_I32 ? 0 : 1;
      int i2 = dec ? 2 : 0;
      ADD_CODE(CONST_OP[i1]);
      ADD_ULEB128(addend);
      ADD_CODE(ADDSUB_OP[i1 | i2]);
    }
    break;
  case WT_F32:
  case WT_F64:
    {
      static unsigned char ADDSUB_OP[] = {OP_F32_ADD, OP_F64_ADD, OP_F32_SUB, OP_F64_SUB};
      int i2 = dec ? 2 : 0;
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
  default: assert(false); break;
  }
}

void gen_expr(Expr *expr, bool needval) {
  switch (expr->kind) {
  case EX_FIXNUM:
    if (needval) {
      if (type_size(expr->type) <= I32_SIZE) {
        ADD_CODE(OP_I32_CONST);
        ADD_LEB128((int32_t)expr->fixnum);
      } else {
        ADD_CODE(OP_I64_CONST);
        ADD_LEB128(expr->fixnum);
      }
    }
    break;

#ifndef __NO_FLONUM
  case EX_FLONUM:
    if (needval) {
      switch (expr->type->flonum.kind) {
      case FL_FLOAT:
        ADD_CODE(OP_F32_CONST);
        ADD_F32(expr->flonum);
        break;
      case FL_DOUBLE:
        ADD_CODE(OP_F64_CONST);
        ADD_F64(expr->flonum);
        break;
      default: assert(false); break;
      }
    }
    break;
#endif

  case EX_VAR:
    if (needval)
      gen_var(expr);
    break;

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
    gen_expr(expr->bop.lhs, needval);
    gen_expr(expr->bop.rhs, needval);
    if (needval)
      gen_arith(expr->kind, expr->type);
    break;

  case EX_EQ:
  case EX_NE:
  case EX_LT:
  case EX_GT:
  case EX_LE:
  case EX_GE:
  case EX_LOGAND:
  case EX_LOGIOR:
    gen_cond(expr, true, needval);
    break;

  case EX_POS:
    gen_expr(expr->unary.sub, needval);
    break;

  case EX_NEG:
    if (needval) {
      switch (expr->type->kind) {
      case TY_FIXNUM:
        ADD_CODE(type_size(expr->type) <= I32_SIZE ? OP_I32_CONST : OP_I64_CONST);
        ADD_LEB128(0);
        break;
#ifndef __NO_FLONUM
      case TY_FLONUM:
        switch (expr->type->flonum.kind) {
        case FL_FLOAT:
          ADD_CODE(OP_F32_CONST);
          ADD_F32(0);
          break;
        case FL_DOUBLE:
          ADD_CODE(OP_F64_CONST);
          ADD_F64(0);
          break;
        default: assert(false); break;
        }
        break;
#endif
      default: assert(false); break;
      }

      gen_expr(expr->unary.sub, true);
      gen_arith(EX_SUB, expr->type);
    } else {
      gen_expr(expr->unary.sub, false);
    }
    break;

  case EX_BITNOT:
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
    break;

  case EX_PREINC:
  case EX_PREDEC:
  case EX_POSTINC:
  case EX_POSTDEC:
    {
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
      gen_incdec(expr->type, IS_DEC(expr));

      Scope *scope;
      const VarInfo *varinfo = scope_find(target->var.scope, target->var.name, &scope);
      assert(varinfo != NULL);
      if (!is_global_scope(scope) && !(varinfo->storage & (VS_STATIC | VS_EXTERN))) {
        VReg *vreg = varinfo->local.reg;
        assert(vreg != NULL);
        ADD_CODE(needval ? OP_LOCAL_TEE : OP_LOCAL_SET);
        ADD_ULEB128(vreg->prim.local_index);
      } else {
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
    break;

  case EX_ASSIGN:
    {
      assert(expr->type->kind == TY_VOID);
      Expr *lhs = expr->bop.lhs;
      switch (lhs->type->kind) {
      case TY_FIXNUM:
      case TY_PTR:
#ifndef __NO_FLONUM
      case TY_FLONUM:
#endif
        if (expr->bop.lhs->kind == EX_VAR) {
          Scope *scope;
          const VarInfo *varinfo = scope_find(lhs->var.scope, lhs->var.name, &scope);
          assert(varinfo != NULL);
          if (varinfo->storage & VS_REF_TAKEN) {
            gen_lval(lhs, true);
            gen_expr(expr->bop.rhs, true);
            gen_store(lhs->type);
          } else if (!is_global_scope(scope) && !(varinfo->storage & (VS_STATIC | VS_EXTERN))) {
            VReg *vreg = varinfo->local.reg;
            assert(vreg != NULL);
            gen_expr(expr->bop.rhs, true);
            ADD_CODE(OP_LOCAL_SET);
            ADD_ULEB128(vreg->prim.local_index);
          } else {
            GVarInfo *info = get_gvar_info(lhs);
            gen_expr(expr->bop.rhs, true);
            ADD_CODE(OP_GLOBAL_SET);
            ADD_ULEB128(info->prim.index);
          }
        } else {
          gen_lval(expr->bop.lhs, true);
          gen_expr(expr->bop.rhs, true);
          gen_store(expr->bop.lhs->type);
        }
        break;
      case TY_STRUCT:
        {
          size_t size = type_size(expr->bop.lhs->type);
          if (size > 0) {
            gen_lval(expr->bop.lhs, true);
            gen_expr(expr->bop.rhs, true);
            ADD_CODE(type_size(&tySize) <= I32_SIZE ? OP_I32_CONST : OP_I64_CONST);
            ADD_LEB128(size);
            gen_funcall_by_name(alloc_name(MEMCPY_NAME, NULL, false));
          }
        }
        break;
      default: assert(false); break;
      }
    }
    break;

  case EX_MODIFY:
    {
      assert(expr->type->kind == TY_VOID);
      Expr *sub = expr->unary.sub;
      Expr *lhs = sub->bop.lhs;
      switch (lhs->type->kind) {
      case TY_FIXNUM:
      case TY_PTR:
#ifndef __NO_FLONUM
      case TY_FLONUM:
#endif
        {
          bool simple = false;
          switch (lhs->kind) {
          case EX_VAR:
            {
              VarInfo *varinfo = scope_find(lhs->var.scope, lhs->var.name, NULL);
              assert(varinfo != NULL);
              simple = !(varinfo->storage & VS_REF_TAKEN);
            }
            break;
          case EX_DEREF:
            assert(lhs->unary.sub->kind == EX_VAR);
            simple = false;
            break;
          default: assert(false); break;
          }
          if (!simple) {
            const Type *type = lhs->type;
            gen_lval(lhs, true);
            gen_lval(lhs, true);  // We can safely evaluate lhs two times, since lhs has no side effect.
            gen_load(type);
            gen_expr(sub->bop.rhs, true);
            gen_arith(sub->kind, type);
            gen_store(type);
            return;
          }

          assert(lhs->kind == EX_VAR);
          gen_expr(lhs, true);
          gen_expr(sub->bop.rhs, true);
          gen_arith(sub->kind, sub->type);

          Scope *scope;
          const VarInfo *varinfo = scope_find(lhs->var.scope, lhs->var.name, &scope);
          assert(varinfo != NULL);
          if (!is_global_scope(scope) && !(varinfo->storage & (VS_STATIC | VS_EXTERN))) {
            assert(varinfo->local.reg != NULL);
            assert(!(varinfo->storage & VS_REF_TAKEN));
            ADD_CODE(OP_LOCAL_SET);
            ADD_ULEB128(varinfo->local.reg->prim.local_index);
          } else {
            GVarInfo *info = get_gvar_info(lhs);
            ADD_CODE(OP_GLOBAL_SET);
            ADD_ULEB128(info->prim.index);
          }
        }
        return;
      default: assert(false); break;
      }
    }
    break;

  case EX_COMMA:
    gen_expr(expr->bop.lhs, false);
    gen_expr(expr->bop.rhs, needval);
    break;

  case EX_CAST:
    gen_expr(expr->unary.sub, needval);
    if (needval)
      gen_cast(expr->type, expr->unary.sub->type);
    break;

  case EX_REF:
    {
      Expr *sub = expr->unary.sub;
      /*if (sub->kind == EX_VAR && !is_global_scope(sub->var.scope)) {
        const VarInfo *varinfo = scope_find(sub->var.scope, sub->var.name, NULL);
        assert(varinfo != NULL);
        assert(varinfo->local.reg != NULL);
        varinfo->local.reg->flag |= VRF_REF;
      }*/
      gen_lval(sub, needval);
    }
    break;

  case EX_DEREF:
    gen_expr(expr->unary.sub, needval);
    if (needval) {
      switch (expr->type->kind) {
      case TY_FIXNUM:
      case TY_PTR:
#ifndef __NO_FLONUM
      case TY_FLONUM:
#endif
        //result = new_ir_unary(IR_LOAD, reg, to_vtype(expr->type));
        //return result;
        gen_load(expr->type);
        return;

      default:
        assert(false);
        // Fallthrough to suppress compile error.
      case TY_ARRAY:
      case TY_STRUCT:
      case TY_FUNC:
        // array, struct and func values are handled as a pointer.
        return;
      }
    }
    break;

  case EX_TERNARY:
    gen_ternary(expr, needval);
    break;

  case EX_MEMBER:
    gen_lval(expr, needval);
    if (needval && is_prim_type(expr->type)) {
      gen_load(expr->type);
    }
    break;

  case EX_FUNCALL:
    gen_funcall(expr);
    if (!needval && is_prim_type(expr->type))
      ADD_CODE(OP_DROP);
    break;

  case EX_COMPLIT:
    {
      Expr *var = expr->complit.var;
      VarInfo *varinfo = scope_find(var->var.scope, var->var.name, NULL);
      assert(varinfo != NULL);
      gen_clear_local_var(varinfo);
      gen_stmts(expr->complit.inits);
      gen_expr(var, needval);
    }
    break;

  case EX_BLOCK:
    gen_block_expr(expr->block, needval);
    break;

  default: assert(!"Not implemeneted"); break;
  }
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
#ifndef __NO_FLONUM
  if (is_flonum(lhs->type)) {
    index = lhs->type->flonum.kind != FL_FLOAT ? 5 : 4;
  } else
#endif
  {
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
    gen_cond(cond->bop.lhs, true, true);
    ADD_CODE(OP_IF, needval ? WT_I32 : WT_VOID);
    ++cur_depth;
    gen_cond(cond->bop.rhs, tf, needval);
    if (needval) {
      ADD_CODE(OP_ELSE);
      ADD_CODE(OP_I32_CONST, tf ? 0 : 1);
    }
    ADD_CODE(OP_END);
    --cur_depth;
    break;
  case EX_LOGIOR:
    gen_cond(cond->bop.lhs, false, true);
    ADD_CODE(OP_IF, needval ? WT_I32 : WT_VOID);
    ++cur_depth;
    gen_cond(cond->bop.rhs, tf, needval);
    if (needval) {
      ADD_CODE(OP_ELSE);
      ADD_CODE(OP_I32_CONST, tf ? 1 : 0);
    }
    ADD_CODE(OP_END);
    --cur_depth;
    break;
  default:
    assert(false);
    break;
  }
}

static void gen_cond_jmp(Expr *cond, bool tf, uint32_t depth) {
  gen_cond(cond, tf, true);
  ADD_CODE(OP_BR_IF);
  ADD_ULEB128(depth);
}

static void gen_switch_table_jump(Stmt *stmt, Expr *value, Fixnum min, Fixnum max, int default_index) {
  Vector *cases = stmt->switch_.cases;
  int case_count = cases->len;

  unsigned int vrange = max - min + 1;
  bool use_alloca = vrange <= 64;
  int *table = use_alloca ? alloca(sizeof(*table) * vrange) : malloc(sizeof(*table) * vrange);
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
  assert(is_const(value) || value->kind == EX_VAR);  // Must be simple expression, because this is evaluated multiple times.
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
      if (c->kind == ST_DEFAULT) {
        default_index = i;
        continue;
      }
      gen_expr(value, true);
      ADD_CODE(op_const);
      ADD_LEB128(c->case_.value->fixnum);
      ADD_CODE(op_eq,
              OP_BR_IF);
      ADD_ULEB128(i);
    }
    // Jump to default.
    ADD_CODE(OP_BR);
    ADD_ULEB128(default_index);
  }

  // Body.
  gen_stmt(stmt->switch_.body);

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
  gen_stmt(stmt->while_.body);
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
  gen_stmt(stmt->while_.body);
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
  gen_stmt(stmt->for_.body);
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

static void gen_block(Stmt *stmt) {
  if (stmt->block.scope != NULL) {
    assert(curscope == stmt->block.scope->parent);
    curscope = stmt->block.scope;
  }
  gen_stmts(stmt->block.stmts);
  if (stmt->block.scope != NULL)
    curscope = curscope->parent;
}

static void gen_return(Stmt *stmt) {
  assert(curfunc != NULL);
  FuncInfo *finfo = table_get(&func_info_table, curfunc->name);
  assert(finfo != NULL);
  if (stmt->return_.val != NULL) {
    Expr *val = stmt->return_.val;
    const Type *rettype = finfo->type->func.ret;
    if (is_prim_type(rettype)) {
      gen_expr(val, true);
    } else {
      // Local #0 is the pointer for result.
      ADD_CODE(OP_LOCAL_GET, 0);
      gen_expr(val, true);
      ADD_CODE(OP_I32_CONST);
      ADD_LEB128(type_size(rettype));
      gen_funcall_by_name(alloc_name(MEMCPY_NAME, NULL, false));
      // Result.
      ADD_CODE(OP_LOCAL_GET, 0);
    }
  }
  if (finfo->bpname != NULL) {
    assert(cur_depth > 0);
    ADD_CODE(OP_BR);
    ADD_ULEB128(cur_depth - 1);
  } else {
    ADD_CODE(OP_RETURN);
  }
}

static void gen_if(Stmt *stmt) {
  if (is_const(stmt->if_.cond)) {
    if (is_const_truthy(stmt->if_.cond)) {
      gen_stmt(stmt->if_.tblock);
    } else if (stmt->if_.fblock != NULL) {
      gen_stmt(stmt->if_.fblock);
    }
    return;
  }

  gen_cond(stmt->if_.cond, true, true);
  ADD_CODE(OP_IF, WT_VOID);
  ++cur_depth;
  gen_stmt(stmt->if_.tblock);
  if (stmt->if_.fblock != NULL) {
    ADD_CODE(OP_ELSE);
    gen_stmt(stmt->if_.fblock);
  }
  ADD_CODE(OP_END);
  --cur_depth;
}

static void gen_vardecl(Vector *decls, Vector *inits) {
  if (curfunc != NULL) {
    for (int i = 0; i < decls->len; ++i) {
      VarDecl *decl = decls->data[i];
      if (decl->init == NULL)
        continue;
      VarInfo *varinfo = scope_find(curscope, decl->ident->ident, NULL);
      if (varinfo == NULL || (varinfo->storage & (VS_STATIC | VS_EXTERN)) ||
          !(varinfo->type->kind == TY_STRUCT ||
            varinfo->type->kind == TY_ARRAY))
        continue;
      gen_clear_local_var(varinfo);
    }
  }
  gen_stmts(inits);
}

void gen_expr_stmt(Expr *expr) {
  gen_expr(expr, false);
}

static void gen_stmt(Stmt *stmt) {
  if (stmt == NULL)
    return;

  switch (stmt->kind) {
  case ST_EXPR:  gen_expr_stmt(stmt->expr); break;
  case ST_RETURN:  gen_return(stmt); break;
  case ST_BLOCK:  gen_block(stmt); break;
  case ST_IF:  gen_if(stmt); break;
  case ST_SWITCH:  gen_switch(stmt); break;
  case ST_CASE: case ST_DEFAULT:  gen_case(stmt); break;
  case ST_WHILE:  gen_while(stmt); break;
  case ST_DO_WHILE:  gen_do_while(stmt); break;
  case ST_FOR:  gen_for(stmt); break;
  case ST_BREAK:  gen_break(); break;
  case ST_CONTINUE:  gen_continue(); break;
  // case ST_GOTO:  gen_goto(stmt); break;
  // case ST_LABEL:  gen_label(stmt); break;
  case ST_VARDECL:  gen_vardecl(stmt->vardecl.decls, stmt->vardecl.inits); break;
  // case ST_ASM:  gen_asm(stmt); break;
  default: assert(false); break;
  }
}

static void gen_stmts(Vector *stmts) {
  if (stmts == NULL)
    return;

  for (int i = 0, len = stmts->len; i < len; ++i) {
    Stmt *stmt = stmts->data[i];
    if (stmt == NULL)
      continue;
    gen_stmt(stmt);
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
      if (varinfo->storage & (VS_STATIC | VS_EXTERN | VS_ENUM_MEMBER)) {
        // Static entity is allocated in global, not on stack.
        // Extern doesn't have its entity.
        // Enum members are replaced to constant value.
        continue;
      }

      int param_index = -1;
      if (i == 0 && param_count > 0) {
        const Vector *params = functype->func.params;
        for (int i = 0, n = params->len; i < n; ++i) {
          const VarInfo *v = params->data[i];
          if (equal_name(v->name, varinfo->name)) {
            param_index = i;
            if (!is_stack_param(v->type))
              ++pparam_count;
            break;
          }
        }
      }
      if ((varinfo->storage & VS_REF_TAKEN) || (is_stack_param(varinfo->type) && param_index < 0)) {
        size_t size = type_size(varinfo->type);
        if (size < 1)
          size = 1;
        frame_size += size;
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
  emit_uleb128(data, -1, local_index_count);
  int variadic = func->type->func.vaargs;
  unsigned int local_indices[4];
  for (int i = 0; i < 4; ++i) {
    unsigned int count = local_counts[i];
    if (count > 0) {
      emit_uleb128(data, -1, count);
      data_push(data, WT_I32 - i);
    }
    local_indices[i] = i == 0 ? ret_param + variadic + pparam_count : local_indices[i - 1] + local_counts[i - 1];
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
      if (varinfo->storage & (VS_STATIC | VS_EXTERN | VS_ENUM_MEMBER))
        continue;

      VReg *vreg = calloc(1, sizeof(*vreg));
      varinfo->local.reg = vreg;
      int param_index = -1;
      if (i == 0 && param_count > 0) {
        const Vector *params = functype->func.params;
        for (int i = 0, n = params->len; i < n; ++i) {
          const VarInfo *v = params->data[i];
          if (equal_name(v->name, varinfo->name)) {
            param_index = i;
            break;
          }
        }
      }
      vreg->param_index = ret_param + param_index;
      if ((varinfo->storage & VS_REF_TAKEN) || (is_stack_param(varinfo->type) && param_index < 0)) {
        vreg->non_prim.offset = ALIGN(frame_offset, align_size(varinfo->type)) - frame_size;
        size_t size = type_size(varinfo->type);
        if (size < 1)
          size = 1;
        frame_offset += size;
      } else if (!is_stack_param(varinfo->type)) {
        if (param_index < 0) {
          unsigned char wt = to_wtype(varinfo->type);
          int index = WT_I32 - wt;
          vreg->prim.local_index = local_indices[index]++;
        } else {
          vreg->prim.local_index = param_no;
        }
      } else {
        sparam_offset = ALIGN(sparam_offset, align_size(varinfo->type));
        vreg->non_prim.offset = sparam_offset;
        sparam_offset += type_size(varinfo->type);
      }
      if (param_index >= 0 && !is_stack_param(varinfo->type))
        ++param_no;
    }
  }

  return frame_size;
}

static void gen_defun(Function *func) {
  if (func->scopes == NULL)  // Prototype definition
    return;

  curfunc = func;

  DataStorage *code = malloc(sizeof(*code));
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
    VReg *vreg = varinfo->local.reg;
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
    // global.sp -= frame_size;
    if (frame_size > 0) {
      gen_expr_stmt(
          new_expr_unary(EX_MODIFY, &tyVoid, NULL,
                         new_expr_bop(EX_SUB, &tySize, NULL, spvar,
                                      new_expr_fixlit(&tySize, NULL, frame_size))));
    }
  }
  // Store ref-taken parameters to stack frame.
  unsigned int param_count = functype->func.params != NULL ? functype->func.params->len : 0;  // TODO: Consider stack params.
  if (param_count > 0) {
    const Vector *params = functype->func.params;
    for (unsigned int i = 0; i < param_count; ++i) {
      VarInfo *varinfo = params->data[i];
      if (!(varinfo->storage & VS_REF_TAKEN))
        continue;
      VReg *vreg = varinfo->local.reg;
      gen_bpofs(vreg->non_prim.offset);
      ADD_CODE(OP_LOCAL_GET);
      ADD_ULEB128(vreg->param_index);
      gen_store(varinfo->type);
    }
  }

  // Statements
  curscope = func->scopes->data[0];
  {
    unsigned char wt = is_prim_type(functype->func.ret) ? to_wtype(functype->func.ret) : functype->func.ret->kind != TY_VOID ? WT_I32 : WT_VOID;
    if (bpname != NULL) {
      ADD_CODE(OP_BLOCK, wt);
      cur_depth += 1;
    }
    // Push dummy return value to avoid empty fallthrough (or no return statement).
    switch (wt) {
    case WT_I32:  ADD_CODE(OP_I32_CONST); ADD_LEB128(0); break;
    case WT_I64:  ADD_CODE(OP_I64_CONST); ADD_LEB128(0); break;
    case WT_F32:  ADD_CODE(OP_F32_CONST); ADD_F32(0); break;
    case WT_F64:  ADD_CODE(OP_F64_CONST); ADD_F64(0); break;
    case WT_VOID: break;
    default: assert(false); break;
    }
  }
  gen_stmts(func->stmts);

  if (bpname != NULL) {
    ADD_CODE(OP_END);
    cur_depth -= 1;
    assert(cur_depth == 0);

    // Epilogue

    // Restore stack pointer.
    if (bpname != NULL) {
      // global.sp = bp;
      gen_expr_stmt(new_expr_bop(EX_ASSIGN, &tyVoid, NULL, spvar, bpvar));
    }
  }
  curscope = global_scope;

  ADD_CODE(OP_END);

  emit_uleb128(code, 0, code->len);  // Insert code size at the top.

  curfunc = NULL;
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

  default:
    error("Unhandled decl: %d", decl->kind);
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
