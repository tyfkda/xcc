#include "wcc.h"

#include <assert.h>
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

#define ADD_CODE(...)  do { unsigned char buf[] = {__VA_ARGS__}; add_code(buf, sizeof(buf)); } while (0)
#define ADD_LEB128(x)  emit_leb128(code, code->len, x)
#define ADD_ULEB128(x) emit_uleb128(code, code->len, x)

// TODO: Endian.
#define ADD_F32(x)     do { float f = (x); add_code((unsigned char*)&f, sizeof(f)); } while (0)
#define ADD_F64(x)     do { double d = (x); add_code((unsigned char*)&d, sizeof(d)); } while (0)

DataStorage *code;

static void add_code(const unsigned char* buf, size_t size) {
  data_append(code, buf, size);
}

void emit_leb128(DataStorage *data, size_t pos, int64_t val) {
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

void emit_uleb128(DataStorage *data, size_t pos, uint64_t val) {
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

static void gen_cond(Expr *cond, bool tf);

static Expr *get_sp_var(void) {
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
static void gen_expr_stmt(Expr *expr);
static void gen_expr(Expr *expr, bool needval);

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
    switch (type_size(type)) {
    case 1:  ADD_CODE(OP_I32_LOAD8_S, 0, 0); break;
    case 2:  ADD_CODE(OP_I32_LOAD16_S, 1, 0); break;
    case 4:  ADD_CODE(OP_I32_LOAD, 2, 0); break;
    case 8:  ADD_CODE(OP_I64_LOAD, 3, 0); break;
    default: assert(false);
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
  assert(is_number(type));
  int index = 0;
#ifndef __NO_FLONUM
  if (is_flonum(type)) {
    assert(kind < EX_MOD);
    index = type->flonum.kind != FL_FLOAT ? 3 : 2;
  } else
#endif
  {
    index = type_size(type) > I32_SIZE ? 1 : 0;
  }

  // unsigned?
  static const unsigned char kOpTable[][EX_RSHIFT - EX_ADD + 1] = {
    {OP_I32_ADD, OP_I32_SUB, OP_I32_MUL, OP_I32_DIV_S, OP_I32_REM_S, OP_I32_AND, OP_I32_OR, OP_I32_XOR, OP_I32_SHL, OP_I32_SHR_S},
    {OP_I64_ADD, OP_I64_SUB, OP_I64_MUL, OP_I64_DIV_S, OP_I64_REM_S, OP_I64_AND, OP_I64_OR, OP_I64_XOR, OP_I64_SHL, OP_I64_SHR_S},
    {OP_F32_ADD, OP_F32_SUB, OP_F32_MUL, OP_F32_DIV, OP_NOP, OP_NOP, OP_NOP, OP_NOP, OP_NOP, OP_NOP},
    {OP_F64_ADD, OP_F64_SUB, OP_F64_MUL, OP_F64_DIV, OP_NOP, OP_NOP, OP_NOP, OP_NOP, OP_NOP, OP_NOP},
  };

  assert(EX_ADD <= kind && kind <= EX_RSHIFT);
  assert(kOpTable[index][kind - EX_ADD] != OP_NOP);
  ADD_CODE(kOpTable[index][kind - EX_ADD]);
}

static void gen_ptradd(enum ExprKind kind, const Type *type) {
  ssize_t scale = type_size(type->pa.ptrof);
  if (kind == EX_PTRSUB)
    scale = -scale;
  ADD_CODE(OP_I32_CONST);
  ADD_LEB128(scale);
  ADD_CODE(OP_I32_MUL);
  ADD_CODE(OP_I32_ADD);
}

static void gen_cast(const Type *dst, const Type *src) {
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
        switch ((d > I32_SIZE ? 2 : 0) + (s > I32_SIZE ? 1 : 0)) {
        case 1: ADD_CODE(OP_I32_WRAP_I64); break;
        case 2: ADD_CODE(OP_I64_EXTEND_I32_S); break;
        }
      }
      return;
#ifndef __NO_FLONUM
    case TY_FLONUM:
      {
        static const unsigned char OpTable[] = {
          OP_I32_TRUNC_F32_S, OP_I32_TRUNC_F64_S,
          OP_I64_TRUNC_F32_S, OP_I64_TRUNC_F64_S,
        };
        int d = type_size(dst);
        int index = (d > I32_SIZE ? 2 : 0) + (src->flonum.kind != FL_FLOAT ? 1 : 0);
        ADD_CODE(OpTable[index]);
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
        static const unsigned char OpTable[] = {
          OP_F32_CONVERT_I32_S, OP_F32_CONVERT_I64_S,
          OP_F64_CONVERT_I32_S, OP_F64_CONVERT_I64_S,
        };
        int s = type_size(src);
        int index = (dst->flonum.kind != FL_FLOAT ? 2 : 0) + (s > I32_SIZE ? 1 : 0);
        ADD_CODE(OpTable[index]);
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

static void gen_ternary(Expr *expr) {
  gen_cond(expr->ternary.cond, true);
  unsigned char type = expr->type->kind == TY_VOID ? WT_VOID : to_wtype(expr->type);
  ADD_CODE(OP_IF, type);
  ++cur_depth;
  gen_expr(expr->ternary.tval, type != WT_VOID);
  ADD_CODE(OP_ELSE);
  gen_expr(expr->ternary.fval, type != WT_VOID);
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
  assert(func->kind == EX_VAR);
  Vector *args = expr->funcall.args;
  int arg_count = args != NULL ? args->len : 0;

  for (int i = 0; i < arg_count; ++i) {
    Expr *arg = args->data[i];
    gen_expr(arg, true);
  }
  gen_funcall_by_name(func->var.name);
}

static void gen_bpofs(int32_t offset) {
  const Name *bpname = alloc_name(BP_NAME, NULL, false);
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

static void gen_lval(Expr *expr) {
  switch (expr->kind) {
  case EX_VAR:
    {
      Scope *scope;
      const VarInfo *varinfo = scope_find(expr->var.scope, expr->var.name, &scope);
      assert(varinfo != NULL && scope == expr->var.scope);
      assert(!is_prim_type(expr->type) || varinfo->storage & VS_REF_TAKEN);
      if (is_global_scope(scope) || (varinfo->storage & (VS_STATIC | VS_EXTERN))) {
        assert(!is_prim_type(expr->type) || varinfo->storage & VS_REF_TAKEN);
        GVarInfo *info = get_gvar_info(expr);
        ADD_CODE(OP_I32_CONST);
        ADD_LEB128(info->non_prim.address);
      } else {
        VReg *vreg = varinfo->local.reg;
        gen_bpofs(vreg->non_prim.offset);
      }
    }
    return;
  case EX_DEREF:
    gen_expr(expr->unary.sub, true);
    return;
  case EX_MEMBER:
    {
      const Type *type = expr->member.target->type;
      if (ptr_or_array(type))
        type = type->pa.ptrof;
      assert(type->kind == TY_STRUCT);
      const Vector *members = type->struct_.info->members;
      const VarInfo *member = members->data[expr->member.index];

      gen_expr(expr->member.target, true);
      if (member->struct_member.offset == 0)
        return;
      ADD_CODE(OP_I32_CONST);
      ADD_LEB128(member->struct_member.offset);
      ADD_CODE(OP_I32_ADD);
      return;
    }
  /*case EX_COMPLIT:
    {
      Expr *var = expr->complit.var;
      assert(var->var.scope != NULL);
      const VarInfo *varinfo = scope_find(var->var.scope, var->var.name, NULL);
      assert(varinfo != NULL);
      assert(varinfo->local.reg != NULL);
      varinfo->local.reg->flag |= VRF_REF;

      gen_stmts(expr->complit.inits);
      return gen_lval(expr->complit.var);
    }*/
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
        gen_lval(expr);
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
    gen_lval(expr);
    return;
  }
}

static void gen_expr(Expr *expr, bool needval) {
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

  case EX_PTRADD:
  case EX_PTRSUB:
    {
      assert(expr->type->kind == TY_PTR);
      gen_expr(expr->bop.lhs, needval);
      gen_expr(expr->bop.rhs, needval);
      if (needval) {
        gen_cast(&tyInt, expr->bop.rhs->type);
        size_t scale = type_size(expr->type->pa.ptrof);
        ADD_CODE(OP_I32_CONST);
        ADD_LEB128(scale);
        ADD_CODE(OP_I32_MUL);
        ADD_CODE(expr->kind == EX_PTRADD ? OP_I32_ADD : OP_I32_SUB);
      }
    }
    break;

  case EX_EQ:
  case EX_NE:
  case EX_LT:
  case EX_GT:
  case EX_LE:
  case EX_GE:
  case EX_LOGAND:
  case EX_LOGIOR:
    gen_cond(expr, true);
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
        break;
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
    {
      assert(is_prim_type(expr->type));
      Expr *sub = expr->unary.sub;
      assert(sub->kind == EX_VAR);
      gen_expr(sub, true);
      switch (to_wtype(expr->type)) {
      case WT_I32:
        ADD_CODE(OP_I32_CONST);
        ADD_ULEB128(expr->type->kind == TY_PTR ? type_size(expr->type->pa.ptrof) : 1);
        ADD_CODE(expr->kind == EX_PREINC ? OP_I32_ADD : OP_I32_SUB);
        break;
      default: assert(false); break;
      }

      Scope *scope;
      const VarInfo *varinfo = scope_find(sub->var.scope, sub->var.name, &scope);
      assert(varinfo != NULL);
      if (!is_global_scope(scope) && !(varinfo->storage & (VS_STATIC | VS_EXTERN))) {
        VReg *vreg = varinfo->local.reg;
        assert(vreg != NULL);
        ADD_CODE(needval ? OP_LOCAL_TEE : OP_LOCAL_SET);
        ADD_ULEB128(vreg->prim.local_index);
      } else {
        GVarInfo *info = get_gvar_info(sub);
        ADD_CODE(OP_GLOBAL_SET);
        ADD_ULEB128(info->prim.index);
        if (needval) {
          ADD_CODE(OP_GLOBAL_GET);
          ADD_ULEB128(info->prim.index);
        }
      }
    }
    break;

  case EX_POSTINC:
  case EX_POSTDEC:
    {
      assert(is_prim_type(expr->type));
      Expr *sub = expr->unary.sub;
      assert(sub->kind == EX_VAR);
      if (needval)
        gen_expr(sub, true);  // Push the result first.
      gen_expr(sub, true);
      switch (to_wtype(expr->type)) {
      case WT_I32:
        ADD_CODE(OP_I32_CONST);
        ADD_ULEB128(expr->type->kind == TY_PTR ? type_size(expr->type->pa.ptrof) : 1);
        ADD_CODE(expr->kind == EX_POSTINC ? OP_I32_ADD : OP_I32_SUB);
        break;
      default: assert(false); break;
      }

      Scope *scope;
      const VarInfo *varinfo = scope_find(sub->var.scope, sub->var.name, &scope);
      assert(varinfo != NULL);
      if (!is_global_scope(scope) && !(varinfo->storage & (VS_STATIC | VS_EXTERN))) {
        VReg *vreg = varinfo->local.reg;
        assert(vreg != NULL);
        ADD_CODE(OP_LOCAL_SET);
        ADD_ULEB128(vreg->prim.local_index);
      } else {
        GVarInfo *info = get_gvar_info(sub);
        ADD_CODE(OP_GLOBAL_SET);
        ADD_ULEB128(info->prim.index);
      }
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
            gen_lval(lhs);
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
          gen_lval(expr->bop.lhs);
          gen_expr(expr->bop.rhs, true);
          gen_store(expr->bop.lhs->type);
        }
        break;
      case TY_STRUCT:
        {
          size_t size = type_size(expr->bop.lhs->type);
          assert(size > 0);
          gen_lval(expr->bop.lhs);
          gen_expr(expr->bop.rhs, true);
          ADD_CODE(type_size(&tySize) <= I32_SIZE ? OP_I32_CONST : OP_I64_CONST);
          ADD_LEB128(size);
          gen_funcall_by_name(alloc_name(MEMCPY_NAME, NULL, false));
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
            gen_lval(lhs);
            gen_lval(lhs);  // We can safely evaluate lhs two times, since lhs has no side effect.
            gen_load(type);
            gen_expr(sub->bop.rhs, true);
            if (sub->kind == EX_PTRADD || sub->kind == EX_PTRSUB)
              gen_ptradd(sub->kind, sub->type);
            else
              gen_arith(sub->kind, type);
            gen_store(type);
            return;
          }

          assert(lhs->kind == EX_VAR);
          gen_expr(lhs, true);
          gen_expr(sub->bop.rhs, true);
          if (sub->kind == EX_PTRADD || sub->kind == EX_PTRSUB)
            gen_ptradd(sub->kind, sub->type);
          else
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
      gen_lval(sub);
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
    gen_ternary(expr);
    break;

  case EX_MEMBER:
    gen_lval(expr);
    if (is_prim_type(expr->type)) {
      gen_load(expr->type);
    }
    break;

  case EX_FUNCALL:
    gen_funcall(expr);
    if (!needval && expr->type->kind != TY_VOID)
      ADD_CODE(OP_DROP);
    break;

  default: assert(!"Not implemeneted"); break;
  }
}

static void gen_compare_expr(enum ExprKind kind, Expr *lhs, Expr *rhs) {
  assert(lhs->type->kind == rhs->type->kind);
  assert(is_prim_type(lhs->type));

  int index = 0;
#ifndef __NO_FLONUM
  if (is_flonum(lhs->type)) {
    index = lhs->type->flonum.kind != FL_FLOAT ? 3 : 2;
  } else
#endif
  {
    index = type_size(lhs->type) > I32_SIZE ? 1 : 0;
  }

  // unsigned?
  static const unsigned char OpTable[][6] = {
    {OP_I32_EQ, OP_I32_NE, OP_I32_LT_S, OP_I32_LE_S, OP_I32_GE_S, OP_I32_GT_S},
    {OP_I64_EQ, OP_I64_NE, OP_I64_LT_S, OP_I64_LE_S, OP_I64_GE_S, OP_I64_GT_S},
    {OP_F32_EQ, OP_F32_NE, OP_F32_LT, OP_F32_LE, OP_F32_GE, OP_F32_GT},
    {OP_F64_EQ, OP_F64_NE, OP_F64_LT, OP_F64_LE, OP_F64_GE, OP_F64_GT},
  };

  gen_expr(lhs, true);
  gen_expr(rhs, true);
  ADD_CODE(OpTable[index][kind - EX_EQ]);
}

static void gen_cond(Expr *cond, bool tf) {
  enum ExprKind ck = cond->kind;
  switch (ck) {
  case EX_FIXNUM:
    {
      Expr *zero = new_expr_fixlit(&tyInt, NULL, 0);
      gen_compare_expr(tf ? EX_NE : EX_EQ, cond, zero);
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
    gen_compare_expr(ck, cond->bop.lhs, cond->bop.rhs);
    break;
  case EX_LOGAND:
    if (tf) {
      gen_cond(cond->bop.lhs, true);
      ADD_CODE(OP_IF, WT_I32);
      ++cur_depth;
      gen_cond(cond->bop.rhs, true);
      ADD_CODE(OP_ELSE);
      ADD_CODE(OP_I32_CONST, 0);  // false
      ADD_CODE(OP_END);
      --cur_depth;
    } else {
      gen_cond(cond->bop.lhs, false);
      ADD_CODE(OP_IF, WT_I32);
      ++cur_depth;
      ADD_CODE(OP_I32_CONST, 1);  // true
      ADD_CODE(OP_ELSE);
      gen_cond(cond->bop.rhs, false);
      ADD_CODE(OP_END);
      --cur_depth;
    }
    break;
  case EX_LOGIOR:
    if (tf) {
      gen_cond(cond->bop.lhs, true);
      ADD_CODE(OP_IF, WT_I32);
      ++cur_depth;
      ADD_CODE(OP_I32_CONST, 1);  // true
      ADD_CODE(OP_ELSE);
      gen_cond(cond->bop.rhs, true);
      ADD_CODE(OP_END);
      --cur_depth;
    } else {
      gen_cond(cond->bop.lhs, false);
      ADD_CODE(OP_IF, WT_I32);
      ++cur_depth;
      gen_cond(cond->bop.rhs, false);
      ADD_CODE(OP_ELSE);
      ADD_CODE(OP_I32_CONST, 0);  // false
      ADD_CODE(OP_END);
      --cur_depth;
    }
    break;
  default:
    assert(false);
    break;
  }
}

static void gen_cond_jmp(Expr *cond, bool tf, uint32_t depth) {
  gen_cond(cond, tf);
  ADD_CODE(OP_BR_IF);
  ADD_ULEB128(depth);
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
  assert(type_size(value->type) <= I32_SIZE);
  int default_index = case_count;
  for (int i = 0; i < case_count; ++i) {
    Stmt *c = cases->data[i];
    if (c->kind == ST_DEFAULT) {
      default_index = i;
      continue;
    }
    gen_expr(value, true);
    ADD_CODE(OP_I32_CONST);
    ADD_LEB128(c->case_.value->fixnum);
    ADD_CODE(OP_I32_EQ,
             OP_BR_IF);
    ADD_ULEB128(i);
  }
  // Jump to default.
  ADD_CODE(OP_BR);
  ADD_ULEB128(default_index);

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
  int save_break = break_depth;
  int save_continue = continue_depth;
  break_depth = cur_depth;
  continue_depth = cur_depth + 1;

  ADD_CODE(OP_BLOCK, WT_VOID);
  ADD_CODE(OP_LOOP, WT_VOID);
  cur_depth += 2;
  gen_cond_jmp(stmt->while_.cond, false, 1);
  gen_stmt(stmt->while_.body);
  ADD_CODE(OP_BR, 0);
  ADD_CODE(OP_END);
  ADD_CODE(OP_END);
  cur_depth -= 2;
  break_depth = save_break;
  continue_depth = save_continue;
}

static void gen_do_while(Stmt *stmt) {
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
  gen_cond_jmp(stmt->while_.cond, false, 1);
  ADD_CODE(OP_BR, 0);
  ADD_CODE(OP_END);
  ADD_CODE(OP_END);
  cur_depth -= 2;
  break_depth = save_break;
  continue_depth = save_continue;
}

static void gen_for(Stmt *stmt) {
  int save_break = break_depth;
  int save_continue = continue_depth;
  break_depth = cur_depth;
  continue_depth = cur_depth + 2;

  if (stmt->for_.pre != NULL)
    gen_expr_stmt(stmt->for_.pre);

  ADD_CODE(OP_BLOCK, WT_VOID);
  ADD_CODE(OP_LOOP, WT_VOID);
  ADD_CODE(OP_BLOCK, WT_VOID);
  cur_depth += 3;
  if (stmt->for_.cond != NULL)
    gen_cond_jmp(stmt->for_.cond, false, 2);
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
  gen_stmts(stmt->block.stmts);
}

static void gen_return(Stmt *stmt) {
  assert(curfunc != NULL);
  if (stmt->return_.val != NULL) {
    Expr *val = stmt->return_.val;
    gen_expr(val, true);

    const Name *name = alloc_name(RETVAL_NAME, NULL, false);
    VarInfo *varinfo = scope_find(curfunc->scopes->data[0], name, NULL);
    assert(varinfo != NULL);
    ADD_CODE(OP_LOCAL_SET);
    ADD_ULEB128(varinfo->local.reg->prim.local_index);
  }
  ADD_CODE(OP_BR);
  ADD_ULEB128(cur_depth - 1);
}

static void gen_if(Stmt *stmt) {
  gen_cond(stmt->if_.cond, true);
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
    UNUSED(decls);
    /*for (int i = 0; i < decls->len; ++i) {
      VarDecl *decl = decls->data[i];
      if (decl->init == NULL)
        continue;
      VarInfo *varinfo = scope_find(curscope, decl->ident->ident, NULL);
      if (varinfo == NULL || (varinfo->storage & (VS_STATIC | VS_EXTERN)) ||
          !(varinfo->type->kind == TY_STRUCT ||
            varinfo->type->kind == TY_ARRAY))
        continue;
      gen_clear_local_var(varinfo);
    }*/
  }
  gen_stmts(inits);
}

static void gen_expr_stmt(Expr *expr) {
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
  /*case ST_GOTO:  gen_goto(stmt); break;
  case ST_LABEL:  gen_label(stmt); break;*/
  case ST_VARDECL:  gen_vardecl(stmt->vardecl.decls, stmt->vardecl.inits); break;
  /*case ST_ASM:  gen_asm(stmt); break;*/

  default:
    parse_error(stmt->token, "Unhandled stmt: %d", stmt->kind);
    break;
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

static void gen_defun(Function *func) {
  if (func->scopes == NULL)  // Prototype definition
    return;

  const Type *functype = func->type;

  code = malloc(sizeof(*code));
  data_init(code);

  DataStorage *data = malloc(sizeof(*data));
  data_init(data);

  // Allocate local variables.
  unsigned int param_count = functype->func.params != NULL ? functype->func.params->len : 0;  // TODO: Consider stack params.
  unsigned int local_count = 0;

  uint32_t frame_size = 0;
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
      vreg->param_index = param_index;
      if ((param_index >= 0 || is_prim_type(varinfo->type)) && !(varinfo->storage & VS_REF_TAKEN)) {
        if (param_index >= 0) {
          vreg->prim.local_index = param_index;
        } else {
          vreg->prim.local_index = local_count++ + param_count;
          // TODO: Group same type variables.
          emit_uleb128(data, data->len, 1);  // TODO: Set type bytes.
          data_push(data, to_wtype(varinfo->type));
        }
      } else {
        vreg->non_prim.offset = ALIGN(frame_size, align_size(varinfo->type));
        frame_size += type_size(varinfo->type);
      }
    }
  }
  if (frame_size > 0) {
    frame_size = ALIGN(frame_size, 8);  // TODO:
    for (int i = 0; i < func->scopes->len; ++i) {
      Scope *scope = func->scopes->data[i];
      if (scope->vars == NULL)
        continue;
      for (int j = 0; j < scope->vars->len; ++j) {
        VarInfo *varinfo = scope->vars->data[j];
        if (varinfo->storage & (VS_STATIC | VS_EXTERN | VS_ENUM_MEMBER))
          continue;
        VReg *vreg = varinfo->local.reg;
        if ((vreg->param_index < 0 && !is_prim_type(varinfo->type)) ||
            (varinfo->storage & VS_REF_TAKEN))
          vreg->non_prim.offset -= frame_size;
      }
    }
  }


  const Name *bpname = NULL;
  const Token *bpident = NULL;
  if (frame_size > 0) {
    // Allocate base pointer in top scope.
    bpname = alloc_name(BP_NAME, NULL, false);
    bpident = alloc_ident(bpname, NULL, NULL);
    const Type *type = &tySize;
    VarInfo *varinfo = scope_add(func->scopes->data[0], bpident, type, 0);
    VReg *vreg = calloc(1, sizeof(*vreg));
    varinfo->local.reg = vreg;
    vreg->prim.local_index = local_count++ + param_count;
    emit_uleb128(data, data->len, 1);  // TODO: Set type bytes.
    data_push(data, to_wtype(varinfo->type));
  }

  // Insert local count at the top.
  emit_uleb128(data, 0, local_count);  // Put local count at the top.

  curfunc = func;

  // Set up base pointer.
  Expr *bpvar = NULL, *spvar = NULL;
  if (frame_size > 0) {
    bpvar = new_expr_variable(bpname, &tySize, bpident, func->scopes->data[0]);
    spvar = get_sp_var();
    // local.bp = global.sp;
    gen_expr_stmt(new_expr_bop(EX_ASSIGN, &tyVoid, NULL, bpvar, spvar));
    // global.sp -= frame_size;
    gen_expr_stmt(
        new_expr_unary(EX_MODIFY, &tyVoid, NULL,
                       new_expr_bop(EX_SUB, &tySize, NULL, spvar,
                                    new_expr_fixlit(&tySize, NULL, frame_size))));
  }
  // Store ref-taken parameters to stack frame.
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
  ADD_CODE(OP_BLOCK, WT_VOID);
  cur_depth += 1;
  gen_stmts(func->stmts);

  ADD_CODE(OP_END);
  cur_depth -= 1;
  assert(cur_depth == 0);

  // Restore stack pointer.
  if (frame_size > 0) {
    // global.sp = bp;
    gen_expr_stmt(new_expr_bop(EX_ASSIGN, &tyVoid, NULL, spvar, bpvar));
  }
  if (functype->func.ret->kind != TY_VOID) {
    const Name *name = alloc_name(RETVAL_NAME, NULL, false);
    VarInfo *varinfo = scope_find(func->scopes->data[0], name, NULL);
    assert(varinfo != NULL);
    ADD_CODE(OP_LOCAL_GET);
    ADD_ULEB128(varinfo->local.reg->prim.local_index);
  }

  ADD_CODE(OP_END);

  emit_uleb128(data, 0, data->len + code->len);  // Insert code size at the top.

  data_concat(data, code);
  func->bbcon = (BBContainer*)data;  // Store code to `bbcon`.

  curfunc = NULL;
  free(code);
  code = NULL;
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
