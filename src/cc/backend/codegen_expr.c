#include "../../config.h"
#include "codegen.h"

#include <assert.h>
#include <stdlib.h>  // malloc

#include "arch_config.h"
#include "ast.h"
#include "fe_misc.h"  // curscope, extract_bitfield_vale
#include "ir.h"
#include "regalloc.h"
#include "table.h"
#include "type.h"
#include "util.h"
#include "var.h"

bool is_stack_param(const Type *type) {
  return type->kind == TY_STRUCT;
}

enum VRegSize to_vsize(const Type *type) {
  const int MAX_REG_SIZE = 8;
  assert(is_prim_type(type));
  int size = type_size(type);
  assert(1 <= size && size <= MAX_REG_SIZE && IS_POWER_OF_2(size));
  UNUSED(MAX_REG_SIZE);
  for (enum VRegSize vsize = VRegSize1; vsize <= VRegSize8; ++vsize) {
    if (size == 1 << vsize)
      return vsize;
  }
  assert(!"Must not reached");
  return -1;
}

int to_vflag(const Type *type) {
  if (is_flonum(type))
    return VRF_FLONUM;
  return 0;
}

VReg *add_new_vreg(const Type *type) {
  return reg_alloc_spawn(curra, to_vsize(type), to_vflag(type));
}

static Table builtin_function_table;  // <BuiltinFunctionProc>

void add_builtin_function(const char *str, Type *type, BuiltinFunctionProc *proc,
                          bool add_to_scope) {
  const Name *name = alloc_name(str, NULL, false);
  table_put(&builtin_function_table, name, proc);

  if (add_to_scope)
    scope_add(global_scope, name, type, 0);
}

struct CompareExpr {
  enum ConditionKind cond;
  VReg *lhs, *rhs;
};
static struct CompareExpr gen_compare_expr(enum ExprKind kind, Expr *lhs, Expr *rhs) {
  assert(lhs->type->kind == rhs->type->kind);

  assert(EX_EQ <= kind && kind <= EX_GT);
  enum ConditionKind cond = kind + (COND_EQ - EX_EQ);
  if (is_const(lhs)) {
    assert(!is_const(rhs));
    Expr *tmp = lhs;
    lhs = rhs;
    rhs = tmp;
    cond = swap_cond(cond);
  }

  int flag = 0;
  if ((is_fixnum(lhs->type->kind) && lhs->type->fixnum.is_unsigned) ||
       lhs->type->kind == TY_PTR) {
    // unsigned
    flag = COND_UNSIGNED;
  }
  if (is_flonum(lhs->type))
    flag |= COND_FLONUM;

  VReg *lhs_reg = gen_expr(lhs);
  VReg *rhs_reg = gen_expr(rhs);
  if ((rhs_reg->flag & VRF_CONST) != 0 && (lhs_reg->flag & VRF_CONST) != 0) {
    static struct CompareExpr kNone = {.cond = COND_NONE};
    static struct CompareExpr kAny = {.cond = COND_ANY};
    // Const VReg is must be non-flonum.
    assert(!(lhs_reg->flag & VRF_FLONUM));
    assert(!(rhs_reg->flag & VRF_FLONUM));
    assert(!(flag & COND_FLONUM));
    switch (cond | flag) {
    case COND_NONE: return kNone;
    case COND_ANY:  return kAny;
    case COND_EQ:  return lhs_reg->fixnum == rhs_reg->fixnum ? kAny : kNone;
    case COND_NE:  return lhs_reg->fixnum != rhs_reg->fixnum ? kAny : kNone;
    case COND_LT:  return lhs_reg->fixnum <  rhs_reg->fixnum ? kAny : kNone;
    case COND_LE:  return lhs_reg->fixnum <= rhs_reg->fixnum ? kAny : kNone;
    case COND_GE:  return lhs_reg->fixnum >= rhs_reg->fixnum ? kAny : kNone;
    case COND_GT:  return lhs_reg->fixnum >  rhs_reg->fixnum ? kAny : kNone;
    case COND_EQ | COND_UNSIGNED:  return (uint64_t)lhs_reg->fixnum == (uint64_t)rhs_reg->fixnum ? kAny : kNone;
    case COND_NE | COND_UNSIGNED:  return (uint64_t)lhs_reg->fixnum != (uint64_t)rhs_reg->fixnum ? kAny : kNone;
    case COND_LT | COND_UNSIGNED:  return (uint64_t)lhs_reg->fixnum <  (uint64_t)rhs_reg->fixnum ? kAny : kNone;
    case COND_LE | COND_UNSIGNED:  return (uint64_t)lhs_reg->fixnum <= (uint64_t)rhs_reg->fixnum ? kAny : kNone;
    case COND_GE | COND_UNSIGNED:  return (uint64_t)lhs_reg->fixnum >= (uint64_t)rhs_reg->fixnum ? kAny : kNone;
    case COND_GT | COND_UNSIGNED:  return (uint64_t)lhs_reg->fixnum >  (uint64_t)rhs_reg->fixnum ? kAny : kNone;
    default: assert(false); break;
    }
  }

  assert(is_prim_type(lhs->type));

  return (struct CompareExpr){.cond = cond | flag, .lhs = lhs_reg, .rhs = rhs_reg};
}

void gen_cond_jmp(Expr *cond, bool tf, BB *bb) {
  enum ExprKind ck = cond->kind;
  switch (ck) {
  case EX_FIXNUM:
    if (cond->fixnum == 0)
      tf = !tf;
    if (tf)
      new_ir_jmp(bb);
    return;
  case EX_EQ:
  case EX_NE:
  case EX_LT:
  case EX_LE:
  case EX_GE:
  case EX_GT:
    {
      if (!tf) {
        if (ck <= EX_NE)
          ck = (EX_EQ + EX_NE) - ck;  // EQ <-> NE
        else
          ck = EX_LT + ((ck - EX_LT) ^ 2);  // LT <-> GE, LE <-> GT
      }
      struct CompareExpr cmp = gen_compare_expr(ck, cond->bop.lhs, cond->bop.rhs);
      new_ir_cjmp(cmp.lhs, cmp.rhs, cmp.cond, bb);
    }
    return;
  case EX_LOGAND:
  case EX_LOGIOR:
    {
      BB *bb1 = new_bb();
      BB *bb2 = new_bb();
      if (!tf)
        ck = (EX_LOGAND + EX_LOGIOR) - ck;  // LOGAND <-> LOGIOR
      if (ck == EX_LOGAND)
        gen_cond_jmp(cond->bop.lhs, !tf, bb2);
      else
        gen_cond_jmp(cond->bop.lhs, tf, bb);
      set_curbb(bb1);
      gen_cond_jmp(cond->bop.rhs, tf, bb);
      set_curbb(bb2);
    }
    return;
  case EX_COMMA:
    gen_expr(cond->bop.lhs);
    gen_cond_jmp(cond->bop.rhs, tf, bb);
    break;
  default: assert(false); break;
  }
}

static VReg *gen_cast(Expr *expr) {
  Expr *src = expr->unary.sub;
  Type *dst_type = expr->type;
  if (is_bool(dst_type))
    return gen_expr(make_cond(src));

  int dst_size = type_size(dst_type);
#if XCC_TARGET_ARCH == XCC_ARCH_X64 && !defined(__NO_FLONUM)
  // On x64, cannot cast from double to uint64_t directly.
  if (is_flonum(src->type) &&
      is_fixnum(dst_type->kind) && dst_type->fixnum.is_unsigned && dst_size >= 8) {
    // Transform from (uint64_t)flonum
    //   to: (flonum <= INT64_MAX) ? (int64_t)flonum
    //                             : ((int64_t)(flonum - (INT64_MAX + 1ULL)) ^ (1LL << 63))
    const Token *token = expr->token;
    Type *i64t = get_fixnum_type_from_size(dst_size);
    Expr *cond = new_expr_bop(EX_LE, &tyBool, token, src,
                              new_expr_flolit(src->type, src->token, INT64_MAX));
    Expr *offsetted = new_expr_addsub(
        EX_SUB, token, src,
        new_expr_flolit(src->type, src->token, (uint64_t)INT64_MAX + 1ULL));
    Expr *xorred = new_expr_bop(EX_BITXOR, i64t, token, make_cast(i64t, token, offsetted, false),
                                new_expr_fixlit(i64t, token, (uint64_t)1 << 63));
    Expr *ternary = new_expr_ternary(token, cond, make_cast(i64t, token, src, false), xorred, i64t);
    return gen_expr(make_cast(dst_type, token, ternary, false));
  }
#endif

  VReg *vreg = gen_expr(src);
  assert(!is_bool(dst_type));

  switch (dst_type->kind) {
  case TY_VOID:
  case TY_STRUCT:
    return vreg;
  default: break;
  }

  if (vreg->flag & VRF_CONST) {
    assert(!(vreg->flag & VRF_FLONUM));  // No const vreg for flonum.
    Fixnum value = vreg->fixnum;
    if (dst_size < (1 << vreg->vsize) && dst_size < (int)sizeof(Fixnum)) {
      // Assume that integer is represented in Two's complement
      size_t bit = dst_size * TARGET_CHAR_BIT;
      UFixnum mask = (-1ULL) << bit;
      if (!is_unsigned(dst_type) && (value & (1 << (bit - 1))))    // signed && negative
        value |= mask;
      else
        value &= ~mask;
    }

    enum VRegSize vsize = to_vsize(dst_type);
    return new_const_vreg(value, vsize);
  }

  int src_size = 1 << vreg->vsize;
  if (dst_size == src_size &&
      is_flonum(dst_type) == ((vreg->flag & VRF_FLONUM) != 0))
    return vreg;

  IR *ir = new_ir_cast(vreg, to_vsize(dst_type), to_vflag(dst_type));
  if (is_flonum(src->type)) {
    if (is_unsigned(dst_type))
      ir->flag |= IRF_UNSIGNED;
  } else if (is_unsigned(src->type)) {
    ir->flag |= IRF_UNSIGNED;
  }
  return ir->dst;
}

static VReg *gen_ref_sub(Expr *expr) {
  switch (expr->kind) {
  case EX_VAR:
    {
      Scope *scope;
      const VarInfo *varinfo = scope_find(expr->var.scope, expr->var.name, &scope);
      assert(varinfo != NULL && scope == expr->var.scope);
      if (is_global_scope(scope))
        return new_ir_iofs(expr->var.name, (varinfo->storage & VS_STATIC) == 0);
      else if (is_local_storage(varinfo))
        return new_ir_bofs(varinfo->local.frameinfo);
      else if (varinfo->storage & VS_STATIC)
        return new_ir_iofs(varinfo->static_.gvar->name, false);
      else
        return new_ir_iofs(expr->var.name, true);
    }
  case EX_DEREF:
    return gen_expr(expr->unary.sub);
  case EX_MEMBER:
    {
      const MemberInfo *minfo = expr->member.info;
      VReg *vreg = gen_expr(expr->member.target);
      if (minfo->offset == 0)
        return vreg;
      enum VRegSize vsize = to_vsize(&tySize);
      VReg *imm = new_const_vreg(minfo->offset, vsize);
      return new_ir_bop(IR_ADD, vreg, imm, vsize, IRF_UNSIGNED);
    }
  case EX_COMPLIT:
    {
      Expr *var = expr->complit.var;
      assert(var->var.scope != NULL);
      const VarInfo *varinfo = scope_find(var->var.scope, var->var.name, NULL);
      assert(varinfo != NULL);
      VReg *vreg = varinfo->local.vreg;
      if (vreg != NULL)
        vreg->flag |= VRF_REF;

      gen_clear_local_var(varinfo);
      gen_stmts(expr->complit.inits);
      return gen_ref_sub(expr->complit.var);
    }
  default: assert(false); break;
  }
  return NULL;
}

static VReg *gen_lval(Expr *expr) {
  return gen_ref_sub(reduce_refer(expr));
}

static VReg *gen_variable(Expr *expr) {
  switch (expr->type->kind) {
  case TY_FIXNUM:
  case TY_PTR:
  case TY_FLONUM:
    {
      Scope *scope;
      const VarInfo *varinfo = scope_find(expr->var.scope, expr->var.name, &scope);
      assert(varinfo != NULL && scope == expr->var.scope);
      if (!is_global_scope(scope) && is_local_storage(varinfo)) {
        assert(varinfo->local.vreg != NULL);
        return varinfo->local.vreg;
      }

      VReg *vreg = gen_lval(expr);
      int irflag = is_unsigned(expr->type) ? IRF_UNSIGNED : 0;
      VReg *result = new_ir_load(vreg, to_vsize(expr->type), to_vflag(expr->type), irflag);
      return result;
    }
  case TY_ARRAY:   // Use variable address as a pointer.
  case TY_STRUCT:  // struct value is handled as a pointer.
  case TY_FUNC:
    return gen_lval(expr);
  case TY_VOID: break;
  }
  assert(!"Must not reached");
  return NULL;
}

static VReg *gen_ternary(Expr *expr) {
  BB *tbb = new_bb();
  BB *fbb = new_bb();
  BB *nbb = new_bb();
  VReg *result = NULL;
  if (expr->type->kind != TY_VOID) {
    Type *type = expr->type;
    if (!is_number(type) && !ptr_or_array(type))
      type = ptrof(type);
    result = add_new_vreg(type);
  }

  gen_cond_jmp(expr->ternary.cond, false, fbb);

  set_curbb(tbb);
  VReg *tval = gen_expr(expr->ternary.tval);
  if (result != NULL)
    new_ir_mov(result, tval, is_unsigned(expr->ternary.tval->type) ? IRF_UNSIGNED : 0);
  new_ir_jmp(nbb);

  set_curbb(fbb);
  VReg *fval = gen_expr(expr->ternary.fval);
  if (result != NULL)
    new_ir_mov(result, fval, is_unsigned(expr->ternary.fval->type) ? IRF_UNSIGNED : 0);

  set_curbb(nbb);
  return result;
}

//
static Expr *gen_expr_as_tmpvar(Expr *arg) {
  // Precalculate expr and store the result to temporary variable.
  Type *type = arg->type;
  if (type->kind == TY_STRUCT)
    type = ptrof(type);
  Scope *scope = curscope;
  const Name *name = alloc_label();
  VarInfo *varinfo = scope_add(scope, name, type, 0);
  varinfo->local.vreg = gen_expr(arg);
  // Replace the argument to temporary variable reference.
  return new_expr_variable(name, type, NULL, scope);
}

// If an argument is complex expression,
// precalculate it and make function argument simple.
static Expr *simplify_funarg(Expr *arg) {
  switch (arg->kind) {
  case EX_PREINC:
  case EX_PREDEC:
  case EX_POSTINC:
  case EX_POSTDEC:
  case EX_ASSIGN:
  case EX_TERNARY:
  case EX_FUNCALL:
  case EX_INLINED:
  case EX_BLOCK:
  case EX_LOGAND:  // Shortcut must be handled properly.
  case EX_LOGIOR:
    return gen_expr_as_tmpvar(arg);

  case EX_COMPLIT:
    // Precalculate compound literal, and returns its stored variable name.
    gen_expr(arg);
    return arg->complit.var;

  case EX_COMMA:
    // Precalculate first expression in comma.
    gen_expr(arg->bop.lhs);
    return simplify_funarg(arg->bop.rhs);

  // Binary operators
  case EX_MUL:
  case EX_DIV:
  case EX_MOD:
  case EX_LSHIFT:
  case EX_RSHIFT:
#if XCC_TARGET_ARCH == XCC_ARCH_X64
    // On x64, MUL, DIV and MOD instruction implicitly uses (breaks) %rdx
    // and %rdx is used as 3rd argument.
    // Similary, Shift instructions (SHL, SHR) uses %cl which is 4th argument.
    // so must be precalculated.
    return gen_expr_as_tmpvar(arg);
#else
    // Except x64, these opcodes can be used in function argument.
    // Fallthrough
#endif
  case EX_ADD:
  case EX_SUB:
  case EX_BITAND:
  case EX_BITOR:
  case EX_BITXOR:
  case EX_EQ:
  case EX_NE:
  case EX_LT:
  case EX_LE:
  case EX_GE:
  case EX_GT:
    arg->bop.lhs = simplify_funarg(arg->bop.lhs);
    arg->bop.rhs = simplify_funarg(arg->bop.rhs);
    break;

  // Unary operators
  case EX_POS:
  case EX_NEG:
  case EX_BITNOT:
  case EX_REF:
  case EX_DEREF:
  case EX_CAST:
    arg->unary.sub = simplify_funarg(arg->unary.sub);
    break;

  case EX_MEMBER:
    arg->member.target = simplify_funarg(arg->member.target);
    break;

  // Literals
  case EX_FIXNUM:
  case EX_FLONUM:
  case EX_STR:
  case EX_VAR:
    break;
  }
  return arg;
}

static VReg *gen_funcall(Expr *expr) {
  Expr *func = expr->funcall.func;
  if (func->kind == EX_VAR && is_global_scope(func->var.scope)) {
    void *proc = table_get(&builtin_function_table, func->var.name);
    if (proc != NULL)
      return (*(BuiltinFunctionProc*)proc)(expr);
  }
  const Type *functype = get_callee_type(func->type);
  assert(functype != NULL);

  Vector *args = expr->funcall.args;
  int arg_count = args->len;
  // To avoid nested funcall,
  // simplify funargs and precalculate complex expression before funcall.
  for (int i = 0; i < arg_count; ++i) {
    Expr *arg = args->data[i];
    args->data[i] = simplify_funarg(arg);
  }
  func = simplify_funarg(func);

  int offset = 0;

  VarInfo *ret_varinfo = NULL;  // Return value is on the stack.
  if (is_stack_param(expr->type)) {
    const Name *name = alloc_label();
    Type *type = expr->type;
    ret_varinfo = scope_add(curscope, name, type, 0);
    FrameInfo *fi = malloc_or_die(sizeof(*fi));
    fi->offset = 0;
    ret_varinfo->local.frameinfo = fi;
  }

  typedef struct {
    int reg_index;
    int offset;
    int size;
    bool stack_arg;
    bool is_flo;
#if VAARG_FP_AS_GP
    bool fp_as_gp;
#endif
  } ArgInfo;

  ArgInfo *arg_infos = NULL;
  int stack_arg_count = 0;
  int reg_arg_count = 0;
  int freg_arg_count = 0;
  int arg_start = ret_varinfo != NULL ? 1 : 0;
  {
    int ireg_index = arg_start;
    int freg_index = 0;

    // Check stack arguments.
    arg_infos = ALLOCA(sizeof(*arg_infos) * arg_count);
    for (int i = 0; i < arg_count; ++i) {
      ArgInfo *p = &arg_infos[i];
      p->reg_index = -1;
      p->offset = -1;
      Expr *arg = args->data[i];
      assert(arg->type->kind != TY_ARRAY);
      p->size = type_size(arg->type);
      p->is_flo = is_flonum(arg->type);
#if VAARG_FP_AS_GP
      p->fp_as_gp = false;
      if (functype->func.vaargs && functype->func.params != NULL && i >= functype->func.params->len) {
        p->is_flo = false;
        p->fp_as_gp = true;
      }
#endif
      p->stack_arg = is_stack_param(arg->type);
#if VAARG_ON_STACK
      if (functype->func.vaargs && functype->func.params != NULL && i >= functype->func.params->len)
        p->stack_arg = true;
#endif
      if (p->stack_arg || (p->is_flo ? freg_index >= MAX_FREG_ARGS : ireg_index >= MAX_REG_ARGS)) {
        offset = ALIGN(offset, align_size(arg->type));
        p->offset = offset;
        offset += ALIGN(p->size, POINTER_SIZE);
        ++stack_arg_count;
      } else {
        if (p->is_flo) {
          p->reg_index = freg_index++;
          ++freg_arg_count;
        } else {
          p->reg_index = ireg_index++;
          ++reg_arg_count;
        }
      }
    }
  }
  offset = ALIGN(offset, 16);

  IR *precall = new_ir_precall(arg_count - stack_arg_count, offset);

  if (offset > 0)
    new_ir_subsp(new_const_vreg(offset, to_vsize(&tySSize)), NULL);

  int total_arg_count = arg_count + (ret_varinfo != NULL ? 1 : 0);
  VReg **arg_vregs = total_arg_count == 0 ? NULL : calloc_or_die(total_arg_count * sizeof(*arg_vregs));

  {
    // Register arguments.
    int iregarg = 0;
    int fregarg = 0;
    for (int i = arg_count; --i >= 0; ) {
      Expr *arg = args->data[i];
      VReg *vreg = gen_expr(arg);
      const ArgInfo *p = &arg_infos[i];
      if (p->offset < 0) {
        if (p->is_flo) {
          ++fregarg;
          int index = freg_arg_count - fregarg;
          assert(index < MAX_FREG_ARGS);
          new_ir_pusharg(vreg, index);
        } else {
          ++iregarg;
          int index = reg_arg_count - iregarg + arg_start;
          assert(index < MAX_REG_ARGS);
          IR *ir = new_ir_pusharg(vreg, index);
#if !VAARG_FP_AS_GP
          UNUSED(ir);
#else
          if (p->fp_as_gp)
            ir->pusharg.fp_as_gp = true;
#endif
        }
      } else {
        enum VRegSize offset_type = 2;  //{.size = 4, .align = 4};  // TODO:
        int ofs = p->offset;
        VReg *dst = new_ir_sofs(new_const_vreg(ofs, offset_type));
        if (is_stack_param(arg->type)) {
          gen_memcpy(arg->type, dst, vreg);
        } else {
          int flag = is_unsigned(arg->type) ? IRF_UNSIGNED : 0;
          new_ir_store(dst, vreg, flag);
        }
      }
      arg_vregs[i + arg_start] = vreg;
    }
  }
  if (ret_varinfo != NULL) {
    VReg *dst = new_ir_bofs(ret_varinfo->local.frameinfo);
    new_ir_pusharg(dst, 0);
    arg_vregs[0] = dst;
    ++reg_arg_count;
  }

  bool label_call = false;
  bool global = false;
  if (func->kind == EX_VAR) {
    const VarInfo *varinfo = scope_find(func->var.scope, func->var.name, NULL);
    assert(varinfo != NULL);
    label_call = varinfo->type->kind == TY_FUNC;
    global = !(varinfo->storage & VS_STATIC);
  }

  VReg *result_reg = NULL;
  {
    int vaarg_start = !functype->func.vaargs || functype->func.params == NULL ? -1 :
        functype->func.params->len + (ret_varinfo != NULL ? 1 : 0);
    Type *type = expr->type;
    if (ret_varinfo != NULL)
      type = ptrof(type);
    enum VRegSize ret_vsize = -1;
    int ret_vflag = 0;
    if (type->kind != TY_VOID) {
      ret_vsize = to_vsize(type);
      ret_vflag = to_vflag(type);
    }
    if (label_call) {
      result_reg = new_ir_call(func->var.name, global, NULL, total_arg_count,
                               reg_arg_count + freg_arg_count, ret_vsize, ret_vflag, precall,
                               arg_vregs, vaarg_start);
    } else {
      VReg *freg = gen_expr(func);
      result_reg = new_ir_call(NULL, false, freg, total_arg_count, reg_arg_count + freg_arg_count,
                               ret_vsize, ret_vflag, precall, arg_vregs, vaarg_start);
    }
  }

  return result_reg;
}

static VReg *gen_arith(enum ExprKind kind, const Type *type, VReg *lhs, VReg *rhs) {
  assert(EX_ADD <= kind && kind <= EX_RSHIFT);
  int flag = is_unsigned(type) ? IRF_UNSIGNED : 0;
  return new_ir_bop(kind + (IR_ADD - EX_ADD), lhs, rhs, to_vsize(type), flag);
}

static VReg *gen_block_expr(Expr *expr) {
  return gen_block(expr->block);
}

static VReg *gen_fixnum(Expr *expr) {
  return new_const_vreg(expr->fixnum, to_vsize(expr->type));
}

static VReg *gen_flonum(Expr *expr) {
#ifndef __NO_FLONUM
  assert(expr->type->kind == TY_FLONUM);
  Initializer *init = new_initializer(IK_SINGLE, expr->token);
  init->single = expr;

  assert(curscope != NULL);
  Type *type = qualified_type(expr->type, TQ_CONST);
  const Name *name = alloc_label();
  VarInfo *varinfo = scope_add(curscope, name, type, VS_STATIC);
  VarInfo *gvarinfo = is_global_scope(curscope) ? varinfo : varinfo->static_.gvar;
  gvarinfo->global.init = init;

  VReg *src = new_ir_iofs(gvarinfo->name, false);
  return new_ir_load(src, to_vsize(type), to_vflag(type), 0);
#else
  UNUSED(expr);
  assert(false);
  return NULL;
#endif
}

static VReg *gen_str(Expr *expr) {
  UNUSED(expr);
  assert(!"should be handled in parser");
  return NULL;
}

static VReg *gen_ref(Expr *expr) {
  return gen_ref_sub(expr->unary.sub);
}

static VReg *gen_deref(Expr *expr) {
  VReg *vreg = gen_expr(expr->unary.sub);
  // array, struct and func values are handled as a pointer.
  if (is_prim_type(expr->type)) {
    int irflag = is_unsigned(expr->type) ? IRF_UNSIGNED : 0;
    vreg = new_ir_load(vreg, to_vsize(expr->type), to_vflag(expr->type), irflag);
  }
  return vreg;
}

static VReg *gen_member(Expr *expr) {
#ifndef __NO_BITFIELD
  const MemberInfo *minfo = expr->member.info;
  if (minfo->bitfield.width > 0) {
    Type *type = get_fixnum_type(minfo->bitfield.base_kind, minfo->type->fixnum.is_unsigned, 0);
    Expr *ptr = make_cast(ptrof(type), expr->token, make_refer(expr->token, expr), true);
    Expr *load = new_expr_deref(NULL, ptr);
    Expr *e = extract_bitfield_value(load, minfo);
    return gen_expr(e);
  }
#endif

  VReg *vreg = gen_lval(expr);
  VReg *result = vreg;
  if (is_prim_type(expr->type)) {
    int irflag = is_unsigned(expr->type) ? IRF_UNSIGNED : 0;
    result = new_ir_load(vreg, to_vsize(expr->type), to_vflag(expr->type), irflag);
  }
  return result;
}

static VReg *gen_comma(Expr *expr) {
  gen_expr(expr->bop.lhs);
  return gen_expr(expr->bop.rhs);
}

static VReg *gen_assign_sub(Expr *lhs, Expr *rhs) {
  VReg *src = gen_expr(rhs);
  if (lhs->kind == EX_VAR) {
    if (is_prim_type(lhs->type) && !is_global_scope(lhs->var.scope)) {
      const VarInfo *varinfo = scope_find(lhs->var.scope, lhs->var.name, NULL);
      if (is_local_storage(varinfo)) {
        assert(varinfo->local.vreg != NULL);
        new_ir_mov(varinfo->local.vreg, src, is_unsigned(rhs->type) ? IRF_UNSIGNED : 0);
        return src;
      }
    }
  }

  VReg *dst = gen_lval(lhs);

  switch (lhs->type->kind) {
  case TY_ARRAY: case TY_FUNC: case TY_VOID:
    assert(false);
    // Fallthrough to suppress compiler error.
  case TY_FIXNUM:
  case TY_PTR:
  case TY_FLONUM:
    {
      int flag = is_unsigned(rhs->type) ? IRF_UNSIGNED : 0;
      new_ir_store(dst, src, flag);
    }
    break;
  case TY_STRUCT:
    if (lhs->type->struct_.info->size > 0) {
      gen_memcpy(lhs->type, dst, src);
    }
    break;
  }
  return src;
}

static VReg *gen_assign(Expr *expr) {
  assert(same_type(expr->type, expr->bop.lhs->type));
  return gen_assign_sub(expr->bop.lhs, expr->bop.rhs);
}

static VReg *gen_expr_incdec(Expr *expr) {
#define IS_POST(expr)  ((expr)->kind >= EX_POSTINC)
#define IS_DEC(expr)   (((expr)->kind - EX_PREINC) & 1)
  static enum IrKind kOpAddSub[] = {IR_ADD, IR_SUB};

  Expr *target = expr->unary.sub;
  const VarInfo *varinfo = NULL;
  if (target->kind == EX_VAR && !is_global_scope(target->var.scope)) {
    const VarInfo *vi = scope_find(target->var.scope, target->var.name, NULL);
    assert(vi != NULL);
    if (is_local_storage(vi))
      varinfo = vi;
  }

  enum VRegSize vsize = to_vsize(expr->type);
  VReg *before = NULL;
  VReg *lval = NULL;
  VReg *val;
  int flag = is_unsigned(expr->type) ? IRF_UNSIGNED : 0;
  if (varinfo != NULL) {
    val = varinfo->local.vreg;
    if (IS_POST(expr)) {
      before = add_new_vreg(target->type);
      new_ir_mov(before, val, flag);
    }
  } else {
    lval = gen_lval(target);
    val = new_ir_load(lval, vsize, to_vflag(expr->type), flag);
    if (IS_POST(expr))
      before = val;
  }

  VReg *addend =
#ifndef __NO_FLONUM
      is_flonum(target->type) ? gen_flonum(new_expr_flolit(target->type, NULL, 1)) :
#endif
      new_const_vreg(expr->type->kind == TY_PTR ? type_size(expr->type->pa.ptrof) : 1, vsize);
  VReg *after = new_ir_bop(kOpAddSub[IS_DEC(expr)], val, addend, vsize, flag);
  if (varinfo != NULL)  new_ir_mov(varinfo->local.vreg, after, flag);
  else                  new_ir_store(lval, after, flag);
  return before != NULL ? before : after;
#undef IS_POST
#undef IS_DEC
}

static VReg *gen_pos(Expr *expr) {
  return gen_expr(expr->unary.sub);
}

static VReg *gen_neg(Expr *expr) {
  VReg *vreg = gen_expr(expr->unary.sub);
  return new_ir_unary(IR_NEG, vreg, to_vsize(expr->type), is_unsigned(expr->type) ? IRF_UNSIGNED : 0);
}

static VReg *gen_bitnot(Expr *expr) {
  VReg *vreg = gen_expr(expr->unary.sub);
  return new_ir_unary(IR_BITNOT, vreg, to_vsize(expr->type), is_unsigned(expr->type) ? IRF_UNSIGNED : 0);
}

static VReg *gen_relation(Expr *expr) {
  struct CompareExpr cmp = gen_compare_expr(expr->kind, expr->bop.lhs, expr->bop.rhs);
  switch (cmp.cond) {
  case COND_NONE:
  case COND_ANY:
    return new_const_vreg(cmp.cond == COND_ANY, to_vsize(&tyBool));
  default:
    return new_ir_cond(cmp.lhs, cmp.rhs, cmp.cond);
  }
}

static VReg *gen_expr_logandor(Expr *expr) {
  BB *false_bb = new_bb();
  BB *next_bb = new_bb();
  gen_cond_jmp(expr, false, false_bb);
  enum VRegSize vsbool = to_vsize(&tyBool);
  VReg *result = add_new_vreg(&tyBool);
  new_ir_mov(result, new_const_vreg(true, vsbool), 0);
  new_ir_jmp(next_bb);
  set_curbb(false_bb);
  new_ir_mov(result, new_const_vreg(false, vsbool), 0);
  set_curbb(next_bb);
  return result;
}

static VReg *gen_expr_bop(Expr *expr) {
  VReg *lhs = gen_expr(expr->bop.lhs);
  VReg *rhs = gen_expr(expr->bop.rhs);
  return gen_arith(expr->kind, expr->type, lhs, rhs);
}

static VReg *gen_complit(Expr *expr) {
  Expr *var = expr->complit.var;
  const VarInfo *varinfo = scope_find(var->var.scope, var->var.name, NULL);
  assert(varinfo != NULL);
  gen_clear_local_var(varinfo);
  gen_stmts(expr->complit.inits);
  return gen_expr(var);
}

static VReg *gen_inlined(Expr *expr) {
  // Nested inline funcall is transformed so its scope relation is modified.
  // ex. foo(bar(123))
  //     => ({foo-body(({bar-body(123)}))})
  //     => tmp=({bar-body(123)}), ({foo-body(tmp)})
  Scope *bak_curscope = curscope;
  Stmt *embedded = expr->inlined.embedded;
  assert(embedded->kind == ST_BLOCK);
  Scope *top_scope = embedded->block.scope;
  assert(top_scope != NULL);
  Vector *top_scope_vars = top_scope->vars;

  // Assign arguments to variables for embedding function parameter.
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

  // Tweak function information to make `return` statement works for inlined function.
  BB *inline_end_bb = new_bb();
  FuncBackend *fnbe = curfunc->extra;
  BB *bak_retbb = fnbe->ret_bb;
  VReg *bak_retval = fnbe->retval;
  VReg *bak_result_dst = fnbe->result_dst;
  fnbe->ret_bb = inline_end_bb;
  fnbe->retval = NULL;

  Type *rettype = expr->type;
  VReg *dst = NULL;
  if (rettype->kind != TY_VOID) {
    if (!is_prim_type(rettype)) {
      // Receive as its pointer.
      rettype = ptrof(rettype);
    }
    fnbe->result_dst = dst = add_new_vreg(rettype);
  }

  gen_block(embedded);

  fnbe->result_dst = bak_result_dst;
  fnbe->retval = bak_retval;
  fnbe->ret_bb = bak_retbb;

  set_curbb(inline_end_bb);
  return dst;
}

VReg *gen_expr(Expr *expr) {
  typedef VReg *(*GenExprFunc)(Expr *);
  static const GenExprFunc table[] = {
    [EX_FIXNUM] = gen_fixnum, [EX_FLONUM] = gen_flonum, [EX_STR] = gen_str,
    [EX_VAR] = gen_variable,
    [EX_ADD] = gen_expr_bop, [EX_SUB] = gen_expr_bop, [EX_MUL] = gen_expr_bop,
    [EX_DIV] = gen_expr_bop, [EX_MOD] = gen_expr_bop, [EX_BITAND] = gen_expr_bop,
    [EX_BITOR] = gen_expr_bop, [EX_BITXOR] = gen_expr_bop,
    [EX_LSHIFT] = gen_expr_bop, [EX_RSHIFT] = gen_expr_bop,
    [EX_EQ] = gen_relation, [EX_NE] = gen_relation, [EX_LT] = gen_relation,
    [EX_LE] = gen_relation, [EX_GE] = gen_relation, [EX_GT] = gen_relation,
    [EX_LOGAND] = gen_expr_logandor, [EX_LOGIOR] = gen_expr_logandor,
    [EX_ASSIGN] = gen_assign, [EX_COMMA] = gen_comma,
    [EX_POS] = gen_pos, [EX_NEG] = gen_neg, [EX_BITNOT] = gen_bitnot,
    [EX_PREINC] = gen_expr_incdec, [EX_PREDEC] = gen_expr_incdec,
    [EX_POSTINC] = gen_expr_incdec, [EX_POSTDEC] = gen_expr_incdec,
    [EX_REF] = gen_ref, [EX_DEREF] = gen_deref, [EX_CAST] = gen_cast, [EX_TERNARY] = gen_ternary,
    [EX_MEMBER] = gen_member, [EX_FUNCALL] = gen_funcall, [EX_INLINED] = gen_inlined,
    [EX_COMPLIT] = gen_complit, [EX_BLOCK] = gen_block_expr,
  };

  assert(expr->kind < (int)ARRAY_SIZE(table));
  assert(table[expr->kind] != NULL);
  return (*table[expr->kind])(expr);
}
