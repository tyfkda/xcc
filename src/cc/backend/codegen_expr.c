#include "../../config.h"
#include "codegen.h"

#include <assert.h>
#include <stdlib.h>  // malloc

#include "ast.h"
#include "be_aux.h"
#include "expr.h"
#include "fe_misc.h"  // curscope, extract_bitfield_vale
#include "ir.h"
#include "regalloc.h"
#include "table.h"
#include "type.h"
#include "util.h"
#include "var.h"

bool is_stack_param(const Type *type) {
#if STRUCT_ARG_AS_POINTER
  UNUSED(type);
  return false;
#else
  return type->kind == TY_STRUCT;
#endif
}

enum VRegSize to_vsize(const Type *type) {
  const int MAX_REG_SIZE = 8;
  assert(is_prim_type(type));
  int size = type_size(type);
  assert(1 <= size && size <= MAX_REG_SIZE && IS_POWER_OF_2(size));
  UNUSED(MAX_REG_SIZE);
  return most_significant_bit(size);
}

int to_vflag_with_storage(const Type *type, int storage) {
  int flag = 0;
  if (is_flonum(type))
    flag |= VRF_FLONUM;
  if (type->qualifier & TQ_VOLATILE)
    flag |= (storage & VS_REGISTER) ? (VRF_VOLATILEREG | VRF_NO_SPILL) : VRF_VOLATILE;
  return flag;
}

VReg *add_new_vreg_with_storage(const Type *type, int storage) {
  return reg_alloc_spawn(curra, to_vsize(type), to_vflag_with_storage(type, storage));
}

static Table builtin_function_table;  // <BuiltinFunctionProc>

void add_builtin_function(const char *str, Type *type, BuiltinFunctionProc *proc,
                          bool add_to_scope) {
  const Name *name = alloc_name(str, NULL, false);
  table_put(&builtin_function_table, name, proc);

  if (add_to_scope) {
    const Token *token = alloc_ident(name, NULL, name->chars, name->chars + name->bytes);
    scope_add(global_scope, token, type, 0);
  }
}

struct CompareExpr {
  enum ConditionKind cond;
  VReg *lhs, *rhs;
};
static struct CompareExpr gen_compare_expr(enum ExprKind kind, Expr *lhs, Expr *rhs) {
  assert(lhs->type->kind == rhs->type->kind);

  assert(EX_EQ <= kind && kind <= EX_GT);
  enum ConditionKind cond = kind + (COND_EQ - EX_EQ);

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
  if ((lhs_reg->flag & VRF_CONST) != 0) {
    if ((rhs_reg->flag & VRF_CONST) != 0) {
      static struct CompareExpr kNone = {.cond = COND_NONE};
      static struct CompareExpr kAny = {.cond = COND_ANY};
#ifndef __NO_FLONUM
      if (flag & COND_FLONUM) {
        assert(lhs_reg->flag & VRF_FLONUM);
        assert(rhs_reg->flag & VRF_FLONUM);
        switch (cond) {
        case COND_NONE: return kNone;
        case COND_ANY:  return kAny;
        case COND_EQ:  return lhs_reg->flonum.value == rhs_reg->flonum.value ? kAny : kNone;
        case COND_NE:  return lhs_reg->flonum.value != rhs_reg->flonum.value ? kAny : kNone;
        case COND_LT:  return lhs_reg->flonum.value <  rhs_reg->flonum.value ? kAny : kNone;
        case COND_LE:  return lhs_reg->flonum.value <= rhs_reg->flonum.value ? kAny : kNone;
        case COND_GE:  return lhs_reg->flonum.value >= rhs_reg->flonum.value ? kAny : kNone;
        case COND_GT:  return lhs_reg->flonum.value >  rhs_reg->flonum.value ? kAny : kNone;
        default: assert(false); break;
        }
      }
#endif
      assert(!(lhs_reg->flag & VRF_FLONUM));
      assert(!(rhs_reg->flag & VRF_FLONUM));
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

    VReg *tmp = lhs_reg;
    lhs_reg = rhs_reg;
    rhs_reg = tmp;
    cond = swap_cond(cond);
  }

  assert(is_prim_type(lhs->type));

  return (struct CompareExpr){.cond = cond | flag, .lhs = lhs_reg, .rhs = rhs_reg};
}

void gen_cond_jmp(Expr *cond, BB *tbb, BB *fbb) {
  enum ExprKind ck = cond->kind;
  switch (ck) {
  case EX_FIXNUM:
    new_ir_jmp(cond->fixnum != 0 ? tbb : fbb);
    return;
  case EX_EQ:
  case EX_NE:
  case EX_LT:
  case EX_LE:
  case EX_GE:
  case EX_GT:
    {
      struct CompareExpr cmp = gen_compare_expr(ck, cond->bop.lhs, cond->bop.rhs);
      new_ir_cjmp(cmp.lhs, cmp.rhs, cmp.cond, tbb);
      set_curbb(new_bb());
      new_ir_jmp(fbb);
    }
    return;
  case EX_LOGAND:
    {
      BB *bb1 = new_bb();
      gen_cond_jmp(cond->bop.lhs, bb1, fbb);
      set_curbb(bb1);
      gen_cond_jmp(cond->bop.rhs, tbb, fbb);
    }
    break;
  case EX_LOGIOR:
    {
      BB *bb1 = new_bb();
      gen_cond_jmp(cond->bop.lhs, tbb, bb1);
      set_curbb(bb1);
      gen_cond_jmp(cond->bop.rhs, tbb, fbb);
    }
    return;
  case EX_COMMA:
    gen_expr(cond->bop.lhs);
    gen_cond_jmp(cond->bop.rhs, tbb, fbb);
    break;
  default: assert(false); break;
  }
}

static VReg *gen_cast(Expr *expr) {
  Expr *src = expr->unary.sub;
  Type *dst_type = expr->type;
  if (is_bool(dst_type))
    return gen_expr(make_cond(src));

  VReg *vreg = gen_expr(src);
  assert(!is_bool(dst_type));

  switch (dst_type->kind) {
  case TY_VOID:
  case TY_STRUCT:
    return vreg;
  default: break;
  }

  size_t dst_size = type_size(dst_type);
  if (vreg->flag & VRF_CONST) {
    assert(!(vreg->flag & VRF_FLONUM));  // No const vreg for flonum.
    Fixnum value = vreg->fixnum;
    if (dst_size < (1U << vreg->vsize) && dst_size < sizeof(Fixnum)) {
      // Assume that integer is represented in Two's complement
      size_t bit = dst_size * TARGET_CHAR_BIT;
      UFixnum mask = (-1UL) << bit;
      if (!is_unsigned(dst_type) && (value & ((Fixnum)1 << (bit - 1))))  // signed && negative
        value |= mask;
      else
        value &= ~mask;
    }

    enum VRegSize vsize = to_vsize(dst_type);
    return new_const_vreg(value, vsize);
  }

  size_t src_size = 1U << vreg->vsize;
  if (dst_size == src_size &&
      is_flonum(dst_type) == ((vreg->flag & VRF_FLONUM) != 0) &&
      (is_flonum(dst_type) || is_unsigned(dst_type) == is_unsigned(src->type)))
    return vreg;

  IR *ir = new_ir_cast(vreg, is_unsigned(src->type), to_vsize(dst_type), to_vflag(dst_type));
  if (is_unsigned(dst_type))
    ir->flag |= IRF_UNSIGNED;
  return ir->dst;
}

static VReg *gen_ref_sub(Expr *expr) {
  switch (expr->kind) {
  case EX_VAR:
    {
      Scope *scope;
      const Name *name = expr->var.name;
      const VarInfo *varinfo = scope_find(expr->var.scope, name, &scope);
      assert(varinfo != NULL && scope == expr->var.scope);
      bool global = false;
      if (is_global_scope(scope)) {
        global = (varinfo->storage & VS_STATIC) == 0;
      } else {
        if (is_local_storage(varinfo)) {
#if STRUCT_ARG_AS_POINTER
          if (varinfo->storage & VS_PARAM && expr->type->kind == TY_STRUCT) {
            assert(varinfo->type->kind == TY_PTR);  // Already transformed from struct to pointer.
            assert(varinfo->local.vreg != NULL);
            return varinfo->local.vreg;
          }
#endif
          return new_ir_bofs(varinfo->local.frameinfo)->dst;
        }
        if (varinfo->storage & VS_STATIC)
          name = varinfo->static_.svar->ident->ident;
        else
          global = true;
      }
      return new_ir_iofs(name, global)->dst;
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
      IR *ir = new_ir_load(vreg, to_vsize(expr->type),
                           to_vflag_with_storage(expr->type, varinfo->storage), irflag);
      return ir->dst;
    }
  case TY_ARRAY:   // Use variable address as a pointer.
  case TY_STRUCT:  // struct value is handled as a pointer.
  case TY_FUNC:
    return gen_lval(expr);
  case TY_VOID: break;
  case TY_AUTO: assert(false); break;
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

  gen_cond_jmp(expr->ternary.cond, tbb, fbb);

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

static inline void set_call_info(IrCallInfo *call, const Name *label, bool global,
                                 int total_arg_count, int reg_arg_count, VReg **args,
                                 int vaarg_start) {
  call->label = label;
  call->global = global;
  call->args = args;
  call->total_arg_count = total_arg_count;
  call->reg_arg_count = reg_arg_count;
  call->vaarg_start = vaarg_start;
}

typedef struct {
  ssize_t offset;
  ssize_t size;
  int reg_index;
  bool is_flo;
#if VAARG_FP_AS_GP
  bool fp_as_gp;
#endif
} ArgInfo;

typedef struct {
  VarInfo *ret_varinfo;
  VReg **arg_vregs;
  ssize_t offset;
  int stack_arg_count;
  int arg_count;
  int reg_arg_count;
  int freg_arg_count;

  // Register arguments.
  int iregarg;
  int fregarg;
} FuncallWork;

static inline ArgInfo *collect_funargs(const Type *functype, int arg_start, Vector *args, FuncallWork *work, ArgInfo *arg_infos) {
  ssize_t offset = 0;
  int stack_arg_count = 0;
  int reg_arg_count = 0;
  int freg_arg_count = 0;
  const int arg_count = args->len;

  int ireg_index = arg_start;
  int freg_index = 0;

  // Check stack arguments.
  for (int i = 0; i < arg_count; ++i) {
    ArgInfo *p = &arg_infos[i];
    p->reg_index = -1;
    p->offset = -1;
    Expr *arg = args->data[i];
    const Type *arg_type = arg->type;
    assert(arg_type->kind != TY_ARRAY);
    p->size = type_size(arg_type);
    p->is_flo = is_flonum(arg_type);
    bool is_vaarg = functype->func.vaargs && functype->func.params != NULL &&
                    i >= functype->func.params->len;
    UNUSED(is_vaarg);
#if VAARG_FP_AS_GP
    p->fp_as_gp = false;
    if (is_vaarg) {
      p->is_flo = false;
      p->fp_as_gp = true;
    }
#endif
    bool stack_arg = is_stack_param(arg_type) ||
                     (p->is_flo ? freg_index >= kArchSetting.max_freg_args
                                : ireg_index >= kArchSetting.max_reg_args);
#if VAARG_ON_STACK
    if (is_vaarg)
      stack_arg = true;
#endif
    if (stack_arg) {
      offset = ALIGN(offset, align_size(arg_type));
      p->offset = offset;
      offset += ALIGN(p->size, TARGET_POINTER_SIZE);
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

  work->offset = offset;
  work->stack_arg_count = stack_arg_count;
  work->arg_count = arg_count;
  work->reg_arg_count = reg_arg_count;
  work->freg_arg_count = freg_arg_count;

  return arg_infos;
}

static inline VReg *gen_funarg(Expr *arg, int i, ArgInfo *arg_infos, FuncallWork *work) {
  VReg *vreg = gen_expr(arg);
  const ArgInfo *p = &arg_infos[i];
  if (p->offset < 0) {
    if (p->is_flo) {
      int fregarg = ++work->fregarg;
      int index = work->freg_arg_count - fregarg;
      assert(index < kArchSetting.max_freg_args);
      new_ir_pusharg(vreg, index);
    } else {
      const int arg_start = work->ret_varinfo != NULL ? 1 : 0;
      int iregarg = ++work->iregarg;
      int index = work->reg_arg_count - iregarg + arg_start;
      assert(index < kArchSetting.max_reg_args);
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
    ssize_t ofs = p->offset;
    VReg *dst = new_ir_sofs(new_const_vreg(ofs, offset_type))->dst;
    if (is_stack_param(arg->type)) {
      gen_memcpy(arg->type, dst, vreg);
    } else {
      int flag = is_unsigned(arg->type) ? IRF_UNSIGNED : 0;
      new_ir_store(dst, vreg, flag);
    }
  }
  return vreg;
}

static inline void gen_funargs(Expr *expr, FuncallWork *work) {
  work->ret_varinfo = NULL;
  work->arg_vregs = NULL;
  work->offset = 0;
  work->stack_arg_count = 0;
  work->arg_count = 0;
  work->reg_arg_count = 0;
  work->freg_arg_count = 0;
  work->iregarg = 0;
  work->fregarg = 0;

  Expr *func = expr->funcall.func;
  const Type *functype = get_callee_type(func->type);
  assert(functype != NULL);

  VarInfo *ret_varinfo = NULL;  // Return value is on the stack?
  if (expr->type->kind == TY_STRUCT) {
    const Token *token = alloc_dummy_ident();
    Type *type = expr->type;
    ret_varinfo = scope_add(curscope, token, type, 0);
    FrameInfo *fi = malloc_or_die(sizeof(*fi));
    fi->offset = 0;
    ret_varinfo->local.frameinfo = fi;
  }
  work->ret_varinfo = ret_varinfo;

  const int arg_start = ret_varinfo != NULL ? 1 : 0;
  ArgInfo *arg_infos = calloc_or_die(sizeof(*arg_infos) * expr->funcall.args->len);
  collect_funargs(functype, arg_start, expr->funcall.args, work, arg_infos);

  const int arg_count = work->arg_count;
  int total_arg_count = arg_count + (ret_varinfo != NULL ? 1 : 0);
  VReg **arg_vregs = total_arg_count == 0 ? NULL
                                          : calloc_or_die(total_arg_count * sizeof(*arg_vregs));
  work->arg_vregs = arg_vregs;

  Vector *args = expr->funcall.args;
  for (int i = arg_count; --i >= 0; ) {
    Expr *arg = args->data[i];
    VReg *vreg = gen_funarg(arg, i, arg_infos, work);
    arg_vregs[i + arg_start] = vreg;
  }

  if (ret_varinfo != NULL) {
    VReg *dst = new_ir_bofs(ret_varinfo->local.frameinfo)->dst;
    new_ir_pusharg(dst, 0);
    arg_vregs[0] = dst;
  }

  free(arg_infos);
}

static inline VReg *gen_funcall_sub(Expr *expr, FuncallWork *work) {
  Expr *func = expr->funcall.func;
  const VarInfo *ret_varinfo = work->ret_varinfo;
  const int stack_arg_count = work->stack_arg_count;
  const int arg_count = work->arg_count;

  bool label_call = false;
  bool global = false;
  if (func->kind == EX_VAR) {
    const VarInfo *varinfo = scope_find(func->var.scope, func->var.name, NULL);
    assert(varinfo != NULL);
    label_call = varinfo->type->kind == TY_FUNC;
    if (label_call)
      global = !(varinfo->storage & VS_STATIC);
  }

  IrCallInfo *callinfo = calloc_or_die(sizeof(*callinfo));
  callinfo->stack_args_size = work->offset;
  callinfo->arg_count = arg_count - stack_arg_count;
  callinfo->living_pregs = 0;
  callinfo->caller_saves = NULL;

  VReg *dst = NULL;
  Type *type = expr->type;
  if (ret_varinfo != NULL)
    type = ptrof(type);
  if (type->kind != TY_VOID)
    dst = reg_alloc_spawn(curra, to_vsize(type), to_vflag(type));

  const Name *funcname = NULL;
  VReg *freg = NULL;
  if (label_call)
    funcname = func->var.name;
  else
    freg = gen_expr(func);

  const Type *functype = get_callee_type(func->type);
  int ret_count = ret_varinfo != NULL ? 1 : 0;
  int total_arg_count = arg_count + ret_count;
  int vaarg_start = !functype->func.vaargs || functype->func.params == NULL ? -1 :
      functype->func.params->len + ret_count;
  set_call_info(callinfo, funcname, global, total_arg_count,
                work->reg_arg_count + work->freg_arg_count + ret_count,
                work->arg_vregs, vaarg_start);
  IR *call = new_ir_call(callinfo, dst, freg);

  FuncallInfo *funcall_info = calloc_or_die(sizeof(*funcall_info));
  funcall_info->call = call;
  assert(expr->funcall.info == NULL);
  expr->funcall.info = funcall_info;

  FuncBackend *fnbe = curfunc->extra;
  assert(fnbe != NULL);
  Vector *funcalls = fnbe->funcalls;
  if (funcalls == NULL)
    fnbe->funcalls = funcalls = new_vector();
  vec_push(funcalls, expr);

  return dst;
}

static VReg *gen_funcall(Expr *expr) {
  Expr *func = expr->funcall.func;
  if (func->kind == EX_VAR && is_global_scope(func->var.scope)) {
    void *proc = table_get(&builtin_function_table, func->var.name);
    if (proc != NULL)
      return (*(BuiltinFunctionProc*)proc)(expr);
  }

  FuncallWork work;
  gen_funargs(expr, &work);
  return gen_funcall_sub(expr, &work);
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
  VReg *vreg = new_const_vreg(expr->fixnum, to_vsize(expr->type));
  if (expr->type->qualifier & TQ_VOLATILE) {
    VReg *var = add_new_vreg(expr->type);
    new_ir_mov(var, vreg, is_unsigned(expr->type) ? IRF_UNSIGNED : 0);
    vreg = var;
  }
  return vreg;
}

static VReg *gen_flonum(Expr *expr) {
#ifndef __NO_FLONUM
  VReg *vreg = new_const_vfreg(expr->flonum, to_vsize(expr->type));
  if (expr->type->qualifier & TQ_VOLATILE) {
    VReg *var = add_new_vreg(expr->type);
    new_ir_mov(var, vreg, 0);
    vreg = var;
  }
  return vreg;
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
    vreg = new_ir_load(vreg, to_vsize(expr->type), to_vflag(expr->type), irflag)->dst;
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
    result = new_ir_load(vreg, to_vsize(expr->type), to_vflag(expr->type), irflag)->dst;
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
    const VarInfo *varinfo = scope_find(lhs->var.scope, lhs->var.name, NULL);
    assert(varinfo != NULL);
    if (is_prim_type(lhs->type) && !is_global_scope(lhs->var.scope)) {
      if (is_local_storage(varinfo)) {
        assert(varinfo->local.vreg != NULL);
        new_ir_mov(varinfo->local.vreg, src, is_unsigned(rhs->type) ? IRF_UNSIGNED : 0);
        return src;
      }
    }

    if ((varinfo->storage & (VS_STATIC | VS_USED)) == VS_STATIC) {
      // Can be omitted.
      return src;
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
  case TY_AUTO: assert(false); break;
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
    val = new_ir_load(lval, vsize, to_vflag(expr->type), flag)->dst;
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
  return new_ir_unary(IR_NEG, vreg, to_vsize(expr->type),
                      is_unsigned(expr->type) ? IRF_UNSIGNED : 0);
}

static VReg *gen_bitnot(Expr *expr) {
  VReg *vreg = gen_expr(expr->unary.sub);
  return new_ir_unary(IR_BITNOT, vreg, to_vsize(expr->type),
                      is_unsigned(expr->type) ? IRF_UNSIGNED : 0);
}

static VReg *gen_relation(Expr *expr) {
  struct CompareExpr cmp = gen_compare_expr(expr->kind, expr->bop.lhs, expr->bop.rhs);
  switch (cmp.cond) {
  case COND_NONE:
  case COND_ANY:
    return new_const_vreg(cmp.cond == COND_ANY, to_vsize(&tyBool));
  default:
    return new_ir_cond(cmp.lhs, cmp.rhs, cmp.cond)->dst;
  }
}

static VReg *gen_expr_logandor(Expr *expr) {
  BB *tbb = new_bb();
  BB *fbb = new_bb();
  BB *nbb = new_bb();
  gen_cond_jmp(expr, tbb, fbb);
  set_curbb(tbb);
  enum VRegSize vsbool = to_vsize(&tyBool);
  VReg *result = add_new_vreg(&tyBool);
  new_ir_mov(result, new_const_vreg(true, vsbool), 0);
  new_ir_jmp(nbb);
  set_curbb(fbb);
  new_ir_mov(result, new_const_vreg(false, vsbool), 0);
  set_curbb(nbb);
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
      Expr *lhs = new_expr_variable(varinfo->ident->ident, varinfo->type, NULL, top_scope);
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
  typedef VReg *(*GenExprFunc)(Expr*);
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
