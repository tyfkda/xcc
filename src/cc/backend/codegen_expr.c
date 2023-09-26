#include "../../config.h"
#include "codegen.h"

#include <assert.h>
#include <stdlib.h>  // malloc

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
  for (enum VRegSize vsize = VRegSize1; vsize <= VRegSize8 ; ++vsize) {
    if (size == 1 << vsize)
      return vsize;
  }
  assert(!"Must not reached");
  return -1;
}

int to_vflag(const Type *type) {
  if (is_flonum(type))
    return VRF_FLONUM;
  return !is_fixnum(type->kind) || type->fixnum.is_unsigned ? VRF_UNSIGNED : 0;
}

VReg *add_new_vreg(const Type *type, int vflag) {
  return reg_alloc_spawn(curra, to_vsize(type), vflag | to_vflag(type));
}

static Table builtin_function_table;  // <BuiltinFunctionProc>

void add_builtin_function(const char *str, Type *type, BuiltinFunctionProc *proc, bool add_to_scope) {
  const Name *name = alloc_name(str, NULL, false);
  table_put(&builtin_function_table, name, proc);

  if (add_to_scope)
    scope_add(global_scope, name, type, 0);
}

static enum ConditionKind swap_cond(enum ConditionKind cond) {
  assert(COND_EQ <= cond && cond <= COND_GT);
  if (cond >= COND_LT)
    cond = (COND_GT + COND_LT) - cond;
  return cond;
}

static enum ConditionKind gen_compare_expr(enum ExprKind kind, Expr *lhs, Expr *rhs) {
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
    // Const VReg is must be non-flonum.
    assert(!(lhs_reg->flag & VRF_FLONUM));
    assert(!(rhs_reg->flag & VRF_FLONUM));
    assert(!(flag & COND_FLONUM));
    switch (cond | flag) {
    case COND_NONE:
    case COND_ANY:
      return cond;
    case COND_EQ:  return lhs_reg->fixnum == rhs_reg->fixnum ? COND_ANY : COND_NONE;
    case COND_NE:  return lhs_reg->fixnum != rhs_reg->fixnum ? COND_ANY : COND_NONE;
    case COND_LT:  return lhs_reg->fixnum <  rhs_reg->fixnum ? COND_ANY : COND_NONE;
    case COND_LE:  return lhs_reg->fixnum <= rhs_reg->fixnum ? COND_ANY : COND_NONE;
    case COND_GE:  return lhs_reg->fixnum >= rhs_reg->fixnum ? COND_ANY : COND_NONE;
    case COND_GT:  return lhs_reg->fixnum >  rhs_reg->fixnum ? COND_ANY : COND_NONE;
    case COND_EQ | COND_UNSIGNED:  return (uint64_t)lhs_reg->fixnum == (uint64_t)rhs_reg->fixnum ? COND_ANY : COND_NONE;
    case COND_NE | COND_UNSIGNED:  return (uint64_t)lhs_reg->fixnum != (uint64_t)rhs_reg->fixnum ? COND_ANY : COND_NONE;
    case COND_LT | COND_UNSIGNED:  return (uint64_t)lhs_reg->fixnum <  (uint64_t)rhs_reg->fixnum ? COND_ANY : COND_NONE;
    case COND_LE | COND_UNSIGNED:  return (uint64_t)lhs_reg->fixnum <= (uint64_t)rhs_reg->fixnum ? COND_ANY : COND_NONE;
    case COND_GE | COND_UNSIGNED:  return (uint64_t)lhs_reg->fixnum >= (uint64_t)rhs_reg->fixnum ? COND_ANY : COND_NONE;
    case COND_GT | COND_UNSIGNED:  return (uint64_t)lhs_reg->fixnum >  (uint64_t)rhs_reg->fixnum ? COND_ANY : COND_NONE;
    default: assert(false); break;
    }
  }

  assert(is_prim_type(lhs->type));

  new_ir_cmp(lhs_reg, rhs_reg);
  return cond | flag;
}

void gen_cond_jmp(Expr *cond, bool tf, BB *bb) {
  enum ExprKind ck = cond->kind;
  switch (ck) {
  case EX_FIXNUM:
    if (cond->fixnum == 0)
      tf = !tf;
    if (tf)
      new_ir_jmp(COND_ANY, bb);
    return;
  case EX_EQ:
  case EX_NE:
  case EX_LT:
  case EX_LE:
  case EX_GE:
  case EX_GT:
    if (!tf) {
      if (ck <= EX_NE)
        ck = (EX_EQ + EX_NE) - ck;  // EQ <-> NE
      else
        ck = EX_LT + ((ck - EX_LT) ^ 2);  // LT <-> GE, LE <-> GT
    }
    new_ir_jmp(gen_compare_expr(ck, cond->bop.lhs, cond->bop.rhs), bb);
    return;
  case EX_LOGAND:
  case EX_LOGIOR:
    {
      BB *bb1 = new_bb();
      BB *bb2 = new_bb();
      if (!tf)
        ck = (EX_LOGAND + EX_LOGIOR) - ck;  // LOGAND <-> LOGIOR
      if (ck == EX_LOGAND) {
        gen_cond_jmp(cond->bop.lhs, !tf, bb2);
        set_curbb(bb1);
        gen_cond_jmp(cond->bop.rhs, tf, bb);
      } else {
        gen_cond_jmp(cond->bop.lhs, tf, bb);
        set_curbb(bb1);
        gen_cond_jmp(cond->bop.rhs, tf, bb);
      }
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
  VReg *vreg = gen_expr(expr->unary.sub);
  const Type *dst_type = expr->type;

  switch (dst_type->kind) {
  case TY_VOID:
    return NULL;  // Assume void value is not used.
  case TY_STRUCT:
    return vreg;
  default: break;
  }

  if (vreg->flag & VRF_CONST) {
    assert(!(vreg->flag & VRF_FLONUM));  // No const vreg for flonum.
    Fixnum value = vreg->fixnum;
    size_t dst_size = type_size(dst_type);
    if (dst_size < (size_t)(1 << vreg->vsize) && dst_size < sizeof(Fixnum)) {
      // Assume that integer is represented in Two's complement
      size_t bit = dst_size * TARGET_CHAR_BIT;
      UFixnum mask = (-1UL) << bit;
      if (dst_type->kind == TY_FIXNUM && !dst_type->fixnum.is_unsigned &&  // signed
          (value & (1 << (bit - 1))))  // negative
        value |= mask;
      else
        value &= ~mask;
    }

    enum VRegSize vsize = to_vsize(dst_type);
    int vflag = to_vflag(dst_type);
    return new_const_vreg(value, vsize, vflag);
  }

  int dst_size = type_size(dst_type);
  bool lu = dst_type->kind == TY_FIXNUM ? dst_type->fixnum.is_unsigned : dst_type->kind == TY_PTR;
  bool ru = (vreg->flag & VRF_UNSIGNED) ? true : false;
  if (dst_size == 1 << vreg->vsize && lu == ru
      && is_flonum(dst_type) == ((vreg->flag & VRF_FLONUM) != 0))
    return vreg;

  return new_ir_cast(vreg, to_vsize(dst_type), to_vflag(dst_type));
}

static VReg *gen_lval(Expr *expr) {
  switch (expr->kind) {
  case EX_VAR:
    {
      Scope *scope;
      const VarInfo *varinfo = scope_find(expr->var.scope, expr->var.name, &scope);
      assert(varinfo != NULL && scope == expr->var.scope);
      if (is_global_scope(scope))
        return new_ir_iofs(expr->var.name, (varinfo->storage & VS_STATIC) == 0);
      else if (is_local_storage(varinfo))
        return new_ir_bofs(varinfo->local.frameinfo, varinfo->local.vreg);
      else if (varinfo->storage & VS_STATIC)
        return new_ir_iofs(varinfo->static_.gvar->name, false);
      else
        return new_ir_iofs(expr->var.name, true);
    }
  case EX_DEREF:
    return gen_expr(expr->unary.sub);
  case EX_MEMBER:
    {
      const MemberInfo *member = member_info(expr);
      VReg *vreg = gen_expr(expr->member.target);
      if (member->offset == 0)
        return vreg;
      enum VRegSize vsize = to_vsize(&tySize);
      int vflag = vreg->flag & VRF_MASK;
      VReg *imm = new_const_vreg(member->offset, vsize, vflag);
      VReg *result = new_ir_bop(IR_ADD, vreg, imm, vsize);
      return result;
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
      return gen_lval(expr->complit.var);
    }
  default: assert(false); break;
  }
  return NULL;
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
      VReg *result = new_ir_unary(IR_LOAD, vreg, to_vsize(expr->type), to_vflag(expr->type));
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
    result = add_new_vreg(type, 0);
  }

  gen_cond_jmp(expr->ternary.cond, false, fbb);

  set_curbb(tbb);
  VReg *tval = gen_expr(expr->ternary.tval);
  if (result != NULL)
    new_ir_mov(result, tval);
  new_ir_jmp(COND_ANY, nbb);

  set_curbb(fbb);
  VReg *fval = gen_expr(expr->ternary.fval);
  if (result != NULL)
    new_ir_mov(result, fval);

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
#if defined(__x86_64__)
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
      p->stack_arg = is_stack_param(arg->type);
#if defined(VAARG_ON_STACK)
      if (functype->func.vaargs && functype->func.params != NULL && i >= functype->func.params->len)
        p->stack_arg = true;
#endif
      if (p->stack_arg || (p->is_flo ? freg_index >= MAX_FREG_ARGS : ireg_index >= MAX_REG_ARGS)) {
        offset = ALIGN(offset, align_size(arg->type));
        p->offset = offset;
        offset += ALIGN(p->size, WORD_SIZE);
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
    new_ir_subsp(new_const_vreg(offset, to_vsize(&tySSize), VRF_UNSIGNED), NULL);

  int total_arg_count = arg_count + (ret_varinfo != NULL ? 1 : 0);
  VReg **arg_vregs = total_arg_count == 0 ? NULL : calloc(total_arg_count, sizeof(*arg_vregs));

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
          new_ir_pusharg(vreg, index);
        }
      } else {
        enum VRegSize offset_type = 2;  //{.size = 4, .align = 4};  // TODO:
        int ofs = p->offset;
        VReg *dst = new_ir_sofs(new_const_vreg(ofs, offset_type, 0));
        if (is_stack_param(arg->type)) {
          gen_memcpy(arg->type, dst, vreg);
        } else {
          new_ir_store(dst, vreg);
        }
      }
      arg_vregs[i + arg_start] = vreg;
    }
  }
  if (ret_varinfo != NULL) {
    VReg *dst = new_ir_bofs(ret_varinfo->local.frameinfo, ret_varinfo->local.vreg);
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
      result_reg = new_ir_call(func->var.name, global, NULL, total_arg_count, reg_arg_count + freg_arg_count,
                               ret_vsize, ret_vflag, precall, arg_vregs, vaarg_start);
    } else {
      VReg *freg = gen_expr(func);
      result_reg = new_ir_call(NULL, false, freg, total_arg_count, reg_arg_count + freg_arg_count,
                               ret_vsize, ret_vflag, precall, arg_vregs, vaarg_start);
    }
  }

  return result_reg;
}

VReg *gen_arith(enum ExprKind kind, const Type *type, VReg *lhs, VReg *rhs) {
  assert(EX_ADD <= kind && kind <= EX_RSHIFT);
  assert(!(kind == EX_DIV || kind == EX_MOD) || is_number(type));
  return new_ir_bop(kind + (IR_ADD - EX_ADD), lhs, rhs, to_vsize(type));
}

#ifndef __NO_FLONUM
VReg *gen_const_flonum(Expr *expr) {
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
  return new_ir_unary(IR_LOAD, src, to_vsize(type), to_vflag(type));
}
#endif

static VReg *gen_block_expr(Expr *expr) {
  return gen_block(expr->block);
}

static VReg *gen_fixnum(Expr *expr) {
  VReg *vreg = new_const_vreg(expr->fixnum, to_vsize(expr->type), to_vflag(expr->type));
  if (!is_im32(expr->fixnum)) {
    // Large constant value is not allowed in x86,
    // so use mov instruction.
    VReg *tmp = add_new_vreg(expr->type, 0);
    new_ir_mov(tmp, vreg);
    vreg = tmp;
  }
  return vreg;
}

static VReg *gen_flonum(Expr *expr) {
#ifndef __NO_FLONUM
  return gen_const_flonum(expr);
#else
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
  return gen_lval(expr->unary.sub);
}

static VReg *gen_deref(Expr *expr) {
  VReg *vreg = gen_expr(expr->unary.sub);
  // array, struct and func values are handled as a pointer.
  if (is_prim_type(expr->type))
    vreg = new_ir_unary(IR_LOAD, vreg, to_vsize(expr->type), to_vflag(expr->type));
  return vreg;
}

static VReg *gen_member(Expr *expr) {
#ifndef __NO_BITFIELD
  const MemberInfo *minfo = member_info(expr);
  if (minfo->bitfield.width > 0) {
    Type *type = get_fixnum_type(minfo->bitfield.base_kind, minfo->type->fixnum.is_unsigned, 0);
    Expr *ptr = new_expr_unary(EX_REF, ptrof(type), NULL, expr);  // promote-to-int
    Expr *load = new_expr_deref(NULL, ptr);
    Expr *e = extract_bitfield_value(load, minfo);
    return gen_expr(e);
  }
#endif

  VReg *vreg = gen_lval(expr);
  VReg *result = vreg;
  if (is_prim_type(expr->type))
    result = new_ir_unary(IR_LOAD, vreg, to_vsize(expr->type), to_vflag(expr->type));
  return result;
}

static VReg *gen_comma(Expr *expr) {
  gen_expr(expr->bop.lhs);
  return gen_expr(expr->bop.rhs);
}

static VReg *gen_assign(Expr *expr) {
  VReg *src = gen_expr(expr->bop.rhs);
  if (expr->bop.lhs->kind == EX_VAR) {
    Expr *lhs = expr->bop.lhs;
    if (is_prim_type(lhs->type) && !is_global_scope(lhs->var.scope)) {
      const VarInfo *varinfo = scope_find(lhs->var.scope, lhs->var.name, NULL);
      if (is_local_storage(varinfo)) {
        assert(varinfo->local.vreg != NULL);
        new_ir_mov(varinfo->local.vreg, src);
        return src;
      }
    }
  }

  VReg *dst = gen_lval(expr->bop.lhs);

  switch (expr->type->kind) {
  case TY_ARRAY: case TY_FUNC: case TY_VOID:
    assert(false);
    // Fallthrough to suppress compiler error.
  case TY_FIXNUM:
  case TY_PTR:
  case TY_FLONUM:
    new_ir_store(dst, src);
    break;
  case TY_STRUCT:
    if (expr->type->struct_.info->size > 0) {
      gen_memcpy(expr->type, dst, src);
    }
    break;
  }
  return src;
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
  if (varinfo != NULL) {
    val = varinfo->local.vreg;
    if (IS_POST(expr)) {
      before = add_new_vreg(target->type, val->flag & VRF_MASK);
      new_ir_mov(before, val);
    }
  } else {
    lval = gen_lval(target);
    val = new_ir_unary(IR_LOAD, lval, vsize, to_vflag(expr->type));
    if (IS_POST(expr))
      before = val;
  }

  VReg *addend =
#ifndef __NO_FLONUM
      is_flonum(target->type) ? gen_const_flonum(new_expr_flolit(target->type, NULL, 1)) :
#endif
      new_const_vreg(expr->type->kind == TY_PTR ? type_size(expr->type->pa.ptrof) : 1, vsize, val->flag & VRF_MASK);
  VReg *after = new_ir_bop(kOpAddSub[IS_DEC(expr)], val, addend, vsize);
  if (varinfo != NULL)  new_ir_mov(varinfo->local.vreg, after);
                  else  new_ir_store(lval, after);
  return before != NULL ? before : after;
#undef IS_POST
#undef IS_DEC
}

static VReg *gen_pos(Expr *expr) {
  return gen_expr(expr->unary.sub);
}

static VReg *gen_neg(Expr *expr) {
  VReg *vreg = gen_expr(expr->unary.sub);
#ifndef __NO_FLONUM
  if (is_flonum(expr->type)) {
    VReg *zero = gen_expr(new_expr_flolit(expr->type, NULL, 0.0));
    return gen_arith(EX_SUB, expr->type, zero, vreg);
  }
#endif
  VReg *result = new_ir_unary(IR_NEG, vreg, to_vsize(expr->type), to_vflag(expr->type));
  return result;
}

static VReg *gen_bitnot(Expr *expr) {
  VReg *vreg = gen_expr(expr->unary.sub);
  VReg *result = new_ir_unary(IR_BITNOT, vreg, to_vsize(expr->type), to_vflag(expr->type));
  return result;
}

static VReg *gen_relation(Expr *expr) {
  enum ConditionKind cond = gen_compare_expr(expr->kind, expr->bop.lhs, expr->bop.rhs);
  switch (cond) {
  case COND_NONE:
  case COND_ANY:
    return new_const_vreg(cond == COND_ANY, to_vsize(&tyBool), 0);
  default:
    return new_ir_cond(cond);
  }
}

static VReg *gen_expr_logandor(Expr *expr) {
  BB *false_bb = new_bb();
  BB *next_bb = new_bb();
  gen_cond_jmp(expr, false, false_bb);
  enum VRegSize vsbool = to_vsize(&tyBool);
  VReg *result = add_new_vreg(&tyBool, 0);
  new_ir_mov(result, new_const_vreg(true, vsbool, 0));
  new_ir_jmp(COND_ANY, next_bb);
  set_curbb(false_bb);
  new_ir_mov(result, new_const_vreg(false, vsbool, 0));
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
    [EX_MEMBER] = gen_member, [EX_FUNCALL] = gen_funcall, [EX_COMPLIT] = gen_complit,
    [EX_BLOCK] = gen_block_expr,
  };

  assert(expr->kind < (int)sizeof(table) / sizeof(*table));
  return (*table[expr->kind])(expr);
}
