#include "../config.h"
#include <stdio.h>
#include <assert.h>
#include <stdlib.h>
#include <string.h>

#ifndef __NO_FLONUM
#include <math.h>
#endif

#include "ast.h"
#include "be_aux.h"
#include "expr.h"
#include "codegen.h"
#include "fe_misc.h"
#include "ir.h"
#include "lexer.h"
#include "parser.h"
#include "regalloc.h"
#include "table.h"
#include "type.h"
#include "util.h"
#include "var.h"

static Expr *proc_builtin_classify_type(const Token *ident) {
  consume(TK_LPAR, "`(' expected");
  const Type *type = parse_var_def(NULL, NULL, NULL);
  if (type == NULL) {
    Expr *expr = parse_assign();
    type = expr->type;
  }
  consume(TK_RPAR, "`)' expected");

  return new_expr_fixlit(&tySize, ident, type->kind);
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

  const uint64_t MASK = (1UL << 52) - 1UL;
  union { double d; uint64_t q; } u;
  u.d = NAN;
  u.q = (u.q & ~MASK) | (significand & MASK);
  return new_expr_flolit(&tyDouble, ident, u.d);
}
#endif

#if VAARG_ON_STACK
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
  if (is_global_scope(scope) || !is_local_storage(varinfo)) {
    parse_error(PE_NOFATAL, ap->token, "Must be local variable");
    return NULL;
  }

  Expr *var = strip_cast(args->data[1]);
  // if (var->kind == EX_REF)
  //   var = var->unary.sub;

  const Vector *params = curfunc->params;
  assert(params != NULL);
  assert(params->len > 0);

  bool is_last = false;
  if (var->kind == EX_VAR) {
    assert(curfunc->type->func.vaargs);
    VarInfo *varinfo = params->data[params->len - 1];
    is_last = equal_name(var->var.name, varinfo->ident->ident);
  }
  if (!is_last) {
    parse_error(PE_NOFATAL, var->token, "Must be last function argument");
    return NULL;
  }

  int offset = 0;
  int reg_count[2] = {0, 0};  // [0]=gp-reg, [1]=fp-reg
  for (int i = 0; i < params->len; ++i) {
    VarInfo *info = params->data[i];
    const Type *t = info->type;
    int size = 0, align = 0;
    if (t->kind == TY_STRUCT) {
      size = type_size(t);
      align = align_size(t);
    } else {
      bool is_flo = is_flonum(t);
      if (reg_count[is_flo] >= kArchSetting.max_reg_args[is_flo])
        size = align = TARGET_POINTER_SIZE;
      ++reg_count[is_flo];
    }
    if (size > 0)
      offset = ALIGN(offset, align) + size;
  }

  FuncBackend *fnbe = curfunc->extra;
  FrameInfo *fi = &fnbe->vaarg_frame_info;
  VReg *p = new_ir_bofs(fi)->dst;
  if (offset > 0) {
    enum VRegSize vsize = to_vsize(&tyVoidPtr);
    p = new_ir_bop(IR_ADD, p, new_const_vreg(offset, vsize), vsize, IRF_UNSIGNED);
  }
  new_ir_mov(varinfo->local.vreg, p, IRF_UNSIGNED);
  return NULL;
}
#elif XCC_TARGET_ARCH == XCC_ARCH_RISCV64
static VReg *gen_builtin_va_start(Expr *expr) {
  assert(expr->kind == EX_FUNCALL);
  Vector *args = expr->funcall.args;
  assert(args->len == 2);
  assert(curfunc != NULL);
  Expr *var = strip_cast(args->data[1]);
  // if (var->kind == EX_REF)
  //   var = var->unary.sub;

  const Vector *params = curfunc->params;
  assert(params != NULL);
  assert(params->len > 0);

  bool is_last = false;
  if (var->kind == EX_VAR) {
    assert(curfunc->type->func.vaargs);
    VarInfo *varinfo = params->data[params->len - 1];
    is_last = equal_name(var->var.name, varinfo->ident->ident);
  }
  if (!is_last) {
    parse_error(PE_NOFATAL, var->token, "Must be last function argument");
    return NULL;
  }

  int gn = 0;
  for (int i = 0; i < params->len; ++i) {
    VarInfo *info = params->data[i];
    const Type *t = info->type;
    if (t->kind != TY_STRUCT) {
      if (!is_flonum(t))
        ++gn;
    }
  }

  const int MAX_REG_ARGS = kArchSetting.max_reg_args[0];
  const int MAX_FREG_ARGS = kArchSetting.max_reg_args[1];
  int offset = 0;
  if (gn >= MAX_REG_ARGS) {
    offset = (gn - MAX_REG_ARGS) * TARGET_POINTER_SIZE;
  } else {
    // Check whether register arguments saved on stack has padding.
    // RegParamInfo params[MAX_REG_ARGS + MAX_FREG_ARGS];  // Use VLA?
    RegParamInfo params[8 + 8];
    assert(MAX_REG_ARGS <= 8);
    assert(MAX_FREG_ARGS <= 8);
    const int max_reg_args[2] = {MAX_REG_ARGS, MAX_FREG_ARGS};
    int param_count = enumerate_register_params(curfunc, max_reg_args, params);

    int ngp = 0;
    for (int i = 0; i < param_count; ++i) {
      VReg *vreg = params[i].vreg;
      if (!(vreg->flag & VRF_FLONUM))
        ++ngp;
    }
    int n = MAX_REG_ARGS - ngp;
    if (n > 0) {
      int size_org = n * TARGET_POINTER_SIZE;
      int size = ALIGN(n, 2) * TARGET_POINTER_SIZE;
      offset = size - size_org;
    }
  }

  FuncBackend *fnbe = curfunc->extra;
  FrameInfo *fi = &fnbe->vaarg_frame_info;
  VReg *p = new_ir_bofs(fi)->dst;
  if (offset > 0) {
    enum VRegSize vsize = to_vsize(&tyVoidPtr);
    p = new_ir_bop(IR_ADD, p, new_const_vreg(offset, vsize), vsize, IRF_UNSIGNED);
  }

  // (void)(ap = fp + <vaarg saved offset>)
  VReg *ap = gen_expr(args->data[0]);
  new_ir_mov(ap, p, IRF_UNSIGNED);
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

  const Vector *params = curfunc->params;
  assert(params != NULL);
  assert(params->len > 0);

  bool is_last = false;
  if (var->kind == EX_VAR) {
    assert(curfunc->type->func.vaargs);
    VarInfo *varinfo = params->data[params->len - 1];
    is_last = equal_name(var->var.name, varinfo->ident->ident);
  }
  if (!is_last) {
    parse_error(PE_NOFATAL, var->token, "Must be last function argument");
    return NULL;
  }

  int reg_count[2] = {0, 0};  // [0]=gp-reg, [1]=fp-reg
  size_t mem_offset = 0;
  for (int i = 0; i < params->len; ++i) {
    VarInfo *info = params->data[i];
    const Type *t = info->type;
    if (is_stack_param(t)) {
      mem_offset += ALIGN(type_size(t), 8);
    } else {
      bool is_flo = is_flonum(t);
      ++reg_count[is_flo];
    }
  }

  // ap->gp_offset = reg_count[GPREG] * TARGET_POINTER_SIZE
  const int *MAX_REG_ARGS = kArchSetting.max_reg_args;
  VReg *ap = gen_expr(args->data[GPREG]);
  VReg *gp_offset = ap;
  new_ir_store(gp_offset,
               new_const_vreg(MIN(reg_count[GPREG], MAX_REG_ARGS[GPREG]) * TARGET_POINTER_SIZE,
                              to_vsize(&tyInt)),
               0);

  // ap->fp_offset = (MAX_REG_ARGS[FPREG] + reg_count[FPREG]) * TARGET_POINTER_SIZE
  VReg *fp_offset = new_ir_bop(IR_ADD, ap, new_const_vreg(type_size(&tyInt), to_vsize(&tySize)),
                               ap->vsize, IRF_UNSIGNED);
  new_ir_store(fp_offset,
               new_const_vreg((MAX_REG_ARGS[GPREG] + MIN(reg_count[FPREG], MAX_REG_ARGS[FPREG])) * TARGET_POINTER_SIZE,
                              to_vsize(&tySize)),
               0);

  // ap->overflow_arg_area = 2 * TARGET_POINTER_SIZE
  {
    enum VRegSize vsize = to_vsize(&tyVoidPtr);
    VReg *overflow_arg_area = new_ir_bop(
        IR_ADD, ap, new_const_vreg(type_size(&tyInt) + type_size(&tyInt), vsize), vsize,
        IRF_UNSIGNED);
    FuncBackend *fnbe = curfunc->extra;
    FrameInfo *fi = &fnbe->vaarg_frame_info;
    VReg *p = new_ir_bofs(fi)->dst;
    int gs = MAX(reg_count[GPREG] - MAX_REG_ARGS[GPREG], 0), fs = MAX(reg_count[FPREG] - MAX_REG_ARGS[FPREG], 0);
    size_t offset = (gs + fs) * TARGET_POINTER_SIZE + mem_offset;
    if (offset > 0) {
      VReg *addend = new_const_vreg(offset, vsize);
      p = new_ir_bop(IR_ADD, p, addend, vsize, IRF_UNSIGNED);
    }
    new_ir_store(overflow_arg_area, p, 0);
  }

  // ap->reg_save_area = -(MAX_REG_ARGS[GPREG] + MAX_REG_ARGS[FPREG]) * TARGET_POINTER_SIZE
  {
    enum VRegSize vsize = to_vsize(&tyVoidPtr);
    VReg *reg_save_area = new_ir_bop(
        IR_ADD, ap,
        new_const_vreg(type_size(&tyInt) + type_size(&tyInt) + type_size(&tyVoidPtr), vsize),
        vsize, IRF_UNSIGNED);
    FrameInfo *fi = malloc_or_die(sizeof(*fi));
    fi->offset = -(MAX_REG_ARGS[GPREG] + MAX_REG_ARGS[FPREG]) * TARGET_POINTER_SIZE;
    VReg *p = new_ir_bofs(fi)->dst;
    new_ir_store(reg_save_area, p, 0);
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
  Expr *aligned_size = new_expr_int_bop(
      EX_BITAND, token,
      new_expr_addsub(EX_ADD, token, make_cast(&tySSize, token, size, false),
                      new_expr_fixlit(&tySSize, token, stack_align - 1)),
      new_expr_fixlit(&tySSize, token, -stack_align));
  VReg *addend = gen_expr(aligned_size);
  VReg *result = add_new_vreg(&tyVoidPtr);
  new_ir_subsp(addend, result);

  // `stack_work_size` is not calculated in gen phase,
  // so prepare vreg for it and fill the value in emit phase.
  FuncBackend *fnbe = curfunc->extra;
  VReg *offset = fnbe->stack_work_size_vreg;
  if (offset == NULL) {
    // offset = new_const_vreg(0, to_vsize(&tySize));
    // Instead of above, create independent constant vreg.
    offset = reg_alloc_spawn_raw(to_vsize(&tySize), VRF_CONST);
    offset->fixnum = 0;
    fnbe->stack_work_size_vreg = offset;
  }
  new_ir_bop_raw(IR_ADD, result, result, offset, IRF_UNSIGNED);
  return result;
}

static void parse_builtins(Vector *decls) {
#define S(x)   S2(x)
#define S2(x)  #x

#if defined(USE_SYS_LD)
#define POPCOUNT_GENERIC \
    "static inline int __builtin_popcount(unsigned int x) {\n" \
    "  x -= (x >> 1) & 0x55555555U;\n" \
    "  x = (x & 0x33333333U) + ((x >> 2) & 0x33333333U);\n" \
    "  x = (x + (x >>  4)) & 0x0f0f0f0fU;\n" \
    "  x = (x + (x >>  8));\n" \
    "  x = (x + (x >> 16)) & 0x3fU;\n" \
    "  return x;\n" \
    "}\n" \
    "static inline int __builtin_popcountl(unsigned long x) {\n" \
    "  return __builtin_popcount(x);" /* Assume sizeof(long) == sizeof(int) */ \
    "}\n" \
    "static inline int __builtin_popcountll(unsigned long long x) {\n" \
    "  x -= (x >> 1) & 0x5555555555555555ULL;\n" \
    "  x = (x & 0x3333333333333333ULL) + ((x >> 2) & 0x3333333333333333ULL);\n" \
    "  x = (x + (x >>  4)) & 0x0f0f0f0f0f0f0f0fULL;\n" \
    "  x = (x + (x >>  8));\n" \
    "  x = (x + (x >> 16));\n" \
    "  x = (x + (x >> 32)) & 0x7fULL;\n" \
    "  return x;\n" \
    "}\n"
#else
#define POPCOUNT_GENERIC \
    "static inline int __builtin_popcount(unsigned int x) {\n" \
    "  extern int __popcount(unsigned int x);\n" \
    "  return __popcount(x);\n" \
    "}\n" \
    "static inline int __builtin_popcountl(unsigned long x) {\n" \
    "  extern int __popcount(unsigned int x);\n" \
    "  return __popcount(x);\n" /* Assume sizeof(long) == sizeof(int) */ \
    "}\n" \
    "static inline int __builtin_popcountll(unsigned long long x) {\n" \
    "  extern int __popcountll(unsigned long long x);\n" \
    "  return __popcountll(x);\n" \
    "}\n"
#endif

#if XCC_TARGET_ARCH == XCC_ARCH_X64

# define CLZ(T, postfix) \
    "static inline " S(T) " __builtin_clz" S(postfix) "(volatile register unsigned " S(T) " x) {\n" \
    "  " S(T) " result;\n" \
    "  __asm(" \
    "      \"  lzcnt %1, %0\\n\"" \
    "      : \"=r\"(result)" \
    "      : \"ri\"(x));\n" \
    "  return result;\n" \
    "}\n"

# define CTZ(T, postfix) \
    "static inline " S(T) " __builtin_ctz" S(postfix) "(volatile register unsigned " S(T) " x) {\n" \
    "  " S(T) " result;\n" \
    "  __asm(" \
    "      \"  tzcnt %1, %0\\n\"" \
    "      : \"=r\"(result)" \
    "      : \"ri\"(x));\n" \
    "  return result;\n" \
    "}\n"

# define POPCOUNT(T, postfix) \
    "static inline int __builtin_popcount" S(postfix) "(volatile register unsigned " S(T) " x) {\n" \
    "  " S(T) " result;\n" \
    "  __asm(" \
    "      \"  popcnt %1, %0\\n\"" \
    "      : \"=r\"(result)" \
    "      : \"r\"(x));\n" \
    "  return result;\n" \
    "}\n"

  static const char src[] =
    CLZ(int, )
    CLZ(long, l)
    CLZ(long long, ll)
    CTZ(int, )
    CTZ(long, l)
    CTZ(long long, ll)
    POPCOUNT(int, )
    POPCOUNT(long, l)
    POPCOUNT(long long, ll)
  ;
#elif XCC_TARGET_ARCH == XCC_ARCH_AARCH64

# define CLZ(T, postfix) \
    "static inline " S(T) " __builtin_clz" S(postfix) "(volatile register unsigned " S(T) " x) {\n" \
    "  " S(T) " result;\n" \
    "  __asm(" \
    "      \"  clz %0, %1\\n\"" \
    "      : \"=r\"(result)" \
    "      : \"r\"(x));\n" \
    "  return result;\n" \
    "}\n"

# define CTZ(T, postfix) \
    "static inline " S(T) " __builtin_ctz" S(postfix) "(volatile register unsigned " S(T) " x) {\n" \
    "  " S(T) " result;\n" \
    "  __asm(" \
    "      \"  rbit %0, %1\\n\"" \
    "      \"  clz %0, %0\\n\"" \
    "      : \"=r\"(result)" \
    "      : \"r\"(x));\n" \
    "  return result;\n" \
    "}\n"

  static const char src[] =
    CLZ(int, )
    CLZ(long, l)
    CLZ(long long, ll)
    CTZ(int, )
    CTZ(long, l)
    CTZ(long long, ll)
    POPCOUNT_GENERIC
  ;
#elif XCC_TARGET_ARCH == XCC_ARCH_RISCV64

# define CLZ(T, postfix, w) \
    "static inline int __builtin_clz" S(postfix) "(volatile register unsigned " S(T) " x) {\n" \
    "  " S(T) " result;\n" \
    "  __asm(" \
    "      \"  clz" S(w) " %0, %1\\n\""  /* Requires ISA `zbb` extension. */ \
    "      : \"=r\"(result)" \
    "      : \"r\"(x));\n" \
    "  return result;\n" \
    "}\n"

# define CTZ(T, postfix, w) \
    "static inline int __builtin_ctz" S(postfix) "(volatile register unsigned " S(T) " x) {\n" \
    "  " S(T) " result;\n" \
    "  __asm(" \
    "      \"  ctz" S(w) " %0, %1\\n\""  /* Requires ISA `zbb` extension. */ \
    "      : \"=r\"(result)" \
    "      : \"r\"(x));\n" \
    "  return result;\n" \
    "}\n"

# define POPCOUNT(T, postfix, w) \
    "static inline int __builtin_popcount" S(postfix) "(volatile register unsigned " S(T) " x) {\n" \
    "  " S(T) " result;\n" \
    "  __asm(" \
    "      \"  cpop" S(w) " %0, %1\\n\""  /* Requires ISA `zbb` extension. */ \
    "      : \"=r\"(result)" \
    "      : \"r\"(x));\n" \
    "  return result;\n" \
    "}\n"

  static const char src[] =
    CLZ(int, , w)
    CLZ(long, l, )
    CLZ(long long, ll, )
    CTZ(int, , w)
    CTZ(long, l, )
    CTZ(long long, ll, )
    POPCOUNT(int, , w)
    POPCOUNT(long, l, )
    POPCOUNT(long long, ll, )
  ;
#else
  UNUSED(decls);
  return;
#endif

  FILE *fp = fmemopen((void*)src, sizeof(src) - 1, "r");
  if (fp != NULL) {
    set_source_file(fp, "*builtins*");
    parse(decls);
    fclose(fp);
  }
#undef S
#undef S2
}

void install_builtins(Vector *decls) {
  parse_builtins(decls);

  static BuiltinExprProc p_function_name = &proc_builtin_function_name;
  add_builtin_expr_ident("__FUNCTION__", &p_function_name);
  add_builtin_expr_ident("__func__", &p_function_name);

  static BuiltinExprProc p_classify_type = &proc_builtin_classify_type;
  add_builtin_expr_ident("__builtin_classify_type", &p_classify_type);

#ifndef __NO_FLONUM
  static BuiltinExprProc p_nan = &proc_builtin_nan;
  add_builtin_expr_ident("__builtin_nan", &p_nan);
#endif

  {
#if VAARG_ON_STACK || XCC_TARGET_ARCH == XCC_ARCH_RISCV64
    Type *tyVaList = ptrof(&tyVoidPtr);
#else
    Type *tyVaElem = create_struct_type(NULL, alloc_name("__va_elem", NULL, false), 0);
    Type *tyVaList = ptrof(tyVaElem);
#endif
    static BuiltinFunctionProc p_va_start = &gen_builtin_va_start;
    Type *rettype = &tyVoid;
    Vector *params = new_vector();
    vec_push(params, tyVaList);
    // vec_push(params, &tyVoidPtr);
    Type *type = new_func_type(rettype, params, true);  // To accept any types, pretend the function as variadic.
    add_builtin_function("__builtin_va_start", type, &p_va_start, true);
  }
  {
    static BuiltinFunctionProc p_alloca = &gen_alloca;
    Type *rettype = &tyVoidPtr;
    Vector *params = new_vector();
    vec_push(params, &tySize);
    Type *type = new_func_type(rettype, params, false);
    add_builtin_function("alloca", type, &p_alloca, false);
  }
}
