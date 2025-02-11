#include "../../config.h"
#include "fe_misc.h"

#include <assert.h>
#include <stdarg.h>
#include <stdlib.h>  // exit
#include <string.h>

#include "ast.h"
#include "initializer.h"
#include "lexer.h"
#include "table.h"
#include "type.h"
#include "util.h"
#include "var.h"

#define MAX_ERROR_COUNT  (25)

Function *curfunc;
Scope *curscope;
VarInfo *curvarinfo;
LoopScope loop_scope;

int compile_warning_count;
int compile_error_count;

CcFlags cc_flags = {
  .warn_as_error = false,
  .common = false,
  .optimize_level = 0,
};

typedef struct {
  const char *flag_name;
  off_t flag_offset;
} FlagTable;

static bool parse_flag_table(const char *optarg, bool value, const FlagTable *table, size_t count) {
  for (size_t i = 0; i < count; ++i) {
    const FlagTable *p = &table[i];
    if (strcmp(optarg, p->flag_name) == 0) {
      size_t len = strlen(p->flag_name);
      if (optarg[len] != '\0')
        continue;
      bool *b = (bool*)((char*)&cc_flags + p->flag_offset);
      *b = value;
      return true;
    }
  }
  return false;
}

bool parse_fopt(const char *optarg, bool value) {
  static const FlagTable kFlagTable[] = {
    {"common", offsetof(CcFlags, common)},
  };
  return parse_flag_table(optarg, value, kFlagTable, ARRAY_SIZE(kFlagTable));
}

bool parse_wopt(const char *optarg, bool value) {
  static const FlagTable kFlagTable[] = {
    {"unused-variable", offsetof(CcFlags, warn.unused_variable)},
    {"unused-function", offsetof(CcFlags, warn.unused_function)},
  };
  return parse_flag_table(optarg, value, kFlagTable, ARRAY_SIZE(kFlagTable));
}

void parse_error(enum ParseErrorLevel level, const Token *token, const char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  if (fmt != NULL) {
    if (token == NULL)
      token = fetch_token();
    if (token->line != NULL) {
      fprintf(stderr, "%s(%d): ", token->line->filename, token->line->lineno);
    }

    if (level == PE_WARNING && !cc_flags.warn_as_error)
      fprintf(stderr, "warning: ");
    vfprintf(stderr, fmt, ap);
    fprintf(stderr, "\n");
  }

  if (token != NULL && token->line != NULL && token->begin != NULL)
    show_error_line(token->line->buf, token->begin, token->end - token->begin);
  va_end(ap);

  if (level == PE_WARNING) {
    ++compile_warning_count;
  } else {
    ++compile_error_count;
    if (level == PE_FATAL || compile_error_count >= MAX_ERROR_COUNT)
      exit(1);
  }
}

bool not_void(const Type *type, const Token *token) {
  if (type->kind != TY_VOID)
    return true;
  parse_error(PE_NOFATAL, token, "`void' not allowed");
  return false;
}

void not_const(const Type *type, const Token *token) {
  if (type->qualifier & TQ_CONST)
    parse_error(PE_NOFATAL, token, "Cannot modify `const'");
}

const enum FixnumKind kLongKinds[] = {
  FX_INT, FX_LONG, FX_LLONG,
};

void check_type_combination(const TypeCombination *tc, const Token *tok) {
  if (tc->unsigned_num > 1 || tc->signed_num > 1 ||
      tc->char_num > 1 || tc->short_num > 1 || tc->int_num > 1 ||
      tc->long_num >= (int)ARRAY_SIZE(kLongKinds) ||
      ((tc->char_num > 0) + (tc->short_num > 0) + (tc->long_num > 0) > 1) ||
      tc->float_num > 1 || tc->double_num > 1 ||
      ((tc->float_num > 0 || tc->double_num > 0) &&
       (tc->char_num > 0 || tc->short_num > 0 || tc->int_num > 0 || tc->long_num > 0 ||
        tc->unsigned_num > 0 || tc->signed_num > 0) &&
       !(tc->double_num == 1 && tc->float_num <= 0 && tc->long_num <= 1 &&
         tc->char_num <= 0 && tc->short_num <= 0 && tc->int_num <= 0 &&
         tc->unsigned_num <= 0 && tc->signed_num <= 0)
      )
  ) {
    parse_error(PE_NOFATAL, tok, "Illegal type combination");
  }
}

bool no_type_combination(const TypeCombination *tc, int storage_mask, int qualifier_mask) {
  return tc->unsigned_num == 0 && tc->signed_num == 0 &&
      tc->char_num == 0 && tc->short_num == 0 && tc->int_num == 0 && tc->long_num == 0 &&
      (tc->storage & storage_mask) == 0 && (tc->qualifier & qualifier_mask) == 0 &&
      tc->float_num == 0 && tc->double_num == 0;
}

VarInfo *find_var_from_scope(Scope *scope, const Token *ident, Type *type, int storage) {
  if (scope->vars != NULL) {
    assert(ident != NULL);
    const Name *name = ident->ident;
    assert(name != NULL);
    int idx = var_find(scope->vars, name);
    if (idx >= 0) {
      VarInfo *varinfo = scope->vars->data[idx];
      if (!same_type(type, varinfo->type)) {
        parse_error(PE_NOFATAL, ident, "`%.*s' type conflict", NAMES(name));
      } else if (!(storage & VS_EXTERN)) {
        if (varinfo->storage & VS_EXTERN)
          varinfo->storage &= ~VS_EXTERN;
        else if (is_global_scope(scope) && varinfo->global.init == NULL)
          ;  // Ignore variable duplication if predecessor doesn't have initializer.
        else
          parse_error(PE_NOFATAL, ident, "`%.*s' already defined", NAMES(name));
      }
      return varinfo;
    }
  }
  return NULL;
}

VarInfo *add_var_to_scope(Scope *scope, const Token *ident, Type *type, int storage) {
  VarInfo *varinfo = find_var_from_scope(scope, ident, type, storage);
  if (varinfo != NULL)
    return varinfo;

  // Check conflict with typedef
  if (scope->typedef_table != NULL && table_try_get(scope->typedef_table, ident->ident, NULL))
    parse_error(PE_NOFATAL, ident, "conflict with typedef");

  return scope_add(scope, ident, type, storage);
}

Token *alloc_dummy_ident(void) {
  const Name *label = alloc_label();
  return alloc_ident(label, NULL, label->chars, label->chars + label->bytes);
}

Expr *alloc_tmp_var(Scope *scope, Type *type) {
  const Token *ident = alloc_dummy_ident();
  // No need to use `add_var_to_scope`, because `name` must be unique.
  const Name *name = ident->ident;
  scope_add(scope, ident, type, VS_USED);
  return new_expr_variable(name, type, ident, scope);
}

void define_enum_member(Type *type, const Token *ident, int value) {
  VarInfo *varinfo = add_var_to_scope(curscope, ident, type, VS_ENUM_MEMBER);
  varinfo->enum_member.value = value;
}

Expr *string_expr(const Token *token, const char *str, ssize_t len, enum StrKind kind) {
  enum FixnumKind fxkind = FX_CHAR;
  bool is_unsigned = false;
#ifndef __NO_WCHAR
  switch (kind) {
  case STR_CHAR:  break;
  case STR_WIDE:  fxkind = FX_INT; is_unsigned = true; break;  // TODO: Match with wchar_t.
  }
#endif
  Type *ctype = get_fixnum_type(fxkind, is_unsigned, 0);  // not const.
  Type *type = arrayof(ctype, len);
  type->qualifier = TQ_CONST;

  Expr *expr = new_expr(EX_STR, type, token);
  expr->str.buf = str;
  expr->str.len = len;
  expr->str.kind = kind;
  return expr;
}

Expr *proc_builtin_function_name(const Token *tok) {
  if (curfunc == NULL) {
    parse_error(PE_NOFATAL, tok, "must be inside function");
    static const char nulstr[] = "";
    return string_expr(tok, nulstr, 0, STR_CHAR);
  }

  // Make nul-terminated function name.
  const Name *name = curfunc->ident->ident;
  size_t len = name->bytes;
  char *str = malloc_or_die(len + 1);
  memcpy(str, name->chars, len);
  str[len] = '\0';
  return string_expr(tok, str, len + 1, STR_CHAR);
}

Scope *enter_scope(Function *func) {
  Scope *scope = new_scope(curscope);
  curscope = scope;
  vec_push(func->scopes, scope);
  return scope;
}

void exit_scope(void) {
  assert(!is_global_scope(curscope));
  curscope = curscope->parent;
}

// Call before accessing struct member to ensure that struct is declared.
bool ensure_struct(Type *type, const Token *token, Scope *scope) {
  switch (type->kind) {
  case TY_STRUCT:
    {
      if (type->struct_.info == NULL) {
        StructInfo *sinfo = find_struct(scope, type->struct_.name, NULL);
        if (sinfo == NULL) {
          parse_error(PE_NOFATAL, token, "Imcomplete struct: `%.*s'", NAMES(type->struct_.name));
          return false;
        }
        type->struct_.info = sinfo;
      }

      // Recursively.
      StructInfo *sinfo = type->struct_.info;
      for (int i = 0; i < sinfo->member_count; ++i) {
        MemberInfo *minfo = &sinfo->members[i];
        if (minfo->type->kind == TY_STRUCT &&
            !ensure_struct(minfo->type, token, scope))
          return false;
      }
    }
    break;
  case TY_ARRAY:
    return ensure_struct(type->pa.ptrof, token, scope);
  default:
    break;
  }
  return true;
}

Expr *calc_type_size(const Type *type) {
#ifndef __NO_VLA
  if (ptr_or_array(type) && type->pa.vla != NULL) {
    assert(type->pa.size_var != NULL);
    return type->pa.size_var;
  }
#endif
  return new_expr_fixlit(&tySize, NULL, type_size(type));
}

#ifndef __NO_VLA
Expr *reserve_vla_type_size(Type *type) {
  assert(!is_global_scope(curscope));
  if (!ptr_or_array(type))
    return NULL;

  // If VLA is nested, calculate subtype first.
  Type *subtype = type->pa.ptrof;
  Expr *sub = reserve_vla_type_size(subtype);
  if (type->pa.vla == NULL)
    return sub;

  const Token *token = type->pa.vla->token;
  Expr *var = alloc_tmp_var(curscope, &tySize);
  Expr *value = new_expr_num_bop(EX_MUL, token, make_cast(&tySize, token, type->pa.vla, false),
                                 calc_type_size(type->pa.ptrof));
  Expr *assign = new_expr_bop(EX_ASSIGN, &tySize, token, var, value);
  type->pa.size_var = var;

  if (sub != NULL)
    assign = new_expr_bop(EX_COMMA, &tySize, assign->token, sub, assign);
  return assign;
}

Expr *calc_vla_size(Type *type) {
  for (;;) {
    if (ptr_or_array(type) && type->pa.vla != NULL) {
      if (type->pa.size_var != NULL)
        break;
      return reserve_vla_type_size(type);
    }

    if (type->kind != TY_PTR)
      break;
    type = type->pa.ptrof;
  }
  return NULL;
}
#endif

bool check_cast(const Type *dst, const Type *src, bool zero, bool is_explicit, const Token *token) {
  bool ok = can_cast(dst, src, zero, is_explicit);
  if (!ok || dst->kind == TY_ARRAY) {
    if (token == NULL)
      token = fetch_token();
    fprintf(stderr, "%s(%d): ", token->line->filename, token->line->lineno);

    enum ParseErrorLevel level = PE_WARNING;
    if (dst->kind == TY_ARRAY || !is_prim_type(dst) ||
        !(is_prim_type(src) || (src->kind == TY_ARRAY && dst->kind == TY_PTR)))
      level = PE_NOFATAL;
    else if (!cc_flags.warn_as_error)
      fprintf(stderr, "warning: ");
    fprintf(stderr, "convert value from type `");
    print_type(stderr, src);
    fprintf(stderr, "' to %s`", dst->kind == TY_ARRAY ? "array type " : "");
    print_type(stderr, dst);
    fprintf(stderr, "'\n");
    parse_error(level, token, NULL);
    return false;
  }
  return true;
}

Expr *make_cast(Type *type, const Token *token, Expr *sub, bool is_explicit) {
  check_cast(type, sub->type, is_zero(sub), is_explicit, token);
  if (same_type(type, sub->type)) {
    sub->type = type;
    return sub;
  }

  if (sub->kind == EX_FIXNUM || sub->kind == EX_FLONUM) {
#ifndef __NO_FLONUM
    switch (sub->kind) {
    case EX_FLONUM:
      if (type->kind == TY_FIXNUM) {
        Fixnum fixnum;
        if (is_bool(type))
          fixnum = sub->flonum != 0;
        else
          fixnum = wrap_value(sub->flonum, type_size(type), type->fixnum.is_unsigned);
        return new_expr_fixlit(type, sub->token, fixnum);
      }
      assert(type->kind == TY_FLONUM);
      sub->type = type;
      return sub;
    case EX_FIXNUM:
      if (type->kind == TY_FLONUM) {
        Flonum flonum = (sub->type->kind != TY_FIXNUM || sub->type->fixnum.is_unsigned)
                            ? (Flonum)(UFixnum)sub->fixnum
                            : (Flonum)sub->fixnum;
        return new_expr_flolit(type, sub->token, flonum);
      }
      break;
    default:
      break;
    }
#endif

    assert(sub->kind == EX_FIXNUM);
    assert(!is_flonum(type));
    Fixnum value;
    if (is_bool(type))
      value = sub->fixnum != 0;
    else
      value = wrap_value(sub->fixnum, type_size(type), type->fixnum.is_unsigned);
    sub->fixnum = value;
    sub->type = type;
    return sub;
  }

  if (is_bool(type))
    return make_cond(sub);

#if XCC_TARGET_ARCH == XCC_ARCH_X64 && !defined(__NO_FLONUM)
  // On x64, cannot cast from double to uint64_t directly.
  size_t dst_size = type_size(type);
  if (is_flonum(sub->type) &&
      is_fixnum(type->kind) && type->fixnum.is_unsigned && dst_size >= 8) {
    // Transform from (uint64_t)flonum
    //   to: (flonum <= INT64_MAX) ? (int64_t)flonum
    //                             : ((int64_t)(flonum - (INT64_MAX + 1UL)) ^ (1L << 63))
    Type *i64t = get_fixnum_type_from_size(dst_size);
    Expr *cond = new_expr_bop(EX_LE, &tyBool, token, sub,
                              new_expr_flolit(sub->type, sub->token, INT64_MAX));
    Expr *offsetted = new_expr_addsub(
        EX_SUB, token, sub,
        new_expr_flolit(sub->type, sub->token, (uint64_t)INT64_MAX + 1UL));
    Expr *xorred = new_expr_bop(EX_BITXOR, i64t, token, make_cast(i64t, token, offsetted, false),
                                new_expr_fixlit(i64t, token, (uint64_t)1 << 63));
    sub = new_expr_ternary(token, cond, make_cast(i64t, token, sub, false), xorred, i64t);
  }
#endif

  return new_expr_cast(type, token, sub);
}

const MemberInfo *search_from_anonymous(const Type *type, const Name *name, const Token *ident,
                                        Vector *stack) {
  assert(type->kind == TY_STRUCT);
  const StructInfo *sinfo = type->struct_.info;
  for (int i = 0, len = sinfo->member_count; i < len; ++i) {
    const MemberInfo *member = &sinfo->members[i];
    if (member->name != NULL) {
      if (equal_name(member->name, name)) {
        vec_push(stack, INT2VOIDP(i));
        return member;
      }
    } else if (member->type->kind == TY_STRUCT) {
      vec_push(stack, INT2VOIDP(i));
      const MemberInfo *submember = search_from_anonymous(member->type, name, ident, stack);
      if (submember != NULL)
        return submember;
      vec_pop(stack);
    }
  }
  return NULL;
}

static bool cast_numbers(Expr **pLhs, Expr **pRhs, bool make_int) {
  Expr *lhs = *pLhs;
  Expr *rhs = *pRhs;
  Type *ltype = lhs->type;
  Type *rtype = rhs->type;
  assert(ltype != NULL);
  assert(rtype != NULL);
  if (!is_number(ltype)) {
    parse_error(PE_FATAL, lhs->token, "number type expected");
    return false;
  }
  if (!is_number(rtype)) {
    parse_error(PE_FATAL, rhs->token, "number type expected");
    return false;
  }

  {
    bool lflo = is_flonum(ltype), rflo = is_flonum(rtype);
    if (lflo || rflo) {
      int dir = !lflo ? 1 : !rflo ? -1 : (int)rtype->flonum.kind - (int)ltype->flonum.kind;
      if (dir < 0)
        *pRhs = make_cast(ltype, rhs->token, rhs, false);
      else if (dir > 0)
        *pLhs = make_cast(rtype, lhs->token, lhs, false);
      return true;
    }
  }
  enum FixnumKind lkind = ltype->fixnum.kind;
  enum FixnumKind rkind = rtype->fixnum.kind;
  bool changed = false;
  if (ltype->fixnum.kind >= FX_ENUM) {
    ltype = &tyInt;
    lkind = FX_INT;
    changed = true;
  }
  if (rtype->fixnum.kind >= FX_ENUM) {
    rtype = &tyInt;
    rkind = FX_INT;
    changed = true;
  }

  if (make_int && lkind < FX_INT && rkind < FX_INT) {
    *pLhs = promote_to_int(lhs);
    *pRhs = promote_to_int(rhs);
  } else if (changed || !same_type_without_qualifier(ltype, rtype, true)) {
    int l = (type_size(ltype) << 1) | (ltype->fixnum.is_unsigned ? 1 : 0);
    int r = (type_size(rtype) << 1) | (rtype->fixnum.is_unsigned ? 1 : 0);
    Type *type = l >= r ? ltype : rtype;
    *pLhs = make_cast(type, lhs->token, lhs, false);
    *pRhs = make_cast(type, rhs->token, rhs, false);
  }
  return true;
}

static void mark_var_used_sub(Expr *expr, bool for_func) {
  VarInfo *gvarinfo = NULL;

  switch (expr->kind) {
  case EX_VAR:
    {
      Scope *scope = NULL;
      VarInfo *varinfo = scope_find(expr->var.scope, expr->var.name, &scope);
      assert(varinfo != NULL);
      if (is_global_scope(scope)) {
        gvarinfo = varinfo;

        const Type *type = varinfo->type;
        if (type->kind == TY_FUNC && !for_func) {
          varinfo->storage |= VS_REF_TAKEN;
        }
      } else {
        varinfo->storage |= VS_USED;
        if (varinfo->storage & VS_STATIC)
          gvarinfo = varinfo->static_.svar;
      }
    }
    break;
  case EX_COMPLIT:
    mark_var_used(expr->complit.var);
    break;
  case EX_ASSIGN:
    mark_var_used(expr->bop.lhs);
    break;
  default: break;
  }

  if (gvarinfo != NULL && curvarinfo != NULL) {
    Vector *refs = curvarinfo->global.referred_globals;
    if (refs == NULL)
      curvarinfo->global.referred_globals = refs = new_vector();
    vec_push(refs, gvarinfo);
  }
}

void mark_var_used(Expr *expr) {
  mark_var_used_sub(expr, false);
}

void mark_var_used_for_func(Expr *expr) {
  mark_var_used_sub(expr, true);
}

void propagate_var_used(void) {
  Table used, unused;
  table_init(&used);
  table_init(&unused);
  Vector unchecked;
  vec_init(&unchecked);

  const Name *constructor_name = alloc_name("constructor", NULL, false);
  const Name *destructor_name = alloc_name("destructor", NULL, false);

  // Collect public functions into unchecked.
  for (int i = 0; i < global_scope->vars->len; ++i) {
    VarInfo *varinfo = global_scope->vars->data[i];
    const Type *type = varinfo->type;
    if (type->kind == TY_FUNC) {
      Function *func = varinfo->global.func;
      if (func == NULL)  // Prototype definition
        continue;
      if (((varinfo->storage & VS_STATIC) ||
          (varinfo->storage & (VS_INLINE | VS_EXTERN)) == VS_INLINE) &&
          (func->attributes == NULL ||
           (!table_try_get(func->attributes, constructor_name, NULL) &&
            !table_try_get(func->attributes, destructor_name, NULL)))) {
        if (!(varinfo->storage & VS_INLINE))
          table_put(&unused, varinfo->ident->ident, varinfo);
        continue;
      }
    } else {
      if (varinfo->storage & (VS_STATIC | VS_EXTERN | VS_ENUM_MEMBER)) {
        if (varinfo->storage & VS_STATIC)
          table_put(&unused, varinfo->ident->ident, varinfo);
        continue;
      }
    }
    vec_push(&unchecked, varinfo);
  }

  // Propagate usage.
  while (unchecked.len > 0) {
    VarInfo *varinfo = vec_pop(&unchecked);
    if (table_try_get(&used, varinfo->ident->ident, NULL))
      continue;
    table_put(&used, varinfo->ident->ident, NULL);
    table_delete(&unused, varinfo->ident->ident);
    varinfo->storage |= VS_USED;

    Vector *refs = varinfo->global.referred_globals;
    if (refs == NULL)
      continue;
    for (int j = 0; j < refs->len; ++j) {
      VarInfo *ref = refs->data[j];
      vec_push(&unchecked, ref);
    }
  }

  const Name *name;
  VarInfo *varinfo;
  for (int it = 0; (it = table_iterate(&unused, it, &name, (void**)&varinfo)) != -1; ) {
    if (varinfo->type->kind == TY_FUNC) {
      if (cc_flags.warn.unused_function)
        parse_error(PE_WARNING, varinfo->ident, "Unused function: `%.*s'", NAMES(name));
    } else {
      if (cc_flags.warn.unused_variable)
        parse_error(PE_WARNING, varinfo->ident, "Unused variable: `%.*s'", NAMES(name));
    }
  }
}

void check_lval(const Token *tok, Expr *expr, const char *error) {
  switch (expr->kind) {
  case EX_VAR:
  case EX_DEREF:
  case EX_MEMBER:
    break;
  default:
    parse_error(PE_NOFATAL, tok, error);
    break;
  }
}

static void check_referable(const Token *tok, Expr *expr, const char *error) {
  if (expr->kind == EX_COMPLIT)
    return;
  check_lval(tok, expr, error);
}

static Expr *reduce_refer_deref_add(Expr *expr, Type *subtype, Expr *lhs, Fixnum rhs) {
  if (lhs->kind == EX_FIXNUM) {
    return new_expr_unary(EX_DEREF, expr->type, expr->token,
                          new_expr_fixlit(subtype, lhs->token, lhs->fixnum + rhs));
  } else if (lhs->kind == EX_DEREF && lhs->unary.sub->kind == EX_FIXNUM) {
    // Concat 2 deref.
    Expr *sub = lhs->unary.sub;
    return new_expr_unary(EX_DEREF, expr->type, expr->token,
                          new_expr_fixlit(sub->type, sub->token, sub->fixnum + rhs));
  } else if (lhs->kind == EX_ADD && lhs->bop.rhs->kind == EX_FIXNUM) {
    // *(((lhs->lhs) + (lhs->rhs)) + rhs) => *(lhs->lhs + (lhs->rhs + rhs))
    return new_expr_unary(EX_DEREF, expr->type, expr->token,
                          new_expr_bop(EX_ADD, subtype, lhs->token, lhs->bop.lhs,
                                       new_expr_fixlit(lhs->bop.rhs->type, lhs->bop.rhs->token,
                                                       lhs->bop.rhs->fixnum + rhs)));
  }
  return NULL;
}

Expr *reduce_refer(Expr *expr) {
  // target->field => *(target + offset(field))
  switch (expr->kind) {
  case EX_MEMBER:
    {
      Expr *target = reduce_refer(expr->member.target);
      if (target->type->kind == TY_STRUCT && target->kind == EX_DEREF) {
        // (*sub).field => sub->field
        target = target->unary.sub;
      }

      const MemberInfo *minfo = expr->member.info;
      Type *ptype = minfo->type;
      ptype = ptrof(ptype->kind == TY_ARRAY ? array_to_ptr(ptype) : ptype);
      // target->field => *(target + offset(field))
      Expr *result = reduce_refer_deref_add(expr, ptype, target, minfo->offset);
      if (result != NULL)
        return result;

      // Transform member access to pointer dereference, only if target is referenceable.
      // target->field => *(target + offset(field))
      switch (target->kind) {
      case EX_VAR:
      case EX_DEREF:
        if (target->type->kind == TY_STRUCT) {
          // target.field => (&target)->field
          target = new_expr_unary(EX_REF, ptrof(target->type), target->token, target);
        }
        return new_expr_unary(EX_DEREF, minfo->type, expr->token,
                              new_expr_bop(EX_ADD, ptrof(minfo->type), expr->token, target,
                                           new_expr_fixlit(&tySize, expr->token, minfo->offset)));
      default:
        // ex. funcall().x cannot be taken its reference, so keep the expression.
        break;
      }
    }
    break;
  case EX_DEREF:
    {
      Expr *sub = expr->unary.sub;
      if (sub->kind == EX_ADD) {
        // *(lhs + rhs) => *lhs + rhs
        Expr *lhs, *rhs;
        if (sub->bop.lhs->kind != EX_FIXNUM) {
          lhs = sub->bop.lhs;
          rhs = sub->bop.rhs;
        } else {
          lhs = sub->bop.rhs;
          rhs = sub->bop.lhs;
        }
        Expr *lhs2 = reduce_refer(lhs);
        if (rhs->kind == EX_FIXNUM) {
          Expr *result = reduce_refer_deref_add(expr, sub->type, lhs2, rhs->fixnum);
          if (result != NULL)
            return result;
        }
        if (lhs2 != lhs)
          return new_expr_unary(EX_DEREF, expr->type, expr->token,
                                new_expr_bop(EX_ADD, sub->type, sub->token, lhs2, rhs));
      }
    }
    break;
  default: break;
  }
  return expr;
}

Expr *make_refer(const Token *tok, Expr *expr) {
  check_referable(tok, expr, "Cannot take reference");

  expr = reduce_refer(expr);

  if (expr->kind == EX_DEREF)
    return expr->unary.sub;
  Expr *e = expr;
  if (e->kind == EX_COMPLIT)
    e = e->complit.var;
  if (e->kind == EX_VAR) {
    VarInfo *varinfo = scope_find(e->var.scope, e->var.name, NULL);
    if (varinfo == NULL) {
      // Variable undeclared error must be raised already, so ignore here.
    } else {
      varinfo->storage |= VS_REF_TAKEN;
      if (varinfo->storage & VS_STATIC && !is_global_scope(e->var.scope)) {
        VarInfo *gvarinfo = varinfo->static_.svar;
        gvarinfo->storage |= VS_REF_TAKEN;
      }
    }
  }
  return new_expr_unary(EX_REF, ptrof(expr->type), tok, expr);
}

Expr *promote_to_int(Expr *expr) {
  assert(expr->type->kind == TY_FIXNUM);
  enum FixnumKind kind = expr->type->fixnum.kind;
  if (kind >= FX_INT && kind <= FX_LLONG)
    return expr;
  return make_cast(&tyInt, expr->token, expr, false);
}

Expr *new_expr_num_bop(enum ExprKind kind, const Token *tok, Expr *lhs, Expr *rhs) {
  cast_numbers(&lhs, &rhs, true);

  do {
    if (is_const(rhs) && is_number(rhs->type)) {
      if (is_const(lhs) && is_number(lhs->type)) {
#ifndef __NO_FLONUM
        if (is_flonum(lhs->type)) {
          assert(is_flonum(rhs->type));
          Flonum lval = lhs->flonum;
          Flonum rval = rhs->flonum;
          Flonum value;
          switch (kind) {
          case EX_MUL:     value = lval * rval; break;
          case EX_DIV:     value = lval / rval; break;
          default:
            assert(!"err");
            value = -1;  // Dummy
            break;
          }
          Type *type = lhs->type;
          if (is_flonum(rhs->type))
            type = rhs->type;
          if (is_flonum(type)) {
            return new_expr_flolit(type, lhs->token, value);
          } else {
            Fixnum fixnum = value;
            return new_expr_fixlit(type, lhs->token, fixnum);
          }
        }
#endif

        if ((kind == EX_DIV || kind == EX_MOD) && rhs->fixnum == 0)
          break;

#define CALC(kind, lval, rval, value) \
  switch (kind) { \
  default: assert(false); /* Fallthrough */ \
  case EX_MUL:     value = lval * rval; break; \
  case EX_DIV:     value = lval / rval; break; \
  case EX_MOD:     value = lval % rval; break; \
  case EX_BITAND:  value = lval & rval; break; \
  case EX_BITOR:   value = lval | rval; break; \
  case EX_BITXOR:  value = lval ^ rval; break; \
  }

        Fixnum value;
        if (lhs->type->fixnum.is_unsigned) {
          UFixnum lval = lhs->fixnum;
          UFixnum rval = rhs->fixnum;
          CALC(kind, lval, rval, value)
        } else {
          Fixnum lval = lhs->fixnum;
          Fixnum rval = rhs->fixnum;
          CALC(kind, lval, rval, value)
        }
#undef CALC
        Type *type = lhs->type->fixnum.kind >= rhs->type->fixnum.kind ? lhs->type : rhs->type;
        if (type->fixnum.kind < FX_INT)
          type = &tyInt;
        value = wrap_value(value, type_size(type), type->fixnum.is_unsigned);
        return new_expr_fixlit(type, lhs->token, value);
      } else {
#ifndef __NO_FLONUM
        if (is_flonum(rhs->type)) {
          assert(is_flonum(lhs->type));
          Flonum rval = rhs->flonum;
          switch (kind) {
          case EX_MUL:
            if (rval == 0.0)
              return new_expr_bop(EX_COMMA, rhs->type, tok, lhs, rhs);  // 0.0
            if (rval == -1.0)
              return new_expr_unary(EX_NEG, lhs->type, lhs->token, lhs);  // -lhs
            // Fallthrough.
          case EX_DIV:
            if (rval == 1.0)
              return lhs;  // no effect.
            break;
          default: break;
          }
          break;
        }
#endif
        Fixnum rval = rhs->fixnum;
        switch (kind) {
        case EX_MUL:
          if (rval == 0)
            return new_expr_bop(EX_COMMA, rhs->type, tok, lhs, rhs);  // 0
          if (rval == -1)
            return new_expr_unary(EX_NEG, lhs->type, lhs->token, lhs);
          // Fallthrough.
        case EX_DIV:
          if (rval == 1)
            return lhs;  // no effect.
          break;
        case EX_BITAND:
          if (rval == 0)
            return new_expr_bop(EX_COMMA, rhs->type, tok, lhs, rhs);  // 0
          break;
        case EX_BITOR:
        case EX_BITXOR:
          if (rval == 0)
            return lhs;  // no effect.
          break;
        default: break;
        }
      }
    } else {
      if (is_const(lhs) && is_number(lhs->type)) {
#ifndef __NO_FLONUM
        if (is_flonum(lhs->type)) {
          assert(is_flonum(rhs->type));
          Flonum lval = lhs->flonum;
          switch (kind) {
          case EX_MUL:
            if (lval == 0.0)
              return new_expr_bop(EX_COMMA, lhs->type, tok, rhs, lhs);  // 0.0
            if (lval == 1.0)
              return rhs;  // no effect.
            if (lval == -1.0)
              return new_expr_unary(EX_NEG, rhs->type, rhs->token, rhs);  // -rhs
            break;
          default: break;
          }
          break;
        }
#endif
        Fixnum lval = rhs->fixnum;
        switch (kind) {
        case EX_MUL:
          if (lval == 0)
            return new_expr_bop(EX_COMMA, lhs->type, tok, rhs, lhs);  // 0
          if (lval == -1)
            return new_expr_unary(EX_NEG, rhs->type, rhs->token, rhs);  // -rhs
          // Fallthrough.
        case EX_DIV:
          if (lval == 1)
            return rhs;  // no effect.
          break;
        case EX_BITAND:
          if (lval == 0)
            return new_expr_bop(EX_COMMA, lhs->type, tok, rhs, lhs);  // 0
          break;
        case EX_BITOR:
        case EX_BITXOR:
          if (lval == 0)
            return rhs;  // no effect.
          break;
        default: break;
        }
      }
    }
  } while (0);

  if ((kind == EX_DIV || kind == EX_MOD) && is_const(rhs) &&
      is_fixnum(rhs->type->kind) && rhs->fixnum == 0) {
    parse_error(PE_WARNING, rhs->token, "Divide by 0");
  }

  return new_expr_bop(kind, lhs->type, tok, lhs, rhs);
}

Expr *new_expr_int_bop(enum ExprKind kind, const Token *tok, Expr *lhs, Expr *rhs) {
  if (!is_fixnum(lhs->type->kind))
    parse_error(PE_FATAL, lhs->token, "int type expected");
  if (!is_fixnum(rhs->type->kind))
    parse_error(PE_FATAL, rhs->token, "int type expected");
  return new_expr_num_bop(kind, tok, lhs, rhs);
}

Expr *new_expr_addsub(enum ExprKind kind, const Token *tok, Expr *lhs, Expr *rhs) {
  lhs = str_to_char_array_var(curscope, lhs);
  rhs = str_to_char_array_var(curscope, rhs);

  Type *type = NULL;
  Type *ltype = lhs->type;
  Type *rtype = rhs->type;
  assert(ltype != NULL);
  assert(rtype != NULL);
  if (is_number(ltype) && is_number(rtype)) {
    if (is_const(lhs) && is_const(rhs)) {
#ifndef __NO_FLONUM
      if (is_flonum(lhs->type) || is_flonum(rhs->type)) {
        Flonum lval = is_flonum(lhs->type) ? lhs->flonum : lhs->fixnum;
        Flonum rval = is_flonum(rhs->type) ? rhs->flonum : rhs->fixnum;
        Flonum value;
        switch (kind) {
        case EX_ADD:     value = lval + rval; break;
        case EX_SUB:     value = lval - rval; break;
        default:
          assert(!"err");
          value = -1;  // Dummy
          break;
        }
        Type *type = lhs->type;
        if (is_flonum(rhs->type))
          type = rhs->type;
        if (is_flonum(type)) {
          return new_expr_flolit(type, lhs->token, value);
        } else {
          Fixnum fixnum = value;
          return new_expr_fixlit(type, lhs->token, fixnum);
        }
      }
#endif
      enum FixnumKind lnt = ltype->fixnum.kind;
      enum FixnumKind rnt = rtype->fixnum.kind;
      if (lnt >= FX_ENUM) {
        ltype = &tyInt;
        lnt = FX_INT;
      }
      if (rnt >= FX_ENUM) {
        rtype = &tyInt;
        rnt = FX_INT;
      }

      Fixnum lval = lhs->fixnum;
      Fixnum rval = rhs->fixnum;
      Fixnum value;
      switch (kind) {
      case EX_ADD: value = lval + rval; break;
      case EX_SUB: value = lval - rval; break;
      default:
        assert(false);
        value = -1;
        break;
      }
      Type *type = lnt >= rnt ? ltype : rtype;
      if (type->fixnum.kind < FX_INT)
        type = &tyInt;
      return new_expr_fixlit(type, lhs->token,
                             wrap_value(value, type_size(type), type->fixnum.is_unsigned));
    }

    cast_numbers(&lhs, &rhs, true);
    type = lhs->type;
  } else if (ptr_or_array(ltype)) {
    if (is_fixnum(rtype->kind)) {
      type = ltype;
      if (ltype->kind == TY_ARRAY)
        type = array_to_ptr(ltype);
      // lhs + ((size_t)rhs * sizeof(*lhs))
      if (!ensure_struct(type->pa.ptrof, tok, curscope))
        return lhs;
      rhs = new_expr_num_bop(EX_MUL, rhs->token,
                             make_cast(&tySize, rhs->token, rhs, false),
                             calc_type_size(type->pa.ptrof));
    } else if (kind == EX_SUB && ptr_or_array(rtype)) {
      if (ltype->kind == TY_ARRAY)
        ltype = array_to_ptr(ltype);
      if (rtype->kind == TY_ARRAY)
        rtype = array_to_ptr(rtype);
      if (!same_type_without_qualifier(ltype, rtype, true))
        parse_error(PE_FATAL, tok, "Different pointer diff");
      if (is_void_ptr(ltype)) {
        // void* - void*
        parse_error(PE_WARNING, tok, "Pointer subtraction of void*");
        ltype = rtype = ptrof(&tyChar);
        lhs = new_expr_cast(ltype, lhs->token, lhs);
        rhs = new_expr_cast(rtype, rhs->token, rhs);
      }
      // ((size_t)lhs - (size_t)rhs) / sizeof(*lhs)
      if (!ensure_struct(ltype->pa.ptrof, tok, curscope))
        return lhs;
      if (is_const(lhs) && is_const(rhs)) {
        assert(lhs->kind == EX_FIXNUM);
        assert(rhs->kind == EX_FIXNUM);
        return new_expr_fixlit(&tySize, tok,
                               (lhs->fixnum - rhs->fixnum) / type_size(ltype->pa.ptrof));
      }
      return new_expr_bop(EX_DIV, &tySSize, tok,
                          make_cast(&tySSize, tok,
                                    new_expr_bop(EX_SUB, &tySize, tok, lhs, rhs), false),
                          new_expr_fixlit(&tySSize, tok, type_size(ltype->pa.ptrof)));
    }
  } else if (ptr_or_array(rtype)) {
    if (kind == EX_ADD && is_fixnum(ltype->kind)) {
      type = rhs->type;
      if (type->kind == TY_ARRAY)
        type = array_to_ptr(type);
      // ((size_t)lhs * sizeof(*rhs)) + rhs
      if (!ensure_struct(type->pa.ptrof, tok, curscope))
        return rhs;
      Expr *tmp = new_expr_num_bop(EX_MUL, lhs->token,
                                   make_cast(&tySize, lhs->token, lhs, false),
                                   new_expr_fixlit(&tySize, tok, type_size(type->pa.ptrof)));
      lhs = rhs;
      rhs = tmp;
      Type *t = ltype;
      ltype = rtype;
      rtype = t;
    }
  }
  if (type == NULL) {
    parse_error(PE_NOFATAL, tok, "Cannot apply `%.*s'", (int)(tok->end - tok->begin), tok->begin);
    type = ltype;  // Dummy
  } else if (ptr_or_array(ltype) && is_const(lhs) && is_const(rhs)) {
    assert(lhs->kind == EX_FIXNUM);
    if (kind == EX_ADD) {
      lhs->fixnum += rhs->fixnum;
    } else {
      assert(kind == EX_SUB);
      lhs->fixnum -= rhs->fixnum;
    }
    return lhs;
  }
  return new_expr_bop(kind, type, tok, lhs, rhs);
}

#ifndef __NO_BITFIELD
void not_bitfield_member(Expr *expr) {
  if (expr->kind == EX_MEMBER) {
    const MemberInfo *minfo = expr->member.info;
    if (minfo->bitfield.active)
      parse_error(PE_NOFATAL, expr->token, "cannot get size for bitfield");
  }
}

Expr *extract_bitfield_value(Expr *src, const MemberInfo *minfo) {
  Expr *tmp = src;
  Type *stype = src->type;
  assert(stype->kind == TY_FIXNUM);
  if (stype->fixnum.is_unsigned) {
    if (minfo->bitfield.position > 0)
      tmp = new_expr_bop(EX_RSHIFT, stype, tmp->token, tmp,
                         new_expr_fixlit(stype, tmp->token, minfo->bitfield.position));
    UFixnum mask = ((UFixnum)1 << minfo->bitfield.width) - 1;
    tmp = new_expr_bop(EX_BITAND, stype, tmp->token, tmp,
                       new_expr_fixlit(stype, tmp->token, mask));
  } else {
    int w = MAX(type_size(stype), MINREGSIZE) * TARGET_CHAR_BIT;
    int l = w - (minfo->bitfield.position + minfo->bitfield.width);
    if (l > 0)
      tmp = new_expr_bop(EX_LSHIFT, stype, tmp->token, tmp,
                         new_expr_fixlit(stype, tmp->token, l));
    if (minfo->bitfield.width < w)
      tmp = new_expr_bop(EX_RSHIFT, stype, tmp->token, tmp,
                         new_expr_fixlit(stype, tmp->token, w - minfo->bitfield.width));
  }

  Type *mtype = minfo->type;
  assert(mtype->kind == TY_FIXNUM);
  if (mtype->fixnum.is_unsigned && minfo->bitfield.width < (int)type_size(mtype) * TARGET_CHAR_BIT) {
    // Make signed type.
    mtype = get_fixnum_type(mtype->fixnum.kind, false, 0);
  }
  return make_cast(mtype, src->token, tmp, false);
}

Expr *assign_bitfield_member(const Token *tok, Expr *dst, Expr *src, Expr *val,
                             const MemberInfo *minfo) {
  Type *type = dst->type;
  Type *vtype = val->type;

  UFixnum mask = ((UFixnum)1 << minfo->bitfield.width) - 1;
  Expr *val_masked = new_expr_bop(EX_BITAND, vtype, tok, val, new_expr_fixlit(vtype, tok, mask));
  val_masked = make_cast(type, tok, val_masked, false);
  if (minfo->bitfield.position > 0)
    val_masked = new_expr_bop(EX_LSHIFT, type, tok, val_masked,
                              new_expr_fixlit(vtype, tok, minfo->bitfield.position));
  val_masked = make_cast(type, tok, val_masked, false);
  Expr *src_masked = new_expr_bop(EX_BITAND, type, tok, src,
                                  new_expr_fixlit(type, tok, ~(mask << minfo->bitfield.position)));
  return new_expr_bop(EX_ASSIGN, type, tok, dst,
                      new_expr_bop(EX_BITOR, type, tok, val_masked, src_masked));
}

Expr *assign_to_bitfield(const Token *tok, Expr *lhs, Expr *rhs, const MemberInfo *minfo) {
  // Transform expression to (ptr = &lhs, val = rhs, *ptr = (*ptr & ~(mask << bitpos)) | ((val & mask) << bitpos), val)

  Type *type = get_fixnum_type(minfo->bitfield.base_kind, minfo->type->fixnum.is_unsigned, 0);

  Type *ptype = ptrof(type);
  assert(!is_global_scope(curscope));
  Expr *ptr = alloc_tmp_var(curscope, ptype);
  Expr *ptr_assign = new_expr_bop(EX_ASSIGN, ptype, tok, ptr, make_refer(lhs->token, lhs));

  Type *vtype = rhs->type;
  Expr *val = alloc_tmp_var(curscope, vtype);
  Expr *val_assign = new_expr_bop(EX_ASSIGN, vtype, tok, val, rhs);

  Expr *dst = new_expr_unary(EX_DEREF, type, tok, ptr);
  Expr *assign = assign_bitfield_member(tok, dst, dst, val, minfo);
  return new_expr_bop(EX_COMMA, vtype, tok, ptr_assign,
                      new_expr_bop(EX_COMMA, vtype, tok, val_assign,
                                   new_expr_bop(EX_COMMA, vtype, tok, assign, val)));
}

static Expr *transform_incdec_of_bitfield(enum ExprKind kind, Expr *target, const Token *tok,
                                          const MemberInfo *minfo) {
  // ++target => (&ptr = &target, src = *ptr, val = ((src + (1 << bitpos)) >> bitpos), *ptr = (src & (mask << bitpos)) | ((val & mask) << bitpos), val)
  // target++ => (&ptr = &target, src = *ptr, val = src >> bitpos, *ptr = (src & (mask << bitpos)) | (((val + 1) & mask) << bitpos), val)

  Type *type = get_fixnum_type(minfo->bitfield.base_kind, target->type->fixnum.is_unsigned, 0);

  Type *ptype = ptrof(type);
  assert(!is_global_scope(curscope));
  Expr *ptr = alloc_tmp_var(curscope, ptype);
  Expr *ptr_assign = new_expr_bop(EX_ASSIGN, ptype, tok, ptr, make_refer(target->token, target));
  Expr *dst = new_expr_unary(EX_DEREF, type, tok, ptr);

  Expr *src = alloc_tmp_var(curscope, type);
  Expr *src_assign = new_expr_bop(EX_ASSIGN, type, tok, src, dst);

  Type *vtype = minfo->type;
  Expr *val = alloc_tmp_var(curscope, vtype);

  enum {
    INCDEC  = 1 << 0,
    PREPOST = 1 << 1,
  };
  int dec = (kind - EX_PREINC) & INCDEC;
  int post = (kind - EX_PREINC) & PREPOST;

  Expr *val_assign, *after;
  if (post) {
    Expr *before = extract_bitfield_value(src, minfo);
    val_assign = new_expr_bop(EX_ASSIGN, type, tok, val, before);
    after = new_expr_bop(!dec ? EX_ADD : EX_SUB, type, tok, before, new_expr_fixlit(type, NULL, 1));
  } else {
    Expr *tmp = extract_bitfield_value(
        new_expr_bop(!dec ? EX_ADD : EX_SUB, type, tok, src,
                     new_expr_fixlit(type, NULL, 1 << minfo->bitfield.position)),
        minfo);
    val_assign = new_expr_bop(EX_ASSIGN, type, tok, val, tmp);
    after = val;
  }
  Expr *store = assign_bitfield_member(tok, dst, src, after, minfo);

  return new_expr_bop(EX_COMMA, vtype, tok,
                      new_expr_bop(EX_COMMA, vtype, tok, ptr_assign,
                                   new_expr_bop(EX_COMMA, vtype, tok, src_assign,
                                                new_expr_bop(EX_COMMA, vtype, tok, val_assign, store))),
                      val);
}
#endif

Expr *incdec_of(enum ExprKind kind, Expr *target, const Token *tok) {
  check_referable(tok, target, "lvalue expected");
#ifndef __NO_BITFIELD
  if (target->kind == EX_MEMBER) {
    const MemberInfo *minfo = target->member.info;
    if (minfo->bitfield.width > 0)
      return transform_incdec_of_bitfield(kind, target, tok, minfo);
  }
#endif
  return new_expr_unary(kind, target->type, tok, target);
}

Expr *new_expr_cmp(enum ExprKind kind, const Token *tok, Expr *lhs, Expr *rhs) {
  if (lhs->type->kind == TY_FUNC)
    lhs = make_refer(lhs->token, lhs);
  if (rhs->type->kind == TY_FUNC)
    rhs = make_refer(rhs->token, rhs);

  // Adjust type for comparison.
  {
    Type *lt = lhs->type, *rt = rhs->type;
    if (is_number(lt) && is_number(rt)) {
      if (is_fixnum(lt->kind) && is_fixnum(rt->kind)) {
        if (lt->fixnum.kind < FX_INT)
          lhs = promote_to_int(lhs);
        if (rt->fixnum.kind < FX_INT)
          rhs = promote_to_int(rhs);
      }
      if (!cast_numbers(&lhs, &rhs, false))
        parse_error(PE_FATAL, tok, "Cannot compare except numbers");
    }
  }

  if (is_const(lhs) && is_const(rhs)) {
#define JUDGE(kind, tf, l, r)               \
  switch (kind) {                           \
  default: assert(false); /* Fallthrough */ \
  case EX_EQ: tf = l == r; break;           \
  case EX_NE: tf = l != r; break;           \
  case EX_LT: tf = l < r; break;            \
  case EX_LE: tf = l <= r; break;           \
  case EX_GE: tf = l >= r; break;           \
  case EX_GT: tf = l > r; break;            \
  }
    int tf = -1;
    switch (lhs->kind) {
    default:
      assert(false);
      // Fallthrough to suppress warning.
    case EX_FIXNUM:
      switch (rhs->kind) {
      case EX_FIXNUM:
        if (lhs->type->fixnum.is_unsigned || rhs->type->fixnum.is_unsigned) {
          UFixnum l = lhs->fixnum, r = rhs->fixnum;
          JUDGE(kind, tf, l, r);
        } else {
          Fixnum l = lhs->fixnum, r = rhs->fixnum;
          JUDGE(kind, tf, l, r);
        }
        break;
#ifndef __NO_FLONUM
      case EX_FLONUM: assert(false); break;
#endif
      case EX_STR:
        if (is_zero(lhs)) {
          switch (kind) {
          case EX_EQ: tf = false; break;
          case EX_NE: tf = true; break;
          default: break;
          }
        }
        break;
      default: break;
      }
      break;
#ifndef __NO_FLONUM
    case EX_FLONUM:
      switch (rhs->kind) {
      case EX_STR:
        break;
      case EX_FIXNUM: assert(false); break;
      case EX_FLONUM:
        {
          Flonum l = lhs->flonum;
          Flonum r;
          if (rhs->kind == EX_FLONUM) {
            r = rhs->flonum;
          } else if (rhs->kind == EX_FIXNUM) {
            r = rhs->type->fixnum.is_unsigned ? (Flonum)(UFixnum)rhs->fixnum : (Flonum)rhs->fixnum;
          } else {
            break;
          }
          JUDGE(kind, tf, l, r);
        }
        break;
      default: break;
      }
      break;
#endif
    case EX_STR:
      if (is_zero(rhs)) {
        switch (kind) {
        case EX_EQ: tf = false; break;
        case EX_NE: tf = true; break;
        default: break;
        }
      }
      break;
    }
    if (tf >= 0)
      return new_expr_fixlit(&tyBool, tok, tf);
#undef JUDGE
  }

  if ((kind == EX_EQ || kind == EX_NE) && (is_const(rhs) || is_const(lhs))) {
    Expr *v, *c;
    if (is_const(lhs)) {
      v = rhs;
      c = lhs;
    } else {
      v = lhs;
      c = rhs;
    }
    enum { NEVER = -1, UNKNOWN = -2 };
    int value = UNKNOWN;
    switch (c->kind) {
#ifndef __NO_FLONUM
    case EX_FLONUM:
      if (c->flonum == 0.0)
        value = 0;
      else if (c->flonum == 1.0)
        value = 1;
      else
        value = NEVER;
      break;
#endif
    case EX_FIXNUM:
      value = c->fixnum == 0 || c->fixnum == 1 ? c->fixnum : NEVER;
      break;
    case EX_STR:
      value = NEVER;
      break;
    default:
      break;
    }

    Expr *p = strip_cast(v);
    int k = kind;
    switch (value) {
    case 1:
      // Swap condition that regard the value as 0.
      k = (EX_EQ + EX_NE) - k;  // EQ <-> NE
      // Fallthrough
    case 0:
      // Eliminate comparing comparison result with 0.
      switch (p->kind) {
      case EX_EQ: case EX_NE:
        if (k == EX_EQ)
          p->kind = (EX_EQ + EX_NE) - p->kind;  // EQ <-> NE
        return p;
      case EX_LT: case EX_LE: case EX_GE: case EX_GT:
        if (k == EX_EQ)
          p->kind = EX_LT + ((p->kind - EX_LT) ^ 2);  // LT <-> GE, LE <-> GT
        return p;
      case EX_LOGAND: case EX_LOGIOR:
        if (k == EX_EQ)
          p = new_expr_bop(
              (EX_LOGAND + EX_LOGIOR) - p->kind,  // LOGAND <-> LOGIOR
              &tyBool, p->token,
              make_not_expr(p->bop.lhs->token, p->bop.lhs),
              make_not_expr(p->bop.rhs->token, p->bop.rhs));
        return p;
      default: break;
      }
      break;
    case NEVER:
      switch (p->kind) {
      case EX_EQ: case EX_NE:
      case EX_LT: case EX_LE: case EX_GE: case EX_GT:
      case EX_LOGAND: case EX_LOGIOR:
        parse_error(PE_WARNING, tok, "Always %s", kind != EX_EQ ? "true" : "false");
        return new_expr_bop(EX_COMMA, &tyBool, tok, v, new_expr_fixlit(&tyBool, tok, (kind != EX_EQ)));
      default: break;
      }
      break;
    case UNKNOWN: default:
      break;
    }
  }

  lhs = str_to_char_array_var(curscope, lhs);
  rhs = str_to_char_array_var(curscope, rhs);

  Type *lt = lhs->type, *rt = rhs->type;
  if (ptr_or_array(lt) || ptr_or_array(rt)) {
    if (lt->kind == TY_ARRAY) {
      lt = array_to_ptr(lt);
      lhs = make_cast(lt, lhs->token, lhs, false);
    }
    if (rt->kind == TY_ARRAY) {
      rt = array_to_ptr(rt);
      rhs = make_cast(rt, rhs->token, rhs, false);
    }

    bool st;
    if (!((st = same_type_without_qualifier(lt, rt, true)) ||
          (lt->kind == TY_PTR && lt->pa.ptrof->kind == TY_VOID) ||
          (rt->kind == TY_PTR && rt->pa.ptrof->kind == TY_VOID) ||
          is_zero(rhs) || is_zero(lhs))) {
      enum ParseErrorLevel err = st || (lt->kind == TY_PTR && rt->kind == TY_PTR) ? PE_WARNING : PE_NOFATAL;
      parse_error(err, tok, "Compare pointer to other types");
    }
    if (rt->kind != TY_PTR)
      rhs = make_cast(lhs->type, rhs->token, rhs, false);
    else if (lt->kind != TY_PTR)
      lhs = make_cast(rhs->type, lhs->token, lhs, false);
  }

  return new_expr_bop(kind, &tyBool, tok, lhs, rhs);
}

//

Expr *make_cond(Expr *expr) {
  switch (expr->kind) {
  case EX_FIXNUM:
    expr = new_expr_fixlit(&tyBool, expr->token, expr->fixnum != 0);
    break;
#ifndef __NO_FLONUM
  case EX_FLONUM:
    expr = new_expr_fixlit(&tyBool, expr->token, expr->flonum != 0);
    break;
#endif
  case EX_STR:
    expr = new_expr_fixlit(&tyBool, expr->token, true);
    break;
  case EX_EQ:
  case EX_NE:
  case EX_LT:
  case EX_LE:
  case EX_GE:
  case EX_GT:
  case EX_LOGAND:
  case EX_LOGIOR:
    break;
  case EX_COMMA:
    expr->bop.rhs = make_cond(expr->bop.rhs);
    break;
  default:
    switch (expr->type->kind) {
    case TY_ARRAY:
    case TY_FUNC:
      expr = new_expr_fixlit(&tyBool, expr->token, true);
      break;
    default:
      expr = new_expr_cmp(
          EX_NE, expr->token, expr,
          make_cast(expr->type, expr->token, new_expr_fixlit(&tyInt, expr->token, 0), false));
      break;
    }
    break;
  }
  return expr;
}

Expr *make_not_expr(const Token *tok, Expr *expr) {
  Type *type = expr->type;
  assert(is_number(type) || ptr_or_array(type));
  if (type->kind == TY_ARRAY)
    type = array_to_ptr(type);
  Expr *zero;
#ifndef __NO_FLONUM
  if (is_flonum(type)) {
    zero = new_expr_flolit(type, tok, 0.0);
  } else
#endif
  {
    zero = new_expr_fixlit(type, tok, 0);
  }
  return new_expr_cmp(EX_EQ, tok, expr, zero);
}

void check_funcall_args(Expr *func, Vector *args, Scope *scope) {
  Type *functype = get_callee_type(func->type);
  if (functype == NULL)
    return;

  const Vector *types = functype->func.params;  // <Type*>
  bool vaargs = functype->func.vaargs;
  if (types != NULL) {
    int argc = args->len;
    int paramc = types->len;
    if (!(argc == paramc || (vaargs && argc >= paramc))) {
      parse_error(PE_NOFATAL, func->token, "function `%.*s' expect %d arguments, but %d",
                  NAMES(func->var.name), paramc, argc);
      return;
    }
  }

  int paramc = types != NULL ? types->len : 0;
  for (int i = 0, len = args->len; i < len; ++i) {
    Expr *arg = args->data[i];
    arg = str_to_char_array_var(scope, arg);
    if (arg->type->kind == TY_ARRAY)
      arg = make_cast(array_to_ptr(arg->type), arg->token, arg, false);
    if (i < paramc) {
      Type *type = types->data[i];
      if (!ensure_struct(type, arg->token, scope))
        continue;
      if (type->kind == TY_ARRAY)
        type = array_to_ptr(type);  // Needed for VLA.
      arg = make_cast(type, arg->token, arg, false);

      if (type->kind == TY_STRUCT) {
        assert(type->struct_.info != NULL);
        if (type->struct_.info->is_flexible)
          parse_error(PE_NOFATAL, arg->token, "flexible array as an argument not allowed");
      }
    } else if (vaargs && i >= paramc) {
      const Type *type = arg->type;
      switch (type->kind) {
      case TY_FIXNUM:
        arg = promote_to_int(arg);
        break;
      case TY_FLONUM:
        if (type->flonum.kind < FL_DOUBLE)  // Promote variadic argument.
          arg = make_cast(&tyDouble, arg->token, arg, false);
        break;
      default: break;
      }
    }
    args->data[i] = arg;
  }
}

Vector *extract_varinfo_types(const Vector *vars) {
  Vector *types = NULL;
  if (vars != NULL) {
    types = new_vector();
    for (int i = 0, len = vars->len; i < len; ++i)
      vec_push(types, ((VarInfo*)vars->data[i])->type);
  }
  return types;
}

static Type *to_ptr_type(Type *type) {
  switch (type->kind) {
  case TY_ARRAY: return array_to_ptr(type);
  case TY_FUNC:  return ptrof(type);
  default:  return type;
  }
}

static Type *apply_ptr_qualifier(Type *type, Type *ptype) {
  assert(type->kind == TY_PTR);
  assert(ptype->kind == TY_PTR);
  Type *dtype = qualified_type(type->pa.ptrof, ptype->pa.ptrof->qualifier & TQ_CONST);
  if (dtype != type->pa.ptrof)
    type = ptrof(dtype);
  return type;
}

Type *choose_ternary_result_type(Expr *tval, Expr *fval) {
  Type *ttype = tval->type;
  Type *ftype = fval->type;

  if (ttype->kind == TY_VOID || ftype->kind == TY_VOID)
    return &tyVoid;

  ttype = to_ptr_type(ttype);
  ftype = to_ptr_type(ftype);

  if (ftype->kind == TY_ARRAY)
    ftype = array_to_ptr(ftype);

  if (same_type(ttype, ftype))
    return ttype;
  if (ttype->kind == TY_PTR) {
    if (ftype->kind == TY_PTR) {  // Both pointer type
      if (same_type_without_qualifier(ttype, ftype, true)) {
        if (ftype->pa.ptrof->qualifier & TQ_CONST)
          return ftype;
        return ttype;
      }
      if (is_void_ptr(ttype))
        return apply_ptr_qualifier(ftype, ttype);
      if (is_void_ptr(ftype))
        return apply_ptr_qualifier(ttype, ftype);
    } else {
      if (can_cast(ttype, ftype, is_zero(fval), false))
        return ttype;
    }
  } else if (ftype->kind == TY_PTR) {
    return choose_ternary_result_type(fval, tval);  // Make ttype to pointer, and check again.
  } else if (is_number(ttype) && is_number(ftype)) {
    if (is_flonum(ttype)) {
      // TODO: Choose lager one.
      // if (is_flonum(ftype)) {
      //   return ttype;
      // }
      return ttype;
    } else if (is_flonum(ftype)) {
      return ftype;
    }
    assert(is_fixnum(ttype->kind));
    assert(is_fixnum(ftype->kind));
    if (ttype->fixnum.kind >= FX_ENUM)
      ttype = &tyInt;
    if (ftype->fixnum.kind >= FX_ENUM)
      ftype = &tyInt;
    return ttype->fixnum.kind > ftype->fixnum.kind ? ttype : ftype;
  }
  return NULL;
}

static Expr *calc_assign_with(const Token *tok, Expr *lhs, Expr *rhs) {
  // Assume token-kind and expr-kind is same arrangement.
  enum ExprKind kind = tok->kind + (EX_ADD - TK_ADD_ASSIGN);
  switch (kind) {
  default:  assert(false);
    // Fallthrough to avoid compile error.
  case EX_ADD: case EX_SUB:
    return new_expr_addsub(kind, tok, lhs, rhs);
  case EX_MUL: case EX_DIV:
    return new_expr_num_bop(kind, tok, lhs, rhs);
  case EX_MOD: case EX_BITAND: case EX_BITOR: case EX_BITXOR:
    return new_expr_int_bop(kind, tok, lhs, rhs);
  case EX_LSHIFT: case EX_RSHIFT:
    {
      Type *ltype = lhs->type;
      Type *rtype = rhs->type;
      if (!is_fixnum(ltype->kind) || !is_fixnum(rtype->kind))
        parse_error(PE_FATAL, tok, "Cannot use `%.*s' except numbers.",
                    (int)(tok->end - tok->begin), tok->begin);
      return new_expr_bop(kind, ltype, tok, lhs, rhs);
    }
  }
}

#ifndef __NO_BITFIELD
static Expr *transform_assign_with_bitfield(const Token *tok, Expr *lhs, Expr *rhs,
                                            const MemberInfo *minfo) {
  // Transform expression to
  // (ptr = &lhs, src = *ptr, tmp = ((src >> bitpos) & mask) + rhs,
  //  *ptr = (src & ~(mask << bitpos)) | ((tmp & mask) << bitpos), (*ptr >> bitpos) & mask)

  Type *type = get_fixnum_type(minfo->bitfield.base_kind, lhs->type->fixnum.is_unsigned, 0);

  Type *ptype = ptrof(type);
  assert(!is_global_scope(curscope));
  Expr *ptr = alloc_tmp_var(curscope, ptype);
  Expr *ptr_assign = new_expr_bop(EX_ASSIGN, ptype, tok, ptr, make_refer(lhs->token, lhs));
  Expr *dst = new_expr_unary(EX_DEREF, type, tok, ptr);

  Expr *src = alloc_tmp_var(curscope, type);
  Expr *src_assign = new_expr_bop(EX_ASSIGN, type, tok, src, dst);

  Expr *tmp = extract_bitfield_value(src, minfo);
  tmp = calc_assign_with(tok, tmp, rhs);
  Expr *store = assign_bitfield_member(tok, dst, src, tmp, minfo);

  Type *vtype = rhs->type;
  return new_expr_bop(EX_COMMA, vtype, tok,
                      new_expr_bop(EX_COMMA, vtype, tok, ptr_assign,
                                   new_expr_bop(EX_COMMA, vtype, tok, src_assign, store)),
                      extract_bitfield_value(dst, minfo));
}
#endif

Expr *transform_assign_with(const Token *tok, Expr *lhs, Expr *rhs) {
  // Transform expression `lhs += rhs` to `lhs = lhs + rhs`.
  // If LHS is not a variable, add temporary variable to keep `&LHS` to avoid side effect.
  // Replace expression to (ptr = &lhs, *ptr = *ptr + rhs)
  Expr *tmp_assign = NULL;
  if (lhs->kind != EX_VAR) {
#ifndef __NO_BITFIELD
    if (lhs->kind == EX_MEMBER) {
      const MemberInfo *minfo = lhs->member.info;
      if (minfo->bitfield.width > 0)
        return transform_assign_with_bitfield(tok, lhs, rhs, minfo);
    }
#endif

    Type *ptype = ptrof(lhs->type);
    assert(!is_global_scope(curscope));
    Expr *ptr = alloc_tmp_var(curscope, ptype);
    tmp_assign = new_expr_bop(EX_ASSIGN, ptype, tok, ptr, make_refer(lhs->token, lhs));
    lhs = new_expr_unary(EX_DEREF, lhs->type, lhs->token, ptr);
  }

  Expr *bop = calc_assign_with(tok, lhs, rhs);
  Expr *result = new_expr_bop(EX_ASSIGN, lhs->type, tok, lhs,
                              make_cast(lhs->type, tok, bop, false));

  return tmp_assign == NULL ? result
                            : new_expr_bop(EX_COMMA, result->type, tok, tmp_assign, result);
}

static Expr *unnest_arg(Expr *arg, Vector *unnested) {
  Type *type = arg->type;
  assert(type->kind != TY_VOID);
  Expr *tmp = alloc_tmp_var(curscope, type);
  Expr *assign = new_expr_bop(EX_ASSIGN, type, arg->token, tmp, arg);
  assert(assign->kind == EX_ASSIGN && assign->bop.lhs->kind == EX_VAR);
  vec_push(unnested, assign);
  return tmp;
}

// If an argument is complex expression,
// precalculate it and make function argument simple.
static Expr *simplify_funarg_recur(Expr *arg, Vector *unnested) {
  switch (arg->kind) {
  case EX_TERNARY:
  case EX_FUNCALL:
  case EX_INLINED:
  case EX_BLOCK:
  case EX_LOGAND:  // Shortcut must be handled properly.
  case EX_LOGIOR:
    return unnest_arg(arg, unnested);

  case EX_COMMA:
    vec_push(unnested, arg->bop.lhs);
    return simplify_funarg_recur(arg->bop.rhs, unnested);

  case EX_COMPLIT:
    vec_push(unnested, arg);
    return arg->complit.var;

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
    return unnest_arg(arg, unnested);
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
    arg->bop.lhs = simplify_funarg_recur(arg->bop.lhs, unnested);
    arg->bop.rhs = simplify_funarg_recur(arg->bop.rhs, unnested);
    break;

  case EX_ASSIGN:
    if (!is_prim_type(arg->type))
      return unnest_arg(arg, unnested);
    arg->bop.lhs = simplify_funarg_recur(arg->bop.lhs, unnested);
    arg->bop.rhs = simplify_funarg_recur(arg->bop.rhs, unnested);
    break;

  // Unary operators
  case EX_POS:
  case EX_NEG:
  case EX_BITNOT:
  case EX_PREINC:
  case EX_PREDEC:
  case EX_POSTINC:
  case EX_POSTDEC:
  case EX_REF:
  case EX_DEREF:
  case EX_CAST:
    arg->unary.sub = simplify_funarg_recur(arg->unary.sub, unnested);
    break;

  case EX_MEMBER:
    arg->member.target = simplify_funarg_recur(arg->member.target, unnested);
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

Expr *simplify_funcall(Expr *funcall) {
  assert(funcall->kind == EX_FUNCALL);

  Vector *args = funcall->funcall.args;
  int arg_count = args->len;
  // To avoid nested funcall,
  // simplify funargs and precalculate complex expression before funcall.
  Vector *unnested = new_vector();
  for (int i = 0; i < arg_count; ++i) {
    Expr *arg = args->data[i];
    args->data[i] = simplify_funarg_recur(arg, unnested);
  }
  funcall->funcall.func = simplify_funarg_recur(funcall->funcall.func, unnested);

  if (unnested->len > 0) {
    const Token *token = funcall->token;
    Expr *comma = unnested->data[0];
    for (int i = 1; i < unnested->len; ++i) {
      Expr *rhs = unnested->data[i];
      comma = new_expr_bop(EX_COMMA, rhs->type, token, comma, rhs);
    }
    funcall = new_expr_bop(EX_COMMA, funcall->type, token, comma, funcall);
  }
  free_vector(unnested);
  return funcall;
}

//

static void check_reachability_stmt(Stmt *stmt);

static int check_reachability_stmts(Vector *stmts) {
  assert(stmts != NULL);
  int reach = 0;
  for (int i = 0, n = stmts->len; i < n; ++i) {
    Stmt *stmt = stmts->data[i];
    if (reach & REACH_STOP) {
      if (!(stmt->kind == ST_LABEL || stmt->kind == ST_CASE))
        continue;
      reach = 0;
    }
    check_reachability_stmt(stmt);
    reach |= stmt->reach;
    if (reach & REACH_STOP) {
      for (; i < n - 1; ++i) {
        Stmt *next = stmts->data[i + 1];
        if ((next->kind == ST_BREAK && next->break_.parent->kind == ST_SWITCH) &&
            (stmt->kind != ST_RETURN && stmt->kind != ST_BREAK))
          continue;
        switch (next->kind) {
        case ST_LABEL:
        case ST_CASE:
          break;

        // Avoid false positive:
        case ST_WHILE: case ST_DO_WHILE:
          // TODO: Check the loop is jumped inside from other place using `goto` statement.
          break;
        case ST_FOR:
          if (next->for_.pre == NULL)
            break;
          // Fallthrough

        default:
          parse_error(PE_WARNING, next->token, "unreachable");
          break;
        }
        break;
      }
    }
  }
  return reach;
}

static void check_unreachability(Stmt *stmt) {
  if (stmt == NULL)
    return;
  switch (stmt->kind) {
  case ST_EMPTY:
    return;
  case ST_BLOCK:
    if (stmt->block.stmts->len == 0)
      return;
    stmt = stmt->block.stmts->data[0];
    break;
  default:
    break;
  }
  parse_error(PE_WARNING, stmt->token, "unreachable");
}

void check_unused_variables(Function *func) {
  assert(func->body_block != NULL);
  assert(func->body_block->kind == ST_BLOCK);
  Vector *stmts = func->body_block->block.stmts;
  if (stmts->len > 0) {
    // If __asm is used, variables might be used implicitly, so skip checking.
    for (int i = 0; i < stmts->len; ++i) {
      Stmt *stmt = stmts->data[i];
      if (stmt->kind == ST_ASM)
        return;
    }
  }

  for (int i = 0; i < func->scopes->len; ++i) {
    Scope *scope = func->scopes->data[i];
    if (scope->vars == NULL)
      continue;
    for (int j = 0; j < scope->vars->len; ++j) {
      VarInfo *varinfo = scope->vars->data[j];
      if (!(varinfo->storage & (VS_USED | VS_ENUM_MEMBER | VS_EXTERN)) && varinfo->ident != NULL) {
        parse_error(PE_WARNING, varinfo->ident, "Unused variable `%.*s'", NAMES(varinfo->ident->ident));
      }
    }
  }
}

static void check_reachability_stmt(Stmt *stmt) {
  if (stmt == NULL)
    return;
  switch (stmt->kind) {
  case ST_IF:
    check_reachability_stmt(stmt->if_.tblock);
    check_reachability_stmt(stmt->if_.fblock);
    if (is_const_truthy(stmt->if_.cond)) {
      stmt->reach = stmt->if_.tblock->reach;
    } else if (is_const_falsy(stmt->if_.cond)) {
      stmt->reach = stmt->if_.fblock != NULL ? stmt->if_.fblock->reach : 0;
    } else {
      stmt->reach = stmt->if_.tblock->reach &
                    (stmt->if_.fblock != NULL ? stmt->if_.fblock->reach : 0);
    }
    break;
  case ST_SWITCH:
    stmt->reach = (stmt->reach & ~REACH_STOP) |
        ((stmt->switch_.default_ != NULL) ? REACH_STOP : 0);
    check_reachability_stmt(stmt->switch_.body);
    stmt->reach &= stmt->switch_.body->reach;
    break;
  case ST_WHILE:
    if (is_const_truthy(stmt->while_.cond))
      stmt->reach |= REACH_STOP;
    if (is_const_falsy(stmt->while_.cond))
      check_unreachability(stmt->while_.body);
    else
      check_reachability_stmt(stmt->while_.body);
    break;
  case ST_DO_WHILE:
    if (is_const_truthy(stmt->while_.cond))
      stmt->reach |= REACH_STOP;
    check_reachability_stmt(stmt->while_.body);
    break;
  case ST_FOR:
    if (stmt->for_.cond != NULL && is_const_falsy(stmt->for_.cond)) {
      check_unreachability(stmt->for_.body);
    } else {
      if (stmt->for_.cond == NULL || is_const_truthy(stmt->for_.cond))
        stmt->reach |= REACH_STOP;
      check_reachability_stmt(stmt->for_.body);
    }
    break;
  case ST_BLOCK:
    stmt->reach = check_reachability_stmts(stmt->block.stmts);
    break;
  case ST_LABEL:
    check_reachability_stmt(stmt->label.stmt);
    stmt->reach = stmt->label.stmt->reach;
    break;
  case ST_RETURN:
    stmt->reach |= REACH_RETURN | REACH_STOP;
    break;
  case ST_BREAK:
    stmt->break_.parent->reach &= ~REACH_STOP;
    stmt->reach |= REACH_STOP;
    break;
  case ST_CASE:
    check_reachability_stmt(stmt->case_.stmt);
    stmt->reach = stmt->case_.stmt->reach;
    break;
  case ST_GOTO:
    // TODO:
    stmt->reach |= REACH_STOP;
    break;
  case ST_CONTINUE:
    stmt->reach |= REACH_STOP;
    break;
  case ST_EXPR:
    {
      // Lazily, check noreturn function call only top of the expression statement.
      Expr *expr = stmt->expr;
      if (expr->kind == EX_FUNCALL) {
        Expr *fexpr = expr->funcall.func;
        if (fexpr->kind == EX_VAR && is_global_scope(fexpr->var.scope)) {
          VarInfo *varinfo = scope_find(fexpr->var.scope, fexpr->var.name, NULL);
          assert(varinfo != NULL);
          Declaration *decl = varinfo->global.funcdecl;
          if (decl != NULL) {
            assert(decl->kind == DCL_DEFUN && decl->defun.func != NULL);
            if (decl->defun.func->flag & FUNCF_NORETURN) {
              stmt->reach |= REACH_STOP;
            }
          }
        }
      }
    }
    break;
  case ST_EMPTY: case ST_VARDECL: case ST_ASM:
    stmt->reach = 0;
    break;
  }
}

static void check_func_return(Function *func) {
  Type *type = func->type;
  Type *rettype = type->func.ret;
  const Token *rbrace = func->body_block->block.rbrace;

  static const Name *main_name;
  if (main_name == NULL)
    main_name = alloc_name("main", NULL, false);
  if (equal_name(func->ident->ident, main_name)) {
    if (rettype->kind == TY_VOID) {
      // Force return type to `int' for `main' function.
      type->func.ret = rettype = &tyInt;
    }
  }

  if (func->flag & FUNCF_NORETURN) {
    if (rettype->kind != TY_VOID) {
      parse_error(PE_WARNING, rbrace, "`noreturn' function should not return value");
    } else if (!(func->body_block->reach & REACH_STOP)) {
      Vector *stmts = func->body_block->block.stmts;
      if (stmts->len == 0 || ((Stmt*)stmts->data[stmts->len - 1])->kind != ST_ASM) {
        parse_error(PE_WARNING, rbrace, "`noreturn' function should not return");
      }
    }
  } else if (rettype->kind != TY_VOID && !(func->body_block->reach & REACH_STOP)) {
    Vector *stmts = func->body_block->block.stmts;
    if (stmts->len == 0 || ((Stmt*)stmts->data[stmts->len - 1])->kind != ST_ASM) {
      if (equal_name(func->ident->ident, main_name)) {
        // Return 0 if `return` statement is omitted in `main` function.
        if (!is_fixnum(rettype->kind) || rettype->fixnum.kind != FX_INT) {
          parse_error(PE_WARNING, rbrace, "`main' return type should be `int'");
        } else {
          vec_push(stmts, new_stmt_return(NULL, new_expr_fixlit(rettype, NULL, 0)));
        }
      } else {
        parse_error(PE_WARNING, rbrace, "`return' required");
      }
    }
  }
}

void check_func_reachability(Function *func) {
  check_reachability_stmt(func->body_block);
  check_func_return(func);
}

bool check_funcend_return(Stmt *stmt) {
  if (stmt == NULL)
    return false;

  switch (stmt->kind) {
  case ST_RETURN:
    return true;
  case ST_IF:
    {
      bool t = check_funcend_return(stmt->if_.tblock);
      bool f = stmt->if_.fblock == NULL || check_funcend_return(stmt->if_.fblock);
      return t && f;  // Return true even if else is not exist.
    }
  case ST_BLOCK:
    {
      Vector *stmts = stmt->block.stmts;
      assert(stmts != NULL);
      if (stmts->len  > 0)
        return check_funcend_return(stmts->data[stmts->len - 1]);
    }
    break;
  default:
    break;
  }
  return false;
}

int get_funparam_index(Function *func, const Name *name) {
  const Vector *params = func->params;
  for (int i = 0, param_count = params->len; i < param_count; ++i) {
    VarInfo *v = params->data[i];
    if (equal_name(v->ident->ident, name))
      return i;
  }
  return -1;
}

//

bool satisfy_inline_criteria(const VarInfo *varinfo) {
  // TODO: Check complexity or length of function body statements.
  const Type *type = varinfo->type;
  if (type->kind == TY_FUNC && (varinfo->storage & VS_INLINE) && !type->func.vaargs) {
    Function *func = varinfo->global.func;
    if (func != NULL) {
      // Self-recursion or mutual recursion are prevented,
      // because some inline function must not be defined at funcall point.
      return func->body_block != NULL && func->label_table == NULL && func->gotos == NULL;
    }
  }
  return false;
}

static Stmt *duplicate_inline_function_stmt(Function *targetfunc, Scope *targetscope, Stmt *stmt);

static Expr *duplicate_inline_function_expr(Function *targetfunc, Scope *targetscope, Expr *expr) {
  if (expr == NULL)
    return NULL;

  switch (expr->kind) {
  case EX_FIXNUM:
  case EX_FLONUM:
  case EX_STR:
    return expr;
  case EX_VAR:
    {
      if (is_global_scope(expr->var.scope))
        return expr;

      const Name *name = expr->var.name;
      VarInfo *varinfo = scope_find(expr->var.scope, name, NULL);
      if (varinfo->storage & (VS_EXTERN | VS_ENUM_MEMBER)) {
        // No need to duplicate.
        return expr;
      }

      // Detect relative scope.
      Scope *scope = curscope;
      for (Scope *p = targetscope; !is_global_scope(p); p = p->parent, scope = scope->parent) {
        if (expr->var.scope == p)
          break;
      }
      if (varinfo->storage & VS_PARAM) {
        // Assume parameters are stored in top scope in order.
        Vector *top_scope_vars = ((Scope*)targetfunc->scopes->data[0])->vars;
        int i;
        for (i = 0; i < top_scope_vars->len; ++i) {
          VarInfo *vi = top_scope_vars->data[i];
          if (vi == varinfo)
            break;
        }
        assert(i < top_scope_vars->len);
        // Rename.
        assert(i < scope->vars->len);
        name = ((VarInfo*)scope->vars->data[i])->ident->ident;
      }
      return new_expr_variable(name, varinfo->type, expr->token, scope);
    }

  case EX_ADD: case EX_SUB: case EX_MUL: case EX_DIV: case EX_MOD:
  case EX_BITAND: case EX_BITOR: case EX_BITXOR: case EX_LSHIFT: case EX_RSHIFT:
  case EX_EQ: case EX_NE: case EX_LT: case EX_LE: case EX_GE: case EX_GT:
  case EX_LOGAND: case EX_LOGIOR: case EX_ASSIGN: case EX_COMMA:
    {
      Expr *lhs = duplicate_inline_function_expr(targetfunc, targetscope, expr->bop.lhs);
      Expr *rhs = duplicate_inline_function_expr(targetfunc, targetscope, expr->bop.rhs);
      return new_expr_bop(expr->kind, expr->type, expr->token, lhs, rhs);
    }
  case EX_POS: case EX_NEG: case EX_BITNOT:
  case EX_PREINC: case EX_PREDEC: case EX_POSTINC: case EX_POSTDEC:
  case EX_REF: case EX_DEREF: case EX_CAST:
    {
      Expr *sub = duplicate_inline_function_expr(targetfunc, targetscope, expr->unary.sub);
      return new_expr_unary(expr->kind, expr->type, expr->token, sub);
    }
  case EX_TERNARY:
    {
      Expr *cond = duplicate_inline_function_expr(targetfunc, targetscope, expr->ternary.cond);
      Expr *tval = duplicate_inline_function_expr(targetfunc, targetscope, expr->ternary.tval);
      Expr *fval = duplicate_inline_function_expr(targetfunc, targetscope, expr->ternary.fval);
      return new_expr_ternary(expr->token, cond, tval, fval, expr->type);
    }
  case EX_MEMBER:
    {
      Expr *target = duplicate_inline_function_expr(targetfunc, targetscope, expr->member.target);
      return new_expr_member(expr->token, expr->type, target, expr->member.ident,
                             expr->member.info);
    }
  case EX_FUNCALL:
    {
      Expr *func = duplicate_inline_function_expr(targetfunc, targetscope, expr->funcall.func);
      Vector *args = new_vector();
      Vector *src_args = expr->funcall.args;
      for (int i = 0; i < src_args->len; ++i) {
        Expr *arg = src_args->data[i];
        vec_push(args, duplicate_inline_function_expr(targetfunc, targetscope, arg));
      }
      return new_expr_funcall(expr->token, get_callee_type(func->type), func, args);
    }
  case EX_INLINED:
    {
      Vector *args = new_vector();
      Vector *src_args = expr->inlined.args;
      for (int i = 0; i < src_args->len; ++i) {
        Expr *arg = src_args->data[i];
        vec_push(args, duplicate_inline_function_expr(targetfunc, targetscope, arg));
      }

      // Duplicate from original to receive function parameters correctly.
      VarInfo *varinfo = scope_find(global_scope, expr->inlined.funcname, NULL);
      assert(varinfo != NULL);
      assert(satisfy_inline_criteria(varinfo));
      return new_expr_inlined(expr->token, varinfo->ident->ident, expr->type, args,
                              embed_inline_funcall(varinfo));
    }
  case EX_COMPLIT:
    {
      Vector *inits = new_vector();
      Vector *src_inits = expr->complit.inits;
      for (int i = 0; i < src_inits->len; ++i) {
        Stmt *stmt = duplicate_inline_function_stmt(targetfunc, targetscope, src_inits->data[i]);
        vec_push(inits, stmt);
      }

      // Refer duplicated local variable.
      const Expr *org_var = expr->complit.var;
      assert(org_var->kind == EX_VAR);
#if !defined(NDEBUG)
      // Variable for complit must be in current scope.
      Scope *scope;
      VarInfo *varinfo = scope_find(curscope, org_var->var.name, &scope);
      assert(varinfo != NULL);
      assert(scope == curscope);
#else
      Scope *scope = curscope;
#endif
      Expr *var = new_expr_variable(org_var->var.name, org_var->type, expr->token, scope);
      return new_expr_complit(expr->type, expr->token, var, inits, expr->complit.original_init);
    }
  case EX_BLOCK:
    {
      Stmt *block = duplicate_inline_function_stmt(targetfunc, targetscope, expr->block);
      return new_expr_block(block);
    }
  }
  return NULL;
}

static Stmt *duplicate_inline_function_stmt(Function *targetfunc, Scope *targetscope, Stmt *stmt) {
  if (stmt == NULL)
    return NULL;

  static Scope *original_scope;

  switch (stmt->kind) {
  case ST_EXPR:
    {
      Expr *expr = duplicate_inline_function_expr(targetfunc, targetscope, stmt->expr);
      return new_stmt_expr(expr);
    }
  case ST_BLOCK:
    {
      Scope *bak_original_scope = original_scope;
      Scope *scope = curscope;
      if (stmt->block.scope != NULL) {
        original_scope = stmt->block.scope;
        Vector *vars = NULL;
        Vector *org_vars = stmt->block.scope->vars;
        if (org_vars != NULL) {
          vars = new_vector();
          for (int i = 0; i < org_vars->len; ++i) {
            VarInfo *vi = org_vars->data[i];
            const Token *token;
            if (vi->storage & VS_PARAM) {  // Rename parameter to be unique.
              token = alloc_dummy_ident();
            } else {
              token = vi->ident;
            }
            // The new variable is no longer a parameter.
            VarInfo *dup = var_add(vars, token, vi->type, vi->storage & ~VS_PARAM);
            if (vi->storage & VS_STATIC)
              dup->static_.svar = vi->static_.svar;
          }
        }
        scope = enter_scope(curfunc);
        scope->vars = vars;
        targetscope = stmt->block.scope;
      }
      assert(stmt->block.stmts != NULL);
      Vector *stmts = new_vector();
      for (int i = 0, len = stmt->block.stmts->len; i < len; ++i) {
        Stmt *st = stmt->block.stmts->data[i];
        if (st == NULL)
          continue;
        Stmt *dup = duplicate_inline_function_stmt(targetfunc, targetscope, st);
        vec_push(stmts, dup);
      }

      if (stmt->block.scope != NULL)
        exit_scope();
      Stmt *dup = new_stmt_block(stmt->token, stmts, scope, stmt->block.rbrace);
      dup->reach = stmt->reach;
      original_scope = bak_original_scope;
      return dup;
    }
    break;
  case ST_IF:
    {
      Expr *cond = duplicate_inline_function_expr(targetfunc, targetscope, stmt->if_.cond);
      Stmt *tblock = duplicate_inline_function_stmt(targetfunc, targetscope, stmt->if_.tblock);
      Stmt *fblock = duplicate_inline_function_stmt(targetfunc, targetscope, stmt->if_.fblock);
      return new_stmt_if(stmt->token, cond, tblock, fblock);
    }
  case ST_SWITCH:
    {
      Expr *value = duplicate_inline_function_expr(targetfunc, targetscope, stmt->switch_.value);
      Stmt *dup = new_stmt_switch(stmt->token, value);
      // Prepare buffer for cases.
      Vector *cases = new_vector();
      for (int i = 0; i < stmt->switch_.cases->len; ++i)
        vec_push(cases, NULL);
      dup->switch_.cases = cases;

      SAVE_LOOP_SCOPE(save, stmt, NULL); loop_scope.swtch = dup; {
        // cases, default_ will be updated according to the body statements duplication.
        Stmt *body = duplicate_inline_function_stmt(targetfunc, targetscope, stmt->switch_.body);
        dup->switch_.body = body;
      } RESTORE_LOOP_SCOPE(save);

      return dup;
    }
  case ST_WHILE:
    {
      Expr *cond = duplicate_inline_function_expr(targetfunc, targetscope, stmt->while_.cond);
      Stmt *dup = new_stmt_while(stmt->token, cond, NULL);
      SAVE_LOOP_SCOPE(save, dup, dup); {
        dup->while_.body = duplicate_inline_function_stmt(targetfunc, targetscope,
                                                          stmt->while_.body);
      } RESTORE_LOOP_SCOPE(save);
      return dup;
    }
  case ST_DO_WHILE:
    {
      Stmt *dup = new_stmt(ST_DO_WHILE, stmt->token);
      SAVE_LOOP_SCOPE(save, dup, dup); {
        dup->while_.body = duplicate_inline_function_stmt(targetfunc, targetscope,
                                                          stmt->while_.body);
      } RESTORE_LOOP_SCOPE(save);
      dup->while_.cond = duplicate_inline_function_expr(targetfunc, targetscope, stmt->while_.cond);
      return dup;
    }
  case ST_FOR:
    {
      Expr *pre = duplicate_inline_function_expr(targetfunc, targetscope, stmt->for_.pre);
      Expr *cond = duplicate_inline_function_expr(targetfunc, targetscope, stmt->for_.cond);
      Expr *post = duplicate_inline_function_expr(targetfunc, targetscope, stmt->for_.post);
      Stmt *dup = new_stmt_for(stmt->token, pre, cond, post, NULL);
      SAVE_LOOP_SCOPE(save, dup, dup); {
        dup->for_.body = duplicate_inline_function_stmt(targetfunc, targetscope, stmt->for_.body);
      } RESTORE_LOOP_SCOPE(save);
      return dup;
    }
  case ST_BREAK:
  case ST_CONTINUE:
    {
      Stmt *dup = new_stmt(stmt->kind, stmt->token);
      Stmt *parent = stmt->kind == ST_BREAK ? loop_scope.break_ : loop_scope.continu;
      assert(parent != NULL);
      dup->break_.parent = parent;
      return dup;
    }
  case ST_RETURN:
    {
      Expr *val = duplicate_inline_function_expr(targetfunc, targetscope, stmt->return_.val);
      Stmt *dup = new_stmt_return(stmt->token, val);
      return dup;
    }
  case ST_CASE:
    {
      Stmt *swtch = loop_scope.swtch;
      assert(swtch != NULL);
      Stmt *dup = new_stmt_case(stmt->token, swtch, stmt->case_.value);
      if (stmt->case_.value == NULL) {
        swtch->switch_.default_ = dup;
      } else {
        // Value is constant so reuse.
        assert(is_const(stmt->case_.value));
      }
      dup->case_.stmt = duplicate_inline_function_stmt(targetfunc, targetscope, stmt->case_.stmt);

      // Find index.
      Stmt *org_swtch = stmt->case_.swtch;
      Vector *org_cases = org_swtch->switch_.cases;
      int index = 0;
      for (int len = org_cases->len; index < len; ++index) {
        if (org_cases->data[index] == stmt)
          break;
      }
      assert(index < org_cases->len);
      swtch->switch_.cases->data[index] = dup;
      return dup;
    }
  case ST_LABEL:
    {
      Stmt *follow = duplicate_inline_function_stmt(targetfunc, targetscope, stmt->label.stmt);
      Stmt *dup = new_stmt_label(stmt->token, follow);
      dup->label.used = stmt->label.used;
      return dup;
    }
  case ST_VARDECL:
    {
      VarDecl *d = stmt->vardecl;
      if (d->varinfo->storage & VS_STATIC)
        return NULL;
      VarInfo *varinfo = scope_find(curscope, d->varinfo->ident->ident, NULL);
      assert(varinfo != NULL);
      VarDecl *decl = new_vardecl(varinfo);
      decl->init_stmt = duplicate_inline_function_stmt(targetfunc, targetscope, d->init_stmt);
      return new_stmt_vardecl(decl);
    }
  case ST_EMPTY: case ST_GOTO: case ST_ASM:
    return stmt;
  }
  return NULL;
}

Stmt *embed_inline_funcall(VarInfo *varinfo) {
  assert(varinfo->type->kind == TY_FUNC);
  Function *targetfunc = varinfo->global.func;
  return duplicate_inline_function_stmt(targetfunc, NULL, targetfunc->body_block);
}
