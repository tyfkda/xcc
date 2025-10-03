#include "../../config.h"
#include "fe_misc.h"

#include <assert.h>
#include <stdarg.h>
#include <stdlib.h>  // exit
#include <string.h>

#include "ast.h"
#include "expr.h"
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

static VarInfo *find_var_from_scope(Scope *scope, const Token *ident, Type *type, int storage,
                                    bool allow_defined) {
  assert(ident != NULL);
  const Name *name = ident->ident;
  assert(name != NULL);
  int idx = var_find(scope->vars, name);
  if (idx >= 0) {
    VarInfo *varinfo = scope->vars->data[idx];
    if (!same_type(type, varinfo->type)) {
      parse_error(PE_NOFATAL, ident, "`%.*s' type conflict", NAMES(name));
    } else if (!(storage & VS_EXTERN)) {
      if (varinfo->storage & VS_EXTERN) {
        varinfo->storage &= ~VS_EXTERN;
      } else if (is_global_scope(scope) && varinfo->global.init == NULL) {
        ;  // Ignore variable duplication if predecessor doesn't have initializer.
      } else {
        if (!allow_defined)
          parse_error(PE_NOFATAL, ident, "`%.*s' already defined", NAMES(name));
      }
    }
    return varinfo;
  }
  return NULL;
}

VarInfo *add_var_to_scope(Scope *scope, const Token *ident, Type *type, int storage,
                          bool allow_defined) {
  VarInfo *varinfo = find_var_from_scope(scope, ident, type, storage, allow_defined);
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
  VarInfo *varinfo = add_var_to_scope(curscope, ident, type, VS_ENUM_MEMBER, false);
  varinfo->enum_member.value = value;
}

Expr *proc_builtin_function_name(const Token *tok) {
  if (curfunc == NULL) {
    parse_error(PE_NOFATAL, tok, "must be inside function");
    static const char nulstr[] = "";
    return string_expr(tok, strdup(nulstr), 0, STR_CHAR);
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
    mark_var_used_sub(expr->complit.var, false);
    break;
  case EX_ASSIGN:
    mark_var_used_sub(expr->bop.lhs, false);
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

#ifndef __NO_BITFIELD
void not_bitfield_member(Expr *expr) {
  if (expr->kind == EX_MEMBER) {
    const MemberInfo *minfo = expr->member.info;
    if (minfo->bitfield.active)
      parse_error(PE_NOFATAL, expr->token, "cannot get size for bitfield");
  }
}
#endif

#if STRUCT_ARG_AS_POINTER || VAARG_STRUCT_AS_POINTER
// Convert to struct pointer using compound literal: &(type){arg}
static Expr *struct_arg_as_pointer(Expr *arg, Type *type) {
  const Token *tok = arg->token;
  if (arg->kind != EX_COMPLIT) {
    Expr *var = alloc_tmp_var(curscope, type);
    Initializer *init = new_initializer(IK_SINGLE, tok);
    init->single = arg;
    Vector *inits = assign_initial_value(var, init, NULL);
    arg = new_expr_complit(type, tok, var, inits, init);
  }
  return make_refer(tok, arg);
}
#endif

bool is_small_struct(const Type *type) {
  if (type->kind != TY_STRUCT)
    return false;
  const StructInfo *sinfo = type->struct_.info;
  assert(sinfo != NULL);
  if (sinfo->is_flexible)
    return false;

#if XCC_TARGET_ARCH == XCC_ARCH_WASM
  if (sinfo->is_union) {
    if (sinfo->member_count < 1)
      return false;
  } else {
    // In WASM, only single-element struct is considered as small struct.
    if (sinfo->member_count != 1)
      return false;
  }
  const MemberInfo *minfo = &sinfo->members[0];
  return is_prim_type(minfo->type) || is_small_struct(minfo->type);
#else
  return type_size(type) <= TARGET_POINTER_SIZE * 2;
#endif
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
#if STRUCT_ARG_AS_POINTER
        if (!is_small_struct(type))
          arg = struct_arg_as_pointer(arg, type);
#endif
      }
    } else if (vaargs && i >= paramc) {
      Type *type = arg->type;
      switch (type->kind) {
      case TY_FIXNUM:
        arg = promote_to_int(arg);
        break;
      case TY_FLONUM:
        if (type->flonum.kind < FL_DOUBLE)  // Promote variadic argument.
          arg = make_cast(&tyDouble, arg->token, arg, false);
        break;
#if STRUCT_ARG_AS_POINTER || VAARG_STRUCT_AS_POINTER
      case TY_STRUCT:
#if STRUCT_ARG_AS_POINTER
        if (is_small_struct(type))
          break;
#endif
        arg = struct_arg_as_pointer(arg, type);
        break;
#endif
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
  for (int i = 0; i < func->scopes->len; ++i) {
    Scope *scope = func->scopes->data[i];
    for (int j = 0; j < scope->vars->len; ++j) {
      VarInfo *varinfo = scope->vars->data[j];
      if (!(varinfo->storage & (VS_USED | VS_ENUM_MEMBER | VS_EXTERN)) && varinfo->ident != NULL) {
        parse_error(PE_WARNING, varinfo->ident, "Unused variable `%.*s'",
                    NAMES(varinfo->ident->ident));
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

static bool check_func_return(Function *func) {
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

  bool result = true;
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
        assert(rettype->kind == TY_FIXNUM && rettype->fixnum.kind == FX_INT);
        vec_push(stmts, new_stmt_return(NULL, new_expr_fixlit(rettype, NULL, 0)));
        func->body_block->reach |= REACH_RETURN;
      } else {
        parse_error(PE_WARNING, rbrace, "`return' required");
        result = false;
      }
    }
  }
  return result;
}

static inline void insert_return_stmt(Function *func) {
#if XCC_TARGET_ARCH == XCC_ARCH_WASM
  // To avoid runtime validation error, put return with compound literal:
  //   return (rettype){};
  const Token *token = func->body_block->token;
  Type *rettype = func->type->func.ret;
  Expr *var = alloc_tmp_var(func->scopes->data[0], rettype);
  Initializer *init = new_initializer(IK_MULTI, token);
  init->multi = new_vector();
  init = flatten_initializer(rettype, init);
  Vector *inits = assign_initial_value(var, init, NULL);
  Expr *value = new_expr_complit(rettype, token, var, inits, init);
  Stmt *ret = new_stmt_return(token, value);
  vec_push(func->body_block->block.stmts, ret);
  ret->reach = REACH_STOP | REACH_RETURN;
  func->body_block->reach |= REACH_RETURN;
#else
  UNUSED(func);
#endif
}

void check_func_reachability(Function *func) {
  check_reachability_stmt(func->body_block);
  if (!check_func_return(func))
    insert_return_stmt(func);
}

bool check_funcend_return(Stmt *stmt) {
  assert(stmt != NULL);
  return stmt->reach & REACH_RETURN;
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

static Vector *duplicate_inline_function_asm_args(Function *targetfunc, Scope *targetscope, Vector *srcs) {
  if (srcs == NULL)
    return NULL;
  Vector *dups = new_vector();
  for (int i = 0; i < srcs->len; ++i) {
    AsmArg *src = srcs->data[i];
    AsmArg *dup = calloc_or_die(sizeof(*dup));
    dup->constraint = src->constraint;
    dup->expr = duplicate_inline_function_expr(targetfunc, targetscope, src->expr);
    vec_push(dups, dup);
  }
  return dups;
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
  case ST_ASM:
    {
      Vector *outputs = duplicate_inline_function_asm_args(targetfunc, targetscope, stmt->asm_.outputs);
      Vector *inputs = duplicate_inline_function_asm_args(targetfunc, targetscope, stmt->asm_.inputs);
      Stmt *dup = new_stmt_asm(stmt->token, stmt->asm_.templates, outputs, inputs, stmt->asm_.flag);
      return dup;
    }
    break;
  case ST_EMPTY: case ST_GOTO:
    return stmt;
  }
  return NULL;
}

Stmt *embed_inline_funcall(VarInfo *varinfo) {
  assert(varinfo->type->kind == TY_FUNC);
  Function *targetfunc = varinfo->global.func;
  return duplicate_inline_function_stmt(targetfunc, NULL, targetfunc->body_block);
}
