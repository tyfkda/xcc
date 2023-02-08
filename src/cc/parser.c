#include "../config.h"
#include "parser.h"

#include <assert.h>
#include <inttypes.h>  // PRIdPTR
#include <stdarg.h>
#include <stdbool.h>
#include <stdlib.h>  // malloc

#include "ast.h"
#include "lexer.h"
#include "table.h"
#include "type.h"
#include "util.h"
#include "var.h"

#define MAX_ERROR_COUNT  (25)

Function *curfunc;
Scope *curscope;
Vector *toplevel;

bool error_warning;
int compile_warning_count;
int compile_error_count;

static Stmt *parse_stmt(void);

typedef struct {
  Stmt *swtch;
  Stmt *break_;
  Stmt *continu;
} LoopScope;

static LoopScope loop_scope;

#define SAVE_LOOP_SCOPE(var, b, c)  LoopScope var = loop_scope; if (b != NULL) loop_scope.break_ = b; if (c != NULL) loop_scope.continu = c;
#define RESTORE_LOOP_SCOPE(var)     loop_scope = var

VarInfo *add_var_to_scope(Scope *scope, const Token *ident, Type *type, int storage) {
  assert(ident != NULL);
  const Name *name = ident->ident;
  assert(name != NULL);
  if (scope->vars != NULL) {
    int idx = var_find(scope->vars, name);
    if (idx >= 0) {
      VarInfo *varinfo = scope->vars->data[idx];
      if (!same_type(type, varinfo->type)) {
        parse_error(PE_NOFATAL, ident, "`%.*s' type conflict", name->bytes, name->chars);
      } else if (!(storage & VS_EXTERN)) {
        if (varinfo->storage & VS_EXTERN)
          varinfo->storage &= ~VS_EXTERN;
        else
          parse_error(PE_NOFATAL, ident, "`%.*s' already defined", name->bytes, name->chars);
      }
      return varinfo;
    }
  }
  return scope_add(scope, name, type, storage);
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

    if (level == PE_WARNING && !error_warning)
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

Token *consume(enum TokenKind kind, const char *error) {
  Token *tok = match(kind);
  if (tok == NULL)
    parse_error(PE_NOFATAL, tok, error);
  return tok;
}

Type *fix_array_size(Type *type, Initializer *init) {
  assert(init != NULL);
  assert(type->kind == TY_ARRAY);

  bool is_str = (is_char_type(type->pa.ptrof) &&
                 init->kind == IK_SINGLE &&
                 init->single->kind == EX_STR);
  if (!is_str && init->kind != IK_MULTI) {
    // Error will be reported in another place.
    return type;
  }

  ssize_t arr_len = type->pa.length;
  if (arr_len == -1) {
    if (is_str) {
      arr_len = init->single->str.size;
    } else {
      ssize_t index = 0;
      ssize_t max_index = 0;
      for (ssize_t i = 0; i < init->multi->len; ++i) {
        Initializer *init_elem = init->multi->data[i];
        if (init_elem->kind == IK_ARR) {
          index = init_elem->arr.index;
        }
        ++index;
        if (max_index < index)
          max_index = index;
      }
      arr_len = max_index;
    }
    Type *cloned = clone_type(type);
    cloned->pa.length = arr_len;
    return cloned;
  } else {
    assert(arr_len > 0);
    assert(!is_str || init->single->kind == EX_STR);
    ssize_t init_len = is_str ? (ssize_t)init->single->str.size : (ssize_t)init->multi->len;
    if (init_len > arr_len && (!is_str || init_len - 1 > arr_len))  // Allow non-nul string.
      parse_error(PE_FATAL, NULL, "Initializer more than array size");
    return type;
  }
}

static Stmt *build_memcpy(Expr *dst, Expr *src, size_t size) {
  assert(!is_global_scope(curscope));
  Type *charptr_type = ptrof(&tyChar);
  VarInfo *dstvar = add_var_to_scope(curscope, alloc_dummy_ident(), charptr_type, 0);
  VarInfo *srcvar = add_var_to_scope(curscope, alloc_dummy_ident(), charptr_type, 0);
  VarInfo *sizevar = add_var_to_scope(curscope, alloc_dummy_ident(), &tySize, 0);
  Expr *dstexpr = new_expr_variable(dstvar->name, dstvar->type, NULL, curscope);
  Expr *srcexpr = new_expr_variable(srcvar->name, srcvar->type, NULL, curscope);
  Expr *sizeexpr = new_expr_variable(sizevar->name, sizevar->type, NULL, curscope);

  Fixnum size_num_lit = size;
  Expr *size_num = new_expr_fixlit(&tySize, NULL, size_num_lit);

  Fixnum zero = 0;
  Expr *zeroexpr = new_expr_fixlit(&tySize, NULL, zero);

  Vector *stmts = new_vector();
  vec_push(stmts, new_stmt_expr(new_expr_bop(EX_ASSIGN, charptr_type, NULL, dstexpr, dst)));
  vec_push(stmts, new_stmt_expr(new_expr_bop(EX_ASSIGN, charptr_type, NULL, srcexpr, src)));
  vec_push(stmts, new_stmt_for(
      NULL,
      new_expr_bop(EX_ASSIGN, &tySize, NULL, sizeexpr, size_num),    // for (_size = size;
      new_expr_bop(EX_GT, &tyBool, NULL, sizeexpr, zeroexpr),        //      _size > 0;
      new_expr_unary(EX_PREDEC, sizeexpr->type, NULL, sizeexpr),     //      --_size)
      new_stmt_expr(                                                 //   *_dst++ = *_src++;
          new_expr_bop(EX_ASSIGN, &tyChar, NULL,
                       new_expr_unary(EX_DEREF, &tyChar, NULL,
                                      new_expr_unary(EX_POSTINC, dstexpr->type, NULL, dstexpr)),
                       new_expr_unary(EX_DEREF, &tyChar, NULL,
                                      new_expr_unary(EX_POSTINC, srcexpr->type, NULL, srcexpr))))));
  return new_stmt_block(NULL, stmts, NULL, NULL);
}

static Stmt *init_char_array_by_string(Expr *dst, Initializer *src) {
  // Initialize char[] with string literal (char s[] = "foo";).
  assert(src->kind == IK_SINGLE);
  const Expr *str = src->single;
  assert(str->kind == EX_STR);
  assert(dst->type->kind == TY_ARRAY && is_char_type(dst->type->pa.ptrof));

  ssize_t size = str->str.size;
  ssize_t dstsize = dst->type->pa.length;
  if (dstsize == -1) {
    dst->type->pa.length = dstsize = size;
  } else {
    if (dstsize < size - 1)
      parse_error(PE_FATAL, NULL, "Buffer is shorter than string: %d for \"%s\"", (int)dstsize, str);
  }

  Type *strtype = dst->type;
  VarInfo *varinfo = str_to_char_array(curscope, strtype, src, toplevel);
  Expr *var = new_expr_variable(varinfo->name, strtype, NULL, curscope);
  return build_memcpy(dst, var, size);
}

static int compare_desig_start(const void *a, const void *b) {
  const ssize_t *pa = *(ssize_t**)a;
  const ssize_t *pb = *(ssize_t**)b;
  ssize_t d = *pa - *pb;
  return d > 0 ? 1 : d < 0 ? -1 : 0;
}

static Initializer *flatten_array_initializer(Initializer *init) {
  // Check whether IK_DOT or IK_ARR exists.
  int i = 0, len = init->multi->len;
  for (; i < len; ++i) {
    Initializer *init_elem = init->multi->data[i];
    if (init_elem->kind == IK_DOT)
      parse_error(PE_FATAL, NULL, "dot initializer for array");
    if (init_elem->kind == IK_ARR)
      break;
  }
  if (i >= len)  // IK_ARR not exits.
    return init;

  // Enumerate designated initializer.
  Vector *ranges = new_vector();  // <(start, count)>
  size_t lastStartIndex = 0;
  size_t lastStart = 0;
  size_t index = i;
  for (; i <= len; ++i, ++index) {  // '+1' is for last range.
    Initializer *init_elem = NULL;
    if (i >= len || (init_elem = init->multi->data[i])->kind == IK_ARR) {
      if ((size_t)i > lastStartIndex) {
        size_t *range = malloc(sizeof(size_t) * 3);
        range[0] = lastStart;
        range[1] = lastStartIndex;
        range[2] = index - lastStart;
        vec_push(ranges, range);
      }
      if (i >= len)
        break;
      lastStart = index = init_elem->arr.index;
      lastStartIndex = i;
    } else if (init_elem->kind == IK_DOT)
      parse_error(PE_FATAL, NULL, "dot initializer for array");
  }

  // Sort
  qsort(ranges->data, ranges->len, sizeof(size_t *), compare_desig_start);

  // Reorder
  Vector *reordered = new_vector();
  index = 0;
  for (int i = 0; i < ranges->len; ++i) {
    size_t *p = ranges->data[i];
    size_t start = p[0];
    size_t index = p[1];
    size_t count = p[2];
    if (i > 0) {
      size_t *q = ranges->data[i - 1];
      if (start < q[0] + q[2])
        parse_error(PE_FATAL, NULL, "Initializer for array overlapped");
    }
    for (size_t j = 0; j < count; ++j) {
      Initializer *elem = init->multi->data[index + j];
      if (j == 0 && index != start && elem->kind != IK_ARR) {
        Initializer *arr = new_initializer(IK_ARR, elem->token);
        arr->arr.index = start;
        arr->arr.value = elem;
        elem = arr;
      }
      vec_push(reordered, elem);
    }
  }

  Initializer *init2 = new_initializer(IK_MULTI, init->token);
  init2->multi = reordered;
  return init2;
}

static bool is_multi_type(enum TypeKind kind) {
  return kind == TY_STRUCT || kind == TY_ARRAY;
}

static Initializer *flatten_initializer_multi0(Type *type, Initializer *init);

static Initializer *flatten_initializer_multi(Type *type, Initializer *init, int *pindex) {
  assert(init != NULL);
  switch (init->kind) {
  case IK_SINGLE:
    *pindex += 1;
    return flatten_initializer(type, init);
  case IK_MULTI:
    break;
  default:
    parse_error(PE_NOFATAL, init->token, "Illegal initializer");
    return init;
  }

  if (*pindex < 0) {
    *pindex = 0;
  } else if (*pindex < init->multi->len) {
    Initializer *e = init->multi->data[*pindex];
    if (e->kind == IK_MULTI && is_multi_type(type->kind)) {
      *pindex += 1;
      return flatten_initializer_multi0(type, e);
    }
  }

  if (!is_multi_type(type->kind)) {
    Initializer *elem_init = init->multi->data[*pindex];
    *pindex += 1;
    return flatten_initializer(type, elem_init);
  }

  switch (type->kind) {
  case TY_STRUCT:
    {
      const StructInfo *sinfo = type->struct_.info;
      int n = sinfo->members->len;
      int m = init->multi->len;
      if (n <= 0) {
        return init;
      }

      if (*pindex < m) {
        Initializer *elem_init = init->multi->data[*pindex];
        if (elem_init->kind == IK_SINGLE && same_type_without_qualifier(type, elem_init->single->type, true)) {
          *pindex += 1;
          return elem_init;
        }
      }

      Initializer **values = malloc(sizeof(Initializer*) * n);
      for (int i = 0; i < n; ++i)
        values[i] = NULL;

      int midx = 0;
      while (*pindex < m) {
        Initializer *value = init->multi->data[*pindex];
        if (value->kind == IK_ARR) {
          parse_error(PE_NOFATAL, value->token, "indexed initializer for struct");
          *pindex += 1;
          continue;
        }

        bool dot = value->kind == IK_DOT;
        if (dot) {
          const Name *name = value->dot.name;
          midx = var_find(sinfo->members, name);
          if (midx >= 0) {
            value = value->dot.value;
          } else {
            Vector *stack = new_vector();
            if (search_from_anonymous(type, name, NULL, stack) == NULL) {
              parse_error(PE_NOFATAL, value->token, "`%.*s' is not member of struct", name->bytes, name->chars);
              *pindex += 1;
              continue;
            }

            midx = (intptr_t)stack->data[0];
            Vector *multi = new_vector();
            vec_push(multi, value);
            Initializer *init2 = new_initializer(IK_MULTI, value->token);
            init2->multi = multi;
            value = init2;
          }
        }

        if (midx >= n)
          break;

        MemberInfo *minfo = sinfo->members->data[midx];
        if (dot) {
          value = flatten_initializer(minfo->type, value);
          *pindex += 1;
        } else {
          value = flatten_initializer_multi(minfo->type, init, pindex);
        }
        values[midx++] = value;

        if (sinfo->is_union)
          break;
      }

      Initializer *flat = new_initializer(IK_MULTI, init->token);
      Vector *v = new_vector();
      v->len = v->capacity = n;
      v->data = (void**)values;
      flat->multi = v;
      return flat;
    }
  case TY_ARRAY:
    {
      if (is_char_type(type->pa.ptrof)) {
        Initializer *elem_init = init->multi->data[*pindex];
        if (elem_init->kind == IK_SINGLE && elem_init->single->kind == EX_STR) {
          *pindex += 1;
          return elem_init;
        }
      }

      Type *elem_type = type->pa.ptrof;
      Vector *elems = new_vector();
      ssize_t eidx = 0;
      while (*pindex < init->multi->len) {
        Initializer *elem_init = init->multi->data[*pindex];
        if (elem_init->kind == IK_ARR) {
          elem_init->arr.value = flatten_initializer(elem_type, elem_init->arr.value);
          eidx = elem_init->arr.index;
          assert(type->pa.length < 0 || eidx < type->pa.length);
          *pindex += 1;
        } else {
          if (type->pa.length >= 0 && eidx >= type->pa.length)
            break;
          elem_init = flatten_initializer_multi(elem_type, init, pindex);
        }
        vec_push(elems, elem_init);
        ++eidx;
      }

      Initializer *init2 = new_initializer(IK_MULTI, init->token);
      init2->multi = elems;
      return init2;
    }
  default: assert(false); break;
  }
  return init;
}

static Initializer *flatten_initializer_multi0(Type *type, Initializer *init) {
  if (type->kind == TY_ARRAY)
    init = flatten_array_initializer(init);

  int index = -1;
  Initializer *flat = flatten_initializer_multi(type, init, &index);
  switch (type->kind) {
  case TY_ARRAY:
  case TY_STRUCT:
    if (index < init->multi->len) {
      const char *tstr = type->kind == TY_ARRAY ? "array" : "struct";
      parse_error(PE_WARNING, ((Initializer*)init->multi->data[index])->token, "Excess elements in %s initializer", tstr);
    }
    break;
  default: assert(false); break;
  }
  return flat;
}

Initializer *flatten_initializer(Type *type, Initializer *init) {
  if (init == NULL)
    return NULL;

  if (init->kind == IK_MULTI && is_multi_type(type->kind))
    return flatten_initializer_multi0(type, init);

  switch (type->kind) {
  case TY_STRUCT:
    if (init->kind == IK_SINGLE) {
      Expr *e = init->single;
      if (!same_type_without_qualifier(type, e->type, true))
        parse_error(PE_NOFATAL, init->token, "Incompatible type");
      if (e->kind == EX_COMPLIT) {
        init = flatten_initializer(type, e->complit.original_init);
        assert(init->kind == IK_MULTI);
        Expr *var = e->complit.var;
        VarInfo *varinfo = scope_find(var->var.scope, var->var.name, NULL);
        assert(varinfo != NULL);
        if (is_global_scope(var->var.scope))
          varinfo->global.init = init;
      }
    }
    break;
  case TY_ARRAY:
    switch (init->kind) {
    case IK_SINGLE:
      // Special handling for string (char[]), and accept length difference.
      if (init->single->type->kind == TY_ARRAY &&
          can_cast(type->pa.ptrof, init->single->type->pa.ptrof, is_zero(init->single), false))
        break;
      // Fallthrough
    default:
      // Error will be reported in another place.
      break;
    }
    break;
  default:
    switch (init->kind) {
    case IK_MULTI:
      if (init->multi->len != 1 || ((Initializer*)init->multi->data[0])->kind != IK_SINGLE) {
        parse_error(PE_NOFATAL, init->token, "Requires scaler");
        break;
      }
      init = init->multi->data[0];
      // Fallthrough
    case IK_SINGLE:
      break;
    default:
      parse_error(PE_NOFATAL, init->token, "Error initializer");
      // TODO: Modify init
      break;
    }
    break;
  }
  return init;
}

static Expr *check_global_initializer_fixnum(Expr *value, bool *isconst) {
  switch (value->kind) {
  case EX_FIXNUM:
#ifndef __NO_FLONUM
  case EX_FLONUM:
#endif
    *isconst = true;
    break;
  case EX_STR:
    // Create string and point to it.
    value = str_to_char_array_var(curscope, value, toplevel);
    *isconst = true;
    break;
  case EX_VAR:
    {
      Scope *scope;
      VarInfo *varinfo = scope_find(value->var.scope, value->var.name, &scope);
      if (varinfo == NULL) {
        // Error must be raised already, so exit quietly.
        *isconst = true;
        break;
      }

      if (!is_global_scope(scope) && !(varinfo->storage & VS_STATIC))
        parse_error(PE_FATAL, value->token, "Allowed global reference only");
      *isconst = value->type->kind == TY_ARRAY || value->type->kind == TY_FUNC ||
                 (value->type->kind == TY_PTR && value->type->pa.ptrof->kind == TY_FUNC);
    }
    break;
  case EX_ADD:
  case EX_SUB:
    {
      bool lhs_const = false, rhs_const = false;
      value->bop.lhs = check_global_initializer_fixnum(value->bop.lhs, &lhs_const);
      value->bop.rhs = check_global_initializer_fixnum(value->bop.rhs, &rhs_const);
      *isconst = lhs_const && rhs_const;
    }
    break;
  case EX_REF:
    value->unary.sub = check_global_initializer_fixnum(value->unary.sub, isconst);
    *isconst = true;
    break;
  case EX_DEREF:
  case EX_CAST:
    value->unary.sub = check_global_initializer_fixnum(value->unary.sub, isconst);
    break;
  case EX_MEMBER:
    value->member.target = check_global_initializer_fixnum(value->member.target, isconst);
    if (value->token->kind != TK_DOT)
      parse_error(PE_FATAL, value->token, "Allowed global reference only");
    *isconst = value->type->kind == TY_ARRAY;
    break;
  default:
    *isconst = false;
    break;
  }
  return value;
}

static Initializer *check_global_initializer(Type *type, Initializer *init) {
  if (init == NULL)
    return NULL;

  switch (type->kind) {
#ifndef __NO_FLONUM
  case TY_FLONUM:
    if (init->kind == IK_SINGLE) {
      switch (init->single->kind) {
      case EX_FIXNUM:
        {
          Fixnum fixnum = init->single->fixnum;
          init->single = new_expr_flolit(type, init->single->token, fixnum);
        }
        // Fallthrough
      case EX_FLONUM:
        return init;
      default:
        parse_error(PE_NOFATAL, init->single->token, "Constant expression expected");
        break;
      }
    }
    break;
#endif
  case TY_FIXNUM:
  case TY_PTR:
    {
      assert(init->kind == IK_SINGLE);
      bool isconst = false;
      Expr *value = check_global_initializer_fixnum(init->single, &isconst);
      init->single = make_cast(type, init->single->token, value, false);
      if (!isconst) {
        parse_error(PE_NOFATAL, init->single->token, "Initializer must be constant");
      }
    }
    break;
  case TY_ARRAY:
    switch (init->kind) {
    case IK_MULTI:
      {
        Type *elemtype = type->pa.ptrof;
        Vector *multi = init->multi;
        for (int i = 0, len = multi->len; i < len; ++i) {
          Initializer *eleminit = multi->data[i];
          if (eleminit->kind == IK_ARR) {
            eleminit->arr.value = check_global_initializer(elemtype, eleminit->arr.value);
          } else {
            multi->data[i] = check_global_initializer(elemtype, eleminit);
          }
        }
      }
      break;
    case IK_SINGLE:
      if (is_char_type(type->pa.ptrof)) {
        Expr *e = strip_cast(init->single);
        if (e->kind == EX_STR) {
          assert(type->pa.length > 0);
          if ((ssize_t)e->str.size - 1 > type->pa.length) {  // Allow non-nul string.
            parse_error(PE_NOFATAL, init->single->token, "Array size shorter than initializer");
          }
          break;
        }
      }
      // Fallthrough
    case IK_DOT:
    default:
      parse_error(PE_NOFATAL, init->token, "Array initializer requires `{'");
      break;
    }
    break;
  case TY_STRUCT:
    {
      if (init->kind != IK_MULTI) {
        parse_error(PE_NOFATAL, init->token, "Struct initializer requires `{'");
        return NULL;
      }
      const StructInfo *sinfo = type->struct_.info;
      for (int i = 0, n = sinfo->members->len; i < n; ++i) {
        const MemberInfo *member = sinfo->members->data[i];
        Initializer *init_elem = init->multi->data[i];
        if (init_elem != NULL)
          init->multi->data[i] = check_global_initializer(member->type, init_elem);
      }
    }
    break;
  default:
    parse_error(PE_NOFATAL, NULL, "Global initial value for type %d not implemented (yet)\n", type->kind);
    break;
  }
  return init;
}

Vector *assign_initial_value(Expr *expr, Initializer *init, Vector *inits) {
  if (init == NULL)
    return inits;

  if (inits == NULL)
    inits = new_vector();

  switch (expr->type->kind) {
  case TY_ARRAY:
    switch (init->kind) {
    case IK_MULTI:
      {
        ssize_t arr_len = expr->type->pa.length;
        if (arr_len > 0 && init->multi->len > arr_len)
          parse_error(PE_FATAL, init->token, "Initializer more than array size");

        assert(!is_global_scope(curscope));
        Type *ptr_type = array_to_ptr(expr->type);
        VarInfo *ptr_varinfo = add_var_to_scope(curscope, alloc_dummy_ident(), ptr_type, 0);
        Expr *ptr_var = new_expr_variable(ptr_varinfo->name, ptr_type, NULL, curscope);
        vec_push(inits, new_stmt_expr(new_expr_bop(EX_ASSIGN, ptr_type, NULL, ptr_var, expr)));

        const size_t len = init->multi->len;
        const size_t elem_size = type_size(expr->type->pa.ptrof);
        size_t prev_index = 0, index = 0;
        for (size_t i = 0; i < len; ++i) {
          Initializer *init_elem = init->multi->data[i];
          if (init_elem->kind == IK_ARR) {
            index = init_elem->arr.index;
            init_elem = init_elem->arr.value;
          }

          size_t add = index - prev_index;
          if (add > 0) {
            const Fixnum n = add * elem_size;
            vec_push(inits, new_stmt_expr(
                new_expr_bop(EX_ASSIGN, ptr_type, NULL, ptr_var,
                    new_expr_bop(EX_ADD, ptr_type, NULL, ptr_var,
                        new_expr_fixlit(&tySize, NULL, n)))));
          }

          assign_initial_value(new_expr_deref(NULL, ptr_var), init_elem, inits);
          prev_index = index++;
        }
      }
      break;
    case IK_SINGLE:
      // Special handling for string (char[]).
      if (is_char_type(expr->type->pa.ptrof) &&
          init->single->kind == EX_STR) {
        vec_push(inits, init_char_array_by_string(expr, init));
        break;
      }
      // Fallthrough
    default:
      parse_error(PE_NOFATAL, init->token, "Array initializer requires `{'");
      break;
    }
    break;
  case TY_STRUCT:
    {
      if (init->kind == IK_SINGLE) {
        Expr *e = init->single;
        if (same_type_without_qualifier(expr->type, e->type, true)) {
          vec_push(inits, new_stmt_expr(new_expr_bop(EX_ASSIGN, expr->type, init->token, expr, e)));
          break;
        }
      }
      if (init->kind != IK_MULTI) {
        parse_error(PE_NOFATAL, init->token, "Struct initializer requires `{'");
        break;
      }

      const StructInfo *sinfo = expr->type->struct_.info;
      if (!sinfo->is_union) {
        for (int i = 0, n = sinfo->members->len; i < n; ++i) {
          const MemberInfo *member = sinfo->members->data[i];
          Expr *mem = new_expr_member(NULL, member->type, expr, NULL, i);
          Initializer *init_elem = init->multi->data[i];
          if (init_elem != NULL)
            assign_initial_value(mem, init_elem, inits);
        }
      } else {
        int n = sinfo->members->len;
        int m = init->multi->len;
        if (n <= 0 && m > 0)
          parse_error(PE_FATAL, init->token, "Initializer for empty union");

        int count = 0;
        for (int i = 0; i < n; ++i) {
          Initializer *init_elem = init->multi->data[i];
          if (init_elem == NULL)
            continue;
          if (count > 0) {
            parse_error(PE_FATAL, init_elem->token, "More than one initializer for union");
            break;
          }

          const MemberInfo *member = sinfo->members->data[i];
          Expr *mem = new_expr_member(NULL, member->type, expr, NULL, i);
          assign_initial_value(mem, init_elem, inits);
          ++count;
        }
      }
    }
    break;
  default:
    {
      assert(init->kind == IK_SINGLE);
      Expr *value = str_to_char_array_var(curscope, init->single, toplevel);
      vec_push(inits,
                new_stmt_expr(new_expr_bop(EX_ASSIGN, expr->type, init->token, expr,
                                           make_cast(expr->type, init->token, value, false))));
    }
    break;
  }

  return inits;
}

void construct_initializing_stmts(Vector *decls) {
  for (int i = 0; i < decls->len; ++i) {
    VarDecl *decl = decls->data[i];
    if (decl->storage & (VS_STATIC | VS_EXTERN))
      continue;
    Expr *var = new_expr_variable(decl->ident->ident, decl->type, NULL, curscope);
    if (decl->init != NULL) {
      Vector *inits = assign_initial_value(var, decl->init, NULL);
      decl->init_stmt = new_stmt_block(NULL, inits, NULL, NULL);
      decl->init = NULL;
    }
  }
}

static Initializer *check_vardecl(Type **ptype, const Token *ident, int storage, Initializer *init) {
  Type *type = *ptype;
  if (!(storage & VS_EXTERN))
    ensure_struct(type, ident, curscope);
  init = flatten_initializer(type, init);
  if (type->kind == TY_ARRAY && init != NULL)
    *ptype = type = fix_array_size(type, init);

  if (curfunc != NULL) {
    VarInfo *varinfo = scope_find(curscope, ident->ident, NULL);
    varinfo->type = type;

    // TODO: Check `init` can be cast to `type`.
    if (storage & VS_STATIC) {
      VarInfo *gvarinfo = varinfo->static_.gvar;
      assert(gvarinfo != NULL);
      gvarinfo->global.init = init = check_global_initializer(type, init);
      gvarinfo->type = type;
      // static variable initializer is handled in codegen, same as global variable.
    }
  } else {
    //intptr_t eval;
    //if (find_enum_value(ident->ident, &eval))
    //  parse_error(PE_FATAL, ident, "`%.*s' is already defined", ident->ident->bytes, ident->ident->chars);
    if (storage & VS_EXTERN && init != NULL) {
      parse_error(PE_NOFATAL, init->token, "extern with initializer");
      return NULL;
    }
    // Toplevel
    VarInfo *gvarinfo = scope_find(global_scope, ident->ident, NULL);
    assert(gvarinfo != NULL);
    gvarinfo->global.init = init = check_global_initializer(type, init);
    gvarinfo->type = type;
  }
  return init;
}

static void add_func_label(const Token *tok, Stmt *label) {
  assert(curfunc != NULL);
  Table *table = curfunc->label_table;
  if (table == NULL) {
    curfunc->label_table = table = alloc_table();
  }
  if (!table_put(table, tok->ident, label))
    parse_error(PE_NOFATAL, tok, "Label `%.*s' already defined", tok->ident->bytes, tok->ident->chars);
}

static void add_func_goto(Stmt *stmt) {
  assert(curfunc != NULL);
  if (curfunc->gotos == NULL)
    curfunc->gotos = new_vector();
  vec_push(curfunc->gotos, stmt);
}

static void check_goto_labels(Function *func) {
  Table *label_table = func->label_table;

  // Check whether goto label exist.
  Vector *gotos = func->gotos;
  if (gotos != NULL) {
    for (int i = 0; i < gotos->len; ++i) {
      Stmt *stmt = gotos->data[i];
      Stmt *label;
      if (label_table != NULL && (label = table_get(label_table, stmt->goto_.label->ident)) != NULL) {
        label->label.used = true;
      } else {
        const Name *name = stmt->goto_.label->ident;
        parse_error(PE_NOFATAL, stmt->goto_.label, "`%.*s' not found", name->bytes, name->chars);
      }
    }
  }

  // Check label is used.
  if (label_table != NULL) {
    const Name *name;
    Stmt *label;
    for (int it = 0; (it = table_iterate(label_table, it, &name, (void**)&label)) != -1; ) {
      if (!label->label.used) {
        parse_error(PE_WARNING, label->token, "`%.*s' not used", name->bytes, name->chars);
        // Remove label in safely.
        table_delete(label_table, name);
        *label = *label->label.stmt;
      }
    }
  }
}

// Scope

static Scope *enter_scope(Function *func, Vector *vars) {
  Scope *scope = new_scope(curscope, vars);
  curscope = scope;
  vec_push(func->scopes, scope);
  return scope;
}

static void exit_scope(void) {
  assert(!is_global_scope(curscope));
  curscope = curscope->parent;
}

// Initializer

Initializer *parse_initializer(void) {
  Initializer *result;
  const Token *lblace_tok;
  if ((lblace_tok = match(TK_LBRACE)) != NULL) {
    Vector *multi = new_vector();
    if (!match(TK_RBRACE)) {
      for (;;) {
        Initializer *init = NULL;
        const Token *tok;
        if (match(TK_DOT)) {  // .member=value
          Token *ident = consume(TK_IDENT, "ident expected for dotted initializer");
          consume(TK_ASSIGN, "`=' expected for dotted initializer");
          Initializer *value = parse_initializer();
          if (ident != NULL) {
            init = new_initializer(IK_DOT, ident);
            init->dot.name = ident->ident;
            init->dot.value = value;
          }
        } else if ((tok = match(TK_LBRACKET)) != NULL) {
          Expr *expr = parse_const_fixnum();
          size_t index = 0;
          if (expr->fixnum < 0)
            parse_error(PE_NOFATAL, expr->token, "non negative integer required");
          else
            index = expr->fixnum;
          consume(TK_RBRACKET, "`]' expected");
          match(TK_ASSIGN);  // both accepted: `[1] = 2`, and `[1] 2`
          Initializer *value = parse_initializer();
          init = new_initializer(IK_ARR, tok);
          init->arr.index = index;
          init->arr.value = value;
        } else {
          init = parse_initializer();
        }
        if (init != NULL)
          vec_push(multi, init);

        if (match(TK_COMMA)) {
          if (match(TK_RBRACE))
            break;
        } else {
          consume(TK_RBRACE, "`}' or `,' expected");
          break;
        }
      }
    }
    result = new_initializer(IK_MULTI, lblace_tok);
    result->multi = multi;
  } else {
    Expr *value = parse_assign();
    result = new_initializer(IK_SINGLE, value->token);
    result->single = value;
  }
  return result;
}

static bool def_type(Type *type, Token *ident) {
  const Name *name = ident->ident;
  Scope *scope;
  Type *conflict = find_typedef(curscope, name, &scope);
  if (conflict != NULL && scope == curscope) {
    if (!same_type(type, conflict))
      parse_error(PE_FATAL, ident, "Conflict typedef");
  } else {
    conflict = NULL;
  }

  if (conflict == NULL || (type->kind == TY_STRUCT && type->struct_.info != NULL)) {
    if (type->kind == TY_ARRAY) {
      ensure_struct(type, ident, curscope);
    }
    add_typedef(curscope, name, type);
    return true;
  } else {
    return false;
  }
}

static Vector *parse_vardecl_cont(Type *rawType, Type *type, int storage, Token *ident) {
  Vector *decls = NULL;
  bool first = true;
  do {
    int tmp_storage = storage;
    if (!first) {
      type = parse_var_def(&rawType, &tmp_storage, &ident);
      if (type == NULL || ident == NULL) {
        parse_error(PE_FATAL, NULL, "ident expected");
        return NULL;
      }
    }
    first = false;

    if (match(TK_LPAR)) {  // Function prototype.
      bool vaargs;
      Vector *params = parse_funparams(&vaargs);
      Vector *param_types = extract_varinfo_types(params);
      type = new_func_type(type, params, param_types, vaargs);
    } else {
      not_void(type, NULL);
    }

    if (type->kind == TY_FUNC /* && !is_global_scope(curscope)*/) {
      // Must be prototype.
      tmp_storage |= VS_EXTERN;
    }

    assert(!is_global_scope(curscope));

    if (tmp_storage & VS_TYPEDEF) {
      def_type(type, ident);
      continue;
    }

    VarInfo *varinfo = add_var_to_scope(curscope, ident, type, tmp_storage);
    varinfo->type = type;  // type might be changed.
    Initializer *init = (type->kind != TY_FUNC && match(TK_ASSIGN)) ? parse_initializer() : NULL;
    init = check_vardecl(&type, ident, tmp_storage, init);
    VarDecl *decl = new_vardecl(type, ident, init, tmp_storage);
    if (decls == NULL)
      decls = new_vector();
    vec_push(decls, decl);
  } while (match(TK_COMMA));
  return decls;
}

static bool parse_vardecl(Stmt **pstmt) {
  Type *rawType = NULL;
  int storage;
  Token *ident;
  Type *type = parse_var_def(&rawType, &storage, &ident);
  if (type == NULL)
    return false;

  *pstmt = NULL;
  if (ident == NULL) {
    if ((type->kind == TY_STRUCT ||
         (type->kind == TY_FIXNUM && type->fixnum.kind == FX_ENUM)) &&
         match(TK_SEMICOL)) {
      // Just struct/union or enum definition.
    } else {
      parse_error(PE_FATAL, NULL, "ident expected");
    }
  } else {
    Vector *decls = parse_vardecl_cont(rawType, type, storage, ident);
    if (consume(TK_SEMICOL, "`;' expected")) {
      if (decls != NULL) {
        if (!is_global_scope(curscope))
          construct_initializing_stmts(decls);
        *pstmt = new_stmt_vardecl(decls);
      }
    }
  }
  return true;
}

static Stmt *parse_if(const Token *tok) {
  consume(TK_LPAR, "`(' expected");
  Expr *cond = make_cond(parse_expr());
  consume(TK_RPAR, "`)' expected");
  Stmt *tblock = parse_stmt();
  Stmt *fblock = NULL;
  if (match(TK_ELSE)) {
    fblock = parse_stmt();
  }
  return new_stmt_if(tok, cond, tblock, fblock);
}

static Stmt *parse_switch(const Token *tok) {
  consume(TK_LPAR, "`(' expected");
  Expr *value = parse_expr();
  not_void(value->type, value->token);
  consume(TK_RPAR, "`)' expected");

  Stmt *stmt = new_stmt_switch(tok, value);
  SAVE_LOOP_SCOPE(save, stmt, NULL);
  loop_scope.swtch = stmt;

  stmt->switch_.body = parse_stmt();

  RESTORE_LOOP_SCOPE(save);

  return stmt;
}

static int find_case(Stmt *swtch, Fixnum v) {
  Vector *cases = swtch->switch_.cases;
  for (int i = 0, len = cases->len; i < len; ++i) {
    Stmt *c = cases->data[i];
    if (c->case_.value == NULL)
      continue;
    if (c->case_.value->fixnum == v)
      return i;
  }
  return -1;
}

static Stmt *parse_case(const Token *tok) {
  Expr *value = parse_const_fixnum();
  consume(TK_COLON, "`:' expected");

  Stmt *stmt = NULL;
  Stmt *swtch = loop_scope.swtch;
  if (swtch == NULL) {
    parse_error(PE_NOFATAL, tok, "`case' cannot use outside of `switch`");
  } else if (find_case(swtch, value->fixnum) >= 0) {
    parse_error(PE_NOFATAL, tok, "Case value `%" PRIdPTR "' already defined", value->fixnum);
  } else {
    value = make_cast(swtch->switch_.value->type, value->token, value, false);
    stmt = new_stmt_case(tok, swtch, value);
    vec_push(swtch->switch_.cases, stmt);
  }
  return stmt;
}

static Stmt *parse_default(const Token *tok) {
  consume(TK_COLON, "`:' expected");

  Stmt *stmt = NULL;
  Stmt *swtch = loop_scope.swtch;
  if (swtch == NULL) {
    parse_error(PE_NOFATAL, tok, "`default' cannot use outside of `switch'");
  } else if (swtch->switch_.default_ != NULL) {
    parse_error(PE_NOFATAL, tok, "`default' already defined in `switch'");
  } else {
    stmt = new_stmt_default(tok, swtch);
    swtch->switch_.default_ = stmt;
    vec_push(swtch->switch_.cases, stmt);
  }
  return stmt;
}

static Stmt *parse_while(const Token *tok) {
  consume(TK_LPAR, "`(' expected");
  Expr *cond = make_cond(parse_expr());
  consume(TK_RPAR, "`)' expected");

  Stmt *stmt = new_stmt_while(tok, cond, NULL);

  SAVE_LOOP_SCOPE(save, stmt, stmt);

  stmt->while_.body = parse_stmt();

  RESTORE_LOOP_SCOPE(save);

  return stmt;
}

static Stmt *parse_do_while(const Token *tok) {
  Stmt *stmt = new_stmt(ST_DO_WHILE, tok);

  SAVE_LOOP_SCOPE(save, stmt, stmt);
  loop_scope.break_ = loop_scope.continu = stmt;

  stmt->while_.body = parse_stmt();

  RESTORE_LOOP_SCOPE(save);

  consume(TK_WHILE, "`while' expected");
  consume(TK_LPAR, "`(' expected");
  stmt->while_.cond = make_cond(parse_expr());
  consume(TK_RPAR, "`)' expected");
  consume(TK_SEMICOL, "`;' expected");
  return stmt;
}

static Stmt *parse_for(const Token *tok) {
  consume(TK_LPAR, "`(' expected");
  Expr *pre = NULL;
  Vector *decls = NULL;
  Scope *scope = NULL;
  if (!match(TK_SEMICOL)) {
    Type *rawType = NULL;
    int storage;
    Token *ident;
    Type *type = parse_var_def(&rawType, &storage, &ident);
    if (type != NULL) {
      if (ident == NULL)
        parse_error(PE_FATAL, NULL, "ident expected");
      scope = enter_scope(curfunc, NULL);
      decls = parse_vardecl_cont(rawType, type, storage, ident);
    } else {
      pre = parse_expr();
    }
    consume(TK_SEMICOL, "`;' expected");
  }

  Expr *cond = NULL;
  Expr *post = NULL;
  if (!match(TK_SEMICOL)) {
    cond = make_cond(parse_expr());
    consume(TK_SEMICOL, "`;' expected");
  }
  if (!match(TK_RPAR)) {
    post = parse_expr();
    consume(TK_RPAR, "`)' expected");
  }

  Stmt *stmt = new_stmt_for(tok, pre, cond, post, NULL);

  SAVE_LOOP_SCOPE(save, stmt, stmt);

  stmt->for_.body = parse_stmt();

  RESTORE_LOOP_SCOPE(save);

  Vector *stmts = new_vector();
  if (decls != NULL) {
    construct_initializing_stmts(decls);
    vec_push(stmts, new_stmt_vardecl(decls));
  }

  if (scope != NULL)
    exit_scope();

  vec_push(stmts, stmt);
  return new_stmt_block(tok, stmts, scope, NULL);
}

static Stmt *parse_break_continue(enum StmtKind kind, const Token *tok) {
  consume(TK_SEMICOL, "`;' expected");
  Stmt *parent = kind == ST_BREAK ? loop_scope.break_ : loop_scope.continu;
  if (parent == NULL) {
    parse_error(PE_NOFATAL, tok, "`%.*s' cannot be used outside of loop",
                        (int)(tok->end - tok->begin), tok->begin);
    return NULL;
  }
  Stmt *stmt = new_stmt(kind, tok);
  stmt->break_.parent = parent;
  return stmt;
}

static Stmt *parse_goto(const Token *tok) {
  Token *label = consume(TK_IDENT, "label for goto expected");
  consume(TK_SEMICOL, "`;' expected");

  Stmt *stmt = new_stmt_goto(tok, label);
  if (label != NULL)
    add_func_goto(stmt);
  return stmt;
}

static Stmt *parse_label(const Token *tok) {
  Stmt *stmt = new_stmt_label(tok, parse_stmt());
  add_func_label(tok, stmt);
  return stmt;
}

static Stmt *parse_return(const Token *tok) {
  Expr *val = NULL;
  if (!match(TK_SEMICOL)) {
    val = parse_expr();
    consume(TK_SEMICOL, "`;' expected");
    val = str_to_char_array_var(curscope, val, toplevel);
  }

  assert(curfunc != NULL);
  Type *rettype = curfunc->type->func.ret;
  if (val == NULL) {
    if (rettype->kind != TY_VOID)
      parse_error(PE_NOFATAL, tok, "`return' required a value");
  } else {
    if (rettype->kind == TY_VOID)
      parse_error(PE_NOFATAL, val->token, "void function `return' a value");
    else
      val = make_cast(rettype, val->token, val, false);
  }

  return new_stmt_return(tok, val);
}

static Expr *parse_asm_arg(void) {
  /*const Token *str =*/ consume(TK_STR, "string literal expected");
  consume(TK_LPAR, "`(' expected");
  Expr *var = parse_expr();
  if (var == NULL || var->kind != EX_VAR) {
    parse_error(PE_FATAL, var != NULL ? var->token : NULL, "string literal expected");
  }
  consume(TK_RPAR, "`)' expected");
  return var;
}

static Stmt *parse_asm(const Token *tok) {
  consume(TK_LPAR, "`(' expected");

  Expr *str = parse_expr();
  if (str == NULL || str->kind != EX_STR) {
    parse_error(PE_FATAL, str != NULL ? str->token : NULL, "`__asm' expected string literal");
  }

  Expr *arg = NULL;
  if (match(TK_COLON)) {
    arg = parse_asm_arg();
  }

  consume(TK_RPAR, "`)' expected");
  consume(TK_SEMICOL, "`;' expected");
  return new_stmt_asm(tok, str, arg);
}

// Multiple stmt-s, also accept `case` and `default`.
static Vector *parse_stmts(const Token **prbrace) {
  Vector *stmts = new_vector();
  for (;;) {
    Stmt *stmt;
    Token *tok;
    if (parse_vardecl(&stmt)) {
      if (stmt == NULL)
        continue;
    } else if ((tok = match(TK_CASE)) != NULL)
      stmt = parse_case(tok);
    else if ((tok = match(TK_DEFAULT)) != NULL)
      stmt = parse_default(tok);
    else
      stmt = parse_stmt();

    if (stmt == NULL) {
      if ((tok = match(TK_RBRACE)) != NULL) {
        if (prbrace != NULL)
          *prbrace = tok;
        return stmts;
      }
      parse_error(PE_FATAL, NULL, "`}' expected");
    }
    vec_push(stmts, stmt);
  }
}

Stmt *parse_block(const Token *tok, Vector *vars) {
  Scope *scope = enter_scope(curfunc, vars);
  const Token *rbrace;
  Vector *stmts = parse_stmts(&rbrace);
  Stmt *stmt = new_stmt_block(tok, stmts, scope, rbrace);
  exit_scope();
  return stmt;
}

static Stmt *parse_stmt(void) {
  Token *tok = match(-1);
  switch (tok->kind) {
  case TK_RBRACE:
  case TK_EOF:
    unget_token(tok);
    return NULL;
  case TK_IDENT:
    if (match(TK_COLON))
      return parse_label(tok);
    break;
  case TK_SEMICOL:
    return new_stmt_block(tok, NULL, NULL, NULL);
  case TK_LBRACE:
    return parse_block(tok, NULL);
  case TK_IF:
    return parse_if(tok);
  case TK_SWITCH:
    return parse_switch(tok);
  case TK_WHILE:
    return parse_while(tok);
  case TK_DO:
    return parse_do_while(tok);
  case TK_FOR:
    return parse_for(tok);
  case TK_BREAK: case TK_CONTINUE:
    return parse_break_continue(tok->kind == TK_BREAK ? ST_BREAK : ST_CONTINUE, tok);
  case TK_GOTO:
    return parse_goto(tok);
  case TK_RETURN:
    return parse_return(tok);
  case TK_ASM:
    return parse_asm(tok);
  default:
    break;
  }

  unget_token(tok);

  // expression statement.
  Expr *val = parse_expr();
  consume(TK_SEMICOL, "`;' expected");
  return new_stmt_expr(str_to_char_array_var(curscope, val, toplevel));
}

static void check_reachability(Stmt *stmt);

static int check_reachability_stmts(Vector *stmts) {
  int reach = 0;
  if (stmts != NULL) {
    for (int i = 0, n = stmts->len; i < n; ++i) {
      Stmt *stmt = stmts->data[i];
      if (reach & REACH_STOP) {
        if (!(stmt->kind == ST_LABEL || stmt->kind == ST_CASE || stmt->kind == ST_DEFAULT))
          continue;
        reach = 0;
      }
      check_reachability(stmt);
      reach |= stmt->reach;
      if (reach & REACH_STOP) {
        if (i < n - 1) {
          Stmt *next = stmts->data[i + 1];
          if (!(next->kind == ST_LABEL || next->kind == ST_CASE || next->kind == ST_DEFAULT))
            parse_error(PE_WARNING, next->token, "unreachable");
        }
      }
    }
  }
  return reach;
}

static void check_reachability(Stmt *stmt) {
  if (stmt == NULL)
    return;
  switch (stmt->kind) {
  case ST_IF:
    check_reachability(stmt->if_.tblock);
    check_reachability(stmt->if_.fblock);
    if (is_const_truthy(stmt->if_.cond)) {
      stmt->reach = stmt->if_.tblock->reach;
    } else if (is_const_falsy(stmt->if_.cond)) {
      stmt->reach = stmt->if_.fblock != NULL ? stmt->if_.fblock->reach : 0;
    } else {
      stmt->reach = stmt->if_.tblock->reach & (stmt->if_.fblock != NULL ? stmt->if_.fblock->reach : 0);
    }
    break;
  case ST_SWITCH:
    stmt->reach = (stmt->reach & ~REACH_STOP) |
        ((stmt->switch_.default_ != NULL) ? REACH_STOP : 0);
    check_reachability(stmt->switch_.body);
    stmt->reach &= stmt->switch_.body->reach;
    break;
  case ST_WHILE:
    if (!is_const_truthy(stmt->while_.cond))
      stmt->reach &= REACH_STOP;
    if (!is_const_falsy(stmt->while_.cond))
      check_reachability(stmt->while_.body);
    break;
  case ST_DO_WHILE:
    check_reachability(stmt->while_.body);
    stmt->reach = stmt->reach;  // Reload.
    if (!is_const_truthy(stmt->while_.cond))
      stmt->reach &= stmt->while_.body->reach;
    break;
  case ST_FOR:
    if (stmt->for_.cond != NULL && is_const_falsy(stmt->for_.cond)) {
      stmt->reach &= ~REACH_STOP;
    } else {
      stmt->reach = (stmt->reach & ~REACH_STOP) |
          ((stmt->for_.cond == NULL || is_const_truthy(stmt->for_.cond)) ? REACH_STOP : 0);
      check_reachability(stmt->for_.body);
    }
    break;
  case ST_BLOCK:
    stmt->reach = check_reachability_stmts(stmt->block.stmts);
    break;
  case ST_LABEL:
    check_reachability(stmt->label.stmt);
    stmt->reach = stmt->label.stmt->reach;
    break;
  case ST_RETURN:
    stmt->reach |= REACH_RETURN | REACH_STOP;
    break;
  case ST_BREAK:
    stmt->break_.parent->reach &= ~REACH_STOP;
    stmt->reach |= REACH_STOP;
    break;
  case ST_GOTO:
    // TODO:
    stmt->reach |= REACH_STOP;
    break;
  case ST_CONTINUE:
    stmt->reach |= REACH_STOP;
    break;
  default:
    stmt->reach = 0;
    break;
  }
}

static void check_funcend_return(Function *func) {
  const Type *functype = func->type;
  if (functype->func.ret->kind == TY_VOID)
    return;

  Vector *stmts = func->body_block->block.stmts;
  if (stmts->len == 0)
    return;
  Stmt *last = stmts->data[stmts->len - 1];
  if (last->kind == ST_RETURN) {
    last->return_.func_end = true;
  }
}

static Declaration *parse_defun(Type *functype, int storage, Token *ident) {
  assert(functype->kind == TY_FUNC);

  bool prototype = match(TK_SEMICOL) != NULL;
  if (!prototype && functype->func.params == NULL) { // Old-style
    // Treat it as a zero-parameter function.
    functype->func.params = new_vector();
    functype->func.param_types = new_vector();
    functype->func.vaargs = false;
  }

  Function *func = new_func(functype, ident->ident);
  VarInfo *varinfo = scope_find(global_scope, func->name, NULL);
  bool err = false;
  if (varinfo == NULL) {
    varinfo = add_var_to_scope(global_scope, ident, functype, storage);
  } else {
    if (varinfo->type->kind != TY_FUNC ||
        !same_type(varinfo->type->func.ret, functype->func.ret) ||
        (varinfo->type->func.params != NULL && !same_type(varinfo->type, functype))) {
      parse_error(PE_NOFATAL, ident, "Definition conflict: `%.*s'", func->name->bytes, func->name->chars);
      err = true;
    } else {
      if (varinfo->global.func == NULL) {
        if (varinfo->type->func.params == NULL)  // Old-style prototype definition.
          varinfo->type = functype;  // Overwrite with actual function type.
      }
    }
  }

  if (prototype) {
    // Prototype declaration.
  } else {
    const Token *tok = consume(TK_LBRACE, "`;' or `{' expected");

    if (!err && varinfo->global.func != NULL) {
      parse_error(PE_NOFATAL, ident, "`%.*s' function already defined", func->name->bytes,
                  func->name->chars);
    } else {
      varinfo->global.func = func;
    }

    assert(curfunc == NULL);
    assert(is_global_scope(curscope));
    curfunc = func;
    Vector *top_vars = NULL;
    const Vector *params = func->type->func.params;
    if (params != NULL) {
      top_vars = new_vector();
      for (int i = 0; i < params->len; ++i) {
        VarInfo *varinfo = params->data[i];
        vec_push(top_vars, varinfo);
        ensure_struct(varinfo->type, tok, curscope);
      }
    }
    func->scopes = new_vector();
    func->body_block = parse_block(tok, top_vars);
    assert(is_global_scope(curscope));

    check_goto_labels(func);

    check_reachability(func->body_block);
    if (functype->func.ret->kind != TY_VOID &&
        !(func->body_block->reach & REACH_STOP)) {
      Vector *stmts = func->body_block->block.stmts;
      if (stmts->len == 0 || ((Stmt*)stmts->data[stmts->len - 1])->kind != ST_ASM) {
        if (equal_name(func->name, alloc_name("main", NULL, false))) {
          // Return 0 if `return` statement is omitted in `main` function.
          if (!is_fixnum(functype->func.ret->kind)) {
            parse_error(PE_WARNING, func->body_block->block.rbrace, "`main' return type should be `int'");
          } else {
            vec_push(stmts, new_stmt_return(NULL, new_expr_fixlit(functype->func.ret, NULL, 0)));
          }
        } else {
          parse_error(PE_WARNING, func->body_block->block.rbrace, "`return' required");
        }
      }
    }

    check_funcend_return(func);

    curfunc = NULL;
  }
  return new_decl_defun(func);
}

static Declaration *parse_global_var_decl(
    Type *rawtype, int storage, Type *type, Token *ident
) {
  Vector *decls = NULL;
  for (;;) {
    if (!(type->kind == TY_PTR && type->pa.ptrof->kind == TY_FUNC) &&
        type->kind != TY_VOID)
      type = parse_type_suffix(type);

    if (storage & VS_TYPEDEF) {
      if (ident != NULL)
        def_type(type, ident);
    } else {
      if (type->kind == TY_VOID) {
        if (ident != NULL)
          parse_error(PE_NOFATAL, ident, "`void' not allowed");
      } else {
        VarInfo *varinfo = NULL;
        if (ident != NULL)
          varinfo = add_var_to_scope(global_scope, ident, type, storage);
        Initializer *init = NULL;
        if (match(TK_ASSIGN) != NULL) {
          init = parse_initializer();
          if (varinfo != NULL)
            varinfo->global.init = init;
        }

        if (ident != NULL) {
          init = check_vardecl(&type, ident, storage, init);
          varinfo->type = type;  // type might be changed.
          VarDecl *decl = new_vardecl(type, ident, init, storage);
          if (decls == NULL)
            decls = new_vector();
          vec_push(decls, decl);
        }
      }
    }

    if (!match(TK_COMMA))
      break;

    // Next declaration.
    type = parse_type_modifier(rawtype);
    ident = consume(TK_IDENT, "ident expected");
  }
  consume(TK_SEMICOL, "`;' or `,' expected");
  return decls == NULL ? NULL : new_decl_vardecl(decls);
}

static Declaration *parse_declaration(void) {
  Type *rawtype = NULL;
  int storage;
  Token *ident;
  Type *type = parse_var_def(&rawtype, &storage, &ident);
  if (type != NULL) {
    if (ident == NULL) {
      if ((type->kind == TY_STRUCT ||
           (type->kind == TY_FIXNUM && type->fixnum.kind == FX_ENUM)) &&
          match(TK_SEMICOL)) {
        // Just struct/union or enum definition.
      } else {
        parse_error(PE_FATAL, NULL, "ident expected");
      }
      return NULL;
    }

    if (type->kind == TY_FUNC) {
      if (storage & VS_TYPEDEF) {
        consume(TK_SEMICOL, "`;' expected");
        assert(ident != NULL);
        def_type(type, ident);
        return NULL;
      }
      return parse_defun(type, storage, ident);
    }

    return parse_global_var_decl(rawtype, storage, type, ident);
  }
  parse_error(PE_NOFATAL, NULL, "Unexpected token");
  match(-1);  // Drop the token.
  return NULL;
}

void parse(Vector *decls) {
  curscope = global_scope;

  while (!match(TK_EOF)) {
    Declaration *decl = parse_declaration();
    if (decl != NULL)
      vec_push(decls, decl);
  }
}
