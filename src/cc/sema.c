#include "sema.h"

#include <assert.h>
#include <inttypes.h>  // PRIdPTR
#include <stdlib.h>  // malloc

#include "ast.h"
#include "lexer.h"
#include "parser.h"
#include "table.h"
#include "type.h"
#include "util.h"
#include "var.h"

const int LF_BREAK = 1 << 0;
const int LF_CONTINUE = 1 << 0;

Defun *curdefun;
static int curloopflag;
static Stmt *curswitch;

static Stmt *sema_stmt(Stmt *stmt);

// Scope

static Scope *enter_scope(Defun *defun, Vector *vars) {
  Scope *scope = new_scope(curscope, vars);
  curscope = scope;
  vec_push(defun->func->scopes, scope);
  return scope;
}

static void exit_scope(void) {
  assert(curscope != NULL);
  curscope = curscope->parent;
}

static VarInfo *add_cur_scope(const Token *ident, const Type *type, int flag) {
  if (curscope->vars == NULL)
    curscope->vars = new_vector();
  return var_add(curscope->vars, ident, type, flag);
}

static void fix_array_size(Type *type, Initializer *init) {
  assert(init != NULL);
  assert(type->kind == TY_ARRAY);

  bool is_str = (is_char_type(type->pa.ptrof) &&
                 init->kind == IK_SINGLE &&
                 init->single->kind == EX_STR);
  if (!is_str && init->kind != IK_MULTI) {
    parse_error(init->token, "Error initializer");
  }

  size_t arr_len = type->pa.length;
  if (arr_len == (size_t)-1) {
    if (is_str) {
      type->pa.length = init->single->str.size;
    } else {
      size_t index = 0;
      size_t max_index = 0;
      size_t i, len = init->multi->len;
      for (i = 0; i < len; ++i) {
        Initializer *init_elem = init->multi->data[i];
        if (init_elem->kind == IK_ARR) {
          assert(init_elem->arr.index->kind == EX_NUM);
          index = init_elem->arr.index->num.ival;
        }
        ++index;
        if (max_index < index)
          max_index = index;
      }
      type->pa.length = max_index;
    }
  } else {
    assert(!is_str || init->single->kind == EX_STR);
    size_t init_len = is_str ? init->single->str.size : (size_t)init->multi->len;
    if (init_len > arr_len)
      parse_error(NULL, "Initializer more than array size");
  }
}

static void add_func_label(const Token *label) {
  assert(curdefun != NULL);
  if (curdefun->label_table == NULL) {
    curdefun->label_table = malloc(sizeof(*curdefun->label_table));
    table_init(curdefun->label_table);
  }
  BB *bb;
  if (table_try_get(curdefun->label_table, label->ident, (void**)&bb))
    parse_error(label, "Label `%.*s' already defined", label->ident->bytes, label->ident->chars);
  table_put(curdefun->label_table, label->ident, (void*)-1);  // Put dummy value.
}

static void add_func_goto(Stmt *stmt) {
  assert(curdefun != NULL);
  if (curdefun->gotos == NULL)
    curdefun->gotos = new_vector();
  vec_push(curdefun->gotos, stmt);
}

static Initializer *analyze_initializer(Initializer *init) {
  if (init == NULL)
    return NULL;

  switch (init->kind) {
  case IK_SINGLE:
    init->single = analyze_expr(init->single, false);
    break;
  case IK_MULTI:
    for (int i = 0; i < init->multi->len; ++i)
      init->multi->data[i] = analyze_initializer(init->multi->data[i]);
    break;
  case IK_DOT:
    init->dot.value = analyze_initializer(init->dot.value);
    break;
  case IK_ARR:
    init->arr.value = analyze_initializer(init->arr.value);
    break;
  }
  return init;
}

VarInfo *str_to_char_array(const Type *type, Initializer *init) {
  assert(type->kind == TY_ARRAY && is_char_type(type->pa.ptrof));
  const Token *ident = alloc_ident(alloc_label(), NULL, NULL);
  VarInfo *varinfo = define_global(type, VF_CONST | VF_STATIC, ident, NULL);
  varinfo->global.init = init;
  return varinfo;
}

static void string_initializer(Expr *dst, Initializer *src, Vector *inits) {
  // Initialize char[] with string literal (char s[] = "foo";).
  assert(src->kind == IK_SINGLE);
  const Expr *str = src->single;
  assert(str->kind == EX_STR);
  assert(dst->type->kind == TY_ARRAY && is_char_type(dst->type->pa.ptrof));

  size_t size = str->str.size;
  size_t dstsize = dst->type->pa.length;
  if (dstsize == (size_t)-1) {
    ((Type*)dst->type)->pa.length = dstsize = size;
  } else {
    if (dstsize < size)
      parse_error(NULL, "Buffer is shorter than string: %d for \"%s\"", (int)dstsize, str);
  }

  const Type* strtype = dst->type;
  VarInfo *varinfo = str_to_char_array(strtype, src);
  Expr *var = new_expr_variable(varinfo->name, strtype, NULL);

  for (size_t i = 0; i < size; ++i) {
    Num n = {.ival=i};
    Expr *index = new_expr_numlit(&tyInt, NULL, &n);
    vec_push(inits,
             new_stmt_expr(new_expr_bop(EX_ASSIGN, &tyChar, NULL,
                                        new_expr_deref(NULL, add_expr(NULL, dst, index, true)),
                                        new_expr_deref(NULL, add_expr(NULL, var, index, true)))));
  }
}

static int compare_desig_start(const void *a, const void *b) {
  const size_t *pa = *(size_t**)a;
  const size_t *pb = *(size_t**)b;
  intptr_t d = *pa - *pb;
  return d > 0 ? 1 : d < 0 ? -1 : 0;
}

static Initializer *flatten_array_initializer(Initializer *init) {
  // Check whether IK_DOT or IK_ARR exists.
  int i = 0, len = init->multi->len;
  for (; i < len; ++i) {
    Initializer *init_elem = init->multi->data[i];
    if (init_elem->kind == IK_DOT)
      parse_error(NULL, "dot initializer for array");
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
      if (i < len && init_elem->arr.index->kind != EX_NUM)
        parse_error(NULL, "Constant value expected");
      if ((size_t)i > lastStartIndex) {
        size_t *range = malloc(sizeof(size_t) * 3);
        range[0] = lastStart;
        range[1] = lastStartIndex;
        range[2] = index - lastStart;
        vec_push(ranges, range);
      }
      if (i >= len)
        break;
      lastStart = index = init_elem->arr.index->num.ival;
      lastStartIndex = i;
    } else if (init_elem->kind == IK_DOT)
      parse_error(NULL, "dot initializer for array");
  }

  // Sort
  myqsort(ranges->data, ranges->len, sizeof(size_t*), compare_desig_start);

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
        parse_error(NULL, "Initializer for array overlapped");
    }
    for (size_t j = 0; j < count; ++j) {
      Initializer *elem = init->multi->data[index + j];
      if (j == 0 && index != start && elem->kind != IK_ARR) {
        Initializer *arr = malloc(sizeof(*arr));
        arr->kind = IK_ARR;
        Num n = {.ival = start};
        arr->arr.index = new_expr_numlit(&tyInt, NULL, &n);
        arr->arr.value = elem;
        elem = arr;
      }
      vec_push(reordered, elem);
    }
  }

  Initializer *init2 = malloc(sizeof(*init2));
  init2->kind = IK_MULTI;
  init2->multi = reordered;
  return init2;
}

Initializer *flatten_initializer(const Type *type, Initializer *init) {
  if (init == NULL)
    return NULL;

  switch (type->kind) {
  case TY_STRUCT:
    if (init->kind == IK_MULTI) {
      ensure_struct((Type*)type, NULL);
      const StructInfo *sinfo = type->struct_.info;
      int n = sinfo->members->len;
      int m = init->multi->len;
      if (n <= 0) {
        if (m > 0)
          parse_error(init->token, "Initializer for empty struct");
        return NULL;
      }
      if (sinfo->is_union && m > 1)
        parse_error(((Initializer*)init->multi->data[1])->token, "Initializer for union more than 1");

      Initializer **values = malloc(sizeof(Initializer*) * n);
      for (int i = 0; i < n; ++i)
        values[i] = NULL;

      int index = 0;
      for (int i = 0; i < m; ++i) {
        Initializer *value = init->multi->data[i];
        if (value->kind == IK_ARR)
          parse_error(NULL, "indexed initializer for struct");

        if (value->kind == IK_DOT) {
          const Name *name = value->dot.name;
          index = var_find(sinfo->members, name);
          if (index >= 0) {
            value = value->dot.value;
          } else {
            Vector *stack = new_vector();
            bool res = search_from_anonymous(type, name, NULL, stack);
            if (!res)
              parse_error(value->token, "`%.*s' is not member of struct", name->bytes, name->chars);

            index = (intptr_t)stack->data[0];
            Vector *multi = new_vector();
            vec_push(multi, value);
            Initializer *init2 = malloc(sizeof(*init2));
            init2->kind = IK_MULTI;
            init2->multi = multi;
            value = init2;
          }
        }
        if (index >= n)
          parse_error(NULL, "Too many init values");

        // Allocate string literal for char* as a char array.
        if (value->kind == IK_SINGLE && value->single->kind == EX_STR) {
          const VarInfo *member = sinfo->members->data[index];
          if (member->type->kind == TY_PTR &&
              is_char_type(member->type->pa.ptrof)) {
            Expr *expr = value->single;
            Initializer *strinit = malloc(sizeof(*strinit));
            strinit->kind = IK_SINGLE;
            strinit->single = expr;

            // Create string and point to it.
            Type* strtype = arrayof(&tyChar, expr->str.size);
            const Name * label = alloc_label();
            const Token *ident = alloc_ident(label, NULL, NULL);
            VarInfo *varinfo = define_global(strtype, VF_CONST | VF_STATIC, ident, NULL);
            varinfo->global.init = strinit;

            // Replace initializer from string literal to string array defined in global.
            value->single = new_expr_variable(label, strtype, ident);
          }
        }

        values[index++] = value;
      }

      Initializer *flat = malloc(sizeof(*flat));
      flat->kind = IK_MULTI;
      Vector *v = malloc(sizeof(*v));
      v->len = v->capacity = n;
      v->data = (void**)values;
      flat->multi = v;

      return flat;
    }
    break;
  case TY_ARRAY:
    switch (init->kind) {
    case IK_MULTI:
      init = flatten_array_initializer(init);
      break;
    case IK_SINGLE:
      // Special handling for string (char[]).
      if (can_cast(type, init->single->type, init->single, false))
        break;
      // Fallthrough
    default:
      parse_error(NULL, "Illegal initializer");
      break;
    }
  default:
    break;
  }
  return init;
}

static Initializer *check_global_initializer(const Type *type, Initializer *init) {
  if (init == NULL)
    return NULL;

  init = flatten_initializer(type, init);

  switch (type->kind) {
  case TY_NUM:
    if (init->kind == IK_SINGLE) {
      switch (init->single->kind) {
      case EX_NUM:
        return init;
      default:
        parse_error(init->single->token, "Constant expression expected");
        break;
      }
    }
    break;
  case TY_PTR:
    {
      if (init->kind != IK_SINGLE)
        parse_error(NULL, "initializer type error");

      Expr *value = init->single;
      while (value->kind == EX_CAST || value->kind == EX_GROUP) {
        value = value->unary.sub;
      }

      switch (value->kind) {
      case EX_REF:
        {
          value = value->unary.sub;
          if (value->kind != EX_VARIABLE)
            parse_error(value->token, "pointer initializer must be variable");
          if (value->variable.scope != NULL)
            parse_error(value->token, "Allowed global reference only");

          VarInfo *info = find_global(value->variable.name);
          assert(info != NULL);

          if (!same_type(type->pa.ptrof, info->type))
            parse_error(value->token, "Illegal type");

          return init;
        }
      case EX_VARIABLE:
        {
          if (value->variable.scope != NULL)
            parse_error(value->token, "Allowed global reference only");

          VarInfo *info = find_global(value->variable.name);
          assert(info != NULL);

          if ((info->type->kind != TY_ARRAY && info->type->kind != TY_FUNC) ||
              !can_cast(type, info->type, value, false))
            parse_error(value->token, "Illegal type");

          return init;
        }
      case EX_NUM:
        {
          Initializer *init2 = malloc(sizeof(*init2));
          init2->kind = IK_SINGLE;
          init2->single = value;
          return init2;
        }
      case EX_STR:
        {
          if (!(is_char_type(type->pa.ptrof) && value->kind == EX_STR))
            parse_error(value->token, "Illegal type");

          // Create string and point to it.
          Type* strtype = arrayof(type->pa.ptrof, value->str.size);
          VarInfo *varinfo = str_to_char_array(strtype, init);

          Initializer *init2 = malloc(sizeof(*init2));
          init2->kind = IK_SINGLE;
          init2->single = new_expr_variable(varinfo->name, strtype, NULL);
          return init2;
        }
      default:
        break;
      }
      parse_error(value->token, "initializer type error: kind=%d", value->kind);
    }
    break;
  case TY_ARRAY:
    switch (init->kind) {
    case IK_MULTI:
      {
        const Type *elemtype = type->pa.ptrof;
        Vector *multi = init->multi;
        for (int i = 0, len = multi->len; i < len; ++i) {
          Initializer *eleminit = multi->data[i];
          multi->data[i] = check_global_initializer(elemtype, eleminit);
        }
      }
      break;
    case IK_SINGLE:
      if (is_char_type(type->pa.ptrof) && init->single->kind == EX_STR) {
        assert(type->pa.length != (size_t)-1);
        if (type->pa.length < init->single->str.size) {
          parse_error(init->single->token, "Array size shorter than initializer");
        }
        break;
      }
      // Fallthrough
    case IK_DOT:
    default:
      parse_error(NULL, "Illegal initializer");
      break;
    }
    break;
  case TY_STRUCT:
    {
      const StructInfo *sinfo = type->struct_.info;
      for (int i = 0, n = sinfo->members->len; i < n; ++i) {
        const VarInfo* member = sinfo->members->data[i];
        Initializer *init_elem = init->multi->data[i];
        if (init_elem != NULL)
          init->multi->data[i] = check_global_initializer(member->type, init_elem);
      }
    }
    break;
  default:
    parse_error(NULL, "Global initial value for type %d not implemented (yet)\n", type->kind);
    break;
  }
  return init;
}

static Vector *assign_initial_value(Expr *expr, Initializer *init, Vector *inits) {
  if (init == NULL)
    return inits;

  if (inits == NULL)
    inits = new_vector();

  Initializer *org_init = init;
  init = flatten_initializer(expr->type, init);

  switch (expr->type->kind) {
  case TY_ARRAY:
    switch (init->kind) {
    case IK_MULTI:
      {
        size_t arr_len = expr->type->pa.length;
        assert(arr_len != (size_t)-1);
        if ((size_t)init->multi->len > arr_len)
          parse_error(NULL, "Initializer more than array size");
        size_t len = init->multi->len;
        size_t index = 0;
        for (size_t i = 0; i < len; ++i, ++index) {
          Initializer *init_elem = init->multi->data[i];
          if (init_elem->kind == IK_ARR) {
            Expr *ind = init_elem->arr.index;
            if (ind->kind != EX_NUM)
              parse_error(NULL, "Number required");
            index = ind->num.ival;
            init_elem = init_elem->arr.value;
          }

          Num n = {.ival=index};
          Expr *add = add_expr(NULL, expr, new_expr_numlit(&tyInt, NULL, &n), true);

          assign_initial_value(new_expr_deref(NULL, add), init_elem, inits);
        }
      }
      break;
    case IK_SINGLE:
      // Special handling for string (char[]).
      if (is_char_type(expr->type->pa.ptrof) &&
          init->single->kind == EX_STR) {
        string_initializer(expr, init, inits);
        break;
      }
      // Fallthrough
    default:
      parse_error(init->token, "Error initializer");
      break;
    }
    break;
  case TY_STRUCT:
    {
      if (init->kind != IK_MULTI) {
        vec_push(inits,
                 new_stmt_expr(new_expr_bop(EX_ASSIGN, expr->type, NULL, expr,
                                            init->single)));
        break;
      }

      const StructInfo *sinfo = expr->type->struct_.info;
      if (!sinfo->is_union) {
        for (int i = 0, n = sinfo->members->len; i < n; ++i) {
          const VarInfo* member = sinfo->members->data[i];
          Expr *mem = new_expr_member(NULL, member->type, expr, NULL, i);
          Initializer *init_elem = init->multi->data[i];
          if (init_elem != NULL)
            assign_initial_value(mem, init_elem, inits);
        }
      } else {
        int n = sinfo->members->len;
        int m = init->multi->len;
        if (n <= 0 && m > 0)
          parse_error(NULL, "Initializer for empty union");
        if (org_init->multi->len > 1)
          parse_error(NULL, "More than one initializer for union");

        for (int i = 0; i < n; ++i) {
          Initializer *init_elem = init->multi->data[i];
          if (init_elem == NULL)
            continue;
          const VarInfo* member = sinfo->members->data[i];
          Expr *mem = new_expr_member(NULL, member->type, expr, NULL, i);
          assign_initial_value(mem, init_elem, inits);
          break;
        }
      }
    }
    break;
  default:
    if (init->kind != IK_SINGLE)
      parse_error(init->token, "Error initializer");
    vec_push(inits,
             new_stmt_expr(new_expr_bop(EX_ASSIGN, expr->type, NULL, expr,
                                        make_cast(expr->type, NULL, init->single, false))));
    break;
  }

  return inits;
}

static Vector *sema_vardecl(Vector *decls) {
  Vector *inits = NULL;
  for (int i = 0, len = decls->len; i < len; ++i) {
    VarDecl *decl = decls->data[i];
    const Type *type = decl->type;
    const Token *ident = decl->ident;
    int flag = decl->flag;
    Initializer *init = decl->init;

    if (type->kind == TY_ARRAY && init != NULL)
      fix_array_size((Type*)type, init);

    if (curdefun != NULL) {
      VarInfo *varinfo = add_cur_scope(ident, type, flag);
      init = analyze_initializer(init);

      // TODO: Check `init` can be cast to `type`.
      if (flag & VF_STATIC) {
        varinfo->global.init = check_global_initializer(type, init);
        // static variable initializer is handled in codegen, same as global variable.
      } else if (init != NULL) {
        Expr *var = new_expr_variable(ident->ident, type, NULL);
        var->variable.scope = curscope;
        inits = assign_initial_value(var, init, inits);
      }
    } else {
      intptr_t eval;
      if (find_enum_value(ident->ident, &eval))
        parse_error(ident, "`%.*s' is already defined", ident->ident->bytes, ident->ident->chars);
      if (flag & VF_EXTERN && init != NULL)
        parse_error(init->token, "extern with initializer");
      // Toplevel
      VarInfo *varinfo = define_global(type, flag, ident, NULL);
      init = analyze_initializer(init);
      varinfo->global.init = check_global_initializer(type, init);
    }
  }

  return inits;
}

static void sema_stmts(Vector *stmts) {
  assert(stmts != NULL);
  for (int i = 0, len = stmts->len; i < len; ++i)
    stmts->data[i] = sema_stmt(stmts->data[i]);
}

static void sema_defun(Defun *defun) {
  const Token *ident = NULL;

  VarInfo *def = find_global(defun->func->name);
  if (def == NULL) {
    define_global(defun->func->type, defun->flag | VF_CONST, ident, defun->func->name);
  } else {
    if (def->type->kind != TY_FUNC)
      parse_error(ident, "Definition conflict: `%s'");
    // TODO: Check type.
    // TODO: Check duplicated definition.
    if (def->global.init != NULL)
      parse_error(ident, "`%.*s' function already defined", defun->func->name->bytes, defun->func->name->chars);
  }

  if (defun->stmts != NULL) {  // Not prototype defintion.
    curdefun = defun;
    Vector *top_vars = NULL;
    Vector *params = defun->func->params;
    if (params != NULL) {
      top_vars = new_vector();
      for (int i = 0; i < params->len; ++i)
        vec_push(top_vars, params->data[i]);
    }
    defun->func->scopes = new_vector();
    curscope = enter_scope(defun, top_vars);  // Scope for parameters.
    sema_stmts(defun->stmts);
    exit_scope();
    curdefun = NULL;
    curscope = NULL;

    // Check goto labels.
    if (defun->gotos != NULL) {
      Vector *gotos = defun->gotos;
      Table *label_table = defun->label_table;
      for (int i = 0; i < gotos->len; ++i) {
        Stmt *stmt = gotos->data[i];
        void *bb;
        if (label_table == NULL || !table_try_get(label_table, stmt->goto_.label->ident, &bb)) {
          const Name *name = stmt->goto_.label->ident;
          parse_error(stmt->goto_.label, "`%.*s' not found", name->bytes, name->chars);
        }
      }
    }
  }
}

static Stmt *sema_stmt(Stmt *stmt) {
  if (stmt == NULL)
    return stmt;

  switch (stmt->kind) {
  case ST_EXPR:
    stmt->expr = analyze_expr(stmt->expr, false);
    break;

  case ST_BLOCK:
    {
      Scope *parent_scope = curscope;
      if (curdefun != NULL)
        stmt->block.scope = curscope = enter_scope(curdefun, NULL);
      sema_stmts(stmt->block.stmts);
      curscope = parent_scope;
    }
    break;

  case ST_IF:
    stmt->if_.cond = analyze_expr(stmt->if_.cond, false);
    stmt->if_.tblock = sema_stmt(stmt->if_.tblock);
    stmt->if_.fblock = sema_stmt(stmt->if_.fblock);
    break;

  case ST_SWITCH:
    {
      Stmt *save_switch = curswitch;
      int save_flag = curloopflag;
      curloopflag |= LF_BREAK;
      curswitch = stmt;

      stmt->switch_.value = analyze_expr(stmt->switch_.value, false);
      stmt->switch_.body = sema_stmt(stmt->switch_.body);

      curloopflag = save_flag;
      curswitch = save_switch;
    }
    break;

  case ST_WHILE:
  case ST_DO_WHILE:
    {
      stmt->while_.cond = analyze_expr(stmt->while_.cond, false);

      int save_flag = curloopflag;
      curloopflag |= LF_BREAK | LF_CONTINUE;

      stmt->while_.body = sema_stmt(stmt->while_.body);

      curloopflag = save_flag;
    }
    break;

  case ST_FOR:
    {
      stmt->for_.pre = analyze_expr(stmt->for_.pre, false);
      stmt->for_.cond = analyze_expr(stmt->for_.cond, false);
      stmt->for_.post = analyze_expr(stmt->for_.post, false);

      int save_flag = curloopflag;
      curloopflag |= LF_BREAK | LF_CONTINUE;

      stmt->for_.body = sema_stmt(stmt->for_.body);

      curloopflag = save_flag;
    }
    break;

  case ST_BREAK:
    if ((curloopflag & LF_BREAK) == 0)
      parse_error(stmt->token, "`break' cannot be used outside of loop");
    break;

  case ST_CONTINUE:
    if ((curloopflag & LF_CONTINUE) == 0)
      parse_error(stmt->token, "`continue' cannot be used outside of loop");
    break;

  case ST_RETURN:
    {
      assert(curdefun != NULL);
      const Type *rettype = curdefun->func->type->func.ret;
      Expr *val = stmt->return_.val;
      if (val == NULL) {
        if (rettype->kind != TY_VOID)
          parse_error(stmt->token, "`return' required a value");
      } else {
        if (rettype->kind == TY_VOID)
          parse_error(val->token, "void function `return' a value");

        val = analyze_expr(val, false);
        stmt->return_.val = make_cast(rettype, val->token, val, false);
      }
    }
    break;

  case ST_CASE:
    {
      if (curswitch == NULL)
        parse_error(stmt->case_.value->token, "`case' cannot use outside of `switch`");

      stmt->case_.value = analyze_expr(stmt->case_.value, false);
      if (!is_number(stmt->case_.value->type->kind))
        parse_error(stmt->case_.value->token, "Cannot use expression");
      intptr_t value = stmt->case_.value->num.ival;

      // Check duplication.
      Vector *values = curswitch->switch_.case_values;
      for (int i = 0, len = values->len; i < len; ++i) {
        if ((intptr_t)values->data[i] == value)
          parse_error(stmt->case_.value->token, "Case value `%"PRIdPTR"' already defined", value);
      }
      vec_push(values, (void*)value);
    }
    break;

  case ST_DEFAULT:
    if (curswitch == NULL)
      parse_error(stmt->token, "`default' cannot use outside of `switch'");
    if (curswitch->switch_.has_default)
      parse_error(stmt->token, "`default' already defined in `switch'");

    curswitch->switch_.has_default = true;
    break;

  case ST_GOTO:
    add_func_goto(stmt);
    break;

  case ST_LABEL:
    add_func_label(stmt->token);
    stmt->label.stmt = sema_stmt(stmt->label.stmt);
    break;

  case ST_VARDECL:
    stmt->vardecl.inits = sema_vardecl(stmt->vardecl.decls);
    break;

  case ST_ASM:
    break;

  default:
    fprintf(stderr, "sema: Unhandled stmt, kind=%d\n", stmt->kind);
    assert(false);
    break;
  }
  return stmt;
}

static Declaration *sema_decl(Declaration *decl) {
  if (decl == NULL)
    return decl;

  switch (decl->kind) {
  case DCL_DEFUN:
    sema_defun(decl->defun);
    break;

  case DCL_VARDECL:
    {
      Vector *inits = sema_vardecl(decl->vardecl.decls);
      UNUSED(inits);
      assert(inits == NULL);
    }
    break;

  default:
    fprintf(stderr, "sema: Unhandled decl, kind=%d\n", decl->kind);
    assert(false);
    break;
  }
  return decl;
}

void sema(Vector *toplevel) {
  if (toplevel == NULL)
    return;

  for (int i = 0, len = toplevel->len; i < len; ++i)
    toplevel->data[i] = sema_decl(toplevel->data[i]);
}
