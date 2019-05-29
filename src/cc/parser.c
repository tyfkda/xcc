#include <assert.h>
#include <stdbool.h>
#include <stdlib.h>  // malloc
#include <string.h>

#include "xcc.h"
#include "expr.h"
#include "lexer.h"
#include "util.h"

const int LF_BREAK = 1 << 0;
const int LF_CONTINUE = 1 << 0;

static int curloopflag;
static Defun *curfunc;
static Node *curswitch;

static Node *stmt(void);

static Expr *parse_analyze_expr(void) {
  return analyze_expr(parse_expr(), false);
}

static Defun *new_defun(const Type *type, const char *name) {
  Defun *defun = malloc(sizeof(*defun));
  defun->type = type;
  defun->name = name;
  defun->top_scope = NULL;
  defun->stmts = NULL;
  defun->all_scopes = new_vector();
  defun->labels = NULL;
  defun->gotos = NULL;
  defun->ret_label = NULL;
  return defun;
}

static void add_func_label(const char *label) {
  assert(curfunc != NULL);
  if (curfunc->labels == NULL)
    curfunc->labels = new_map();
  map_put(curfunc->labels, label, label);  // Put dummy value.
}

static void add_func_goto(Node *node) {
  assert(curfunc != NULL);
  if (curfunc->gotos == NULL)
    curfunc->gotos = new_vector();
  vec_push(curfunc->gotos, node);
}

static Node *new_node(enum NodeType type) {
  Node *node = malloc(sizeof(Node));
  node->type = type;
  return node;
}

static Node *new_node_expr(Expr *e) {
  Node *node = new_node(ND_EXPR);
  node->u.expr = e;
  return node;
}

static Node *new_node_block(Scope *scope, Vector *nodes) {
  Node *node = new_node(ND_BLOCK);
  node->u.block.scope = scope;
  node->u.block.nodes = nodes;
  return node;
}

static Node *new_node_if(Expr *cond, Node *tblock, Node *fblock) {
  Node *node = new_node(ND_IF);
  node->u.if_.cond = cond;
  node->u.if_.tblock = tblock;
  node->u.if_.fblock = fblock;
  return node;
}

static Node *new_node_switch(Expr *value) {
  Node *node = new_node(ND_SWITCH);
  node->u.switch_.value = value;
  node->u.switch_.body = NULL;
  node->u.switch_.case_values = NULL;
  node->u.switch_.has_default = false;
  return node;
}

static Node *new_node_case(int value) {
  Node *node = new_node(ND_CASE);
  node->u.case_.value = value;
  return node;
}

static Node *new_node_default(void) {
  Node *node = new_node(ND_DEFAULT);
  return node;
}

static Node *new_node_while(Expr *cond, Node *body) {
  Node *node = new_node(ND_WHILE);
  node->u.while_.cond = cond;
  node->u.while_.body = body;
  return node;
}

static Node *new_node_do_while(Node *body, Expr *cond) {
  Node *node = new_node(ND_DO_WHILE);
  node->u.do_while.body = body;
  node->u.do_while.cond = cond;
  return node;
}

static Node *new_node_for(Expr *pre, Expr *cond, Expr *post, Node *body) {
  Node *node = new_node(ND_FOR);
  node->u.for_.pre = pre;
  node->u.for_.cond = cond;
  node->u.for_.post = post;
  node->u.for_.body = body;
  return node;
}

static Node *new_node_return(Expr *val) {
  Node *node = new_node(ND_RETURN);
  node->u.return_.val = val;
  return node;
}

static Node *new_node_goto(const Token *label) {
  Node *node = new_node(ND_GOTO);
  node->u.goto_.tok = label;
  node->u.goto_.ident = label->u.ident;
  return node;
}

static Node *new_node_label(const char *name, Node *stmt) {
  Node *node = new_node(ND_LABEL);
  node->u.label.name = name;
  node->u.label.stmt = stmt;
  return node;
}

static Node *new_node_defun(Defun *defun) {
  Node *node = new_node(ND_DEFUN);
  node->u.defun = defun;
  return node;
}

static Node *parse_if(void) {
  if (consume(TK_LPAR)) {
    Expr *cond = parse_analyze_expr();
    if (consume(TK_RPAR)) {
      Node *tblock = stmt();
      Node *fblock = NULL;
      if (consume(TK_ELSE)) {
        fblock = stmt();
      }
      return new_node_if(cond, tblock, fblock);
    }
  }
  parse_error(NULL, "Illegal syntax in `if'");
  return NULL;
}

static Node *parse_switch(void) {
  if (consume(TK_LPAR)) {
    Expr *value = parse_analyze_expr();
    if (consume(TK_RPAR)) {
      Node *swtch = new_node_switch(value);

      Node *save_switch = curswitch;
      int save_flag = curloopflag;
      curloopflag |= LF_BREAK;
      curswitch = swtch;

      swtch->u.switch_.body = stmt();

      curloopflag = save_flag;
      curswitch = save_switch;

      return swtch;
    }
  }
  parse_error(NULL, "Illegal syntax in `switch'");
  return NULL;
}

static Node *parse_case(Token *tok) {
  if (curswitch == NULL)
    parse_error(tok, "`case' cannot use outside of `switch`");

  tok = fetch_token();
  Expr *valnode = analyze_expr(parse_const(), false);
  if (!is_const(valnode))
    parse_error(tok, "Cannot use expression");
  intptr_t value = valnode->u.value;

  if (!consume(TK_COLON))
    parse_error(NULL, "`:' expected");

  Vector *values = curswitch->u.switch_.case_values;
  if (values == NULL)
    curswitch->u.switch_.case_values = values = new_vector();

  // Check duplication.
  for (int i = 0, len = values->len; i < len; ++i) {
    if ((intptr_t)values->data[i] == value)
      parse_error(tok, "Case value `%lld' already defined: %s", value);
  }

  vec_push(values, (void*)value);

  return new_node_case(value);
}

static Node *parse_default(Token *tok) {
  if (curswitch == NULL)
    parse_error(tok, "`default' cannot use outside of `switch'");
  if (curswitch->u.switch_.has_default)
    parse_error(tok, "`default' already defined in `switch'");

  if (!consume(TK_COLON))
    parse_error(NULL, "`:' expected");

  curswitch->u.switch_.has_default = true;

  return new_node_default();
}

static Node *parse_while(void) {
  if (consume(TK_LPAR)) {
    Expr *cond = parse_analyze_expr();
    if (consume(TK_RPAR)) {
      int save_flag = curloopflag;
      curloopflag |= LF_BREAK | LF_CONTINUE;
      Node *body = stmt();
      curloopflag = save_flag;

      return new_node_while(cond, body);
    }
  }
  parse_error(NULL, "Illegal syntax in `while'");
  return NULL;
}

static Node *parse_do_while(void) {
  int save_flag = curloopflag;
  curloopflag |= LF_BREAK | LF_CONTINUE;
  Node *body = stmt();
  curloopflag = save_flag;

  if (consume(TK_WHILE)) {
    if (consume(TK_LPAR)) {
      Expr *cond = parse_analyze_expr();
      if (consume(TK_RPAR) && consume(TK_SEMICOL)) {
        return new_node_do_while(body, cond);
      }
    }
  }
  parse_error(NULL, "Illegal syntax in `do-while'");
  return NULL;
}

static Vector *parse_vardecl_cont(const Type *rawType, Type *type, int flag, Token *ident);
static Node *parse_for(void) {
  Scope *scope = NULL;
  if (consume(TK_LPAR)) {
    assert(curfunc != NULL);
    Expr *pre = NULL;
    bool nopre = false;
    Vector *stmts = NULL;
    if (consume(TK_SEMICOL)) {
      nopre = true;
    } else {
      const Type *rawType = NULL;
      Type *type;
      int flag;
      Token *ident;
      if (parse_var_def(&rawType, (const Type**)&type, &flag, &ident)) {
        if (ident == NULL)
          parse_error(NULL, "Ident expected");
        scope = enter_scope(curfunc, NULL);
        stmts = parse_vardecl_cont(rawType, type, flag, ident);
        if (!consume(TK_SEMICOL))
          scope = NULL;  // Error
      } else {
        pre = parse_analyze_expr();
        if (!consume(TK_SEMICOL))
          pre = NULL;  // Error
      }
    }
    if (nopre || pre != NULL || scope != NULL) {
      Expr *cond = NULL;
      Expr *post = NULL;
      Node *body = NULL;
      if ((consume(TK_SEMICOL) || (cond = parse_analyze_expr(), consume(TK_SEMICOL))) &&
          (consume(TK_RPAR) || (post = parse_analyze_expr(), consume(TK_RPAR)))) {
        int save_flag = curloopflag;
        curloopflag |= LF_BREAK | LF_CONTINUE;
        body = stmt();
        curloopflag = save_flag;

        Node *node = new_node_for(pre, cond, post, body);
        if (scope != NULL) {
          exit_scope();
          if (stmts == NULL)
            stmts = new_vector();
          vec_push(stmts, node);
          return new_node_block(scope, stmts);
        } else {
          return node;
        }
      }
    }
  }
  if (scope != NULL)
    exit_scope();
  parse_error(NULL, "Illegal syntax in `for'");
  return NULL;
}

static Node *parse_break_continue(enum NodeType type) {
  if (!consume(TK_SEMICOL))
    parse_error(NULL, "`;' expected");
  return new_node(type);
}

static Node *parse_goto(void) {
  Token *label = consume(TK_IDENT);
  if (label == NULL)
    parse_error(NULL, "label for goto expected");
  if (!consume(TK_SEMICOL))
    parse_error(NULL, "`;' expected");
  Node *node = new_node_goto(label);
  assert(curfunc != NULL);
  add_func_goto(node);
  return node;
}

static Node *parse_return(void) {
  assert(curfunc != NULL);

  Expr *val = NULL;
  Token *tok;
  const Type *rettype = curfunc->type->u.func.ret;
  if ((tok = consume(TK_SEMICOL)) != NULL) {
    if (rettype->type != TY_VOID)
      parse_error(tok, "`return' required a value");
  } else {
    tok = fetch_token();
    val = parse_analyze_expr();
    if (!consume(TK_SEMICOL))
      parse_error(NULL, "`;' expected");

    if (rettype->type == TY_VOID)
      parse_error(tok, "void function `return' a value");
    val = new_expr_cast(rettype, tok, val, false);
  }
  return new_node_return(val);
}

// Initializer

static Initializer *parse_initializer(void) {
  Initializer *result = malloc(sizeof(*result));
  if (consume(TK_LBRACE)) {
    Vector *multi = new_vector();
    if (!consume(TK_RBRACE)) {
      for (;;) {
        Initializer *init;
        if (consume(TK_DOT)) {  // .member=value
          Token *ident = consume(TK_IDENT);
          if (ident == NULL)
            parse_error(NULL, "`ident' expected for dotted initializer");
          if (!consume(TK_ASSIGN))
            parse_error(NULL, "`=' expected for dotted initializer");
          Initializer *value = parse_initializer();
          init = malloc(sizeof(*init));
          init->type = vDot;
          init->u.dot.name = ident->u.ident;
          init->u.dot.value = value;
        } else {
          init = parse_initializer();
        }
        vec_push(multi, init);

        if (consume(TK_COMMA)) {
          if (consume(TK_RBRACE))
            break;
        } else {
          if (!consume(TK_RBRACE))
            parse_error(NULL, "`}' or `,' expected");
          break;
        }
      }
    }
    result->type = vMulti;
    result->u.multi = multi;
  } else {
    result->type = vSingle;
    result->u.single = analyze_expr(parse_assign(), false);
  }
  return result;
}

static Vector *clear_initial_value(Expr *expr, Vector *inits) {
  if (inits == NULL)
    inits = new_vector();

  switch (expr->valType->type) {
  case TY_CHAR:
  case TY_INT:
  case TY_LONG:
  case TY_ENUM:
    vec_push(inits,
             new_node_expr(new_expr_bop(EX_ASSIGN, expr->valType, NULL, expr,
                                        new_expr_cast(expr->valType, NULL, new_expr_numlit(EX_INT, NULL, 0), true))));
    break;
  case TY_PTR:
    vec_push(inits,
             new_node_expr(new_expr_bop(EX_ASSIGN, expr->valType, NULL, expr,
                                        new_expr_cast(expr->valType, NULL, new_expr_numlit(EX_LONG, NULL, 0), true))));  // intptr_t
    break;
  case TY_ARRAY:
    {
      size_t arr_len = expr->valType->u.pa.length;
      for (size_t i = 0; i < arr_len; ++i)
        clear_initial_value(new_expr_deref(NULL,
                                           add_expr(NULL, expr, new_expr_numlit(EX_INT, NULL, i), true)),
                            inits);
    }
    break;
  case TY_STRUCT:
    {
      const StructInfo *sinfo = expr->valType->u.struct_.info;
      assert(sinfo != NULL);
      for (int i = 0; i < sinfo->members->len; ++i) {
        VarInfo* varinfo = sinfo->members->data[i];
        Expr *member = new_expr_member(NULL, varinfo->type, expr, NULL, NULL, i);
        clear_initial_value(member, inits);
      }
    }
    break;
  case TY_UNION:
  default:
    assert(!"Not implemented");
    break;
  }

  return inits;
}

static void string_initializer(Expr *dst, Expr *src, Vector *inits) {
  // Initialize char[] with string literal (char s[] = "foo";).
  assert(dst->valType->type == TY_ARRAY && dst->valType->u.pa.ptrof->type == TY_CHAR);
  assert(src->valType->type == TY_ARRAY && src->valType->u.pa.ptrof->type == TY_CHAR);

  const char *str = src->u.str.buf;
  size_t size = src->u.str.size;
  size_t dstsize = dst->valType->u.pa.length;
  if (dstsize == (size_t)-1) {
    ((Type*)dst->valType)->u.pa.length = dstsize = size;
  } else {
    if (dstsize < size)
      parse_error(NULL, "Buffer is shorter than string: %d for \"%s\"", (int)dstsize, str);
  }

  for (size_t i = 0; i < size; ++i) {
    Expr *index = new_expr_numlit(EX_INT, NULL, i);
    vec_push(inits,
             new_node_expr(new_expr_bop(EX_ASSIGN, &tyChar, NULL,
                                        new_expr_deref(NULL, add_expr(NULL, dst, index, true)),
                                        new_expr_deref(NULL, add_expr(NULL, src, index, true)))));
  }
  if (dstsize > size) {
    Expr *zero = new_expr_numlit(EX_CHAR, NULL, 0);
    for (size_t i = size; i < dstsize; ++i) {
      Expr *index = new_expr_numlit(EX_INT, NULL, i);
      vec_push(inits,
               new_node_expr(new_expr_bop(EX_ASSIGN, &tyChar, NULL,
                                          new_expr_deref(NULL, add_expr(NULL, dst, index, true)),
                                          zero)));
    }
  }
}

static void fix_array_size(Type *type, Initializer *init) {
  if (type->type != TY_ARRAY)
    return;

  bool is_str = false;
  if (init->type != vMulti &&
      !(type->u.pa.ptrof->type == TY_CHAR &&
        init->type == vSingle &&
        can_cast(type, init->u.single->valType, init->u.single, false) &&
        (is_str = true))) {
    parse_error(NULL, "Error initializer");
  }

  size_t arr_len = type->u.pa.length;
  if (arr_len == (size_t)-1) {
    type->u.pa.length = is_str ? init->u.single->u.str.size : (size_t)init->u.multi->len;
  } else {
    if ((size_t)init->u.multi->len > arr_len)
      parse_error(NULL, "Initializer more than array size");
  }
}

static Vector *assign_initial_value(Expr *expr, Initializer *init, Vector *inits) {
  if (inits == NULL)
    inits = new_vector();

  switch (expr->valType->type) {
  case TY_ARRAY:
    {
      // Special handling for string (char[]).
      if (expr->valType->u.pa.ptrof->type == TY_CHAR &&
          init->type == vSingle &&
          can_cast(expr->valType, init->u.single->valType, init->u.single, false)) {
        string_initializer(expr, init->u.single, inits);
        break;
      }

      if (init->type != vMulti)
        parse_error(NULL, "Error initializer");
      size_t arr_len = expr->valType->u.pa.length;
      assert(arr_len != (size_t)-1);
      if ((size_t)init->u.multi->len > arr_len)
        parse_error(NULL, "Initializer more than array size");
      int len = init->u.multi->len;
      for (int i = 0; i < len; ++i) {
        assign_initial_value(new_expr_deref(NULL, add_expr(NULL, expr, new_expr_numlit(EX_INT, NULL, i), true)),
                             init->u.multi->data[i], inits);
      }
      // Clear left.
      for (size_t i = len; i < arr_len; ++i)
        clear_initial_value(new_expr_deref(NULL, add_expr(NULL, expr, new_expr_numlit(EX_INT, NULL, i), true)), inits);
    }
    break;
  case TY_STRUCT:
    {
      if (init->type != vMulti)
        parse_error(NULL, "`{...}' expected for initializer");

      ensure_struct((Type*)expr->valType, NULL);
      const StructInfo *sinfo = expr->valType->u.struct_.info;
      int n = sinfo->members->len;
      int m = init->u.multi->len;
      if (n <= 0) {
        if (m > 0)
          parse_error(NULL, "Initializer for empty struct");
        break;
      }
      Initializer **values = malloc(sizeof(Initializer*) * n);
      for (int i = 0; i < n; ++i)
        values[i] = NULL;

      int dst = -1;
      for (int i = 0; i < m; ++i) {
        Initializer *value = init->u.multi->data[i];
        if (value->type == vDot) {
          int idx = var_find(sinfo->members, value->u.dot.name);
          if (idx < 0)
            parse_error(NULL, "`%s' is not member of struct", value->u.dot.name);
          values[idx] = value->u.dot.value;
          dst = idx;
          continue;
        }
        if (++dst >= n)
          break;  // TODO: Check extra.
        values[dst] = value;
      }
      for (int i = 0; i < n; ++i) {
        VarInfo* varinfo = sinfo->members->data[i];
        Expr *member = new_expr_member(NULL, varinfo->type, expr, NULL, NULL, i);
        if (values[i] != NULL)
          assign_initial_value(member, values[i], inits);
        else
          clear_initial_value(member, inits);
      }
    }
    break;
  case TY_UNION:
    {
      if (init->type != vMulti)
        parse_error(NULL, "`{...}' expected for initializer");

      const StructInfo *sinfo = expr->valType->u.struct_.info;
      ensure_struct((Type*)expr->valType, NULL);
      int n = sinfo->members->len;
      int m = init->u.multi->len;
      if (n <= 0 && m > 0)
        parse_error(NULL, "Initializer for empty union");

      int dst = 0;
      Initializer *value = init->u.multi->data[0];
      if (value->type == vDot) {
        int idx = var_find(sinfo->members, value->u.dot.name);
        if (idx < 0)
          parse_error(NULL, "`%s' is not member of struct", value->u.dot.name);
        dst = idx;
        value = value->u.dot.value;
      }
      VarInfo* varinfo = sinfo->members->data[dst];
      Expr *member = new_expr_member(NULL, varinfo->type, expr, NULL, NULL, dst);
      assign_initial_value(member, value, inits);
    }
    break;
  default:
    if (init->type != vSingle)
      parse_error(NULL, "Error initializer");
    vec_push(inits,
             new_node_expr(new_expr_bop(EX_ASSIGN, expr->valType, NULL, expr,
                                        new_expr_cast(expr->valType, NULL, init->u.single, false))));
    break;
  }

  return inits;
}

static Vector *parse_vardecl_cont(const Type *rawType, Type *type, int flag, Token *ident) {
  Vector *inits = NULL;
  bool first = true;
  do {
    if (!first) {
      if (!parse_var_def(&rawType, (const Type**)&type, &flag, &ident) || ident == NULL) {
        parse_error(NULL, "`ident' expected");
        return NULL;
      }
    }
    first = false;
    not_void(type);

    VarInfo *varinfo = add_cur_scope(ident, type, flag);
    Initializer *init = NULL;
    if (consume(TK_ASSIGN)) {
      init = parse_initializer();
      fix_array_size(type, init);

      // TODO: Check `init` can be cast to `type`.
      if (flag & VF_STATIC) {
        varinfo->u.g.init = init;
      } else {
        inits = assign_initial_value(new_expr_varref(ident->u.ident, type, false, NULL), init, inits);
      }
    }
  } while (consume(TK_COMMA));

  return inits;
}

static bool parse_vardecl(Node **pnode) {
  assert(curfunc != NULL);

  const Type *rawType = NULL;
  Type *type;
  int flag;
  Token *ident;
  if (!parse_var_def(&rawType, (const Type**)&type, &flag, &ident))
    return false;
  if (ident == NULL)
    parse_error(NULL, "Ident expected");

  Vector *inits = parse_vardecl_cont(rawType, type, flag, ident);

  if (!consume(TK_SEMICOL))
    parse_error(NULL, "`;' expected");

  if (inits != NULL && inits->len == 1)
    *pnode = inits->data[0];
  else
    *pnode = new_node_block(NULL, inits);
  return true;
}

// Multiple stmt-s, also accept `case` and `default`.
static Vector *read_stmts(void) {
  Vector *nodes = NULL;
  for (;;) {
    if (consume(TK_RBRACE))
      return nodes;
    if (nodes == NULL)
      nodes = new_vector();

    Node *node;
    Token *tok;
    if (parse_vardecl(&node))
      ;
    else if ((tok = consume(TK_CASE)) != NULL)
      node = parse_case(tok);
    else if ((tok = consume(TK_DEFAULT)) != NULL)
      node = parse_default(tok);
    else
      node = stmt();
    vec_push(nodes, node);
  }
}

static Node *parse_block(void) {
  assert(curfunc != NULL);
  Scope *scope = enter_scope(curfunc, NULL);
  Vector *nodes = read_stmts();
  exit_scope();
  return new_node_block(scope, nodes);
}

static Node *stmt(void) {
  Token *label = consume(TK_IDENT);
  if (label != NULL) {
    if (consume(TK_COLON)) {
      add_func_label(label->u.ident);
      return new_node_label(label->u.ident, stmt());
    }
    unget_token(label);
  }

  if (consume(TK_SEMICOL))
    return new_node_block(NULL, NULL);

  if (consume(TK_LBRACE))
    return parse_block();

  if (consume(TK_IF))
    return parse_if();

  if (consume(TK_SWITCH))
    return parse_switch();

  if (consume(TK_WHILE))
    return parse_while();

  if (consume(TK_DO))
    return parse_do_while();

  if (consume(TK_FOR))
    return parse_for();

  Token *tok;
  if ((tok = consume(TK_BREAK)) != NULL) {
    if ((curloopflag & LF_BREAK) == 0)
      parse_error(tok, "`break' cannot be used outside of loop");
    return parse_break_continue(ND_BREAK);
  }
  if ((tok = consume(TK_CONTINUE)) != NULL) {
    if ((curloopflag & LF_CONTINUE) == 0)
      parse_error(tok, "`continue' cannot be used outside of loop");
    return parse_break_continue(ND_CONTINUE);
  }
  if ((tok = consume(TK_GOTO)) != NULL) {
    return parse_goto();
  }

  if (consume(TK_RETURN))
    return parse_return();

  // expression statement.
  Expr *val = parse_analyze_expr();
  if (!consume(TK_SEMICOL))
    parse_error(NULL, "Semicolon required");
  return new_node_expr(val);
}

static Node *parse_defun(const Type *rettype, int flag, Token *ident) {
  const char *name = ident->u.ident;
  bool vaargs;
  Vector *params = funparams(&vaargs);

  const Type *functype = new_func_type(rettype, params, vaargs);

  Defun *defun = NULL;
  if (consume(TK_SEMICOL)) {  // Prototype declaration.
  } else {
    if (!consume(TK_LBRACE)) {
      parse_error(NULL, "`;' or `{' expected");
      return NULL;
    }
    // Definition.
    defun = new_defun(functype, name);
  }

  VarInfo *def = find_global(name);
  if (def == NULL) {
    define_global(functype, flag | VF_CONST, ident);
  } else {
    if (def->type->type != TY_FUNC)
      parse_error(ident, "Definition conflict: `%s'");
    // TODO: Check type.
    // TODO: Check duplicated definition.
    if (def->u.g.init != NULL)
      parse_error(ident, "`%s' function already defined");
  }

  if (defun != NULL) {
    curfunc = defun;

    enter_scope(defun, params);  // Scope for parameters.
    defun->top_scope = enter_scope(defun, NULL);
    defun->stmts = read_stmts();
    exit_scope();
    exit_scope();
    curfunc = NULL;

    // Check goto labels.
    if (defun->gotos != NULL) {
      Vector *gotos = defun->gotos;
      Map *labels = defun->labels;
      for (int i = 0; i < gotos->len; ++i) {
        Node *node = gotos->data[i];
        if (labels == NULL || map_get(labels, node->u.goto_.ident) == NULL)
          parse_error(node->u.goto_.tok, "`%s' not found", node->u.goto_.ident);
      }
    }
  }
  return defun != NULL ? new_node_defun(defun) : NULL;
}

static void parse_typedef(void) {
  int flag;
  Token *ident;
  const Type *type = parse_full_type(&flag, &ident);
  if (type == NULL)
    parse_error(NULL, "type expected");
  not_void(type);

  if (ident == NULL) {
    ident = consume(TK_IDENT);
    if (ident == NULL)
      parse_error(NULL, "ident expected");
  }
  const char *name = ident->u.ident;

  map_put(typedef_map, name, type);

  if (!consume(TK_SEMICOL))
    parse_error(NULL, "`;' expected");
}

static Initializer *check_global_initializer(const Type *type, Initializer *init) {
  switch (type->type) {
  case TY_CHAR:
  case TY_SHORT:
  case TY_INT:
  case TY_LONG:
    if (init->type == vSingle) {
      switch (init->u.single->type) {
      case EX_CHAR:
      case EX_SHORT:
      case EX_INT:
      case EX_LONG:
        return init;
      default:
        parse_error(NULL, "initializer type error");
        break;
      }
    }
    break;
  case TY_PTR:
    {
      if (init->type != vSingle)
        parse_error(NULL, "initializer type error");
      Expr *value = init->u.single;
      switch (value->type) {
      case EX_REF:
        {
          value = value->u.unary.sub;
          if (value->type != EX_VARREF)
            parse_error(NULL, "pointer initializer must be varref");
          if (!value->u.varref.global)
            parse_error(NULL, "Allowed global reference only");

          VarInfo *info = find_global(value->u.varref.ident);
          assert(info != NULL);

          if (!same_type(type->u.pa.ptrof, info->type))
            parse_error(NULL, "Illegal type");

          return init;
        }
      case EX_VARREF:
        {
          if (!value->u.varref.global)
            parse_error(NULL, "Allowed global reference only");

          VarInfo *info = find_global(value->u.varref.ident);
          assert(info != NULL);

          if (info->type->type != TY_ARRAY || !same_type(type->u.pa.ptrof, info->type->u.pa.ptrof))
            parse_error(NULL, "Illegal type");

          return init;
        }
      case EX_CAST:
        {  // Handle NULL assignment.
          while (value->type == EX_CAST)
            value = value->u.unary.sub;
          if (is_number(value->valType->type)) {
            Initializer *init2 = malloc(sizeof(*init2));
            init2->type = vSingle;
            init2->u.single = value;
            return init2;
          }
        }
        break;
      case EX_STR:
        if (!(type->u.pa.ptrof->type == TY_CHAR && value->type == EX_STR)) {
          parse_error(NULL, "Illegal type");
        }
        return init;
      default:
        break;
      }
      parse_error(NULL, "initializer type error: type=%d", value->type);
    }
    break;
  case TY_ARRAY:
    switch (init->type) {
    case vMulti:
      break;
    case vSingle:
      if (type->u.pa.ptrof->type == TY_CHAR && init->u.single->type == EX_STR) {
        assert(type->u.pa.length != (size_t)-1);
        if (type->u.pa.length < init->u.single->u.str.size) {
          parse_error(NULL, "Array size shorter than initializer");
        }
        return init;
      }
      // Fallthrough
    case vDot:
    default:
      parse_error(NULL, "Illegal initializer");
      break;
    }
    break;
  case TY_STRUCT:
  case TY_UNION:
    if (init->type != vMulti)
      parse_error(NULL, "initializer type error");
    // TODO: More check.
    break;
  default:
    parse_error(NULL, "Global initial value for type %d not implemented (yet)\n", type->type);
    break;
  }
  return init;
}

static Node *define_global_var(const Type *rawtype, int flag, const Type *type, Token *ident) {
  bool first = true;
  for (;;) {
    if (!first) {
      type = parse_type_modifier(rawtype);
      if ((ident = consume(TK_IDENT)) == NULL)
        parse_error(NULL, "`ident' expected");
    }
    first = false;

    if (type->type == TY_VOID)
      parse_error(ident, "`void' not allowed");

    type = parse_type_suffix(type);
    VarInfo *varinfo = define_global(type, flag, ident);
    Initializer *initializer = NULL;
    const Token *tok;
    if ((tok = consume(TK_ASSIGN)) != NULL) {
      if (flag & VF_EXTERN)
        parse_error(tok, "extern with initializer");
      initializer = parse_initializer();
      fix_array_size((Type*)type, initializer);
      initializer = check_global_initializer(type, initializer);
    }
    varinfo->u.g.init = initializer;

    if (consume(TK_COMMA))
      continue;
    if (!consume(TK_SEMICOL))
      parse_error(NULL, "`;' or `,' expected");
    break;
  }
  return NULL;
}

static Node *toplevel(void) {
  int flag;
  const Type *rawtype = parse_raw_type(&flag);
  if (rawtype != NULL) {
    const Type *type = parse_type_modifier(rawtype);
    if ((is_struct_or_union(type->type) || type->type == TY_ENUM) &&
        consume(TK_SEMICOL))  // Just struct/union definition.
      return NULL;

    Token *ident;
    if ((ident = consume(TK_IDENT)) != NULL) {
      if (consume(TK_LPAR))  // Function.
        return parse_defun(type, flag, ident);

      define_global_var(rawtype, flag, type, ident);
      return NULL;
    }
    parse_error(NULL, "ident expected");
    return NULL;
  }
  if (consume(TK_TYPEDEF)) {
    parse_typedef();
    return NULL;
  }
  parse_error(NULL, "Unexpected token");
  return NULL;
}

Vector *parse_program(void) {
  Vector *node_vector = new_vector();
  while (!consume(TK_EOF)) {
    Node *node = toplevel();
    if (node != NULL)
      vec_push(node_vector, node);
  }
  return node_vector;
}
