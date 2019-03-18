#include <assert.h>
#include <stdbool.h>
#include <stdlib.h>  // malloc
#include <string.h>

#include "xcc.h"
#include "expr.h"
#include "lexer.h"
#include "util.h"

static const Type tyVoid = {.type=TY_VOID};
static const Type tyChar = {.type=TY_CHAR};

const int LF_BREAK = 1 << 0;
const int LF_CONTINUE = 1 << 0;

static int curloopflag;
static Node *curswitch;

static Defun *new_defun(const Type *type, const char *name) {
  Defun *defun = malloc(sizeof(*defun));
  defun->type = type;
  defun->name = name;
  defun->top_scope = NULL;
  defun->stmts = NULL;
  defun->all_scopes = new_vector();
  defun->ret_label = NULL;
  return defun;
}

static Node *new_node_block(Scope *scope, Vector *nodes) {
  Node *node = new_node(ND_BLOCK, &tyVoid);
  node->u.block.scope = scope;
  node->u.block.nodes = nodes;
  return node;
}

static Node *new_node_switch(Node *value) {
  Node *node = new_node(ND_SWITCH, &tyVoid);
  node->u.switch_.value = value;
  node->u.switch_.body = NULL;
  node->u.switch_.case_values = NULL;
  node->u.switch_.has_default = false;
  return node;
}

static Node *new_node_case(int value) {
  Node *node = new_node(ND_LABEL, &tyVoid);
  node->u.label.type = lCASE;
  node->u.label.u.case_value = value;
  return node;
}

static Node *new_node_default(void) {
  Node *node = new_node(ND_LABEL, &tyVoid);
  node->u.label.type = lDEFAULT;
  return node;
}

static Node *new_node_while(Node *cond, Node *body) {
  Node *node = new_node(ND_WHILE, &tyVoid);
  node->u.while_.cond = cond;
  node->u.while_.body = body;
  return node;
}

static Node *new_node_do_while(Node *body, Node *cond) {
  Node *node = new_node(ND_DO_WHILE, &tyVoid);
  node->u.do_while.body = body;
  node->u.do_while.cond = cond;
  return node;
}

static Node *new_node_for(Node *pre, Node *cond, Node *post, Node *body) {
  Node *node = new_node(ND_FOR, &tyVoid);
  node->u.for_.pre = pre;
  node->u.for_.cond = cond;
  node->u.for_.post = post;
  node->u.for_.body = body;
  return node;
}

static Node *new_node_return(Node *val) {
  const Type *type = val != NULL ? val->expType : &tyVoid;
  Node *node = new_node(ND_RETURN, type);
  node->u.return_.val = val;
  return node;
}

static Node *new_node_defun(Defun *defun) {
  Node *node = new_node(ND_DEFUN, &tyVoid);
  node->u.defun = defun;
  return node;
}

static Node *parse_if(void) {
  if (consume(TK_LPAR)) {
    Node *cond = expr();
    if (consume(TK_RPAR)) {
      Node *tblock = stmt();
      Node *fblock = NULL;
      if (consume(TK_ELSE)) {
        fblock = stmt();
      }
      return new_node_if(cond, tblock, fblock, &tyVoid);
    }
  }
  parse_error(NULL, "Illegal syntax in `if'");
  return NULL;
}

static Node *parse_switch(void) {
  if (consume(TK_LPAR)) {
    Node *value = expr();
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
  Node *valnode = expr();
  intptr_t value;
  switch (valnode->type) {  // TODO: Accept const expression.
  case ND_CHAR:
  case ND_INT:
  case ND_LONG:
    value = valnode->u.value;
    break;
  default:
    parse_error(tok, "Cannot use expression");
    break;
  }
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
    Node *cond = expr();
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
      Node *cond = expr();
      if (consume(TK_RPAR) && consume(TK_SEMICOL)) {
        return new_node_do_while(body, cond);
      }
    }
  }
  parse_error(NULL, "Illegal syntax in `do-while'");
  return NULL;
}

static Vector *parse_vardecl_cont(const Type *rawType, const Type *type, int flag, Token *ident);
static Node *parse_for(void) {
  Scope *scope = NULL;
  if (consume(TK_LPAR)) {
    assert(curfunc != NULL);
    Node *pre = NULL;
    bool nopre = false;
    Vector *stmts = NULL;
    if (consume(TK_SEMICOL)) {
      nopre = true;
    } else {
      const Type *rawType = NULL;
      const Type *type;
      int flag;
      Token *ident;
      if (parse_var_def(&rawType, &type, &flag, &ident)) {
        if (ident == NULL)
          parse_error(NULL, "Ident expected");
        scope = enter_scope(curfunc, NULL);
        stmts = parse_vardecl_cont(rawType, type, flag, ident);
        if (!consume(TK_SEMICOL))
          scope = NULL;  // Error
      } else {
        pre = expr();
        if (!consume(TK_SEMICOL))
          pre = NULL;  // Error
      }
    }
    if (nopre || pre != NULL || scope != NULL) {
      Node *cond = NULL, *post = NULL;
      Node *body = NULL;
      if ((consume(TK_SEMICOL) || (cond = expr(), consume(TK_SEMICOL))) &&
          (consume(TK_RPAR) || (post = expr(), consume(TK_RPAR)))) {
        int save_flag = curloopflag;
        curloopflag |= LF_BREAK | LF_CONTINUE;
        body = stmt();
        curloopflag= save_flag;

        Node *node = new_node_for(pre, cond, post, body);
        if (stmts != NULL) {
          vec_push(stmts, node);
          exit_scope();
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
  return new_node(type, &tyVoid);
}

static Node *parse_return(void) {
  assert(curfunc != NULL);

  Node *val = NULL;
  Token *tok;
  const Type *rettype = curfunc->type->u.func.ret;
  if ((tok = consume(TK_SEMICOL)) != NULL) {
    if (rettype->type != TY_VOID)
      parse_error(tok, "`return' required a value");
  } else {
    tok = fetch_token();
    val = expr();
    if (!consume(TK_SEMICOL))
      parse_error(NULL, "`;' expected");

    if (rettype->type == TY_VOID)
      parse_error(tok, "void function `return' a value");
    val = new_node_cast(rettype, val, false);
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
    result->u.single = expr();
  }
  return result;
}

static Vector *clear_initial_value(Node *node, Vector *inits) {
  if (inits == NULL)
    inits = new_vector();

  switch (node->expType->type) {
  case TY_CHAR:
  case TY_INT:
  case TY_LONG:
  case TY_ENUM:
    vec_push(inits,
             new_node_bop(ND_ASSIGN, node->expType, node,
                          new_node_cast(node->expType, new_node_numlit(ND_INT, 0), true)));
    break;
  case TY_PTR:
    vec_push(inits,
             new_node_bop(ND_ASSIGN, node->expType, node,
                          new_node_cast(node->expType, new_node_numlit(ND_LONG, 0), true)));  // intptr_t
    break;
  case TY_ARRAY:
    {
      size_t arr_len = node->expType->u.pa.length;
      for (size_t i = 0; i < arr_len; ++i)
        clear_initial_value(new_node_deref(add_node(NULL, node, new_node_numlit(ND_INT, i))), inits);
    }
    break;
  case TY_STRUCT:
    {
      const StructInfo *sinfo = node->expType->u.struct_.info;
      assert(sinfo != NULL);
      for (int i = 0; i < sinfo->members->len; ++i) {
        VarInfo* varinfo = sinfo->members->data[i];
        Node *member = new_node_member(node, i, varinfo->type);
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

static void string_initializer(Node *dst, Node *src, Vector *inits) {
  // Initialize char[] with string literal (char s[] = "foo";).
  assert(dst->expType->type == TY_ARRAY && dst->expType->u.pa.ptrof->type == TY_CHAR);
  assert(src->expType->type == TY_ARRAY && src->expType->u.pa.ptrof->type == TY_CHAR);

  const char *str = src->u.str.buf;
  size_t len = src->u.str.len;
  size_t dstlen = dst->expType->u.pa.length;
  if (dstlen == (size_t)-1) {
    ((Type*)dst->expType)->u.pa.length = dstlen = len;
  } else {
    if (dstlen < len)
      parse_error(NULL, "Buffer is shorter than string: %d for \"%s\"", (int)dstlen, str);
  }

  for (size_t i = 0; i < len; ++i) {
    Node *index = new_node_numlit(ND_INT, i);
    vec_push(inits,
             new_node_bop(ND_ASSIGN, &tyChar,
                          new_node_deref(add_node(NULL, dst, index)),
                          new_node_deref(add_node(NULL, src, index))));
  }
  if (dstlen > len) {
    Node *zero = new_node_numlit(ND_CHAR, 0);
    for (size_t i = len; i < dstlen; ++i) {
      Node *index = new_node_numlit(ND_INT, i);
      vec_push(inits,
               new_node_bop(ND_ASSIGN, &tyChar,
                            new_node_deref(add_node(NULL, dst, index)),
                            zero));
    }
  }
}

static Vector *assign_initial_value(Node *node, Initializer *init, Vector *inits) {
  if (inits == NULL)
    inits = new_vector();

  switch (node->expType->type) {
  case TY_ARRAY:
    {
      // Special handling for string (char[]).
      if (node->expType->u.pa.ptrof->type == TY_CHAR &&
          init->type == vSingle &&
          can_cast(node->expType, init->u.single->expType, init->u.single, false)) {
        string_initializer(node, init->u.single, inits);
        break;
      }

      if (init->type != vMulti)
        parse_error(NULL, "Error initializer");
      size_t arr_len = node->expType->u.pa.length;
      if (arr_len == (size_t)-1) {
        ((Type*)node->expType)->u.pa.length = arr_len = init->u.multi->len;
      } else {
        if ((size_t)init->u.multi->len > arr_len)
          parse_error(NULL, "Initializer more than array size");
      }
      int len = init->u.multi->len;
      for (int i = 0; i < len; ++i) {
        assign_initial_value(new_node_deref(add_node(NULL, node, new_node_numlit(ND_INT, i))),
                             init->u.multi->data[i], inits);
      }
      // Clear left.
      for (size_t i = len; i < arr_len; ++i)
        clear_initial_value(new_node_deref(add_node(NULL, node, new_node_numlit(ND_INT, i))), inits);
    }
    break;
  case TY_STRUCT:
    {
      if (init->type != vMulti)
        parse_error(NULL, "`{...}' expected for initializer");

      ensure_struct((Type*)node->expType, NULL);
      const StructInfo *sinfo = node->expType->u.struct_.info;
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
        Node *member = new_node_member(node, i, varinfo->type);
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

      const StructInfo *sinfo = node->expType->u.struct_.info;
      ensure_struct((Type*)node->expType, NULL);
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
      Node *member = new_node_member(node, dst, varinfo->type);
      assign_initial_value(member, value, inits);
    }
    break;
  default:
    if (init->type != vSingle)
      parse_error(NULL, "Error initializer");
    vec_push(inits,
             new_node_bop(ND_ASSIGN, node->expType, node, new_node_cast(node->expType, init->u.single, false)));
    break;
  }

  return inits;
}

static Vector *parse_vardecl_cont(const Type *rawType, const Type *type, int flag, Token *ident) {
  Vector *inits = NULL;
  bool first = true;
  do {
    if (!first) {
      if (!parse_var_def(&rawType, &type, &flag, &ident) || ident == NULL) {
        parse_error(NULL, "`ident' expected");
        return NULL;
      }
    }
    first = false;
    not_void(type);

    add_cur_scope(ident, type, flag);

    if (consume(TK_ASSIGN)) {
      Initializer *initializer = parse_initializer();
      inits = assign_initial_value(new_node_varref(ident->u.ident, type, false), initializer, inits);
    }
  } while (consume(TK_COMMA));

  return inits;
}

static bool parse_vardecl(Node **pnode) {
  assert(curfunc != NULL);

  const Type *rawType = NULL;
  const Type *type;
  int flag;
  Token *ident;
  if (!parse_var_def(&rawType, &type, &flag, &ident))
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

Node *stmt(void) {
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

  if (consume(TK_RETURN))
    return parse_return();

  // expression statement.
  Node *node = expr();
  if (!consume(TK_SEMICOL))
    parse_error(NULL, "Semicolon required");
  return node;
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

  GlobalVarInfo *def = find_global(name);
  if (def == NULL) {
    define_global(functype, flag | VF_CONST, ident, NULL);
  } else {
    if (def->type->type != TY_FUNC)
      parse_error(ident, "Definition conflict: `%s'");
    // TODO: Check type.
    // TODO: Check duplicated definition.
    if (def->init != NULL)
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
    Initializer *initializer = NULL;
    if (consume(TK_ASSIGN)) {
      if (flag & VF_EXTERN)
        parse_error(NULL, "extern with initializer");
      initializer = parse_initializer();
    }
    define_global(type, flag, ident, initializer);

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
