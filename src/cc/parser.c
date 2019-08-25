#include "parser.h"

#include <assert.h>
#include <stdbool.h>
#include <stdlib.h>  // malloc

#include "expr.h"
#include "lexer.h"
#include "type.h"
#include "util.h"

static Node *stmt(void);

static VarDecl *new_vardecl(const Type *type, const Token *ident, Initializer *init, int flag) {
  VarDecl *decl = malloc(sizeof(*decl));
  decl->type = type;
  decl->ident = ident;
  decl->init = init;
  decl->flag = flag;
  return decl;
}

static Defun *new_defun(const Type *rettype, const char *name, Vector *params, int flag, bool vaargs) {
  Defun *defun = malloc(sizeof(*defun));
  defun->rettype = rettype;
  defun->name = name;
  defun->params = params;
  defun->flag = flag;
  defun->vaargs = vaargs;

  defun->type = NULL;
  defun->stmts = NULL;
  defun->top_scope = NULL;
  defun->all_scopes = new_vector();
  defun->labels = NULL;
  defun->gotos = NULL;
  defun->ret_label = NULL;
  return defun;
}

static Node *new_node(enum NodeType type) {
  Node *node = malloc(sizeof(Node));
  node->type = type;
  return node;
}

Node *new_node_expr(Expr *e) {
  Node *node = new_node(ND_EXPR);
  node->u.expr = e;
  return node;
}

static Node *new_node_block(Vector *nodes) {
  Node *node = new_node(ND_BLOCK);
  node->u.block.scope = NULL;
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
  node->u.switch_.case_values = new_vector();
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
  node->u.while_.body = body;
  node->u.while_.cond = cond;
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

static Node *new_node_vardecl(Vector *decls) {
  Node *node = new_node(ND_VARDECL);
  node->u.vardecl.decls = decls;
  node->u.vardecl.inits = NULL;
  return node;
}

static Node *new_node_defun(Defun *defun) {
  Node *node = new_node(ND_DEFUN);
  node->u.defun = defun;
  return node;
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
        } else if (consume(TK_LBRACKET)) {
          Expr *index = parse_const();
          if (!consume(TK_RBRACKET))
            parse_error(NULL, "`]' expected");
          consume(TK_ASSIGN);  // both accepted: `[1] = 2`, and `[1] 2`
          Initializer *value = parse_initializer();
          init = malloc(sizeof(*init));
          init->type = vArr;
          init->u.arr.index = index;
          init->u.arr.value = value;
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
    result->u.single = parse_assign();
  }
  return result;
}

static Vector *parse_vardecl_cont(const Type *rawType, Type *type, int flag, Token *ident) {
  Vector *decls = NULL;
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

    Initializer *init = NULL;
    if (consume(TK_ASSIGN)) {
      init = parse_initializer();
    }

    VarDecl *decl = new_vardecl(type, ident, init, flag);
    if (decls == NULL)
      decls = new_vector();
    vec_push(decls, decl);
  } while (consume(TK_COMMA));
  return decls;
}

static Node *parse_vardecl(void) {
  const Type *rawType = NULL;
  Type *type;
  int flag;
  Token *ident;
  if (!parse_var_def(&rawType, (const Type**)&type, &flag, &ident))
    return NULL;
  if (ident == NULL)
    parse_error(NULL, "Ident expected");

  Vector *decls = parse_vardecl_cont(rawType, type, flag, ident);

  if (!consume(TK_SEMICOL))
    parse_error(NULL, "`;' expected");

  return decls != NULL ? new_node_vardecl(decls) : NULL;
}

static Node *parse_if(void) {
  if (consume(TK_LPAR)) {
    Expr *cond = parse_expr();
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
    Expr *value = parse_expr();
    if (consume(TK_RPAR)) {
      Node *swtch = new_node_switch(value);
      swtch->u.switch_.body = stmt();
      return swtch;
    }
  }
  parse_error(NULL, "Illegal syntax in `switch'");
  return NULL;
}

static Node *parse_case(void) {
  Token *tok = fetch_token();
  Expr *valnode = analyze_expr(parse_const(), false);
  if (!is_const(valnode))
    parse_error(tok, "Cannot use expression");
  intptr_t value = valnode->u.num.ival;

  if (!consume(TK_COLON))
    parse_error(NULL, "`:' expected");

  return new_node_case(value);
}

static Node *parse_default(void) {
  if (!consume(TK_COLON))
    parse_error(NULL, "`:' expected");
  return new_node_default();
}

static Node *parse_while(void) {
  if (consume(TK_LPAR)) {
    Expr *cond = parse_expr();
    if (consume(TK_RPAR)) {
      Node *body = stmt();

      return new_node_while(cond, body);
    }
  }
  parse_error(NULL, "Illegal syntax in `while'");
  return NULL;
}

static Node *parse_do_while(void) {
  Node *body = stmt();

  if (consume(TK_WHILE) && consume(TK_LPAR)) {
    Expr *cond = parse_expr();
    if (consume(TK_RPAR) && consume(TK_SEMICOL)) {
      return new_node_do_while(body, cond);
    }
  }
  parse_error(NULL, "Illegal syntax in `do-while'");
  return NULL;
}

static Node *parse_for(void) {
  if (consume(TK_LPAR)) {
    Expr *pre = NULL;
    bool nopre = false;
    Vector *decls = NULL;
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
        decls = parse_vardecl_cont(rawType, type, flag, ident);
        if (!consume(TK_SEMICOL))
          decls = NULL;  // Error
      } else {
        pre = parse_expr();
        if (!consume(TK_SEMICOL))
          pre = NULL;  // Error
      }
    }
    if (nopre || pre != NULL || decls != NULL) {
      Expr *cond = NULL;
      Expr *post = NULL;
      Node *body = NULL;
      if ((consume(TK_SEMICOL) || (cond = parse_expr(), consume(TK_SEMICOL))) &&
          (consume(TK_RPAR) || (post = parse_expr(), consume(TK_RPAR)))) {
        body = stmt();

        Node *node = new_node_for(pre, cond, post, body);
        if (decls != NULL) {
          Vector *stmts = new_vector();
          vec_push(stmts, new_node_vardecl(decls));
          vec_push(stmts, node);
          return new_node_block(stmts);
        } else {
          return node;
        }
      }
    }
  }
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
  return new_node_goto(label);
}

static Node *parse_return(void) {
  Expr *val = NULL;
  Token *tok;
  if ((tok = consume(TK_SEMICOL)) != NULL) {
  } else {
    tok = fetch_token();
    val = parse_expr();
    if (!consume(TK_SEMICOL))
      parse_error(NULL, "`;' expected");
  }
  return new_node_return(val);
}

// Multiple stmt-s, also accept `case` and `default`.
static Vector *read_stmts(void) {
  Vector *nodes = NULL;
  for (;;) {
    if (consume(TK_RBRACE))
      return nodes;

    Node *node;
    Token *tok;
    if ((node = parse_vardecl()) != NULL)
      ;
    else if ((tok = consume(TK_CASE)) != NULL)
      node = parse_case();
    else if ((tok = consume(TK_DEFAULT)) != NULL)
      node = parse_default();
    else
      node = stmt();

    if (node == NULL)
      continue;
    if (nodes == NULL)
      nodes = new_vector();
    vec_push(nodes, node);
  }
}

static Node *parse_block(void) {
  Vector *nodes = read_stmts();
  return new_node_block(nodes);
}

static Node *stmt(void) {
  Token *label = consume(TK_IDENT);
  if (label != NULL) {
    if (consume(TK_COLON)) {
      return new_node_label(label->u.ident, stmt());
    }
    unget_token(label);
  }

  if (consume(TK_SEMICOL))
    return NULL;

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
    return parse_break_continue(ND_BREAK);
  }
  if ((tok = consume(TK_CONTINUE)) != NULL) {
    return parse_break_continue(ND_CONTINUE);
  }
  if ((tok = consume(TK_GOTO)) != NULL) {
    return parse_goto();
  }

  if (consume(TK_RETURN))
    return parse_return();

  // expression statement.
  Expr *val = parse_expr();
  if (!consume(TK_SEMICOL))
    parse_error(NULL, "Semicolon required");
  return new_node_expr(val);
}

static Node *parse_defun(const Type *rettype, int flag, Token *ident) {
  const char *name = ident->u.ident;
  bool vaargs;
  Vector *params = parse_funparams(&vaargs);

  // Definition.
  Defun *defun = new_defun(rettype, name, params, flag, vaargs);
  if (consume(TK_SEMICOL)) {  // Prototype declaration.
  } else {
    if (!consume(TK_LBRACE)) {
      parse_error(NULL, "`;' or `{' expected");
      return NULL;
    }

    defun->stmts = read_stmts();
    // Ensure stmts to be non-null to indicate this is not prototype definition.
    if (defun->stmts == NULL)
      defun->stmts = new_vector();
  }
  return new_node_defun(defun);
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
  add_typedef(name, type);

  if (!consume(TK_SEMICOL))
    parse_error(NULL, "`;' expected");
}

static Node *parse_global_var_decl(const Type *rawtype, int flag, const Type *type, Token *ident) {
  bool first = true;
  Vector *decls = NULL;
  do {
    if (!first) {
      type = parse_type_modifier(rawtype);
      if ((ident = consume(TK_IDENT)) == NULL)
        parse_error(NULL, "`ident' expected");
    }
    first = false;

    if (type->type == TY_VOID)
      parse_error(ident, "`void' not allowed");

    type = parse_type_suffix(type);
    Initializer *init = NULL;
    const Token *tok;
    if ((tok = consume(TK_ASSIGN)) != NULL) {
      init = parse_initializer();
    }

    VarDecl *decl = new_vardecl(type, ident, init, flag);
    if (decls == NULL)
      decls = new_vector();
    vec_push(decls, decl);
  } while (consume(TK_COMMA));

  if (!consume(TK_SEMICOL))
    parse_error(NULL, "`;' or `,' expected");

  return decls != NULL ? new_node_vardecl(decls) : NULL;
}

static Node *toplevel(void) {
  int flag;
  const Type *rawtype = parse_raw_type(&flag);
  if (rawtype != NULL) {
    const Type *type = parse_type_modifier(rawtype);
    if ((type->type == TY_STRUCT ||
         (type->type == TY_NUM && type->u.num.type == NUM_ENUM)) &&
        consume(TK_SEMICOL))  // Just struct/union definition.
      return NULL;

    Token *ident;
    if ((ident = consume(TK_IDENT)) != NULL) {
      if (consume(TK_LPAR))  // Function.
        return parse_defun(type, flag, ident);

      return parse_global_var_decl(rawtype, flag, type, ident);
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

Node *parse_program(void) {
  Vector *nodes = new_vector();
  while (!consume(TK_EOF)) {
    Node *node = toplevel();
    if (node != NULL)
      vec_push(nodes, node);
  }

  Node *node = new_node(ND_TOPLEVEL);
  node->u.toplevel.nodes = nodes;
  return node;
}
