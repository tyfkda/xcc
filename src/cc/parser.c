#include "parser.h"

#include <assert.h>
#include <stdbool.h>
#include <stdlib.h>  // malloc

#include "expr.h"
#include "lexer.h"
#include "type.h"
#include "util.h"
#include "var.h"

static Node *stmt(void);

static VarDecl *new_vardecl(const Type *type, const Token *ident, Initializer *init, int flag) {
  VarDecl *decl = malloc(sizeof(*decl));
  decl->type = type;
  decl->ident = ident;
  decl->init = init;
  decl->flag = flag;
  return decl;
}

static Defun *new_defun(Function *func, int flag) {
  Defun *defun = malloc(sizeof(*defun));
  defun->func = func;
  defun->flag = flag;

  defun->stmts = NULL;
  defun->label_map = NULL;
  defun->gotos = NULL;
  return defun;
}

static Node *new_node(enum NodeKind kind, const Token *token) {
  Node *node = malloc(sizeof(Node));
  node->kind = kind;
  node->token = token;
  return node;
}

Node *new_node_expr(Expr *e) {
  Node *node = new_node(ND_EXPR, e->token);
  node->expr = e;
  return node;
}

static Node *new_node_block(const Token *token, Vector *nodes) {
  Node *node = new_node(ND_BLOCK, token);
  node->block.scope = NULL;
  node->block.nodes = nodes;
  return node;
}

static Node *new_node_if(const Token *token, Expr *cond, Node *tblock, Node *fblock) {
  Node *node = new_node(ND_IF, token);
  node->if_.cond = cond;
  node->if_.tblock = tblock;
  node->if_.fblock = fblock;
  return node;
}

static Node *new_node_switch(const Token *token, Expr *value) {
  Node *node = new_node(ND_SWITCH, token);
  node->switch_.value = value;
  node->switch_.body = NULL;
  node->switch_.case_values = new_vector();
  node->switch_.has_default = false;
  return node;
}

static Node *new_node_case(const Token *token, Expr *value) {
  Node *node = new_node(ND_CASE, token);
  node->case_.value = value;
  return node;
}

static Node *new_node_default(const Token *token) {
  Node *node = new_node(ND_DEFAULT, token);
  return node;
}

static Node *new_node_while(const Token *token, Expr *cond, Node *body) {
  Node *node = new_node(ND_WHILE, token);
  node->while_.cond = cond;
  node->while_.body = body;
  return node;
}

static Node *new_node_do_while(Node *body, const Token *token, Expr *cond) {
  Node *node = new_node(ND_DO_WHILE, token);
  node->while_.body = body;
  node->while_.cond = cond;
  return node;
}

static Node *new_node_for(const Token *token, Expr *pre, Expr *cond, Expr *post, Node *body) {
  Node *node = new_node(ND_FOR, token);
  node->for_.pre = pre;
  node->for_.cond = cond;
  node->for_.post = post;
  node->for_.body = body;
  return node;
}

static Node *new_node_return(const Token *token, Expr *val) {
  Node *node = new_node(ND_RETURN, token);
  node->return_.val = val;
  return node;
}

static Node *new_node_goto(const Token *label) {
  Node *node = new_node(ND_GOTO, NULL);
  node->goto_.label = label;
  return node;
}

static Node *new_node_label(const Token *label, Node *stmt) {
  Node *node = new_node(ND_LABEL, label);
  node->label.stmt = stmt;
  return node;
}

static Node *new_node_vardecl(Vector *decls) {
  Node *node = new_node(ND_VARDECL, NULL);
  node->vardecl.decls = decls;
  node->vardecl.inits = NULL;
  return node;
}

static Node *new_node_asm(const Token *token, Expr *str) {
  Node *node = new_node(ND_ASM, token);
  node->asm_.str = str;
  return node;
}

static Node *new_node_defun(Defun *defun) {
  Node *node = new_node(ND_DEFUN, NULL);
  node->defun = defun;
  return node;
}

Node *new_top_node(Vector *nodes) {
  Node *top = new_node(ND_TOPLEVEL, NULL);
  top->toplevel.nodes = nodes;
  return top;
}

// Initializer

static Initializer *parse_initializer(void) {
  Initializer *result = malloc(sizeof(*result));
  const Token *lblace_tok;
  if ((lblace_tok = consume(TK_LBRACE)) != NULL) {
    Vector *multi = new_vector();
    if (!consume(TK_RBRACE)) {
      for (;;) {
        Initializer *init;
        const Token *tok;
        if (consume(TK_DOT)) {  // .member=value
          Token *ident = consume(TK_IDENT);
          if (ident == NULL)
            parse_error(NULL, "`ident' expected for dotted initializer");
          if (!consume(TK_ASSIGN))
            parse_error(NULL, "`=' expected for dotted initializer");
          Initializer *value = parse_initializer();
          init = malloc(sizeof(*init));
          init->kind = vDot;
          init->token = ident;
          init->dot.name = ident->ident;
          init->dot.value = value;
        } else if ((tok = consume(TK_LBRACKET)) != NULL) {
          Expr *index = parse_const();
          if (!consume(TK_RBRACKET))
            parse_error(NULL, "`]' expected");
          consume(TK_ASSIGN);  // both accepted: `[1] = 2`, and `[1] 2`
          Initializer *value = parse_initializer();
          init = malloc(sizeof(*init));
          init->kind = vArr;
          init->token = tok;
          init->arr.index = index;
          init->arr.value = value;
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
    result->kind = vMulti;
    result->token = lblace_tok;
    result->multi = multi;
  } else {
    result->kind = vSingle;
    result->single = parse_assign();
    result->token = result->single->token;
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

    Initializer *init = NULL;
    if (consume(TK_LPAR)) {  // Function prototype.
      bool vaargs;
      Vector *param_types = parse_funparam_types(&vaargs);
      type = ptrof(new_func_type(type, param_types, vaargs));
      flag |= VF_EXTERN;
    } else {
      not_void(type);
      if (consume(TK_ASSIGN)) {
        init = parse_initializer();
      }
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

static Node *parse_if(const Token *tok) {
  if (consume(TK_LPAR)) {
    Expr *cond = parse_expr();
    if (consume(TK_RPAR)) {
      Node *tblock = stmt();
      Node *fblock = NULL;
      if (consume(TK_ELSE)) {
        fblock = stmt();
      }
      return new_node_if(tok, cond, tblock, fblock);
    }
  }
  parse_error(NULL, "Illegal syntax in `if'");
  return NULL;
}

static Node *parse_switch(const Token *tok) {
  if (consume(TK_LPAR)) {
    Expr *value = parse_expr();
    if (consume(TK_RPAR)) {
      Node *swtch = new_node_switch(tok, value);
      swtch->switch_.body = stmt();
      return swtch;
    }
  }
  parse_error(NULL, "Illegal syntax in `switch'");
  return NULL;
}

static Node *parse_case(const Token *tok) {
  // Token *tok = fetch_token();
  Expr *valnode = parse_const();

  if (!consume(TK_COLON))
    parse_error(NULL, "`:' expected");

  return new_node_case(tok, valnode);
}

static Node *parse_default(const Token *tok) {
  if (!consume(TK_COLON))
    parse_error(NULL, "`:' expected");
  return new_node_default(tok);
}

static Node *parse_while(const Token *tok) {
  if (consume(TK_LPAR)) {
    Expr *cond = parse_expr();
    if (consume(TK_RPAR)) {
      Node *body = stmt();

      return new_node_while(tok, cond, body);
    }
  }
  parse_error(NULL, "Illegal syntax in `while'");
  return NULL;
}

static Node *parse_do_while(void) {
  Node *body = stmt();

  const Token *tok;
  if ((tok = consume(TK_WHILE)) != NULL && consume(TK_LPAR)) {
    Expr *cond = parse_expr();
    if (consume(TK_RPAR) && consume(TK_SEMICOL)) {
      return new_node_do_while(body, tok, cond);
    }
  }
  parse_error(tok, "Illegal syntax in `do-while'");
  return NULL;
}

static Node *parse_for(const Token *tok) {
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
        if (type->kind == TY_VOID && ident != NULL) {

        }

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

        Node *node = new_node_for(tok, pre, cond, post, body);
        if (decls != NULL) {
          Vector *stmts = new_vector();
          vec_push(stmts, new_node_vardecl(decls));
          vec_push(stmts, node);
          return new_node_block(NULL, stmts);
        } else {
          return node;
        }
      }
    }
  }
  parse_error(NULL, "Illegal syntax in `for'");
  return NULL;
}

static Node *parse_break_continue(enum NodeKind kind, const Token *tok) {
  if (!consume(TK_SEMICOL))
    parse_error(NULL, "`;' expected");
  return new_node(kind, tok);
}

static Node *parse_goto(void) {
  Token *label = consume(TK_IDENT);
  if (label == NULL)
    parse_error(NULL, "label for goto expected");
  if (!consume(TK_SEMICOL))
    parse_error(NULL, "`;' expected");
  return new_node_goto(label);
}

static Node *parse_return(const Token *tok) {
  Expr *val = NULL;
  if (consume(TK_SEMICOL)) {
  } else {
    val = parse_expr();
    if (!consume(TK_SEMICOL))
      parse_error(fetch_token(), "`;' expected");
  }
  return new_node_return(tok, val);
}

static Node *parse_asm(const Token *tok) {
  if (!consume(TK_LPAR))
    parse_error(NULL, "`(' expected");

  Token *token;
  Vector *args = parse_args(&token);

  if (args == NULL || args->len != 1 || ((Expr*)args->data[0])->kind != EX_STR)
    parse_error(token, "`__asm' expected one string");

  return new_node_asm(tok, args->data[0]);
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
      node = parse_case(tok);
    else if ((tok = consume(TK_DEFAULT)) != NULL)
      node = parse_default(tok);
    else
      node = stmt();

    if (node == NULL)
      continue;
    if (nodes == NULL)
      nodes = new_vector();
    vec_push(nodes, node);
  }
}

static Node *parse_block(const Token *tok) {
  Vector *nodes = read_stmts();
  return new_node_block(tok, nodes);
}

static Node *stmt(void) {
  Token *label = consume(TK_IDENT);
  if (label != NULL) {
    if (consume(TK_COLON)) {
      return new_node_label(label, stmt());
    }
    unget_token(label);
  }

  if (consume(TK_SEMICOL))
    return NULL;

  const Token *tok;
  if ((tok = consume(TK_LBRACE)) != NULL)
    return parse_block(tok);

  if ((tok = consume(TK_IF)) != NULL)
    return parse_if(tok);

  if ((tok = consume(TK_SWITCH)) != NULL)
    return parse_switch(tok);

  if ((tok = consume(TK_WHILE)) != NULL)
    return parse_while(tok);

  if (consume(TK_DO))
    return parse_do_while();

  if ((tok = consume(TK_FOR)) != NULL)
    return parse_for(tok);

  if ((tok = consume(TK_BREAK)) != NULL) {
    return parse_break_continue(ND_BREAK, tok);
  }
  if ((tok = consume(TK_CONTINUE)) != NULL) {
    return parse_break_continue(ND_CONTINUE, tok);
  }
  if (consume(TK_GOTO)) {
    return parse_goto();
  }

  if ((tok = consume(TK_RETURN)) != NULL)
    return parse_return(tok);

  if ((tok = consume(TK_ASM)) != NULL)
    return parse_asm(tok);

  // expression statement.
  Expr *val = parse_expr();
  if (!consume(TK_SEMICOL))
    parse_error(NULL, "Semicolon required");
  return new_node_expr(val);
}

static Node *parse_defun(const Type *rettype, int flag, Token *ident) {
  const char *name = ident->ident;
  bool vaargs;
  Vector *params = parse_funparams(&vaargs);

  // Definition.
  Vector *param_types = extract_varinfo_types(params);
  Function *func = new_func(new_func_type(rettype, param_types, vaargs), name, params);
  Defun *defun = new_defun(func, flag);
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
  const char *name = ident->ident;
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

    if (type->kind == TY_VOID)
      parse_error(ident, "`void' not allowed1");

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
    if ((type->kind == TY_STRUCT ||
         (type->kind == TY_NUM && type->num.kind == NUM_ENUM)) &&
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

Vector *parse_program(Vector *nodes) {
  if (nodes == NULL)
    nodes = new_vector();
  while (!consume(TK_EOF)) {
    Node *node = toplevel();
    if (node != NULL)
      vec_push(nodes, node);
  }
  return nodes;
}
