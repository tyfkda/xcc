#include "parser.h"

#include <assert.h>
#include <stdbool.h>
#include <stdlib.h>  // malloc

#include "expr.h"
#include "lexer.h"
#include "type.h"
#include "util.h"
#include "var.h"

static Stmt *statement(void);

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

static Stmt *new_stmt(enum StmtKind kind, const Token *token) {
  Stmt *stmt = malloc(sizeof(Stmt));
  stmt->kind = kind;
  stmt->token = token;
  return stmt;
}

Stmt *new_stmt_expr(Expr *e) {
  Stmt *stmt = new_stmt(ST_EXPR, e->token);
  stmt->expr = e;
  return stmt;
}

static Stmt *new_stmt_block(const Token *token, Vector *stmts) {
  Stmt *stmt = new_stmt(ST_BLOCK, token);
  stmt->block.scope = NULL;
  stmt->block.stmts = stmts;
  return stmt;
}

static Stmt *new_stmt_if(const Token *token, Expr *cond, Stmt *tblock, Stmt *fblock) {
  Stmt *stmt = new_stmt(ST_IF, token);
  stmt->if_.cond = cond;
  stmt->if_.tblock = tblock;
  stmt->if_.fblock = fblock;
  return stmt;
}

static Stmt *new_stmt_switch(const Token *token, Expr *value) {
  Stmt *stmt = new_stmt(ST_SWITCH, token);
  stmt->switch_.value = value;
  stmt->switch_.body = NULL;
  stmt->switch_.case_values = new_vector();
  stmt->switch_.has_default = false;
  return stmt;
}

static Stmt *new_stmt_case(const Token *token, Expr *value) {
  Stmt *stmt = new_stmt(ST_CASE, token);
  stmt->case_.value = value;
  return stmt;
}

static Stmt *new_stmt_default(const Token *token) {
  Stmt *stmt = new_stmt(ST_DEFAULT, token);
  return stmt;
}

static Stmt *new_stmt_while(const Token *token, Expr *cond, Stmt *body) {
  Stmt *stmt = new_stmt(ST_WHILE, token);
  stmt->while_.cond = cond;
  stmt->while_.body = body;
  return stmt;
}

static Stmt *new_stmt_do_while(Stmt *body, const Token *token, Expr *cond) {
  Stmt *stmt = new_stmt(ST_DO_WHILE, token);
  stmt->while_.body = body;
  stmt->while_.cond = cond;
  return stmt;
}

static Stmt *new_stmt_for(const Token *token, Expr *pre, Expr *cond, Expr *post, Stmt *body) {
  Stmt *stmt = new_stmt(ST_FOR, token);
  stmt->for_.pre = pre;
  stmt->for_.cond = cond;
  stmt->for_.post = post;
  stmt->for_.body = body;
  return stmt;
}

static Stmt *new_stmt_return(const Token *token, Expr *val) {
  Stmt *stmt = new_stmt(ST_RETURN, token);
  stmt->return_.val = val;
  return stmt;
}

static Stmt *new_stmt_goto(const Token *label) {
  Stmt *stmt = new_stmt(ST_GOTO, NULL);
  stmt->goto_.label = label;
  return stmt;
}

static Stmt *new_stmt_label(const Token *label, Stmt *follow) {
  Stmt *stmt = new_stmt(ST_LABEL, label);
  stmt->label.stmt = follow;
  return stmt;
}

static Stmt *new_stmt_vardecl(Vector *decls) {
  Stmt *stmt = new_stmt(ST_VARDECL, NULL);
  stmt->vardecl.decls = decls;
  stmt->vardecl.inits = NULL;
  return stmt;
}

static Stmt *new_stmt_asm(const Token *token, Expr *str) {
  Stmt *stmt = new_stmt(ST_ASM, token);
  stmt->asm_.str = str;
  return stmt;
}

static Stmt *new_stmt_defun(Defun *defun) {
  Stmt *stmt = new_stmt(ST_DEFUN, NULL);
  stmt->defun = defun;
  return stmt;
}

Stmt *new_top_stmt(Vector *stmts) {
  Stmt *top = new_stmt(ST_TOPLEVEL, NULL);
  top->toplevel.stmts = stmts;
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

static Stmt *parse_vardecl(void) {
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

  return decls != NULL ? new_stmt_vardecl(decls) : NULL;
}

static Stmt *parse_if(const Token *tok) {
  if (consume(TK_LPAR)) {
    Expr *cond = parse_expr();
    if (consume(TK_RPAR)) {
      Stmt *tblock = statement();
      Stmt *fblock = NULL;
      if (consume(TK_ELSE)) {
        fblock = statement();
      }
      return new_stmt_if(tok, cond, tblock, fblock);
    }
  }
  parse_error(NULL, "Illegal syntax in `if'");
  return NULL;
}

static Stmt *parse_switch(const Token *tok) {
  if (consume(TK_LPAR)) {
    Expr *value = parse_expr();
    if (consume(TK_RPAR)) {
      Stmt *swtch = new_stmt_switch(tok, value);
      swtch->switch_.body = statement();
      return swtch;
    }
  }
  parse_error(NULL, "Illegal syntax in `switch'");
  return NULL;
}

static Stmt *parse_case(const Token *tok) {
  // Token *tok = fetch_token();
  Expr *value = parse_const();

  if (!consume(TK_COLON))
    parse_error(NULL, "`:' expected");

  return new_stmt_case(tok, value);
}

static Stmt *parse_default(const Token *tok) {
  if (!consume(TK_COLON))
    parse_error(NULL, "`:' expected");
  return new_stmt_default(tok);
}

static Stmt *parse_while(const Token *tok) {
  if (consume(TK_LPAR)) {
    Expr *cond = parse_expr();
    if (consume(TK_RPAR)) {
      Stmt *body = statement();

      return new_stmt_while(tok, cond, body);
    }
  }
  parse_error(NULL, "Illegal syntax in `while'");
  return NULL;
}

static Stmt *parse_do_while(void) {
  Stmt *body = statement();

  const Token *tok;
  if ((tok = consume(TK_WHILE)) != NULL && consume(TK_LPAR)) {
    Expr *cond = parse_expr();
    if (consume(TK_RPAR) && consume(TK_SEMICOL)) {
      return new_stmt_do_while(body, tok, cond);
    }
  }
  parse_error(tok, "Illegal syntax in `do-while'");
  return NULL;
}

static Stmt *parse_for(const Token *tok) {
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
      Stmt *body = NULL;
      if ((consume(TK_SEMICOL) || (cond = parse_expr(), consume(TK_SEMICOL))) &&
          (consume(TK_RPAR) || (post = parse_expr(), consume(TK_RPAR)))) {
        body = statement();

        Stmt *stmt = new_stmt_for(tok, pre, cond, post, body);
        if (decls != NULL) {
          Vector *stmts = new_vector();
          vec_push(stmts, new_stmt_vardecl(decls));
          vec_push(stmts, stmt);
          return new_stmt_block(NULL, stmts);
        } else {
          return stmt;
        }
      }
    }
  }
  parse_error(NULL, "Illegal syntax in `for'");
  return NULL;
}

static Stmt *parse_break_continue(enum StmtKind kind, const Token *tok) {
  if (!consume(TK_SEMICOL))
    parse_error(NULL, "`;' expected");
  return new_stmt(kind, tok);
}

static Stmt *parse_goto(void) {
  Token *label = consume(TK_IDENT);
  if (label == NULL)
    parse_error(NULL, "label for goto expected");
  if (!consume(TK_SEMICOL))
    parse_error(NULL, "`;' expected");
  return new_stmt_goto(label);
}

static Stmt *parse_return(const Token *tok) {
  Expr *val = NULL;
  if (consume(TK_SEMICOL)) {
  } else {
    val = parse_expr();
    if (!consume(TK_SEMICOL))
      parse_error(fetch_token(), "`;' expected");
  }
  return new_stmt_return(tok, val);
}

static Stmt *parse_asm(const Token *tok) {
  if (!consume(TK_LPAR))
    parse_error(NULL, "`(' expected");

  Token *token;
  Vector *args = parse_args(&token);

  if (args == NULL || args->len != 1 || ((Expr*)args->data[0])->kind != EX_STR)
    parse_error(token, "`__asm' expected one string");

  return new_stmt_asm(tok, args->data[0]);
}

// Multiple stmt-s, also accept `case` and `default`.
static Vector *read_stmts(void) {
  Vector *stmts = NULL;
  for (;;) {
    if (consume(TK_RBRACE))
      return stmts;

    Stmt *stmt;
    Token *tok;
    if ((stmt = parse_vardecl()) != NULL)
      ;
    else if ((tok = consume(TK_CASE)) != NULL)
      stmt = parse_case(tok);
    else if ((tok = consume(TK_DEFAULT)) != NULL)
      stmt = parse_default(tok);
    else
      stmt = statement();

    if (stmt == NULL)
      continue;
    if (stmts == NULL)
      stmts = new_vector();
    vec_push(stmts, stmt);
  }
}

static Stmt *parse_block(const Token *tok) {
  Vector *stmts = read_stmts();
  return new_stmt_block(tok, stmts);
}

static Stmt *statement(void) {
  Token *label = consume(TK_IDENT);
  if (label != NULL) {
    if (consume(TK_COLON)) {
      return new_stmt_label(label, statement());
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
    return parse_break_continue(ST_BREAK, tok);
  }
  if ((tok = consume(TK_CONTINUE)) != NULL) {
    return parse_break_continue(ST_CONTINUE, tok);
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
  return new_stmt_expr(val);
}

static Stmt *parse_defun(const Type *rettype, int flag, Token *ident) {
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
  return new_stmt_defun(defun);
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

static Stmt *parse_global_var_decl(const Type *rawtype, int flag, const Type *type, Token *ident) {
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

  return decls != NULL ? new_stmt_vardecl(decls) : NULL;
}

static Stmt *toplevel(void) {
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

Vector *parse_program(Vector *stmts) {
  if (stmts == NULL)
    stmts = new_vector();
  while (!consume(TK_EOF)) {
    Stmt *stmt = toplevel();
    if (stmt != NULL)
      vec_push(stmts, stmt);
  }
  return stmts;
}
