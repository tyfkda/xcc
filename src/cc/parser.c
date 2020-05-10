#include "parser.h"

#include <assert.h>
#include <stdbool.h>
#include <stdlib.h>  // malloc

#include "ast.h"
#include "lexer.h"
#include "type.h"
#include "util.h"
#include "var.h"

Defun *curdefun;

static Stmt *parse_stmt(void);

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

// Initializer

static Initializer *parse_initializer(void) {
  Initializer *result = malloc(sizeof(*result));
  const Token *lblace_tok;
  if ((lblace_tok = match(TK_LBRACE)) != NULL) {
    Vector *multi = new_vector();
    if (!match(TK_RBRACE)) {
      for (;;) {
        Initializer *init;
        const Token *tok;
        if (match(TK_DOT)) {  // .member=value
          Token *ident = consume(TK_IDENT, "`ident' expected for dotted initializer");
          consume(TK_ASSIGN, "`=' expected for dotted initializer");
          Initializer *value = parse_initializer();
          init = malloc(sizeof(*init));
          init->kind = IK_DOT;
          init->token = ident;
          init->dot.name = ident->ident;
          init->dot.value = value;
        } else if ((tok = match(TK_LBRACKET)) != NULL) {
          Expr *index = parse_const();
          consume(TK_RBRACKET, "`]' expected");
          match(TK_ASSIGN);  // both accepted: `[1] = 2`, and `[1] 2`
          Initializer *value = parse_initializer();
          init = malloc(sizeof(*init));
          init->kind = IK_ARR;
          init->token = tok;
          init->arr.index = index;
          init->arr.value = value;
        } else {
          init = parse_initializer();
        }
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
    result->kind = IK_MULTI;
    result->token = lblace_tok;
    result->multi = multi;
  } else {
    result->kind = IK_SINGLE;
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
    if (match(TK_LPAR)) {  // Function prototype.
      bool vaargs;
      Vector *params = parse_funparams(&vaargs);
      Vector *param_types = extract_varinfo_types(params);
      type = new_func_type(type, params, param_types, vaargs);
      flag |= VF_EXTERN;
    } else {
      not_void(type);

      assert(curscope != NULL);
      add_cur_scope(ident, type, flag);

      if (match(TK_ASSIGN)) {
        init = parse_initializer();
      }
    }

    VarDecl *decl = new_vardecl(type, ident, init, flag);
    if (decls == NULL)
      decls = new_vector();
    vec_push(decls, decl);
  } while (match(TK_COMMA));
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

  consume(TK_SEMICOL, "`;' expected");

  return decls != NULL ? new_stmt_vardecl(decls) : NULL;
}

static Stmt *parse_if(const Token *tok) {
  consume(TK_LPAR, "`(' expected");
  Expr *cond = parse_expr();
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
  consume(TK_RPAR, "`)' expected");
  Stmt *swtch = new_stmt_switch(tok, value);
  swtch->switch_.body = parse_stmt();
  return swtch;
}

static Stmt *parse_case(const Token *tok) {
  Expr *value = parse_const();
  consume(TK_COLON, "`:' expected");
  return new_stmt_case(tok, value);
}

static Stmt *parse_default(const Token *tok) {
  consume(TK_COLON, "`:' expected");
  return new_stmt_default(tok);
}

static Stmt *parse_while(const Token *tok) {
  consume(TK_LPAR, "`(' expected");
  Expr *cond = parse_expr();
  consume(TK_RPAR, "`)' expected");
  Stmt *body = parse_stmt();

  return new_stmt_while(tok, cond, body);
}

static Stmt *parse_do_while(void) {
  Stmt *body = parse_stmt();

  const Token *tok = consume(TK_WHILE, "`while' expected");
  consume(TK_LPAR, "`(' expected");
  Expr *cond = parse_expr();
  consume(TK_RPAR, "`)' expected");
  consume(TK_SEMICOL, "`;' expected");
  return new_stmt_do_while(body, tok, cond);
}

static Stmt *parse_for(const Token *tok) {
  consume(TK_LPAR, "`(' expected");
  Expr *pre = NULL;
  Vector *decls = NULL;
  Scope *scope = enter_scope(curdefun, NULL);
  if (!match(TK_SEMICOL)) {
    const Type *rawType = NULL;
    Type *type;
    int flag;
    Token *ident;
    if (parse_var_def(&rawType, (const Type**)&type, &flag, &ident)) {
      if (ident == NULL)
        parse_error(NULL, "Ident expected");
      decls = parse_vardecl_cont(rawType, type, flag, ident);
      consume(TK_SEMICOL, "`;' expected");
    } else {
      pre = parse_expr();
      consume(TK_SEMICOL, "`;' expected");
    }
  }

  Expr *cond = NULL;
  Expr *post = NULL;
  Stmt *body = NULL;
  if (!match(TK_SEMICOL)) {
    cond = parse_expr();
    consume(TK_SEMICOL, "`;' expected");
  }
  if (!match(TK_RPAR)) {
    post = parse_expr();
    consume(TK_RPAR, "`)' expected");
  }
  body = parse_stmt();
  exit_scope();

  Stmt *stmt = new_stmt_for(tok, pre, cond, post, body);
  Vector *stmts = new_vector();
  if (decls != NULL)
    vec_push(stmts, new_stmt_vardecl(decls));
  vec_push(stmts, stmt);
  return new_stmt_block(NULL, stmts, scope);
}

static Stmt *parse_break_continue(enum StmtKind kind, const Token *tok) {
  consume(TK_SEMICOL, "`;' expected");
  return new_stmt(kind, tok);
}

static Stmt *parse_goto(void) {
  Token *label = consume(TK_IDENT, "label for goto expected");
  consume(TK_SEMICOL, "`;' expected");
  return new_stmt_goto(label);
}

static Stmt *parse_return(const Token *tok) {
  Expr *val = NULL;
  if (!match(TK_SEMICOL)) {
    val = parse_expr();
    consume(TK_SEMICOL, "`;' expected");
  }
  return new_stmt_return(tok, val);
}

static Stmt *parse_asm(const Token *tok) {
  consume(TK_LPAR, "`(' expected");

  Token *token;
  Vector *args = parse_args(&token);

  if (args == NULL || args->len != 1 || ((Expr*)args->data[0])->kind != EX_STR)
    parse_error(token, "`__asm' expected one string");

  return new_stmt_asm(tok, args->data[0]);
}

// Multiple stmt-s, also accept `case` and `default`.
static Vector *parse_stmts(void) {
  Vector *stmts = new_vector();
  for (;;) {
    if (match(TK_RBRACE))
      return stmts;

    Stmt *stmt;
    Token *tok;
    if ((stmt = parse_vardecl()) != NULL)
      ;
    else if ((tok = match(TK_CASE)) != NULL)
      stmt = parse_case(tok);
    else if ((tok = match(TK_DEFAULT)) != NULL)
      stmt = parse_default(tok);
    else
      stmt = parse_stmt();

    if (stmt == NULL)
      continue;
    vec_push(stmts, stmt);
  }
}

static Stmt *parse_block(const Token *tok) {
  Scope *scope = enter_scope(curdefun, NULL);
  Vector *stmts = parse_stmts();
  Stmt *stmt = new_stmt_block(tok, stmts, scope);
  exit_scope();
  return stmt;
}

static Stmt *parse_stmt(void) {
  Token *label = match(TK_IDENT);
  if (label != NULL) {
    if (match(TK_COLON)) {
      return new_stmt_label(label, parse_stmt());
    }
    unget_token(label);
  }

  if (match(TK_SEMICOL))
    return NULL;

  const Token *tok;
  if ((tok = match(TK_LBRACE)) != NULL)
    return parse_block(tok);

  if ((tok = match(TK_IF)) != NULL)
    return parse_if(tok);

  if ((tok = match(TK_SWITCH)) != NULL)
    return parse_switch(tok);

  if ((tok = match(TK_WHILE)) != NULL)
    return parse_while(tok);

  if (match(TK_DO))
    return parse_do_while();

  if ((tok = match(TK_FOR)) != NULL)
    return parse_for(tok);

  if ((tok = match(TK_BREAK)) != NULL) {
    return parse_break_continue(ST_BREAK, tok);
  }
  if ((tok = match(TK_CONTINUE)) != NULL) {
    return parse_break_continue(ST_CONTINUE, tok);
  }
  if (match(TK_GOTO)) {
    return parse_goto();
  }

  if ((tok = match(TK_RETURN)) != NULL)
    return parse_return(tok);

  if ((tok = match(TK_ASM)) != NULL)
    return parse_asm(tok);

  // expression statement.
  Expr *val = parse_expr();
  consume(TK_SEMICOL, "`;' expected");
  return new_stmt_expr(val);
}

static Declaration *parse_defun(const Type *functype, int flag, Token *ident) {
  assert(functype->kind == TY_FUNC);
  Function *func = new_func(functype, ident->ident);
  Defun *defun = new_defun(func, flag);
  if (match(TK_SEMICOL)) {
    // Prototype declaration.
  } else {
    consume(TK_LBRACE, "`;' or `{' expected");

    assert(curdefun == NULL);
    assert(curscope == NULL);
    curdefun = defun;
    Vector *top_vars = NULL;
    Vector *params = defun->func->type->func.params;
    if (params != NULL) {
      top_vars = new_vector();
      for (int i = 0; i < params->len; ++i)
        vec_push(top_vars, params->data[i]);
    }
    defun->func->scopes = new_vector();
    enter_scope(defun, top_vars);  // Scope for parameters.
    defun->stmts = parse_stmts();
    exit_scope();
    assert(curscope == NULL);
    curdefun = NULL;
  }
  return new_decl_defun(defun);
}

static void parse_typedef(void) {
  int flag;
  Token *ident;
  const Type *type = parse_full_type(&flag, &ident);
  if (type == NULL)
    parse_error(NULL, "type expected");
  not_void(type);

  if (ident == NULL) {
    ident = consume(TK_IDENT, "ident expected");
  }
  const Name *name = ident->ident;
  const Type *conflict = find_typedef(name);
  if (conflict != NULL) {
    if (!same_type(type, conflict))
      parse_error(ident, "Conflict typedef");
  }

  if (conflict == NULL || type->kind != TY_STRUCT || type->struct_.info != NULL)
    add_typedef(name, type);

  consume(TK_SEMICOL, "`;' expected");
}

static Declaration *parse_global_var_decl(const Type *rawtype, int flag, const Type *type,
                                          Token *ident) {
  Vector *decls = NULL;
  for (;;) {
    if (type->kind == TY_VOID)
      parse_error(ident, "`void' not allowed");

    if (!(type->kind == TY_PTR && type->pa.ptrof->kind == TY_FUNC))
      type = parse_type_suffix(type);
    Initializer *init = NULL;
    if (match(TK_ASSIGN) != NULL)
      init = parse_initializer();

    VarDecl *decl = new_vardecl(type, ident, init, flag);
    if (decls == NULL)
      decls = new_vector();
    vec_push(decls, decl);
    if (!match(TK_COMMA))
      break;

    // Next declaration.
    type = parse_type_modifier(rawtype);
    ident = consume(TK_IDENT, "`ident' expected");
  }

  consume(TK_SEMICOL, "`;' or `,' expected");
  return decls != NULL ? new_decl_vardecl(decls) : NULL;
}

static Declaration *parse_declaration(void) {
  const Type *rawtype = NULL, *type;
  int flag;
  Token *ident;
  if (parse_var_def(&rawtype, &type, &flag, &ident)) {
    if (ident == NULL) {
      if ((type->kind == TY_STRUCT ||
           (type->kind == TY_NUM && type->num.kind == NUM_ENUM)) &&
          match(TK_SEMICOL)) {
        // Just struct/union or enum definition.
      } else {
        parse_error(NULL, "Ident expected");
      }
      return NULL;
    }

    if (type->kind == TY_FUNC)
      return parse_defun(type, flag, ident);

    return parse_global_var_decl(rawtype, flag, type, ident);
  }
  if (match(TK_TYPEDEF)) {
    parse_typedef();
    return NULL;
  }
  parse_error(NULL, "Unexpected token");
  return NULL;
}

Vector *parse(Vector *decls) {
  if (decls == NULL)
    decls = new_vector();
  while (!match(TK_EOF)) {
    Declaration *decl = parse_declaration();
    if (decl != NULL)
      vec_push(decls, decl);
  }
  return decls;
}
