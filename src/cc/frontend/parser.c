#include "../../config.h"
#include "parser.h"

#include <assert.h>
#include <inttypes.h>  // PRId64
#include <stdbool.h>

#include "ast.h"
#include "fe_misc.h"
#include "initializer.h"
#include "lexer.h"
#include "table.h"
#include "type.h"
#include "util.h"
#include "var.h"

static Stmt *parse_stmt(void);

Token *consume(enum TokenKind kind, const char *error) {
  Token *tok = match(kind);
  if (tok == NULL)
    parse_error(PE_NOFATAL, tok, error);
  return tok;
}

static void add_func_label(const Token *tok, Stmt *label) {
  assert(curfunc != NULL);
  Table *table = curfunc->label_table;
  if (table == NULL) {
    curfunc->label_table = table = alloc_table();
  }
  if (!table_put(table, tok->ident, label))
    parse_error(PE_NOFATAL, tok, "Label `%.*s' already defined", NAMES(tok->ident));
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
      if (label_table != NULL &&
          (label = table_get(label_table, stmt->goto_.label->ident)) != NULL) {
        label->label.used = true;
      } else {
        const Name *name = stmt->goto_.label->ident;
        parse_error(PE_NOFATAL, stmt->goto_.label, "`%.*s' not found", NAMES(name));
      }
    }
  }

  // Check label is used.
  if (label_table != NULL) {
    const Name *name;
    Stmt *label;
    for (int it = 0; (it = table_iterate(label_table, it, &name, (void**)&label)) != -1; ) {
      if (!label->label.used) {
        parse_error(PE_WARNING, label->token, "`%.*s' not used", NAMES(name));
        // Remove label in safely.
        table_delete(label_table, name);
        *label = *label->label.stmt;
      }
    }
  }
}

//

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

//

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
      if (!(tmp_storage & VS_TYPEDEF))
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
    Initializer *init = (type->kind != TY_FUNC && match(TK_ASSIGN)) ? parse_initializer() : NULL;
    init = check_vardecl(&type, ident, tmp_storage, init);
    varinfo->type = type;  // type might be changed.
    VarDecl *decl = new_vardecl(ident->ident);
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
    parse_error(PE_NOFATAL, tok, "Case value `%" PRId64 "' already defined", value->fixnum);
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

  if (scope == NULL) {
    assert(decls == NULL);
    return stmt;
  }

  assert(decls != NULL);
  Vector *stmts = new_vector();
  construct_initializing_stmts(decls);
  vec_push(stmts, new_stmt_vardecl(decls));

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
    val = str_to_char_array_var(curscope, val);
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
    } else {
      stmt = parse_stmt();
    }

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
  case TK_CASE:
    return parse_case(tok);
  case TK_DEFAULT:
    return parse_default(tok);
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
  return new_stmt_expr(str_to_char_array_var(curscope, val));
}

static Declaration *parse_defun(Type *functype, int storage, Token *ident) {
  assert(functype->kind == TY_FUNC);

  bool prototype = match(TK_SEMICOL) != NULL;
  if (!prototype && functype->func.params == NULL) {  // Old-style
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
      parse_error(PE_NOFATAL, ident, "Definition conflict: `%.*s'", NAMES(func->name));
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
      parse_error(PE_NOFATAL, ident, "`%.*s' function already defined", NAMES(func->name));
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
            parse_error(PE_WARNING, func->body_block->block.rbrace,
                        "`main' return type should be `int'");
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

static Declaration *parse_global_var_decl(Type *rawtype, int storage, Type *type, Token *ident) {
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
        bool reg = false;
        VarInfo *varinfo = NULL;
        if (ident != NULL) {
          varinfo = find_var_from_scope(curscope, ident, type, storage);
          if (varinfo == NULL) {
            varinfo = add_var_to_scope(global_scope, ident, type, storage);
            reg = true;
          }
        }

        Initializer *init = NULL;
        if (match(TK_ASSIGN) != NULL)
          init = parse_initializer();

        if (ident != NULL) {
          varinfo->global.init = check_vardecl(&type, ident, storage, init);
          varinfo->type = type;  // type might be changed.
          if (reg) {
            VarDecl *decl = new_vardecl(ident->ident);
            if (decls == NULL)
              decls = new_vector();
            vec_push(decls, decl);
          }
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
