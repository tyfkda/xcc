#include "../../config.h"
#include "parser.h"

#include <assert.h>
#include <inttypes.h>  // PRId64
#include <stdbool.h>
#include <string.h>

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

extern inline void add_func_label(const Token *tok, Stmt *label) {
  assert(curfunc != NULL);
  Table *table = curfunc->label_table;
  if (table == NULL) {
    curfunc->label_table = table = alloc_table();
  }
  if (!table_put(table, tok->ident, label))
    parse_error(PE_NOFATAL, tok, "Label `%.*s' already defined", NAMES(tok->ident));
}

extern inline void add_func_goto(Stmt *stmt) {
  assert(curfunc != NULL);
  if (curfunc->gotos == NULL)
    curfunc->gotos = new_vector();
  vec_push(curfunc->gotos, stmt);
}

extern inline void check_goto_labels(Function *func) {
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

static Initializer *parse_initializer_multi(void) {
  Initializer *init = NULL;
  const Token *tok;
  if (match(TK_DOT)) {  // .member=value
    Token *ident = consume(TK_IDENT, "ident expected for dotted initializer");
    Initializer *value = parse_initializer_multi();
    if (value == NULL) {
      consume(TK_ASSIGN, "`=' expected for dotted initializer");
      value = parse_initializer();
    }
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
    Initializer *value = parse_initializer_multi();
    if (value == NULL) {
      match(TK_ASSIGN);  // both accepted: `[1] = 2`, and `[1] 2`
      value = parse_initializer();
    }
    init = new_initializer(IK_BRKT, tok);
    init->bracket.index = index;
    init->bracket.value = value;
  }
  return init;
}

Initializer *parse_initializer(void) {
  Initializer *result;
  const Token *lblace_tok;
  if ((lblace_tok = match(TK_LBRACE)) != NULL) {
    Vector *multi = new_vector();
    if (!match(TK_RBRACE)) {
      for (;;) {
        Initializer *init = parse_initializer_multi();
        if (init == NULL)
          init = parse_initializer();
        assert(init != NULL);
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

static void def_type(Type *type, Token *ident) {
  const Name *name = ident->ident;
  Scope *scope;
  Type *defined = find_typedef(curscope, name, &scope);
  if (defined != NULL && scope == curscope) {
    if (same_type(type, defined))
      return;
    parse_error(PE_NOFATAL, ident, "Conflict typedef");
  } else if (scope_find(curscope, ident->ident, &scope) != NULL && scope == curscope) {
    parse_error(PE_NOFATAL, ident, "Conflict typedef with variable");
    return;
  }

  if (defined == NULL || (type->kind == TY_STRUCT && type->struct_.info != NULL)) {
    if (type->kind == TY_ARRAY) {
      if (!ensure_struct(type, ident, curscope))
        return;
    }
    add_typedef(curscope, name, type);
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
      Vector *param_vars = parse_funparams(&vaargs);
      Vector *param_types = extract_varinfo_types(param_vars);
      type = new_func_type(type, param_types, vaargs);
      type->func.param_vars = param_vars;
    } else {
      if (!(tmp_storage & VS_TYPEDEF)) {
        if (!not_void(type, NULL))
          type = &tyInt;  // Deceive to continue compiling.
      }
    }

    if (type->kind == TY_FUNC /* && !is_global_scope(curscope)*/) {
      // Must be prototype.
      tmp_storage |= VS_EXTERN;
    }

    assert(!is_global_scope(curscope));

#ifndef __NO_VLA
    if (type->kind == TY_ARRAY && type->pa.vla != NULL)
      type = array_to_ptr(type);
#endif

    if (tmp_storage & VS_TYPEDEF) {
#ifndef __NO_VLA
      Expr *assign_sizevar = reserve_vla_type_size(type);
      if (assign_sizevar != NULL) {
        Expr *e = assign_sizevar;
        while (e->kind == EX_COMMA)
          e = e->bop.rhs;
        assert(e->kind == EX_ASSIGN);
        Expr *size_var = e->bop.lhs;
        assert(size_var->kind == EX_VAR);

        VarDecl *decl = new_vardecl(size_var->var.name);
        decl->init_stmt = new_stmt_expr(assign_sizevar);
        if (decls == NULL)
          decls = new_vector();
        vec_push(decls, decl);
      }
#endif
      def_type(type, ident);
      continue;
    }

#ifndef __NO_VLA
    if (curfunc != NULL) {
      Expr *size = calc_vla_size(type);
      if (size != NULL) {
        // If the type has VLA, assign its size to the temporary variable
        // adding dummy declaration (decl.ident = NULL).
        if (decls == NULL)
          decls = new_vector();
        VarDecl *decl = new_vardecl(NULL);
        decl->init_stmt = new_stmt_expr(size);
        vec_push(decls, decl);
      }
    }
#endif

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

extern inline int find_case(Stmt *swtch, Fixnum v) {
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

extern inline Stmt *parse_default(const Token *tok) {
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
      scope = enter_scope(curfunc);
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

extern inline Stmt *parse_break_continue(enum StmtKind kind, const Token *tok) {
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

extern inline Stmt *parse_goto(const Token *tok) {
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
    if (rettype->kind == TY_VOID) {
      if (val->type->kind == TY_VOID) {
        // Allow `return void_fnc();`.
        parse_error(PE_WARNING, val->token, "`return' with void value");
      } else {
        parse_error(PE_NOFATAL, val->token, "void function `return' a value");
      }
    } else {
      val = make_cast(rettype, val->token, val, false);
    }
  }

  return new_stmt_return(tok, val);
}

extern inline Expr *parse_asm_arg(void) {
  /*const Token *str =*/ consume(TK_STR, "string literal expected");
  consume(TK_LPAR, "`(' expected");
  Expr *var = parse_expr();
  if (var == NULL || var->kind != EX_VAR) {
    parse_error(PE_FATAL, var != NULL ? var->token : NULL, "string literal expected");
  }
  consume(TK_RPAR, "`)' expected");
  return var;
}

extern inline Stmt *parse_asm(const Token *tok) {
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

Stmt *parse_block(const Token *tok, Scope *scope) {
  if (scope == NULL)
    scope = enter_scope(curfunc);
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
    return new_stmt(ST_EMPTY, tok);
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

static Table *parse_attribute(Table *attributes) {  // <Token*>
  // __attribute__((name1, name2(p, q, ...), ...))

  if (consume(TK_LPAR, "`(' expected") == NULL || consume(TK_LPAR, "`(' expected") == NULL) {
    match(TK_RPAR);
    match(TK_RPAR);
    return NULL;
  }

  for (;;) {
    if (match(TK_RPAR))
      break;

    const Token *name = consume(TK_IDENT, "attribute name expected");
    if (name == NULL)
      break;

    Vector *params = NULL;
    if (match(TK_LPAR)) {
      params = new_vector();
      if (!match(TK_RPAR)) {
        for (;;) {
          Token *token = match(-1);
          if (token->kind == TK_EOF) {
            parse_error(PE_NOFATAL, token, "`)' expected");
            break;
          }
          vec_push(params, token);
          if (!match(TK_COMMA)) {
            if (!match(TK_RPAR))
              parse_error(PE_NOFATAL, token, "`)' expected");
            break;
          }
        }
      }
    }
    if (attributes == NULL)
      attributes = alloc_table();
    table_put(attributes, name->ident, params);

    if (!match(TK_COMMA)) {
      if (!match(TK_RPAR))
        parse_error(PE_NOFATAL, NULL, "`)' expected");
      break;
    }
  }

  consume(TK_RPAR, "`)' expected");
  return attributes;
}

static Function *define_func(Type *functype, const Token *ident, const Vector *param_vars,
                             int storage, Table *attributes) {
  int flag = 0;
  if (attributes != NULL) {
    if (table_try_get(attributes, alloc_name("noreturn", NULL, false), NULL))
      flag |= FUNCF_NORETURN;
  }

  Function *func = new_func(functype, ident->ident, functype->func.param_vars, attributes, flag);
  func->params = param_vars;
  VarInfo *varinfo = scope_find(global_scope, func->name, NULL);
  if (varinfo == NULL) {
    varinfo = add_var_to_scope(global_scope, ident, functype, storage);
  } else {
    Declaration *predecl = varinfo->global.funcdecl;
    if (predecl != NULL) {
      assert(predecl->kind == DCL_DEFUN);
      if (predecl->defun.func != NULL) {
        int merge_flag = (flag | predecl->defun.func->flag) & FUNCF_NORETURN;
        func->flag |= merge_flag;
        predecl->defun.func->flag |= merge_flag;

        if (attributes != NULL) {
          if (predecl->defun.func->attributes != NULL) {
            const Name *name;
            Vector *params;
            for (int it = 0; (it = table_iterate(predecl->defun.func->attributes, it, &name, (void**)&params)) != -1; ) {
              if (!table_try_get(attributes, name, NULL))
                table_put(attributes, name, params);
            }
          }
          predecl->defun.func->attributes = attributes;
        }
      }
    }

    if (varinfo->type->kind != TY_FUNC ||
        !same_type(varinfo->type->func.ret, functype->func.ret) ||
        (varinfo->type->func.params != NULL && !same_type(varinfo->type, functype))) {
      parse_error(PE_NOFATAL, ident, "Definition conflict: `%.*s'", NAMES(func->name));
    } else {
      if (varinfo->global.func == NULL) {
        if (varinfo->type->func.params == NULL)  // Old-style prototype definition.
          varinfo->type = functype;  // Overwrite with actual function type.
      }
    }
  }
  return func;
}

#ifndef __NO_VLA
static void modify_funparam_vla_type(Type *type, Scope *scope) {
  if (type->kind == TY_ARRAY && type->pa.vla != NULL) {
    type->kind = TY_PTR;  // array_to_ptr, but must apply the change to the original type.
  }

  for (;;) {
    if (!ptr_or_array(type))
      break;
    if (type->pa.vla != NULL) {
      assert(type->pa.vla->kind == EX_VAR);
      type->pa.vla->var.scope = scope;  // Fix scope, which set in `parse_direct_declarator_suffix()`
    }
    type = type->pa.ptrof;
  }
}
#endif

static Declaration *parse_defun(Type *functype, int storage, Token *ident, const Token *tok,
                                Table *attributes) {
  assert(functype->kind == TY_FUNC);

  const Vector *param_vars = functype->func.param_vars;
  if (functype->func.params == NULL) {  // Old-style
    // Treat it as a zero-parameter function.
    functype->func.params = new_vector();
    functype->func.vaargs = false;
    param_vars = new_vector();
  }

  Function *func = define_func(functype, ident, param_vars, storage, attributes);
  VarInfo *varinfo = scope_find(global_scope, ident->ident, NULL);
  assert(varinfo != NULL);
  if (varinfo->global.func != NULL) {
    parse_error(PE_NOFATAL, ident, "`%.*s' function already defined", NAMES(func->name));
  } else {
    varinfo->global.func = func;
  }

  assert(curfunc == NULL);
  assert(is_global_scope(curscope));
  curfunc = func;
  Vector *top_vars = new_vector();
  for (int i = 0; i < param_vars->len; ++i) {
    VarInfo *vi = param_vars->data[i];
    vec_push(top_vars, vi);
    ensure_struct(vi->type, tok, curscope);
  }
  func->scopes = new_vector();
  Scope *scope = enter_scope(func);
  scope->vars = top_vars;

#ifndef __NO_VLA
  Vector *vla_inits = NULL;  // <Stmt*>
  for (int i = 0; i < top_vars->len; ++i) {
    VarInfo *vi = top_vars->data[i];
    Type *type = vi->type;
    Expr *vla_size = calc_vla_size(type);
    if (vla_size != NULL) {
      if (vla_inits == NULL)
        vla_inits = new_vector();
      vec_push(vla_inits, new_stmt_expr(vla_size));
    }
    modify_funparam_vla_type(type, scope);
  }
#endif

  func->body_block = parse_block(tok, scope);
  assert(is_global_scope(curscope));
  match(TK_SEMICOL);  // Ignore redundant semicolon.
  curfunc = NULL;

#ifndef __NO_VLA
  if (vla_inits != NULL) {
    assert(func->body_block->kind == ST_BLOCK);
    assert(func->body_block->block.stmts != NULL);
    Vector *stmts = func->body_block->block.stmts;
    for (int i = vla_inits->len; i-- > 0; ) {
      Stmt *stmt = vla_inits->data[i];
      vec_insert(stmts, 0, stmt);
    }
  }
#endif

  check_goto_labels(func);
  check_func_reachability(func);

  Declaration *decl = new_decl_defun(func);
  varinfo->global.funcdecl = decl;
  return decl;
}

static Declaration *parse_global_var_decl(Type *rawtype, int storage, Type *type, Token *ident,
                                          Table *attributes) {
  Vector *decls = NULL;
  for (;;) {
    while (match(TK_ATTRIBUTE))
      attributes = parse_attribute(attributes);

    if (!(type->kind == TY_PTR && type->pa.ptrof->kind == TY_FUNC) &&
        type->kind != TY_VOID)
      type = parse_type_suffix(type);

#ifndef __NO_VLA
    if (type->kind == TY_ARRAY && type->pa.vla != NULL)
      type = array_to_ptr(type);
#endif

    if (storage & VS_TYPEDEF) {
      if (ident != NULL) {
#ifndef __NO_VLA
        if (is_global_scope(curscope) && type->kind == TY_PTR && type->pa.vla != NULL)
          parse_error(PE_NOFATAL, ident, "Variable length array cannot use in global scope");
#endif
        def_type(type, ident);
      }
    } else {
      if (type->kind == TY_VOID) {
        if (ident != NULL)
          parse_error(PE_NOFATAL, ident, "`void' not allowed");
      } else if (type->kind == TY_FUNC) {
        // Prototype declaration.
        if (ident == NULL) {
          parse_error(PE_NOFATAL, NULL, "ident expected");
        } else {
          Function *func = define_func(type, ident, type->func.param_vars, storage | VS_EXTERN,
                                       attributes);
          VarInfo *varinfo = scope_find(global_scope, ident->ident, NULL);
          assert(varinfo != NULL);

          Declaration *decl = new_decl_defun(func);
          varinfo->global.funcdecl = decl;
        }
        // Check LBRACE?
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
            VarDecl *vardecl = new_vardecl(ident->ident);
            if (decls == NULL)
              decls = new_vector();
            vec_push(decls, vardecl);
          }
        }
      }
    }

    if (!match(TK_COMMA))
      break;

    attributes = NULL;  // TODO: Confirm.

    // Next declaration.
    type = parse_declarator(rawtype, &ident);
  }
  consume(TK_SEMICOL, "`;' or `,' expected");
  return decls == NULL ? NULL : new_decl_vardecl(decls);
}

static Declaration *parse_declaration(void) {
  Token *tok;
  if ((tok = match(TK_ASM)) != NULL) {
    Stmt *asm_ = parse_asm(tok);
    if (asm_->asm_.arg != NULL)
      parse_error(PE_NOFATAL, asm_->token, "no argument required");
    return new_decl_asm(asm_->asm_.str);
  }

  Table *attributes = NULL;
  while (match(TK_ATTRIBUTE))
    attributes = parse_attribute(attributes);

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

      const Token *tok = match(TK_LBRACE);
      if (tok != NULL)
        return parse_defun(type, storage, ident, tok, attributes);
      // Function prototype declaration:
      // Join with global variable declaration to handle multiple prototype declarations.
    }

    return parse_global_var_decl(rawtype, storage, type, ident, attributes);
  }
  parse_error(PE_NOFATAL, NULL, "Unexpected token");
  match(-1);  // Drop the token.
  return NULL;
}

#if XCC_TARGET_PLATFORM == XCC_PLATFORM_APPLE || XCC_TARGET_PLATFORM == XCC_PLATFORM_WASI
static Function *generate_dtor_caller_func(Vector *dtors) {
  // Generate function:
  //  void dtor_caller(void*) {
  //    dtor1();
  //    ...
  //  }

  Vector *param_types = new_vector();
  vec_push(param_types, &tyVoidPtr);
  Type *functype = new_func_type(&tyVoid, param_types, false);

  Vector *top_vars = new_vector();
  var_add(top_vars, alloc_name("p", NULL, false), &tyVoidPtr, VS_PARAM);

  const Token *functok = alloc_dummy_ident();
  Table *attributes = NULL;
  Function *func = define_func(functype, functok, top_vars, VS_STATIC, attributes);

  assert(curfunc == NULL);
  assert(is_global_scope(curscope));
  curfunc = func;

  func->scopes = new_vector();
  Scope *scope = enter_scope(func);
  scope->vars = top_vars;

  // Construct function body: call destructors.
  Vector *stmts = new_vector();
  for (int i = 0; i < dtors->len; ++i) {
    Function *dtor = dtors->data[i];
    const Token *token = NULL;
    Vector *args = new_vector();
    Expr *func = new_expr_variable(dtor->name, dtor->type, token, global_scope);
    Expr *call = new_expr_funcall(token, func, args);
    vec_push(stmts, new_stmt_expr(call));
  }

  func->body_block = new_stmt_block(NULL, stmts, scope, NULL);

  exit_scope();
  curfunc = NULL;

  return func;
}

static Function *generate_dtor_register_func(Function *dtor_caller_func) {
  // Generate function:
  //  __attribute__((constructor))
  //  void dtor_register(void) {
  //    __cxa_atexit(dtor_caller, NULL, &__dso_handle);
  //  }

  // Declare: extern void *__dso_handle;
  const Name *dso_handle_name = alloc_name("__dso_handle", NULL, false);
  scope_add(global_scope, dso_handle_name, &tyVoidPtr, VS_EXTERN);

  // Declare: extern int __cxa_atexit(void (*)(void*), void*, void*);
  const Name *cxa_atexit_name = alloc_name("__cxa_atexit", NULL, false);
  Type *cxa_atexit_functype;
  {
    Vector *cxa_atexit_param_types = new_vector();
    vec_push(cxa_atexit_param_types, &tyVoidPtr);  // void (*func)(void*)
    vec_push(cxa_atexit_param_types, &tyVoidPtr);
    vec_push(cxa_atexit_param_types, &tyVoidPtr);

    Vector *param_vars = new_vector();
    var_add(param_vars, alloc_name("x", NULL, false), &tyVoidPtr, VS_PARAM);
    var_add(param_vars, alloc_name("y", NULL, false), &tyVoidPtr, VS_PARAM);
    var_add(param_vars, alloc_name("z", NULL, false), &tyVoidPtr, VS_PARAM);

    cxa_atexit_functype = new_func_type(&tyInt, cxa_atexit_param_types, false);
    define_func(cxa_atexit_functype, alloc_ident(cxa_atexit_name, NULL, cxa_atexit_name->chars, NULL), param_vars, VS_EXTERN, NULL);
  }

  Vector *param_types = new_vector();
  Type *functype = new_func_type(&tyVoid, param_types, false);
  Vector *top_vars = new_vector();

  const Token *functok = alloc_dummy_ident();
  Table *attributes = alloc_table();
  assert(attributes != NULL);
  table_put(attributes, alloc_name("constructor", NULL, false), NULL);
  Function *func = define_func(functype, functok, top_vars, VS_STATIC, attributes);
  func->flag |= FUNCF_HAS_FUNCALL;  // Make frame pointer saved on prologue.

  assert(curfunc == NULL);
  assert(is_global_scope(curscope));
  curfunc = func;

  func->scopes = new_vector();
  Scope *scope = enter_scope(func);
  scope->vars = top_vars;

  Vector *stmts = new_vector();
  const Token *token = NULL;
  Vector *args = new_vector();
  vec_push(args, make_refer(token, new_expr_variable(dtor_caller_func->name, dtor_caller_func->type, token, global_scope)));
  vec_push(args, new_expr_fixlit(&tyVoidPtr, token, 0));
  vec_push(args, new_expr_unary(EX_REF, &tyVoidPtr, NULL, new_expr_variable(dso_handle_name, &tyVoidPtr, token, global_scope)));
  // __cxa_atexit(dtor_caller, NULL, &__dso_handle);
  vec_push(stmts, new_stmt_expr(
      new_expr_funcall(token, new_expr_variable(cxa_atexit_name, cxa_atexit_functype, token, global_scope), args)));

  func->body_block = new_stmt_block(NULL, stmts, scope, NULL);

  exit_scope();
  curfunc = NULL;

  return func;
}

static void modify_dtor_func(Vector *decls) {
  const Name *destructor_name = alloc_name("destructor", NULL, false);
  Vector *dtors = NULL;
  for (int i = 0, len = decls->len; i < len; ++i) {
    Declaration *decl = decls->data[i];
    if (decl == NULL || decl->kind != DCL_DEFUN)
      continue;
    Function *func = decl->defun.func;
    if (func->attributes != NULL) {
      if (table_try_get(func->attributes, destructor_name, NULL)) {
        const Type *type = func->type;
        if (type->func.params == NULL || type->func.params->len > 0 || type->func.ret->kind != TY_VOID) {
          const Token *token = func->body_block != NULL ? func->body_block->token : NULL;
          parse_error(PE_NOFATAL, token, "destructor must have no parameters and return void");
        } else {
          if (dtors == NULL)
            dtors = new_vector();
          vec_push(dtors, func);
        }
      }
    }
  }
  if (dtors == NULL)
    return;

  Function *caller_func = generate_dtor_caller_func(dtors);
  vec_push(decls, new_decl_defun(caller_func));

  Function *register_func = generate_dtor_register_func(caller_func);
  vec_push(decls, new_decl_defun(register_func));
}
#endif

void parse(Vector *decls) {
  curscope = global_scope;

  while (!match(TK_EOF)) {
    Declaration *decl = parse_declaration();
    if (decl != NULL)
      vec_push(decls, decl);
  }

#if XCC_TARGET_PLATFORM == XCC_PLATFORM_APPLE || XCC_TARGET_PLATFORM == XCC_PLATFORM_WASI
  modify_dtor_func(decls);
#endif
}
