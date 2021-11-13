// Parser

#pragma once

#include <stdbool.h>

#include "ast.h"  // ExprKind

typedef struct Expr Expr;
typedef struct Function Function;
typedef struct Initializer Initializer;
typedef struct MemberInfo MemberInfo;
typedef struct Name Name;
typedef struct Scope Scope;
typedef struct Stmt Stmt;
typedef struct Token Token;
typedef struct Type Type;
typedef struct VarInfo VarInfo;
typedef struct Vector Vector;

extern Function *curfunc;
extern Scope *curscope;
extern Stmt *curswitch;
extern Vector *toplevel;  // <Declaration*>

void parse(Vector *decls);  // <Declaraion*>

//

typedef Expr *(*BuiltinExprProc)(const Token*);
void add_builtin_expr_ident(const char *str, BuiltinExprProc *proc);

Type *parse_raw_type(int *pstorage);
const Type *parse_type_modifier(const Type *type);
const Type *parse_type_suffix(const Type *type);
const Type *parse_full_type(int *pstorage, Token **pident);

Vector *parse_args(Token **ptoken);
Vector *parse_funparams(bool *pvaargs);  // Vector<VarInfo*>, NULL=>old style.
bool parse_var_def(Type **prawType, const Type **ptype, int *pstorage, Token **pident);
Vector *extract_varinfo_types(const Vector *params);  // <VarInfo*> => <Type*>
Expr *parse_const(void);
Expr *parse_assign(void);
Expr *parse_expr(void);

void not_void(const Type *type, const Token *token);
void not_const(const Type *type, const Token *token);
void ensure_struct(Type *type, const Token *token, Scope *scope);
bool check_cast(const Type *dst, const Type *src, bool zero, bool is_explicit, const Token *token);
Expr *make_cast(const Type *type, const Token *token, Expr *sub, bool is_explicit);
Expr *make_cond(Expr *expr);
const MemberInfo *search_from_anonymous(const Type *type, const Name *name, const Token *ident,
                                        Vector *stack);
VarInfo *str_to_char_array(Scope *scope, const Type *type, Initializer *init, Vector *toplevel);
Expr *str_to_char_array_var(Scope *scope, Expr *str, Vector *toplevel);
Expr *new_expr_addsub(enum ExprKind kind, const Token *tok, Expr *lhs, Expr *rhs, bool keep_left);

Initializer *parse_initializer(void);
void fix_array_size(Type *type, Initializer *init);
Vector *assign_initial_value(Expr *expr, Initializer *init, Vector *inits);
Expr *make_refer(const Token *tok, Expr *expr);

const Type *get_callee_type(Expr *func);
void check_funcall_args(Expr *func, Vector *args, Scope *scope, Vector *toplevel);

Stmt *parse_block(const Token *tok);

VarInfo *add_var_to_scope(Scope *scope, const Token *ident, const Type *type, int storage);
