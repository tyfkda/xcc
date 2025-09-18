#pragma once

#include <stdbool.h>

#include "type.h"  // FixnumKind

typedef struct Expr Expr;
typedef struct Function Function;
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
extern VarInfo *curvarinfo;

extern int compile_warning_count;
extern int compile_error_count;

enum ParseErrorLevel {
  PE_WARNING,
  PE_NOFATAL,
  PE_FATAL,
};

typedef struct {
  bool unused_variable;
  bool unused_function;
} WarningFlags;

typedef struct {
  bool warn_as_error;  // Treat warnings as errors
  bool common;
  int optimize_level;
  WarningFlags warn;
} CcFlags;

extern CcFlags cc_flags;

bool parse_fopt(const char *optarg, bool value);
bool parse_wopt(const char *optarg, bool value);

typedef struct {
  Stmt *swtch;
  Stmt *break_;
  Stmt *continu;
} LoopScope;

#define SAVE_LOOP_SCOPE(var, b, c)  LoopScope var = loop_scope; if (b != NULL) loop_scope.break_ = b; if (c != NULL) loop_scope.continu = c;
#define RESTORE_LOOP_SCOPE(var)     loop_scope = var

extern LoopScope loop_scope;

void parse_error(enum ParseErrorLevel level, const Token *token, const char *fmt, ...);

bool not_void(const Type *type, const Token *token);
void not_const(const Type *type, const Token *token);

typedef struct {
  int storage, qualifier;
  int unsigned_num, signed_num;
  int char_num, short_num, int_num, long_num;
  int float_num, double_num;
} TypeCombination;

extern const enum FixnumKind kLongKinds[];

void check_type_combination(const TypeCombination *tc, const Token *tok);
bool no_type_combination(const TypeCombination *tc, int storage_mask, int qualifier_mask);

VarInfo *add_var_to_scope(Scope *scope, const Token *ident, Type *type, int storage,
                          bool allow_defined);
Token *alloc_dummy_ident(void);
Expr *alloc_tmp_var(Scope *scope, Type *type);
void define_enum_member(Type *type, const Token *ident, int value);
Expr *proc_builtin_function_name(const Token *tok);

Scope *enter_scope(Function *func);
void exit_scope(void);

bool ensure_struct(Type *type, const Token *token, Scope *scope);
bool check_cast(const Type *dst, const Type *src, bool zero, bool is_explicit, const Token *token);
const MemberInfo *search_from_anonymous(const Type *type, const Name *name, const Token *ident,
                                        Vector *stack);
void mark_var_used(Expr *expr);
void mark_var_used_for_func(Expr *expr);
void propagate_var_used(void);
void check_lval(const Token *tok, Expr *expr, const char *error);
#ifndef __NO_BITFIELD
void not_bitfield_member(Expr *expr);
#else
static inline void not_bitfield_member(Expr *expr)  { (void)expr; }  // Must not, so noop.
#endif
bool is_small_struct(const Type *type);
void check_funcall_args(Expr *func, Vector *args, Scope *scope);
Vector *extract_varinfo_types(const Vector *vars);  // <VarInfo*> => <Type*>
Type *choose_ternary_result_type(Expr *tval, Expr *fval);

void check_unused_variables(Function *func);
void check_func_reachability(Function *func);
bool check_funcend_return(Stmt *stmt);

int get_funparam_index(Function *func, const Name *name);  // -1: Not funparam.

bool satisfy_inline_criteria(const VarInfo *varinfo);
Stmt *embed_inline_funcall(VarInfo *varinfo);
