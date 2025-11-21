#pragma once

#include <stdbool.h>
#include <stddef.h>  // size_t
#include <sys/types.h>  // ssize_t

typedef struct Expr Expr;
typedef struct MemberInfo MemberInfo;
typedef struct Token Token;
typedef struct Type Type;

#if defined(__GNUC__) || defined(__clang__)
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wpedantic"
#endif
enum ExprKind;
enum StrKind;
#if defined(__GNUC__) || defined(__clang__)
#pragma GCC diagnostic pop
#endif

Expr *string_expr(const Token *token, char *str, ssize_t len, enum StrKind kind);
Expr *calc_type_size(const Type *type);
#ifndef __NO_VLA
Expr *reserve_vla_type_size(Type *type);
Expr *calc_vla_size(Type *type);
#endif
Expr *make_cast(Type *type, const Token *token, Expr *sub, bool is_explicit);
Expr *reduce_refer(Expr *expr);
Expr *make_refer(const Token *tok, Expr *expr);
Expr *promote_to_int(Expr *expr);
Expr *new_expr_num_bop(enum ExprKind kind, const Token *tok, Expr *lhs, Expr *rhs);
Expr *new_expr_int_bop(enum ExprKind kind, const Token *tok, Expr *lhs, Expr *rhs);
Expr *new_expr_addsub(enum ExprKind kind, const Token *tok, Expr *lhs, Expr *rhs);
#ifndef __NO_BITFIELD
Expr *extract_bitfield_value(Expr *src, const MemberInfo *minfo);
Expr *assign_bitfield_member(const Token *tok, Expr *dst, Expr *src, Expr *val,
                             const MemberInfo *minfo);
Expr *assign_to_bitfield(const Token *tok, Expr *lhs, Expr *rhs, const MemberInfo *minfo);
#endif
Expr *incdec_of(enum ExprKind kind, Expr *target, const Token *tok);
Expr *new_expr_cmp(enum ExprKind kind, const Token *tok, Expr *lhs, Expr *rhs);
Expr *make_cond(Expr *expr);
Expr *make_not_expr(const Token *tok, Expr *expr);
Expr *transform_assign_with(const Token *tok, Expr *lhs, Expr *rhs);
Expr *simplify_funcall(Expr *funcall);
