#pragma once

#include <stdint.h>  // intptr_t
#include <stddef.h>  // size_t
#include <sys/types.h>  // ssize_t

typedef struct Name Name;
typedef struct Type Type;
typedef struct Token Token;
typedef struct Vector Vector;

// PpExpr

enum PpExprKind {
  // Literals
  EX_NUM,     // 1234
  EX_STR,     // "foobar"

  EX_VARIABLE,  // foobar

  // Binary operators
  EX_ADD,     // +
  EX_SUB,     // -
  EX_MUL,     // *
  EX_DIV,     // /
  EX_MOD,     // %
  EX_BITAND,  // &
  EX_BITOR,   // |
  EX_BITXOR,  // ^
  EX_LSHIFT,  // <<
  EX_RSHIFT,  // >>
  EX_EQ,      // ==
  EX_NE,      // !=
  EX_LT,      // <
  EX_LE,      // <=
  EX_GE,      // >=
  EX_GT,      // >
  EX_LOGAND,  // &&
  EX_LOGIOR,  // ||
  EX_COMMA,   // head, tail
  EX_PTRADD,  // ptr + num
  EX_PTRSUB,  // ptr - num

  // Unary operators
  EX_POS,     // +
  EX_NEG,     // -
  EX_NOT,     // !
  EX_BITNOT,  // ~x
  //EX_PREINC,  // ++e
  //EX_PREDEC,  // --e
  //EX_POSTINC, // e++
  //EX_POSTDEC, // e--

  EX_FUNCALL, // f(x, y, ...)
};

typedef struct PpExpr {
  enum PpExprKind kind;
  //const Type *type;
  const Token *token;
  union {
    intptr_t num;
    struct {
      const char *buf;
      size_t size;  // Include last '\0'.
    } str;
    struct {
      const Name *name;
    } variable;
    struct {
      struct PpExpr *lhs;
      struct PpExpr *rhs;
    } bop;
    struct {
      struct PpExpr *sub;
    } unary;
    struct {
      const Type *type;  // sizeof(Type), or
      struct PpExpr *sub;  // sizeof(value)
    } sizeof_;
    struct {
      struct PpExpr *func;
      Vector *args;  // <PpExpr*>
    } funcall;
  };
} PpExpr;

PpExpr *parse_expr(void);
