#include "wcc.h"

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>  // malloc
#include <string.h>

#include "ast.h"
#include "lexer.h"  // parse_error
#include "parser.h"  // curfunc
#include "type.h"
#include "util.h"
#include "wasm_util.h"

#define ADD_CODE(...)  do { unsigned char buf[] = {__VA_ARGS__}; add_code(buf, sizeof(buf)); } while (0)
#define ADD_LEB128(x)  do { unsigned char buf[5], *p = emit_leb128(buf, x); add_code(buf, p - buf); } while (0)
#define ADD_ULEB128(x) do { unsigned char buf[5], *p = emit_uleb128(buf, x); add_code(buf, p - buf); } while (0)

unsigned char* code;
size_t codesize;

static void add_code(const unsigned char* buf, size_t size) {
  size_t newsize = codesize + size;
  code = realloc(code, newsize);
  if (code == NULL)
    error("not enough memory");
  memcpy(code + codesize, buf, size);
  codesize = newsize;
}

unsigned char *emit_leb128(unsigned char *buf, int32_t val) {
  const int32_t MAX = 1 << 6;
  for (;;) {
    if (val < MAX && val >= -MAX) {
      *buf++ = val & 0x7f;
      return buf;
    }
    *buf++ = (val & 0x7f) | 0x80;
    val >>= 7;
  }
}

unsigned char *emit_uleb128(unsigned char *buf, uint32_t val) {
  const uint32_t MAX = 1 << 7;
  for (;;) {
    if (val < MAX) {
      *buf++ = val & 0x7f;
      return buf;
    }
    *buf++ = (val & 0x7f) | 0x80;
    val >>= 7;
  }
}

////////////////////////////////////////////////

static void gen_arith(enum ExprKind kind, const Type *type) {
  UNUSED(type);
  assert(is_fixnum(type->kind));
  switch (kind) {
  case EX_ADD:  ADD_CODE(OP_I32_ADD);  break;
  case EX_SUB:  ADD_CODE(OP_I32_SUB);  break;
  case EX_MUL:  ADD_CODE(OP_I32_MUL);  break;
  case EX_DIV:  ADD_CODE(OP_I32_DIV_S);  break;
  case EX_MOD:  ADD_CODE(OP_I32_REM_S);  break;

  default:
    fprintf(stderr, "kind=%d, ", kind);
    assert(!"Not implemeneted");
    break;
  }
}

static void gen_expr(Expr *expr) {
  switch (expr->kind) {
  case EX_FIXNUM:
    assert(expr->type->kind == TY_FIXNUM);
    ADD_CODE(OP_I32_CONST);
    ADD_LEB128(expr->fixnum);
    break;

  case EX_ADD:
  case EX_SUB:
  case EX_MUL:
  case EX_DIV:
  case EX_MOD:
  case EX_LSHIFT:
  case EX_RSHIFT:
  case EX_BITAND:
  case EX_BITOR:
  case EX_BITXOR:
    gen_expr(expr->bop.lhs);
    gen_expr(expr->bop.rhs);
    gen_arith(expr->kind, expr->type);
    break;

  default: assert(!"Not implemeneted"); break;
  }
}

static void gen_return(Stmt *stmt) {
  assert(curfunc != NULL);
  //BB *bb = bb_split(curbb);
  if (stmt->return_.val != NULL) {
    Expr *val = stmt->return_.val;
    /*VReg *reg =*/ gen_expr(val);
    assert(curfunc->retval == NULL);
  }
  //new_ir_jmp(COND_ANY, curfunc->ret_bb);
  //set_curbb(bb);
}

static void gen_stmt(Stmt *stmt) {
  if (stmt == NULL)
    return;

  switch (stmt->kind) {
  //case ST_EXPR:  gen_expr_stmt(stmt->expr); break;
  case ST_RETURN:  gen_return(stmt); break;
  /*case ST_BLOCK:  gen_block(stmt); break;
  case ST_IF:  gen_if(stmt); break;
  case ST_SWITCH:  gen_switch(stmt); break;
  case ST_CASE:  gen_case(stmt); break;
  case ST_DEFAULT:  gen_default(); break;
  case ST_WHILE:  gen_while(stmt); break;
  case ST_DO_WHILE:  gen_do_while(stmt); break;
  case ST_FOR:  gen_for(stmt); break;
  case ST_BREAK:  gen_break(); break;
  case ST_CONTINUE:  gen_continue(); break;
  case ST_GOTO:  gen_goto(stmt); break;
  case ST_LABEL:  gen_label(stmt); break;
  case ST_VARDECL:  gen_vardecl(stmt->vardecl.decls, stmt->vardecl.inits); break;
  case ST_ASM:  gen_asm(stmt); break;*/

  default:
    parse_error(stmt->token, "Unhandled stmt: %d", stmt->kind);
    break;
  }
}

static void gen_stmts(Vector *stmts) {
  if (stmts == NULL)
    return;

  for (int i = 0, len = stmts->len; i < len; ++i) {
    Stmt *stmt = stmts->data[i];
    if (stmt == NULL)
      continue;
    gen_stmt(stmt);
  }
}

static void gen_defun(Function *func) {
  if (func->scopes == NULL)  // Prototype definition
    return;

  curfunc = func;

  // Statements
  gen_stmts(func->stmts);
  ADD_CODE(OP_END);

  //
#if 1
  unsigned int local_count = 0;
  unsigned char buf[5], *p = emit_uleb128(buf, local_count);

  size_t newcodesize = (p - buf) + codesize;
  unsigned char *newcode = realloc(code, newcodesize);
  assert(newcode != NULL);
  code = newcode;
  memmove(code + (p - buf), code, codesize);
  memcpy(code, buf, p - buf);
  codesize = newcodesize;
#endif

  curfunc = NULL;
}

static void gen_decl(Declaration *decl) {
  if (decl == NULL)
    return;

  switch (decl->kind) {
  case DCL_DEFUN:
    gen_defun(decl->defun.func);
    break;
  case DCL_VARDECL:
    break;

  default:
    error("Unhandled decl: %d", decl->kind);
    break;
  }
}

void gen(Vector *decls) {
  if (decls == NULL)
    return;

  for (int i = 0, len = decls->len; i < len; ++i) {
    Declaration *decl = decls->data[i];
    if (decl == NULL)
      continue;
    gen_decl(decl);
  }
}
