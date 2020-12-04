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
#include "var.h"
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

// Local variable information for WASM
struct VReg {
  uint32_t local_index;
};

static void gen_stmt(Stmt *stmt);
static void gen_stmts(Vector *stmts);
static void gen_expr_stmt(Expr *expr);

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

  case EX_VAR:
    {
      assert(is_fixnum(expr->type->kind));
      Scope *scope;
      const VarInfo *varinfo = scope_find(expr->var.scope, expr->var.name, &scope);
      assert(varinfo != NULL && scope == expr->var.scope);
      if (!is_global_scope(scope) && !(varinfo->storage & (VS_STATIC | VS_EXTERN))) {
        VReg *vreg = varinfo->local.reg;
        assert(vreg != NULL);
        ADD_CODE(OP_LOCAL_GET);
        ADD_ULEB128(vreg->local_index);
        break;
      }
      assert(!"Not implemented");
    }
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

  case EX_ASSIGN:
    {
      Expr *lhs = expr->bop.lhs;
      switch (lhs->type->kind) {
      case TY_FIXNUM:
      case TY_PTR:
#ifndef __NO_FLONUM
      case TY_FLONUM:
#endif
        if (expr->bop.lhs->kind == EX_VAR) {
          Scope *scope;
          const VarInfo *varinfo = scope_find(lhs->var.scope, lhs->var.name, &scope);
          assert(varinfo != NULL);
          if (!is_global_scope(scope) && !(varinfo->storage & (VS_STATIC | VS_EXTERN))) {
            VReg *vreg = varinfo->local.reg;
            assert(vreg != NULL);
            gen_expr(expr->bop.rhs);
            ADD_CODE(OP_LOCAL_TEE);
            ADD_ULEB128(vreg->local_index);
            break;
          }
        }
        assert(!"Not implemented");
        break;
      default: assert(false); break;
      }
    }
    break;

  default: assert(!"Not implemeneted"); break;
  }
}

static void gen_compare_expr(enum ExprKind kind, Expr *lhs, Expr *rhs) {
  assert(lhs->type->kind == rhs->type->kind);
  assert(is_fixnum(lhs->type->kind));

  // unsigned?

  gen_expr(lhs);
  gen_expr(rhs);
  switch (kind) {
  case EX_EQ:  ADD_CODE(OP_I32_EQ); break;
  case EX_NE:  ADD_CODE(OP_I32_NE); break;
  case EX_LT:  ADD_CODE(OP_I32_LT_S); break;
  case EX_LE:  ADD_CODE(OP_I32_LE_S); break;
  case EX_GE:  ADD_CODE(OP_I32_GE_S); break;
  case EX_GT:  ADD_CODE(OP_I32_GT_S); break;
  default: assert(false); break;
  }
}

static void gen_cond(Expr *cond, bool tf) {
  enum ExprKind ck = cond->kind;
  switch (ck) {
  case EX_FIXNUM:
    {
      Expr *zero = new_expr_fixlit(&tyInt, NULL, 0);
      gen_compare_expr(tf ? EX_NE : EX_EQ, cond, zero);
    }
    break;
  case EX_EQ:
  case EX_NE:
  case EX_LT:
  case EX_GT:
  case EX_LE:
  case EX_GE:
    if (!tf) {
      if (ck <= EX_NE)
        ck = (EX_EQ + EX_NE) - ck;
      else
        ck = EX_LT + ((ck - EX_LT) ^ 2);
    }
    gen_compare_expr(ck, cond->bop.lhs, cond->bop.rhs);
    break;
  case EX_LOGAND:
    if (tf) {
      gen_cond(cond->bop.lhs, true);
      ADD_CODE(OP_IF, WT_I32);
      gen_cond(cond->bop.rhs, true);
      ADD_CODE(OP_ELSE);
      ADD_CODE(OP_I32_CONST, 0);  // false
      ADD_CODE(OP_END);
    } else {
      gen_cond(cond->bop.lhs, false);
      ADD_CODE(OP_IF, WT_I32);
      ADD_CODE(OP_I32_CONST, 1);  // true
      ADD_CODE(OP_ELSE);
      gen_cond(cond->bop.rhs, false);
      ADD_CODE(OP_END);
    }
    break;
  case EX_LOGIOR:
    if (tf) {
      gen_cond(cond->bop.lhs, true);
      ADD_CODE(OP_IF, WT_I32);
      ADD_CODE(OP_I32_CONST, 1);  // true
      ADD_CODE(OP_ELSE);
      gen_cond(cond->bop.rhs, true);
      ADD_CODE(OP_END);
    } else {
      gen_cond(cond->bop.lhs, false);
      ADD_CODE(OP_IF, WT_I32);
      gen_cond(cond->bop.rhs, false);
      ADD_CODE(OP_ELSE);
      ADD_CODE(OP_I32_CONST, 0);  // false
      ADD_CODE(OP_END);
    }
    break;
  default:
    assert(false);
    break;
  }
}

static void gen_cond_jmp(Expr *cond, bool tf, uint32_t depth) {
  gen_cond(cond, tf);
  ADD_CODE(OP_BR_IF);
  ADD_ULEB128(depth);
}

static void gen_while(Stmt *stmt) {
  // TODO: Handle break, continue

  ADD_CODE(OP_BLOCK, WT_VOID);
  ADD_CODE(OP_LOOP, WT_VOID);
  gen_cond_jmp(stmt->while_.cond, false, 1);
  gen_stmt(stmt->while_.body);
  ADD_CODE(OP_BR, 0);
  ADD_CODE(OP_END);
  ADD_CODE(OP_END);
}

static void gen_do_while(Stmt *stmt) {
  // TODO: Handle break, continue

  ADD_CODE(OP_BLOCK, WT_VOID);
  ADD_CODE(OP_LOOP, WT_VOID);
  gen_stmt(stmt->while_.body);
  gen_cond_jmp(stmt->while_.cond, false, 1);
  ADD_CODE(OP_BR, 0);
  ADD_CODE(OP_END);
  ADD_CODE(OP_END);
}

static void gen_for(Stmt *stmt) {
  // TODO: Handle break, continue

  if (stmt->for_.pre != NULL)
    gen_expr_stmt(stmt->for_.pre);

  ADD_CODE(OP_BLOCK, WT_VOID);
  ADD_CODE(OP_LOOP, WT_VOID);
  if (stmt->for_.cond != NULL)
    gen_cond_jmp(stmt->for_.cond, false, 1);
  gen_stmt(stmt->for_.body);
  if (stmt->for_.post != NULL)
    gen_expr_stmt(stmt->for_.post);
  ADD_CODE(OP_BR, 0);
  ADD_CODE(OP_END);
  ADD_CODE(OP_END);
}

static void gen_block(Stmt *stmt) {
  gen_stmts(stmt->block.stmts);
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

static void gen_if(Stmt *stmt) {
  gen_cond(stmt->if_.cond, true);
  ADD_CODE(OP_IF, WT_VOID);
  gen_stmt(stmt->if_.tblock);
  if (stmt->if_.fblock != NULL) {
    ADD_CODE(OP_ELSE);
    gen_stmt(stmt->if_.fblock);
  }
  ADD_CODE(OP_END);
}

static void gen_vardecl(Vector *decls, Vector *inits) {
  if (curfunc != NULL) {
    UNUSED(decls);
    /*for (int i = 0; i < decls->len; ++i) {
      VarDecl *decl = decls->data[i];
      if (decl->init == NULL)
        continue;
      VarInfo *varinfo = scope_find(curscope, decl->ident->ident, NULL);
      if (varinfo == NULL || (varinfo->storage & (VS_STATIC | VS_EXTERN)) ||
          !(varinfo->type->kind == TY_STRUCT ||
            varinfo->type->kind == TY_ARRAY))
        continue;
      gen_clear_local_var(varinfo);
    }*/
  }
  gen_stmts(inits);
}

static void gen_expr_stmt(Expr *expr) {
  gen_expr(expr);
  if (expr->type->kind != TY_VOID)
    ADD_CODE(OP_DROP);
}

static void gen_stmt(Stmt *stmt) {
  if (stmt == NULL)
    return;

  switch (stmt->kind) {
  case ST_EXPR:  gen_expr_stmt(stmt->expr); break;
  case ST_RETURN:  gen_return(stmt); break;
  case ST_BLOCK:  gen_block(stmt); break;
  case ST_IF:  gen_if(stmt); break;
  /*case ST_SWITCH:  gen_switch(stmt); break;
  case ST_CASE:  gen_case(stmt); break;
  case ST_DEFAULT:  gen_default(); break;*/
  case ST_WHILE:  gen_while(stmt); break;
  case ST_DO_WHILE:  gen_do_while(stmt); break;
  case ST_FOR:  gen_for(stmt); break;
  /*case ST_BREAK:  gen_break(); break;
  case ST_CONTINUE:  gen_continue(); break;
  case ST_GOTO:  gen_goto(stmt); break;
  case ST_LABEL:  gen_label(stmt); break;*/
  case ST_VARDECL:  gen_vardecl(stmt->vardecl.decls, stmt->vardecl.inits); break;
  /*case ST_ASM:  gen_asm(stmt); break;*/

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

  // Allocate local variables.
  unsigned int local_count = 0;

  for (int i = 0; i < func->scopes->len; ++i) {
    Scope *scope = func->scopes->data[i];
    if (scope->vars == NULL)
      continue;

    for (int j = 0; j < scope->vars->len; ++j) {
      VarInfo *varinfo = scope->vars->data[j];
      if (varinfo->storage & (VS_STATIC | VS_EXTERN | VS_ENUM_MEMBER)) {
        // Static entity is allocated in global, not on stack.
        // Extern doesn't have its entity.
        // Enum members are replaced to constant value.
        continue;
      }

      VReg *vreg = malloc(sizeof(*vreg));
      vreg->local_index = local_count++;
      varinfo->local.reg = vreg;
    }
  }

  curfunc = func;

  // Statements
  gen_stmts(func->stmts);
  ADD_CODE(OP_END);

  //
#if 1
  unsigned char buf[32], *p;
  p = emit_uleb128(buf, local_count);
  if (local_count > 0) {
    for (int i = 0; i < func->scopes->len; ++i) {
      Scope *scope = func->scopes->data[i];
      if (scope->vars == NULL)
        continue;

      for (int j = 0; j < scope->vars->len; ++j) {
        VarInfo *varinfo = scope->vars->data[j];
        if (varinfo->storage & (VS_STATIC | VS_EXTERN | VS_ENUM_MEMBER)) {
          // Static entity is allocated in global, not on stack.
          // Extern doesn't have its entity.
          // Enum members are replaced to constant value.
          continue;
        }

        assert(is_fixnum(varinfo->type->kind));
        assert((p + 1) - buf < (ssize_t)sizeof(buf));
        // TODO: Group same type variables.
        p = emit_uleb128(p, 1);  // TODO: Set type bytes.
        *p++ = WT_I32;
      }
    }
  }

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
