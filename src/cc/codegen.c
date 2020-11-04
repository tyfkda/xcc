#include "codegen.h"

#include <assert.h>
#include <inttypes.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

#include "ast.h"
#include "ir.h"
#include "lexer.h"
#include "regalloc.h"
#include "table.h"
#include "type.h"
#include "util.h"
#include "var.h"

#include "parser.h"  // curscope

const int FRAME_ALIGN = 8;
const int STACK_PARAM_BASE_OFFSET = (2 - MAX_REG_ARGS) * 8;

static void gen_stmt(Stmt *stmt);
static void gen_expr_stmt(Expr *expr);

void set_curbb(BB *bb) {
  assert(curdefun != NULL);
  curbb = bb;
  vec_push(curdefun->func->bbcon->bbs, bb);
}

//

static BB *s_break_bb;
static BB *s_continue_bb;

static void pop_break_bb(BB *save) {
  s_break_bb = save;
}

static void pop_continue_bb(BB *save) {
  s_continue_bb = save;
}

static BB *push_continue_bb(BB *parent_bb, BB **save) {
  *save = s_continue_bb;
  BB *bb = bb_split(parent_bb);
  s_continue_bb = bb;
  return bb;
}

static BB *push_break_bb(BB *parent_bb, BB **save) {
  *save = s_break_bb;
  BB *bb = bb_split(parent_bb);
  s_break_bb = bb;
  return bb;
}

static const char RET_VAR_NAME[] = ".ret";

static void alloc_variable_registers(Function *func) {
  assert(func->type->kind == TY_FUNC);
  const Type *rettype = func->type->func.ret;
  const Name *retval_name = NULL;
  int param_index_offset = 0;
  if (is_stack_param(rettype)) {
    // Insert vreg for return value pointer into top of the function scope.
    retval_name = alloc_name(RET_VAR_NAME, NULL, false);
    const Type *retptrtype = ptrof(rettype);
    Scope *top_scope = func->scopes->data[0];
    var_add(top_scope->vars, retval_name, retptrtype, 0, NULL);
    ++param_index_offset;
  }

  for (int i = 0; i < func->scopes->len; ++i) {
    Scope *scope = func->scopes->data[i];
    if (scope->vars == NULL)
      continue;

    for (int j = 0; j < scope->vars->len; ++j) {
      VarInfo *varinfo = scope->vars->data[j];
      if (varinfo->flag & (VF_STATIC | VF_EXTERN))
        continue;  // Static variable is not allocated on stack.

      VReg *vreg = add_new_reg(varinfo->type, VRF_LOCAL);
      if (i == 0) {
        if (param_index_offset > 0 && equal_name(varinfo->name, retval_name)) {
          vreg->flag |= VRF_PARAM;
          vreg->param_index = 0;
          func->retval = vreg;
        } else if (func->type->func.params != NULL) {
          int param_index = var_find(func->type->func.params, varinfo->name);
          if (param_index >= 0) {
            vreg->flag |= VRF_PARAM;
            vreg->param_index = param_index + param_index_offset;
          }
        }
      }
      varinfo->reg = vreg;
    }
  }
}

static void gen_asm(Stmt *stmt) {
  assert(stmt->asm_.str->kind == EX_STR);
  new_ir_asm(stmt->asm_.str->str.buf);
}

void gen_stmts(Vector *stmts) {
  assert(stmts != NULL);

  for (int i = 0, len = stmts->len; i < len; ++i) {
    Stmt *stmt = stmts->data[i];
    if (stmt == NULL)
      continue;
    gen_stmt(stmt);
  }
}

static void gen_block(Stmt *stmt) {
  if (stmt->block.scope != NULL) {
    assert(curscope == stmt->block.scope->parent);
    curscope = stmt->block.scope;
  }
  gen_stmts(stmt->block.stmts);
  if (stmt->block.scope != NULL)
    curscope = curscope->parent;
}

static void gen_return(Stmt *stmt) {
  assert(curdefun != NULL);
  BB *bb = bb_split(curbb);
  if (stmt->return_.val != NULL) {
    VReg *reg = gen_expr(stmt->return_.val);
    VReg *retval = curdefun->func->retval;
    if (retval == NULL) {
      new_ir_result(reg);
    } else {
      new_ir_memcpy(retval, reg, type_size(stmt->return_.val->type));
      new_ir_result(retval);
    }
  }
  new_ir_jmp(COND_ANY, curdefun->func->ret_bb);
  set_curbb(bb);
}

static void gen_if(Stmt *stmt) {
  BB *tbb = bb_split(curbb);
  BB *fbb = bb_split(tbb);
  gen_cond_jmp(stmt->if_.cond, false, fbb);
  set_curbb(tbb);
  gen_stmt(stmt->if_.tblock);
  if (stmt->if_.fblock == NULL) {
    set_curbb(fbb);
  } else {
    BB *nbb = bb_split(fbb);
    new_ir_jmp(COND_ANY, nbb);
    set_curbb(fbb);
    gen_stmt(stmt->if_.fblock);
    set_curbb(nbb);
  }
}

static Vector *cur_case_values;
static Vector *cur_case_bbs;

static int compare_cases(const void *pa, const void *pb) {
  const int ia = *(int *)pa;
  const int ib = *(int *)pb;
  intptr_t d = (intptr_t)cur_case_values->data[ia] - (intptr_t)cur_case_values->data[ib];
  return d > 0 ? 1 : d < 0 ? -1 : 0;
}

static void gen_switch_cond_recur(Stmt *stmt, VReg *reg, const VRegType *vtype, const int *order,
                                  int len) {
  Vector *case_values = stmt->switch_.case_values;
  if (len <= 2) {
    for (int i = 0; i < len; ++i) {
      BB *nextbb = bb_split(curbb);
      int index = order[i];
      intptr_t x = (intptr_t)case_values->data[index];
      VReg *num = new_const_vreg(x, vtype);
      new_ir_cmp(reg, num);
      new_ir_jmp(COND_EQ, cur_case_bbs->data[index]);
      set_curbb(nextbb);
    }
    new_ir_jmp(COND_ANY, cur_case_bbs->data[cur_case_bbs->len - 2]);  // Jump to default.
  } else {
    BB *bbne = bb_split(curbb);
    int m = len >> 1;
    int index = order[m];
    intptr_t x = (intptr_t)case_values->data[index];
    VReg *num = new_const_vreg(x, vtype);
    new_ir_cmp(reg, num);
    new_ir_jmp(COND_EQ, cur_case_bbs->data[index]);
    set_curbb(bbne);

    BB *bblt = bb_split(curbb);
    BB *bbgt = bb_split(bblt);
    new_ir_jmp(COND_GT, bbgt);
    set_curbb(bblt);
    gen_switch_cond_recur(stmt, reg, vtype, order, m);
    set_curbb(bbgt);
    gen_switch_cond_recur(stmt, reg, vtype, order + (m + 1), len - (m + 1));
  }
}

static void gen_switch_cond(Stmt *stmt) {
  Expr *value = stmt->switch_.value;
  VReg *reg = gen_expr(value);
  {  // Avoid spilled register.
    VReg *tmp = add_new_reg(value->type, 0);
    new_ir_mov(tmp, reg);
    reg = tmp;
  }

  Vector *case_values = stmt->switch_.case_values;
  int len = case_values->len;
  if (len > 0) {
    // Sort cases in increasing order.
    int *order = malloc(sizeof(int) * len);
    for (int i = 0; i < len; ++i)
      order[i] = i;
    myqsort(order, len, sizeof(int), compare_cases);

    gen_switch_cond_recur(stmt, reg, to_vtype(stmt->switch_.value->type), order, len);
    free(order);
  } else {
    new_ir_jmp(COND_ANY, cur_case_bbs->data[cur_case_bbs->len - 2]);  // Jump to default.
  }
  set_curbb(bb_split(curbb));
}

static void gen_switch(Stmt *stmt) {
  BB *pbb = curbb;

  Vector *save_case_values = cur_case_values;
  Vector *save_case_bbs = cur_case_bbs;
  BB *save_break;
  BB *break_bb = push_break_bb(pbb, &save_break);

  Vector *bbs = new_vector();
  Vector *case_values = stmt->switch_.case_values;
  int len = case_values->len;
  for (int i = 0; i < len; ++i) {
    BB *bb = bb_split(pbb);
    vec_push(bbs, bb);
    pbb = bb;
  }
  vec_push(bbs, new_bb());  // len+0: Extra label for default.
  vec_push(bbs, break_bb);  // len+1: Extra label for break.

  cur_case_values = case_values;
  cur_case_bbs = bbs;

  gen_switch_cond(stmt);

  // No bb setting.

  gen_stmt(stmt->switch_.body);

  if (!stmt->switch_.has_default) {
    // No default: Locate at the end of switch statement.
    BB *bb = bbs->data[len];
    bb_insert(curbb, bb);
    set_curbb(bb);
  }
  set_curbb(break_bb);

  cur_case_values = save_case_values;
  cur_case_bbs = save_case_bbs;
  pop_break_bb(save_break);
}

static void gen_case(Stmt *stmt) {
  assert(cur_case_values != NULL);
  assert(cur_case_bbs != NULL);
  Expr *value = stmt->case_.value;
  assert(is_const(value));
  intptr_t x = value->fixnum;
  int i, len = cur_case_values->len;
  for (i = 0; i < len; ++i) {
    if ((intptr_t)cur_case_values->data[i] == x)
      break;
  }
  assert(i < len);
  assert(i < cur_case_bbs->len);
  set_curbb(cur_case_bbs->data[i]);
}

static void gen_default(void) {
  assert(cur_case_values != NULL);
  assert(cur_case_bbs != NULL);
  int i = cur_case_values->len;  // Label for default is stored at the size of values.
  assert(i < cur_case_bbs->len);
  BB *bb = cur_case_bbs->data[i];
  bb_insert(curbb, bb);
  set_curbb(bb);
}

static void gen_while(Stmt *stmt) {
  BB *loop_bb = bb_split(curbb);

  BB *save_break, *save_cont;
  BB *cond_bb = push_continue_bb(loop_bb, &save_cont);
  BB *next_bb = push_break_bb(cond_bb, &save_break);

  new_ir_jmp(COND_ANY, cond_bb);

  set_curbb(loop_bb);
  gen_stmt(stmt->while_.body);

  set_curbb(cond_bb);
  gen_cond_jmp(stmt->while_.cond, true, loop_bb);

  set_curbb(next_bb);
  pop_continue_bb(save_cont);
  pop_break_bb(save_break);
}

static void gen_do_while(Stmt *stmt) {
  BB *loop_bb = bb_split(curbb);

  BB *save_break, *save_cont;
  BB *cond_bb = push_continue_bb(loop_bb, &save_cont);
  BB *next_bb = push_break_bb(cond_bb, &save_break);

  set_curbb(loop_bb);
  gen_stmt(stmt->while_.body);

  set_curbb(cond_bb);
  gen_cond_jmp(stmt->while_.cond, true, loop_bb);

  set_curbb(next_bb);
  pop_continue_bb(save_cont);
  pop_break_bb(save_break);
}

static void gen_for(Stmt *stmt) {
  BB *cond_bb = bb_split(curbb);
  BB *body_bb = bb_split(cond_bb);

  BB *save_break, *save_cont;
  BB *continue_bb = push_continue_bb(body_bb, &save_cont);
  BB *next_bb = push_break_bb(continue_bb, &save_break);

  if (stmt->for_.pre != NULL)
    gen_expr_stmt(stmt->for_.pre);

  set_curbb(cond_bb);

  if (stmt->for_.cond != NULL)
    gen_cond_jmp(stmt->for_.cond, false, next_bb);

  set_curbb(body_bb);
  gen_stmt(stmt->for_.body);

  set_curbb(continue_bb);
  if (stmt->for_.post != NULL)
    gen_expr_stmt(stmt->for_.post);
  new_ir_jmp(COND_ANY, cond_bb);

  set_curbb(next_bb);
  pop_continue_bb(save_cont);
  pop_break_bb(save_break);
}

static void gen_break(void) {
  assert(s_break_bb != NULL);
  BB *bb = bb_split(curbb);
  new_ir_jmp(COND_ANY, s_break_bb);
  set_curbb(bb);
}

static void gen_continue(void) {
  assert(s_continue_bb != NULL);
  BB *bb = bb_split(curbb);
  new_ir_jmp(COND_ANY, s_continue_bb);
  set_curbb(bb);
}

static void gen_goto(Stmt *stmt) {
  assert(curdefun->label_table != NULL);
  BB *bb = table_get(curdefun->label_table, stmt->goto_.label->ident);
  assert(bb != NULL);
  new_ir_jmp(COND_ANY, bb);
}

static void gen_label(Stmt *stmt) {
  assert(curdefun->label_table != NULL);
  BB *bb = table_get(curdefun->label_table, stmt->token->ident);
  assert(bb != NULL);
  bb_insert(curbb, bb);
  set_curbb(bb);
  gen_stmt(stmt->label.stmt);
}

static void gen_clear_local_var(const VarInfo *varinfo) {
  // Fill with zeros regardless of variable type.
  VReg *reg = new_ir_bofs(varinfo->reg);
  new_ir_clear(reg, type_size(varinfo->type));
}

static void gen_vardecl(Vector *decls, Vector *inits) {
  if (curdefun != NULL) {
    for (int i = 0; i < decls->len; ++i) {
      VarDecl *decl = decls->data[i];
      if (decl->init == NULL)
        continue;
      VarInfo *varinfo = scope_find(curscope, decl->ident->ident, NULL);
      if (varinfo == NULL || (varinfo->flag & (VF_STATIC | VF_EXTERN)) ||
          !(varinfo->type->kind == TY_STRUCT ||
            varinfo->type->kind == TY_ARRAY))
        continue;
      gen_clear_local_var(varinfo);
    }
  }
  if (inits != NULL)
    gen_stmts(inits);
}

static void gen_expr_stmt(Expr *expr) {
  gen_expr(expr);
}

void gen_stmt(Stmt *stmt) {
  if (stmt == NULL)
    return;

  switch (stmt->kind) {
  case ST_EXPR:  gen_expr_stmt(stmt->expr); break;
  case ST_RETURN:  gen_return(stmt); break;
  case ST_BLOCK:  gen_block(stmt); break;
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
  case ST_ASM:  gen_asm(stmt); break;

  default:
    error("Unhandled stmt: %d", stmt->kind);
    break;
  }
}

////////////////////////////////////////////////

static void gen_defun(Defun *defun) {
  Function *func = defun->func;
  if (func->scopes == NULL)  // Prototype definition
    return;

  curdefun = defun;
  func->bbcon = new_func_blocks();
  set_curbb(new_bb());
  func->ra = curra = new_reg_alloc(PHYSICAL_REG_MAX);

  // Allocate BBs for goto labels.
  if (defun->label_table != NULL) {
    Table *label_table = defun->label_table;
    for (int i = 0;;) {
      const Name *name;
      i = table_iterate(label_table, i, &name, NULL);
      if (i < 0)
        break;
      table_put(label_table, name, new_bb());
    }
  }

  alloc_variable_registers(func);

  curscope = func->scopes->data[0];
  func->ret_bb = bb_split(curbb);

  // Statements
  gen_stmts(defun->stmts);

  set_curbb(func->ret_bb);
  curbb = NULL;

  prepare_register_allocation(func);
  alloc_physical_registers(func->ra, func->bbcon);

  remove_unnecessary_bb(func->bbcon);

  curdefun = NULL;
  curscope = global_scope;
  curra = NULL;
}

void gen_decl(Declaration *decl) {
  if (decl == NULL)
    return;

  switch (decl->kind) {
  case DCL_DEFUN:
    gen_defun(decl->defun);
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
