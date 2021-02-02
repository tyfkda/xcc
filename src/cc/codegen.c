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
const int STACK_PARAM_BASE_OFFSET = (2 - MAX_REG_ARGS) * WORD_SIZE;

const char RET_VAR_NAME[] = ".ret";

static void gen_stmt(Stmt *stmt);
static void gen_expr_stmt(Expr *expr);

void set_curbb(BB *bb) {
  assert(curfunc != NULL);
  if (curbb != NULL)
    curbb->next = bb;
  curbb = bb;
  vec_push(curfunc->bbcon->bbs, bb);
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

static BB *push_continue_bb(BB **save) {
  *save = s_continue_bb;
  BB *bb = new_bb();
  s_continue_bb = bb;
  return bb;
}

static BB *push_break_bb(BB **save) {
  *save = s_break_bb;
  BB *bb = new_bb();
  s_break_bb = bb;
  return bb;
}

static void alloc_variable_registers(Function *func) {
  assert(func->type->kind == TY_FUNC);

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

      VReg *vreg = add_new_reg(varinfo->type, VRF_LOCAL);
      if (varinfo->storage & VS_REF_TAKEN)
        vreg->flag |= VRF_REF;
      varinfo->local.reg = vreg;
    }
  }

  // Handle if return value is on the stack.
  const Type *rettype = func->type->func.ret;
  const Name *retval_name = NULL;
  int param_index_offset = 0;
  if (is_stack_param(rettype)) {
    // Insert vreg for return value pointer into top of the function scope.
    retval_name = alloc_name(RET_VAR_NAME, NULL, false);
    const Type *retptrtype = ptrof(rettype);
    Scope *top_scope = func->scopes->data[0];
    if (top_scope->vars == NULL)
      top_scope->vars = new_vector();
    VarInfo *varinfo = var_add(top_scope->vars, retval_name, retptrtype, 0, NULL);
    VReg *vreg = add_new_reg(varinfo->type, VRF_LOCAL | VRF_PARAM);
    vreg->param_index = 0;
    varinfo->local.reg = vreg;
    func->retval = vreg;
    ++param_index_offset;
  }

  // Add flag to parameters.
  if (func->type->func.params != NULL) {
    for (int j = 0; j < func->type->func.params->len; ++j) {
      VarInfo *varinfo = func->type->func.params->data[j];
      VReg *vreg = varinfo->local.reg;
      vreg->flag |= VRF_PARAM;
      vreg->param_index = j + param_index_offset;
    }
  }
}

static void gen_asm(Stmt *stmt) {
  assert(stmt->asm_.str->kind == EX_STR);
  new_ir_asm(stmt->asm_.str->str.buf);
}

void gen_stmts(Vector *stmts) {
  if (stmts == NULL)
    return;

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
  assert(curfunc != NULL);
  BB *bb = new_bb();
  if (stmt->return_.val != NULL) {
    Expr *val = stmt->return_.val;
    VReg *reg = gen_expr(val);
    VReg *retval = curfunc->retval;
    if (retval == NULL) {
      new_ir_result(reg);
    } else {
      size_t size = type_size(val->type);
      if (size > 0) {
        // Allocate new register to avoid both spilled.
        VReg *tmp = add_new_reg(&tyVoidPtr, 0);
        new_ir_mov(tmp, reg);
        new_ir_memcpy(retval, tmp, size);
        new_ir_result(retval);
      }
    }
  }
  new_ir_jmp(COND_ANY, curfunc->ret_bb);
  set_curbb(bb);
}

static void gen_if(Stmt *stmt) {
  BB *tbb = new_bb();
  BB *fbb = new_bb();
  gen_cond_jmp(stmt->if_.cond, false, fbb);
  set_curbb(tbb);
  gen_stmt(stmt->if_.tblock);
  if (stmt->if_.fblock == NULL) {
    set_curbb(fbb);
  } else {
    BB *nbb = new_bb();
    new_ir_jmp(COND_ANY, nbb);
    set_curbb(fbb);
    gen_stmt(stmt->if_.fblock);
    set_curbb(nbb);
  }
}

static int compare_cases(const void *pa, const void *pb) {
  const int ia = *(int *)pa;
  const int ib = *(int *)pb;
  Vector *cases = curswitch->switch_.cases;
  Stmt *ca = cases->data[ia];
  Stmt *cb = cases->data[ib];
  if (ca->case_.value == NULL)
    return 1;
  if (cb->case_.value == NULL)
    return -1;
  Fixnum d = ca->case_.value->fixnum - cb->case_.value->fixnum;
  return d > 0 ? 1 : d < 0 ? -1 : 0;
}

static void gen_switch_cond_recur(Stmt *stmt, VReg *reg, const VRegType *vtype, const int *order,
                                  int len) {
  Vector *cases = stmt->switch_.cases;
  if (len <= 2) {
    for (int i = 0; i < len; ++i) {
      BB *nextbb = new_bb();
      int index = order[i];
      Stmt *c = cases->data[index];
      VReg *num = new_const_vreg(c->case_.value->fixnum, vtype);
      new_ir_cmp(reg, num);
      new_ir_jmp(COND_EQ, c->case_.bb);
      set_curbb(nextbb);
    }
    Stmt *def = curswitch->switch_.default_;
    new_ir_jmp(COND_ANY, def != NULL ? def->case_.bb : curswitch->switch_.break_bb);
  } else {
    BB *bbne = new_bb();
    int m = len >> 1;
    int index = order[m];
      Stmt *c = cases->data[index];
    VReg *num = new_const_vreg(c->case_.value->fixnum, vtype);
    new_ir_cmp(reg, num);
    new_ir_jmp(COND_EQ, c->case_.bb);
    set_curbb(bbne);

    BB *bblt = new_bb();
    BB *bbgt = new_bb();
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

  Vector *cases = stmt->switch_.cases;
  int len = cases->len;
  if (len > 0) {
    // Sort cases in increasing order.
    int *order = malloc(sizeof(int) * len);
    for (int i = 0; i < len; ++i)
      order[i] = i;
    QSORT(order, len, sizeof(int), compare_cases);

    if (stmt->switch_.default_ != NULL)
      --len;  // Ignore default.
    gen_switch_cond_recur(stmt, reg, to_vtype(stmt->switch_.value->type), order, len);
    free(order);
  } else {
    Stmt *def = curswitch->switch_.default_;
    new_ir_jmp(COND_ANY, def != NULL ? def->case_.bb : curswitch->switch_.break_bb);
  }
  set_curbb(new_bb());
}

static void gen_switch(Stmt *stmt) {
  Stmt *save_switch = curswitch;
  BB *save_break;
  BB *break_bb = stmt->switch_.break_bb = push_break_bb(&save_break);

  Vector *cases = stmt->switch_.cases;
  for (int i = 0, len = cases->len; i < len; ++i) {
    BB *bb = new_bb();
    Stmt *c = cases->data[i];
    c->case_.bb = bb;
  }

  curswitch = stmt;

  gen_switch_cond(stmt);

  // No bb setting.

  gen_stmt(stmt->switch_.body);

  set_curbb(break_bb);

  curswitch = save_switch;
  pop_break_bb(save_break);
}

static void gen_case(Stmt *stmt) {
  set_curbb(stmt->case_.bb);
}

static void gen_while(Stmt *stmt) {
  BB *loop_bb = new_bb();

  BB *save_break, *save_cont;
  BB *cond_bb = push_continue_bb(&save_cont);
  BB *next_bb = push_break_bb(&save_break);

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
  BB *loop_bb = new_bb();

  BB *save_break, *save_cont;
  BB *cond_bb = push_continue_bb(&save_cont);
  BB *next_bb = push_break_bb(&save_break);

  set_curbb(loop_bb);
  gen_stmt(stmt->while_.body);

  set_curbb(cond_bb);
  gen_cond_jmp(stmt->while_.cond, true, loop_bb);

  set_curbb(next_bb);
  pop_continue_bb(save_cont);
  pop_break_bb(save_break);
}

static void gen_for(Stmt *stmt) {
  BB *cond_bb = new_bb();
  BB *body_bb = new_bb();

  BB *save_break, *save_cont;
  BB *continue_bb = push_continue_bb(&save_cont);
  BB *next_bb = push_break_bb(&save_break);

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
  BB *bb = new_bb();
  new_ir_jmp(COND_ANY, s_break_bb);
  set_curbb(bb);
}

static void gen_continue(void) {
  assert(s_continue_bb != NULL);
  BB *bb = new_bb();
  new_ir_jmp(COND_ANY, s_continue_bb);
  set_curbb(bb);
}

static void gen_goto(Stmt *stmt) {
  assert(curfunc->label_table != NULL);
  BB *bb = table_get(curfunc->label_table, stmt->goto_.label->ident);
  assert(bb != NULL);
  new_ir_jmp(COND_ANY, bb);
}

static void gen_label(Stmt *stmt) {
  assert(curfunc->label_table != NULL);
  BB *bb = table_get(curfunc->label_table, stmt->token->ident);
  assert(bb != NULL);
  set_curbb(bb);
  gen_stmt(stmt->label.stmt);
}

static void gen_clear_local_var(const VarInfo *varinfo) {
  // Fill with zeros regardless of variable type.
  size_t size = type_size(varinfo->type);
  if (size <= 0)
    return;
  VReg *reg = new_ir_bofs(varinfo->local.reg);
  new_ir_clear(reg, size);
}

static void gen_vardecl(Vector *decls, Vector *inits) {
  if (curfunc != NULL) {
    for (int i = 0; i < decls->len; ++i) {
      VarDecl *decl = decls->data[i];
      if (decl->init == NULL)
        continue;
      VarInfo *varinfo = scope_find(curscope, decl->ident->ident, NULL);
      if (varinfo == NULL || (varinfo->storage & (VS_STATIC | VS_EXTERN)) ||
          !(varinfo->type->kind == TY_STRUCT ||
            varinfo->type->kind == TY_ARRAY))
        continue;
      gen_clear_local_var(varinfo);
    }
  }
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
  case ST_CASE: case ST_DEFAULT:  gen_case(stmt); break;
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

static void gen_defun(Function *func) {
  if (func->scopes == NULL)  // Prototype definition
    return;

  curfunc = func;
  func->bbcon = new_func_blocks();
  set_curbb(new_bb());
  func->ra = curra = new_reg_alloc(PHYSICAL_REG_MAX);
#ifndef __NO_FLONUM
  func->ra->fphys_max = PHYSICAL_FREG_MAX;
#endif

  // Allocate BBs for goto labels.
  if (func->label_table != NULL) {
    Table *label_table = func->label_table;
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
  func->ret_bb = new_bb();

  // Statements
  gen_stmts(func->stmts);

  set_curbb(func->ret_bb);
  curbb = NULL;

  prepare_register_allocation(func);
  convert_3to2(func->bbcon);
  alloc_physical_registers(func->ra, func->bbcon);

  remove_unnecessary_bb(func->bbcon);

  curfunc = NULL;
  curscope = global_scope;
  curra = NULL;
}

void gen_decl(Declaration *decl) {
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
