#include "../config.h"
#include "codegen.h"

#include <assert.h>
#include <inttypes.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

#include "ast.h"
#include "ir.h"
#include "lexer.h"
#include "parser.h"  // curfunc, curscope
#include "regalloc.h"
#include "table.h"
#include "type.h"
#include "util.h"
#include "var.h"

const int FRAME_ALIGN = 8;
const int STACK_PARAM_BASE_OFFSET = (2 - MAX_REG_ARGS) * WORD_SIZE;

const char RET_VAR_NAME[] = ".ret";

static void gen_expr_stmt(Expr *expr);

void set_curbb(BB *bb) {
  assert(curfunc != NULL);
  if (curbb != NULL)
    curbb->next = bb;
  curbb = bb;
  vec_push(((FuncBackend*)curfunc->extra)->bbcon->bbs, bb);
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
      if (varinfo->storage & (VS_STATIC | VS_EXTERN | VS_ENUM_MEMBER | VS_TYPEDEF)) {
        // Static entity is allocated in global, not on stack.
        // Extern doesn't have its entity.
        // Enum members are replaced to constant value.
        continue;
      }

      VReg *vreg = add_new_reg(varinfo->type, 0);
      if (varinfo->storage & VS_REF_TAKEN)
        vreg->flag |= VRF_REF;
      varinfo->local.reg = vreg;
    }
  }

  // Handle if return value is on the stack.
  Type *rettype = func->type->func.ret;
  const Name *retval_name = NULL;
  int param_index_offset = 0;
  if (is_stack_param(rettype)) {
    // Insert vreg for return value pointer into top of the function scope.
    retval_name = alloc_name(RET_VAR_NAME, NULL, false);
    Type *retptrtype = ptrof(rettype);
    Scope *top_scope = func->scopes->data[0];
    VarInfo *varinfo = scope_add(top_scope, retval_name, retptrtype, 0);
    VReg *vreg = add_new_reg(varinfo->type, VRF_PARAM);
    vreg->param_index = 0;
    varinfo->local.reg = vreg;
    ((FuncBackend*)func->extra)->retval = vreg;
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
  VReg *result = NULL;
  if (stmt->asm_.arg != NULL) {
    assert(stmt->asm_.arg->kind == EX_VAR);
    result = gen_expr(stmt->asm_.arg);
  }
  new_ir_asm(stmt->asm_.str->str.buf, result);
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
    VReg *retval = ((FuncBackend*)curfunc->extra)->retval;
    if (retval == NULL) {
      new_ir_result(reg);
    } else {
      size_t size = type_size(val->type);
      if (size > 0) {
        new_ir_memcpy(retval, reg, size);
        new_ir_result(retval);
      }
    }
  }
  new_ir_jmp(COND_ANY, ((FuncBackend*)curfunc->extra)->ret_bb);
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
  Stmt *ca = *(Stmt**)pa;
  Stmt *cb = *(Stmt**)pb;
  if (ca->case_.value == NULL)
    return 1;
  if (cb->case_.value == NULL)
    return -1;
  Fixnum d = ca->case_.value->fixnum - cb->case_.value->fixnum;
  return d > 0 ? 1 : d < 0 ? -1 : 0;
}

static void gen_switch_cond_table_jump(Stmt *swtch, VReg *reg, Stmt **cases, int len) {
  Fixnum min = (cases[0])->case_.value->fixnum;
  Fixnum max = (cases[len - 1])->case_.value->fixnum;
  Fixnum range = max - min + 1;

  BB **table = malloc(sizeof(*table) * range);
  Stmt *def = swtch->switch_.default_;
  BB *skip_bb = def != NULL ? def->case_.bb : swtch->switch_.break_bb;
  for (Fixnum i = 0; i < range; ++i)
    table[i] = skip_bb;
  for (int i = 0; i < len; ++i) {
    Stmt *c = cases[i];
    table[c->case_.value->fixnum - min] = c->case_.bb;
  }

  BB *nextbb = new_bb();
  VReg *val = min == 0 ? reg : new_ir_bop(IR_SUB, reg, new_const_vreg(min, reg->vtype), reg->vtype);
  new_ir_cmp(val, new_const_vreg(max - min, val->vtype));
  new_ir_jmp(COND_UGT, skip_bb);
  set_curbb(nextbb);
  new_ir_tjmp(val, table, range);
}

static void gen_switch_cond_recur(Stmt *swtch, VReg *reg, Stmt **cases, int len) {
  if (len <= 2) {
    for (int i = 0; i < len; ++i) {
      BB *nextbb = new_bb();
      Stmt *c = cases[i];
      VReg *num = new_const_vreg(c->case_.value->fixnum, reg->vtype);
      new_ir_cmp(reg, num);
      new_ir_jmp(COND_EQ, c->case_.bb);
      set_curbb(nextbb);
    }
    Stmt *def = swtch->switch_.default_;
    new_ir_jmp(COND_ANY, def != NULL ? def->case_.bb : swtch->switch_.break_bb);
  } else {
    Stmt *min = cases[0];
    Stmt *max = cases[len - 1];
    Fixnum range = max->case_.value->fixnum - min->case_.value->fixnum + 1;
    if (range >= 4 && len > (range >> 1)) {
      gen_switch_cond_table_jump(swtch, reg, cases, len);
      return;
    }

    BB *bbne = new_bb();
    int m = len >> 1;
    Stmt *c = cases[m];
    VReg *num = new_const_vreg(c->case_.value->fixnum, reg->vtype);
    new_ir_cmp(reg, num);
    new_ir_jmp(COND_EQ, c->case_.bb);
    set_curbb(bbne);

    BB *bblt = new_bb();
    BB *bbgt = new_bb();
    new_ir_jmp(COND_GT, bbgt);
    set_curbb(bblt);
    gen_switch_cond_recur(swtch, reg, cases, m);
    set_curbb(bbgt);
    gen_switch_cond_recur(swtch, reg, cases + (m + 1), len - (m + 1));
  }
}

static void gen_switch_cond(Stmt *stmt) {
  Expr *value = stmt->switch_.value;
  VReg *reg = gen_expr(value);

  Vector *cases = stmt->switch_.cases;
  int len = cases->len;

  if (reg->flag & VRF_CONST) {
    intptr_t value = reg->fixnum;
    Stmt *target = stmt->switch_.default_;
    for (int i = 0; i < len; ++i) {
      Stmt *c = cases->data[i];
      if (c->case_.value != NULL && c->case_.value->fixnum == value) {
        target = c;
        break;
      }
    }

    BB *nextbb = new_bb();
    new_ir_jmp(COND_ANY, target != NULL ? target->case_.bb : stmt->switch_.break_bb);
    set_curbb(nextbb);
  } else {
    if (len > 0) {
      // Sort cases in increasing order.
      myqsort(cases->data, len, sizeof(void*), compare_cases);

      if (stmt->switch_.default_ != NULL)
        --len;  // Ignore default.
      gen_switch_cond_recur(stmt, reg, (Stmt**)cases->data, len);
    } else {
      Stmt *def = stmt->switch_.default_;
      new_ir_jmp(COND_ANY, def != NULL ? def->case_.bb : stmt->switch_.break_bb);
    }
  }
  set_curbb(new_bb());
}

static void gen_switch(Stmt *stmt) {
  BB *save_break;
  BB *break_bb = stmt->switch_.break_bb = push_break_bb(&save_break);

  Vector *cases = stmt->switch_.cases;
  for (int i = 0, len = cases->len; i < len; ++i) {
    BB *bb = new_bb();
    Stmt *c = cases->data[i];
    c->case_.bb = bb;
  }

  gen_switch_cond(stmt);

  // No bb setting.

  gen_stmt(stmt->switch_.body);

  set_curbb(break_bb);

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
  set_curbb(new_bb());
}

static void gen_label(Stmt *stmt) {
  assert(curfunc->label_table != NULL);
  BB *bb = table_get(curfunc->label_table, stmt->token->ident);
  assert(bb != NULL);
  set_curbb(bb);
  gen_stmt(stmt->label.stmt);
}

void gen_clear_local_var(const VarInfo *varinfo) {
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
  FuncBackend *fnbe = func->extra = malloc(sizeof(FuncBackend));
  fnbe->ra = NULL;
  fnbe->bbcon = NULL;
  fnbe->ret_bb = NULL;
  fnbe->retval = NULL;

  fnbe->bbcon = new_func_blocks();
  set_curbb(new_bb());
  fnbe->ra = curra = new_reg_alloc(PHYSICAL_REG_MAX);
#ifndef __NO_FLONUM
  fnbe->ra->fphys_max = PHYSICAL_FREG_MAX;
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
  fnbe->ret_bb = new_bb();

  // Statements
  gen_stmts(func->stmts);

  set_curbb(fnbe->ret_bb);
  curbb = NULL;

  remove_unnecessary_bb(fnbe->bbcon);

  prepare_register_allocation(func);
  convert_3to2(fnbe->bbcon);
  int reserved_size = func->type->func.vaargs ? (MAX_REG_ARGS + MAX_FREG_ARGS) * WORD_SIZE : 0;
  alloc_physical_registers(fnbe->ra, fnbe->bbcon, reserved_size);

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
