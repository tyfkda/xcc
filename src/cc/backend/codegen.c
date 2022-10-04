#include "../../config.h"
#include "codegen.h"

#include <assert.h>
#include <inttypes.h>
#include <limits.h>  // CHAR_BIT
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

#include "ast.h"
#include "ir.h"
#include "optimize.h"
#include "parser.h"  // curfunc, curscope
#include "regalloc.h"
#include "table.h"
#include "type.h"
#include "util.h"
#include "var.h"

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

static VarInfo *prepare_retvar(Function *func) {
  // Insert vreg for return value pointer into top of the function scope.
  Type *rettype = func->type->func.ret;
  const Name *retval_name = alloc_label();
  Type *retptrtype = ptrof(rettype);
  Scope *top_scope = func->scopes->data[0];
  VarInfo *varinfo = scope_add(top_scope, retval_name, retptrtype, 0);
  VReg *vreg = add_new_reg(varinfo->type, VRF_PARAM);
  vreg->param_index = 0;
  varinfo->local.vreg = vreg;
  FuncBackend *fnbe = func->extra;
  fnbe->retval = vreg;
  return varinfo;
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

      varinfo->local.vreg = NULL;
      varinfo->local.frameinfo = NULL;
      if (!is_prim_type(varinfo->type)) {
        FrameInfo *fi = malloc_or_die(sizeof(*fi));
        fi->offset = 0;
        varinfo->local.frameinfo = fi;
        continue;
      }

      VReg *vreg = add_new_reg(varinfo->type, 0);
      if (varinfo->storage & VS_REF_TAKEN)
        vreg->flag |= VRF_REF;
      varinfo->local.vreg = vreg;
      varinfo->local.frameinfo = &vreg->frame;
    }
  }

  // Handle if return value is on the stack.
  int param_index_offset = 0;
  if (is_stack_param(func->type->func.ret)) {
    prepare_retvar(func);
    ++param_index_offset;
  }

  // Add flag to parameters.
  if (func->type->func.params != NULL) {
    for (int j = 0; j < func->type->func.params->len; ++j) {
      VarInfo *varinfo = func->type->func.params->data[j];
      VReg *vreg = varinfo->local.vreg;
      if (vreg != NULL) {
        vreg->flag |= VRF_PARAM;
        vreg->param_index = j + param_index_offset;
      }
    }
  }
}

static VRegType *get_elem_vtype(const Type *type) {
  const size_t MAX_REG_SIZE = 8;  // TODO:

  size_t size = type_size(type);
  assert(size > 0);
  size_t align = align_size(type);
  size_t s = MIN(align, size);
  if (!IS_POWER_OF_2(s) || s > MAX_REG_SIZE) {
    for (s = MAX_REG_SIZE; s > 1; s >>= 1) {
      assert(s > 0);
      if (s > size || size % s != 0)
        continue;
    }
  }

  VRegType *vtype = malloc(sizeof(*vtype));
  vtype->size = s;
  vtype->align = s;
  vtype->flag = 0;
  return vtype;
}

void gen_memcpy(const Type *type, VReg *dst, VReg *src) {
  VRegType *elem_vtype = get_elem_vtype(type);
  size_t count = type_size(type) / elem_vtype->size;
  assert(count > 0);
  if (count == 1) {
    VReg *tmp = new_ir_unary(IR_LOAD, src, elem_vtype);
    new_ir_store(dst, tmp);
  } else {
    VReg *srcp = add_new_reg(&tyVoidPtr, 0);
    new_ir_mov(srcp, src);
    VReg *dstp = add_new_reg(&tyVoidPtr, 0);
    new_ir_mov(dstp, dst);

    VRegType *vtySize = to_vtype(&tySize);
    VReg *vcount = add_new_reg(&tySize, 0);
    new_ir_mov(vcount, new_const_vreg(count, vtySize));
    VReg *vadd = new_const_vreg(elem_vtype->size, vtySize);

    BB *loop_bb = new_bb();
    set_curbb(loop_bb);
    VReg *tmp = new_ir_unary(IR_LOAD, srcp, elem_vtype);
    new_ir_mov(srcp, new_ir_bop(IR_ADD, srcp, vadd, srcp->vtype));  // srcp += elem_size
    new_ir_store(dstp, tmp);
    new_ir_mov(dstp, new_ir_bop(IR_ADD, dstp, vadd, dstp->vtype));  // dstp += elem_size
    new_ir_mov(vcount, new_ir_bop(IR_SUB, vcount, new_const_vreg(1, vtySize), vcount->vtype));  // vcount -= 1
    new_ir_cmp(vcount, new_const_vreg(0, vcount->vtype));
    new_ir_jmp(COND_NE, loop_bb);
    set_curbb(new_bb());
  }
}

static void gen_clear(const Type *type, VReg *dst) {
  VRegType *elem_vtype = get_elem_vtype(type);
  size_t count = type_size(type) / elem_vtype->size;
  assert(count > 0);
  VReg *vzero = new_const_vreg(0, elem_vtype);
  if (count == 1) {
    new_ir_store(dst, vzero);
  } else {
    VReg *dstp = add_new_reg(&tyVoidPtr, 0);
    new_ir_mov(dstp, dst);

    VRegType *vtySize = to_vtype(&tySize);
    VReg *vcount = add_new_reg(&tySize, 0);
    new_ir_mov(vcount, new_const_vreg(count, vtySize));
    VReg *vadd = new_const_vreg(elem_vtype->size, vtySize);

    BB *loop_bb = new_bb();
    set_curbb(loop_bb);
    new_ir_store(dstp, vzero);
    new_ir_mov(dstp, new_ir_bop(IR_ADD, dstp, vadd, dstp->vtype));  // dstp += elem_size
    new_ir_mov(vcount, new_ir_bop(IR_SUB, vcount, new_const_vreg(1, vtySize), vcount->vtype));  // vcount -= 1
    new_ir_cmp(vcount, new_const_vreg(0, vcount->vtype));
    new_ir_jmp(COND_NE, loop_bb);
    set_curbb(new_bb());
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
  FuncBackend *fnbe = curfunc->extra;
  if (stmt->return_.val != NULL) {
    Expr *val = stmt->return_.val;
    VReg *vreg = gen_expr(val);
    VReg *retval = fnbe->retval;
    if (retval == NULL) {
      new_ir_result(vreg);
    } else {
      size_t size = type_size(val->type);
      if (size > 0) {
        gen_memcpy(val->type, retval, vreg);
        new_ir_result(retval);
      }
    }
  }
  new_ir_jmp(COND_ANY, fnbe->ret_bb);
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

static void gen_switch_cond_table_jump(Stmt *swtch, VReg *vreg, Stmt **cases, int len) {
  Fixnum min = (cases[0])->case_.value->fixnum;
  Fixnum max = (cases[len - 1])->case_.value->fixnum;
  Fixnum range = max - min + 1;

  BB **table = malloc_or_die(sizeof(*table) * range);
  Stmt *def = swtch->switch_.default_;
  BB *skip_bb = def != NULL ? def->case_.bb : swtch->switch_.break_bb;
  for (Fixnum i = 0; i < range; ++i)
    table[i] = skip_bb;
  for (int i = 0; i < len; ++i) {
    Stmt *c = cases[i];
    table[c->case_.value->fixnum - min] = c->case_.bb;
  }

  BB *nextbb = new_bb();
  VReg *val = min == 0 ? vreg : new_ir_bop(IR_SUB, vreg, new_const_vreg(min, vreg->vtype), vreg->vtype);
  new_ir_cmp(val, new_const_vreg(max - min, val->vtype));
  new_ir_jmp(COND_GT | COND_UNSIGNED, skip_bb);
  set_curbb(nextbb);
  new_ir_tjmp(val, table, range);
}

static void gen_switch_cond_recur(Stmt *swtch, VReg *vreg, Stmt **cases, int len) {
  int cond_flag = vreg->vtype->flag & VRTF_UNSIGNED ? COND_UNSIGNED : 0;
  if (len <= 2) {
    for (int i = 0; i < len; ++i) {
      BB *nextbb = new_bb();
      Stmt *c = cases[i];
      VReg *num = new_const_vreg(c->case_.value->fixnum, vreg->vtype);
      new_ir_cmp(vreg, num);
      new_ir_jmp(COND_EQ | cond_flag, c->case_.bb);
      set_curbb(nextbb);
    }
    Stmt *def = swtch->switch_.default_;
    new_ir_jmp(COND_ANY, def != NULL ? def->case_.bb : swtch->switch_.break_bb);
  } else {
    Stmt *min = cases[0];
    Stmt *max = cases[len - 1];
    Fixnum range = max->case_.value->fixnum - min->case_.value->fixnum + 1;
    if (range >= 4 && len > (range >> 1)) {
      gen_switch_cond_table_jump(swtch, vreg, cases, len);
      return;
    }

    BB *bbne = new_bb();
    int m = len >> 1;
    Stmt *c = cases[m];
    VReg *num = new_const_vreg(c->case_.value->fixnum, vreg->vtype);
    new_ir_cmp(vreg, num);
    new_ir_jmp(COND_EQ | cond_flag, c->case_.bb);
    set_curbb(bbne);

    BB *bblt = new_bb();
    BB *bbgt = new_bb();
    new_ir_jmp(COND_GT | cond_flag, bbgt);
    set_curbb(bblt);
    gen_switch_cond_recur(swtch, vreg, cases, m);
    set_curbb(bbgt);
    gen_switch_cond_recur(swtch, vreg, cases + (m + 1), len - (m + 1));
  }
}

static void gen_switch_cond(Stmt *stmt) {
  Expr *value = stmt->switch_.value;
  VReg *vreg = gen_expr(value);

  Vector *cases = stmt->switch_.cases;
  int len = cases->len;

  if (vreg->flag & VRF_CONST) {
    Fixnum value = vreg->fixnum;
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
      qsort(cases->data, len, sizeof(void*), compare_cases);

      if (stmt->switch_.default_ != NULL)
        --len;  // Ignore default.
      gen_switch_cond_recur(stmt, vreg, (Stmt**)cases->data, len);
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
  BB *save_break, *save_cont;
  BB *loop_bb = new_bb();
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
  BB *save_break, *save_cont;
  BB *loop_bb = new_bb();
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
  BB *save_break, *save_cont;
  BB *loop_bb = new_bb();
  BB *continue_bb = push_continue_bb(&save_cont);
  BB *cond_bb = new_bb();
  BB *next_bb = push_break_bb(&save_break);

  if (stmt->for_.pre != NULL)
    gen_expr_stmt(stmt->for_.pre);

  new_ir_jmp(COND_ANY, cond_bb);

  set_curbb(loop_bb);
  gen_stmt(stmt->for_.body);

  set_curbb(continue_bb);
  if (stmt->for_.post != NULL)
    gen_expr_stmt(stmt->for_.post);

  set_curbb(cond_bb);
  if (stmt->for_.cond != NULL)
    gen_cond_jmp(stmt->for_.cond, true, loop_bb);
  else
    new_ir_jmp(COND_ANY, loop_bb);

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
  Stmt *label = table_get(curfunc->label_table, stmt->goto_.label->ident);
  assert(label != NULL);
  new_ir_jmp(COND_ANY, label->label.bb);
  set_curbb(new_bb());
}

static void gen_label(Stmt *stmt) {
  assert(stmt->label.bb != NULL);
  set_curbb(stmt->label.bb);
  gen_stmt(stmt->label.stmt);
}

void gen_clear_local_var(const VarInfo *varinfo) {
  if (is_prim_type(varinfo->type))
    return;

  size_t size = type_size(varinfo->type);
  if (size <= 0)
    return;
  VReg *vreg = new_ir_bofs(varinfo->local.frameinfo, varinfo->local.vreg);
  gen_clear(varinfo->type, vreg);
}

static void gen_vardecl(Vector *decls) {
  for (int i = 0; i < decls->len; ++i) {
    VarDecl *decl = decls->data[i];
    if (decl->init_stmt != NULL) {
      VarInfo *varinfo = scope_find(curscope, decl->ident, NULL);
      gen_clear_local_var(varinfo);
      gen_stmt(decl->init_stmt);
    }
  }
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
  case ST_VARDECL:  gen_vardecl(stmt->vardecl.decls); break;
  case ST_ASM:  gen_asm(stmt); break;

  default:
    error("Unhandled stmt: %d", stmt->kind);
    break;
  }
}

////////////////////////////////////////////////

static void prepare_register_allocation(Function *func) {
  // Handle function parameters first.
  if (func->type->func.params != NULL) {
    const int DEFAULT_OFFSET = WORD_SIZE * 2;  // Return address, saved base pointer.
    assert((Scope*)func->scopes->data[0] != NULL);
    int ireg_index = is_stack_param(func->type->func.ret) ? 1 : 0;
#ifndef __NO_FLONUM
    int freg_index = 0;
#endif
    int offset = DEFAULT_OFFSET;
    for (int j = 0; j < func->type->func.params->len; ++j) {
      VarInfo *varinfo = func->type->func.params->data[j];
      if (!is_prim_type(varinfo->type)) {
        // stack parameters
        FrameInfo *fi = varinfo->local.frameinfo;
        fi->offset = offset = ALIGN(offset, align_size(varinfo->type));
        offset += ALIGN(type_size(varinfo->type), WORD_SIZE);
        continue;
      }

      VReg *vreg = varinfo->local.vreg;
      assert(vreg != NULL);

      // Currently, all parameters are force spilled.
      spill_vreg(vreg);

      if (func->type->func.vaargs) {  // Variadic function parameters.
#ifndef __NO_FLONUM
        if (is_flonum(varinfo->type))
          vreg->frame.offset = (freg_index - MAX_FREG_ARGS) * WORD_SIZE;
        else
#endif
          vreg->frame.offset = (ireg_index - MAX_REG_ARGS - MAX_FREG_ARGS) * WORD_SIZE;
      }
      bool through_stack;
#ifndef __NO_FLONUM
      if (is_flonum(varinfo->type)) {
        through_stack = freg_index >= MAX_FREG_ARGS;
        ++freg_index;
      } else
#endif
      {
        through_stack = ireg_index >= MAX_REG_ARGS;
        ++ireg_index;
      }

      if (through_stack) {
        // Function argument passed through the stack.
        vreg->frame.offset = offset;
        offset += WORD_SIZE;
      }
    }
  }

  for (int i = 0; i < func->scopes->len; ++i) {
    Scope *scope = (Scope*)func->scopes->data[i];
    if (scope->vars == NULL)
      continue;

    for (int j = 0; j < scope->vars->len; ++j) {
      VarInfo *varinfo = scope->vars->data[j];
      if (varinfo->storage & (VS_STATIC | VS_EXTERN | VS_ENUM_MEMBER))
        continue;
      VReg *vreg = varinfo->local.vreg;
      if (vreg == NULL || vreg->flag & VRF_PARAM)
        continue;

      assert(is_prim_type(varinfo->type));
      if (vreg->flag & VRF_REF)
        spill_vreg(vreg);
    }
  }
}

static void map_virtual_to_physical_registers(RegAlloc *ra) {
  for (int i = 0, vreg_count = ra->vregs->len; i < vreg_count; ++i) {
    VReg *vreg = ra->vregs->data[i];
    if (vreg == NULL)
      continue;
    if (!(vreg->flag & VRF_CONST))
      vreg->phys = ra->intervals[vreg->virt].phys;
  }
}

// Detect living registers for each instruction.
static void detect_living_registers(RegAlloc *ra, BBContainer *bbcon) {
#ifdef __NO_FLONUM
  int maxbit = ra->phys_max;
#else
  int maxbit = ra->phys_max + ra->fphys_max;
#endif
  unsigned long living_pregs = 0;
  assert((int)sizeof(living_pregs) * CHAR_BIT >= maxbit);
  LiveInterval **livings = ALLOCA(sizeof(*livings) * maxbit);
  for (int i = 0; i < maxbit; ++i)
    livings[i] = NULL;

  int nip = 0, head = 0;
  for (int i = 0; i < bbcon->bbs->len; ++i) {
    BB *bb = bbcon->bbs->data[i];
    for (int j = 0; j < bb->irs->len; ++j, ++nip) {
      // Eliminate deactivated registers.
      for (int k = 0; k < maxbit; ++k) {
        LiveInterval *li = livings[k];
        if (li != NULL && nip == li->end) {
          int phys = li->phys;
#ifndef __NO_FLONUM
          if (((VReg*)ra->vregs->data[li->virt])->vtype->flag & VRTF_FLONUM)
            phys += ra->phys_max;
#endif
          assert(phys == k);
          living_pregs &= ~(1U << phys);
          livings[k] = NULL;
        }
      }

      // Store living vregs to IR_CALL.
      IR *ir = bb->irs->data[j];
      if (ir->kind == IR_CALL) {
        // Store it into corresponding precall.
        IR *ir_precall = ir->call.precall;
        ir_precall->precall.living_pregs = living_pregs;
      }

      // Add activated registers.
      for (; head < ra->vregs->len; ++head) {
        LiveInterval *li = ra->sorted_intervals[head];
        if (ra->vregs->data[li->virt] == NULL)
          continue;
        if (li->state != LI_NORMAL)
          continue;
        if (li->start > nip)
          break;
        int phys = li->phys;
#ifndef __NO_FLONUM
        if (((VReg*)ra->vregs->data[li->virt])->vtype->flag & VRTF_FLONUM)
          phys += ra->phys_max;
#endif
        if (nip == li->start) {
          living_pregs |= 1UL << phys;
          livings[phys] = li;
        }
      }
    }
  }
}

static void alloc_stack_variables_onto_stack_frame(Function *func) {
  FuncBackend *fnbe = func->extra;
  size_t frame_size = fnbe->frame_size;

  for (int i = 0; i < func->scopes->len; ++i) {
    Scope *scope = func->scopes->data[i];
    if (scope->vars == NULL)
      continue;

    for (int j = 0; j < scope->vars->len; ++j) {
      VarInfo *varinfo = scope->vars->data[j];
      if (varinfo->storage & (VS_STATIC | VS_EXTERN | VS_ENUM_MEMBER | VS_TYPEDEF) ||
          is_prim_type(varinfo->type))
        continue;

      assert(varinfo->local.vreg == NULL);
      FrameInfo *fi = varinfo->local.frameinfo;
      assert(fi != NULL);
      if (fi->offset != 0) {
        // Variadic function parameter or stack parameter, set in `prepare_register_allocation`.
        continue;
      }

      Type *type = varinfo->type;
      size_t size = type_size(type);
      if (size < 1)
        size = 1;
      size_t align = align_size(type);

      frame_size = ALIGN(frame_size + size, align);
      fi->offset = -(int)frame_size;
    }
  }

  RegAlloc *ra = fnbe->ra;
  for (int i = 0; i < ra->vregs->len; ++i) {
    LiveInterval *li = ra->sorted_intervals[i];
    if (li->state != LI_SPILL)
      continue;
    VReg *vreg = ra->vregs->data[li->virt];
    if (vreg->frame.offset != 0) {
      // Variadic function parameter or stack parameter, set in `prepare_register_allocation`.
      if (-vreg->frame.offset > (int)frame_size)
        frame_size = -vreg->frame.offset;
      continue;
    }

    int size, align;
    const VRegType *vtype = vreg->vtype;
    assert(vtype != NULL);
    size = vtype->size;
    align = vtype->align;
    if (size < 1)
      size = 1;

    frame_size = ALIGN(frame_size + size, align);
    vreg->frame.offset = -(int)frame_size;
  }

  fnbe->frame_size = frame_size;
}

static void gen_defun(Function *func) {
  if (func->scopes == NULL)  // Prototype definition
    return;

  curfunc = func;
  FuncBackend *fnbe = func->extra = malloc_or_die(sizeof(FuncBackend));
  fnbe->ra = NULL;
  fnbe->bbcon = NULL;
  fnbe->ret_bb = NULL;
  fnbe->retval = NULL;
  fnbe->frame_size = func->type->func.vaargs ? (MAX_REG_ARGS + MAX_FREG_ARGS) * WORD_SIZE : 0;

  fnbe->bbcon = new_func_blocks();
  set_curbb(new_bb());
  fnbe->ra = curra = new_reg_alloc(PHYSICAL_REG_MAX);
#ifndef __NO_FLONUM
  fnbe->ra->fphys_max = PHYSICAL_FREG_MAX;
#endif

  // Allocate BBs for goto labels.
  if (func->label_table != NULL) {
    Table *label_table = func->label_table;
    const Name *name;
    Stmt *label;
    for (int it = 0; (it = table_iterate(label_table, it, &name, (void**)&label)) != -1; ) {
      assert(label->label.used);
      label->label.bb = new_bb();
    }
  }

  alloc_variable_registers(func);

  fnbe->ret_bb = new_bb();

  // Statements
  gen_stmt(func->body_block);

  set_curbb(fnbe->ret_bb);
  curbb = NULL;

  optimize(fnbe->ra, fnbe->bbcon);

  prepare_register_allocation(func);
  tweak_irs(fnbe);
  analyze_reg_flow(fnbe->bbcon);

  alloc_physical_registers(fnbe->ra, fnbe->bbcon);
  map_virtual_to_physical_registers(fnbe->ra);
  detect_living_registers(fnbe->ra, fnbe->bbcon);

  alloc_stack_variables_onto_stack_frame(func);

  curfunc = NULL;
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
