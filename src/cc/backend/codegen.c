#include "../../config.h"
#include "codegen.h"

#include <assert.h>
#include <inttypes.h>
#include <limits.h>  // CHAR_BIT
#include <stdbool.h>
#include <stdlib.h>  // qsort
#include <string.h>

#include "arch_config.h"
#include "ast.h"
#include "fe_misc.h"  // curfunc, curscope
#include "ir.h"
#include "optimize.h"
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
  VReg *vreg = add_new_vreg(varinfo->type);
  vreg->flag |= VRF_PARAM;
  vreg->reg_param_index = 0;
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
      if (!is_local_storage(varinfo)) {
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

      VReg *vreg = add_new_vreg(varinfo->type);
      if (varinfo->storage & VS_REF_TAKEN)
        vreg->flag |= VRF_REF;
      varinfo->local.vreg = vreg;
      varinfo->local.frameinfo = &vreg->frame;
    }
  }

  struct RegSet {
    int index;
    int max;
  } regparams[2] = {
    {.index = 0, .max = MAX_REG_ARGS},
    {.index = 0, .max = MAX_FREG_ARGS},
  };
  enum RegKind { IREG = 0, FREG = 1 };

  // Handle if return value is on the stack.
  if (is_stack_param(func->type->func.ret)) {
    prepare_retvar(func);
    ++regparams[IREG].index;
  }

  // Count register parameters, or set flag.
  const Vector *params = func->params;
  if (params != NULL) {
    for (int i = 0; i < params->len; ++i) {
      VarInfo *varinfo = params->data[i];
      VReg *vreg = varinfo->local.vreg;
      if (vreg != NULL) {
        vreg->flag |= VRF_PARAM;
        enum RegKind k = (vreg->flag & VRF_FLONUM) ? FREG : IREG;
        struct RegSet *p = &regparams[k];
        if (p->index < p->max)
          vreg->reg_param_index = p->index++;
        else
          vreg->flag |= VRF_STACK_PARAM;
      }
    }
  }
}

void enumerate_register_params(
    Function *func, RegParamInfo iargs[], int max_ireg, RegParamInfo fargs[], int max_freg,
    int *piarg_count, int *pfarg_count) {
  int iarg_count = 0;
  int farg_count = 0;

  VReg *retval = ((FuncBackend*)func->extra)->retval;
  if (retval != NULL) {
    RegParamInfo *p = &iargs[iarg_count++];
    p->type = &tyVoidPtr;
    p->vreg = retval;
    p->index = 0;
  }

  const Vector *params = func->params;
  if (params != NULL) {
    for (int i = 0, len = params->len; i < len; ++i) {
      const VarInfo *varinfo = params->data[i];
      const Type *type = varinfo->type;
      if (is_stack_param(type))
        continue;
      VReg *vreg = varinfo->local.vreg;
      assert(vreg != NULL);
      RegParamInfo *p = NULL;
      int index = 0;
      if (is_flonum(type)) {
        if (farg_count < max_freg)
          p = &fargs[index = farg_count++];
      } else {
        if (iarg_count < max_ireg)
          p = &iargs[index = iarg_count++];
      }
      if (p != NULL) {
        p->type = type;
        p->vreg = vreg;
        p->index = index;
      }
    }
  }

  *piarg_count = iarg_count;
  *pfarg_count = farg_count;
}

static enum VRegSize get_elem_vtype(const Type *type) {
  const size_t MAX_REG_SIZE = 8;  // TODO:

  size_t size = type_size(type);
  assert(size > 0);
  size_t align = align_size(type);
  size_t s = MIN(align, size);
  if (!IS_POWER_OF_2(s) || s > MAX_REG_SIZE) {
    for (s = MAX_REG_SIZE; s > 1; s >>= 1) {
      if (s <= size && size % s == 0)
        break;
    }
  }

  assert(s > 0);
  enum VRegSize vsize = VRegSize1;
  for (; s >>= 1, s > 0; ++vsize)
    ;
  return vsize;
}

void gen_memcpy(const Type *type, VReg *dst, VReg *src) {
  size_t size = type_size(type);
  if (size == 0)
    return;
  enum VRegSize elem_vsize = get_elem_vtype(type);
  size_t count = size >> elem_vsize;
  assert(count > 0);
  if (count == 1) {
    VReg *tmp = new_ir_load(src, elem_vsize, to_vflag(type), 0);
    new_ir_store(dst, tmp, 0);
  } else {
    VReg *srcp = add_new_vreg(&tyVoidPtr);
    new_ir_mov(srcp, src, IRF_UNSIGNED);
    VReg *dstp = add_new_vreg(&tyVoidPtr);
    new_ir_mov(dstp, dst, IRF_UNSIGNED);

    enum VRegSize vsSize = to_vsize(&tySize);
    VReg *vcount = add_new_vreg(&tySize);
    new_ir_mov(vcount, new_const_vreg(count, vsSize), IRF_UNSIGNED);
    VReg *vadd = new_const_vreg(1 << elem_vsize, vsSize);

    BB *loop_bb = new_bb();
    set_curbb(loop_bb);
    VReg *tmp = new_ir_load(srcp, elem_vsize, to_vflag(type), 0);
    new_ir_mov(srcp, new_ir_bop(IR_ADD, srcp, vadd, srcp->vsize, IRF_UNSIGNED), IRF_UNSIGNED);  // srcp += elem_size
    new_ir_store(dstp, tmp, 0);
    new_ir_mov(dstp, new_ir_bop(IR_ADD, dstp, vadd, dstp->vsize, IRF_UNSIGNED), IRF_UNSIGNED);  // dstp += elem_size
    new_ir_mov(vcount, new_ir_bop(IR_SUB, vcount, new_const_vreg(1, vsSize),
                                  vcount->vsize, IRF_UNSIGNED), IRF_UNSIGNED);  // vcount -= 1
    new_ir_cjmp(vcount, new_const_vreg(0, vcount->vsize), COND_NE, loop_bb);
    set_curbb(new_bb());
  }
}

static void gen_clear(const Type *type, VReg *dst) {
  size_t size = type_size(type);
  if (size == 0)
    return;
  enum VRegSize elem_vtype = get_elem_vtype(type);
  size_t count = size >> elem_vtype;
  assert(count > 0);
  VReg *vzero = new_const_vreg(0, elem_vtype);
  if (count == 1) {
    new_ir_store(dst, vzero, 0);
  } else {
    VReg *dstp = add_new_vreg(&tyVoidPtr);
    new_ir_mov(dstp, dst, IRF_UNSIGNED);

    enum VRegSize vsSize = to_vsize(&tySize);
    VReg *vcount = add_new_vreg(&tySize);
    new_ir_mov(vcount, new_const_vreg(count, vsSize), IRF_UNSIGNED);
    VReg *vadd = new_const_vreg(1 << elem_vtype, vsSize);

    BB *loop_bb = new_bb();
    set_curbb(loop_bb);
    new_ir_store(dstp, vzero, 0);
    new_ir_mov(dstp, new_ir_bop(IR_ADD, dstp, vadd, dstp->vsize, IRF_UNSIGNED), IRF_UNSIGNED);  // dstp += elem_size
    new_ir_mov(vcount, new_ir_bop(IR_SUB, vcount, new_const_vreg(1, vsSize),
                                  vcount->vsize, IRF_UNSIGNED), IRF_UNSIGNED);  // vcount -= 1
    new_ir_cjmp(vcount, new_const_vreg(0, vcount->vsize), COND_NE, loop_bb);
    set_curbb(new_bb());
  }
}

extern inline void gen_asm(Stmt *stmt) {
  assert(stmt->asm_.str->kind == EX_STR);
  VReg *result = NULL;
  const char *str = stmt->asm_.str->str.buf;
  if (stmt->asm_.arg != NULL) {
    assert(stmt->asm_.arg->kind == EX_VAR);
    result = gen_expr(stmt->asm_.arg);

    // In gcc, `%' is handled only if `__asm' takes parameters.
    // Otherwise, `%' is not handled.

    // TODO: Embed parameters.
    //   Here, just escape %.
    if (strchr(str, '%') != NULL) {
      size_t len = strlen(str + 1);
      char *buf = malloc_or_die(len);
      char *dst = buf;
      for (const char *src = str;;) {
        char c = *src++;
        if (c == '%')
          c = *src++;
        *dst++ = c;
        if (c == '\0')
          break;
      }
      str = buf;
    }
  }
  new_ir_asm(str, result);
}

VReg *gen_stmts(Vector *stmts) {
  assert(stmts != NULL);
  int len = stmts->len;
  VReg *result = NULL;
  if (len > 0) {
    int last = stmts->len - 1;
    for (int i = 0; i < last; ++i) {
      Stmt *stmt = stmts->data[i];
      if (stmt == NULL)
        continue;
      gen_stmt(stmt);
    }

    Stmt *last_stmt = stmts->data[last];
    if (last_stmt->kind == ST_EXPR)
      result = gen_expr(last_stmt->expr);
    else
      gen_stmt(last_stmt);
  }
  return result;
}

VReg *gen_block(Stmt *stmt) {
  assert(stmt->kind == ST_BLOCK);
  // AST may moved, so code generation traversal may differ from lexical scope chain.
  Scope *bak_curscope = curscope;
  if (stmt->block.scope != NULL)
    curscope = stmt->block.scope;
  VReg *result = gen_stmts(stmt->block.stmts);
  if (stmt->block.scope != NULL)
    curscope = bak_curscope;
  return result;
}

extern inline void gen_return(Stmt *stmt) {
  assert(curfunc != NULL);
  BB *bb = new_bb();
  FuncBackend *fnbe = curfunc->extra;
  if (stmt->return_.val != NULL) {
    Expr *val = stmt->return_.val;
    VReg *vreg = gen_expr(val);
    if (is_prim_type(val->type)) {
      int flag = is_unsigned(val->type) ? IRF_UNSIGNED : 0;
      new_ir_result(fnbe->result_dst, vreg, flag);
    } else if (val->type->kind != TY_VOID) {
      VReg *retval = fnbe->retval;
      if (retval != NULL) {
        gen_memcpy(val->type, retval, vreg);
        new_ir_result(fnbe->result_dst, retval, IRF_UNSIGNED);  // Pointer is unsigned.
      } else {
        // Embedding inline function: lval (struct pointer) is returned.
        assert(fnbe->result_dst != NULL);
        new_ir_mov(fnbe->result_dst, vreg, IRF_UNSIGNED);
      }
    }
  }
  new_ir_jmp(fnbe->ret_bb);
  set_curbb(bb);
}

extern inline void gen_if(Stmt *stmt) {
  BB *tbb = new_bb();
  BB *fbb = new_bb();
  gen_cond_jmp(stmt->if_.cond, false, fbb);
  set_curbb(tbb);
  gen_stmt(stmt->if_.tblock);
  if (stmt->if_.fblock == NULL) {
    set_curbb(fbb);
  } else {
    BB *nbb = new_bb();
    new_ir_jmp(nbb);
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

static void gen_switch_cond_table_jump(Stmt *swtch, VReg *vreg, Stmt **cases, int len, int cond_flag) {
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
  VReg *val = vreg;
  if (min != 0) {
    int flag = (cond_flag & COND_UNSIGNED) ? IRF_UNSIGNED : 0;
    val = new_ir_bop(IR_SUB, vreg, new_const_vreg(min, vreg->vsize), vreg->vsize, flag);
  }
  new_ir_cjmp(val, new_const_vreg(max - min, val->vsize), COND_GT | COND_UNSIGNED, skip_bb);
  set_curbb(nextbb);
  new_ir_tjmp(val, table, range);
}

static void gen_switch_cond_recur(Stmt *swtch, VReg *vreg, Stmt **cases, int len, int cond_flag) {
  if (len <= 2) {
    for (int i = 0; i < len; ++i) {
      BB *nextbb = new_bb();
      Stmt *c = cases[i];
      VReg *num = new_const_vreg(c->case_.value->fixnum, vreg->vsize);
      new_ir_cjmp(vreg, num, COND_EQ | cond_flag, c->case_.bb);
      set_curbb(nextbb);
    }
    Stmt *def = swtch->switch_.default_;
    new_ir_jmp(def != NULL ? def->case_.bb : swtch->switch_.break_bb);
  } else {
    Stmt *min = cases[0];
    Stmt *max = cases[len - 1];
    Fixnum range = max->case_.value->fixnum - min->case_.value->fixnum + 1;
    if (range >= 4 && len > (range >> 1)) {
      gen_switch_cond_table_jump(swtch, vreg, cases, len, cond_flag);
      return;
    }

    BB *bbne = new_bb();
    int m = len >> 1;
    Stmt *c = cases[m];
    VReg *num = new_const_vreg(c->case_.value->fixnum, vreg->vsize);
    new_ir_cjmp(vreg, num, COND_EQ | cond_flag, c->case_.bb);
    set_curbb(bbne);

    BB *bblt = new_bb();
    BB *bbgt = new_bb();
    new_ir_cjmp(vreg, num, COND_GT | cond_flag, bbgt);
    set_curbb(bblt);
    gen_switch_cond_recur(swtch, vreg, cases, m, cond_flag);
    set_curbb(bbgt);
    gen_switch_cond_recur(swtch, vreg, cases + (m + 1), len - (m + 1), cond_flag);
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
    new_ir_jmp(target != NULL ? target->case_.bb : stmt->switch_.break_bb);
    set_curbb(nextbb);
  } else {
    if (len > 0) {
      // Sort cases in increasing order.
      qsort(cases->data, len, sizeof(void*), compare_cases);

      if (stmt->switch_.default_ != NULL)
        --len;  // Ignore default.
      int cond_flag = is_unsigned(value->type) ? COND_UNSIGNED : 0;
      gen_switch_cond_recur(stmt, vreg, (Stmt**)cases->data, len, cond_flag);
    } else {
      Stmt *def = stmt->switch_.default_;
      new_ir_jmp(def != NULL ? def->case_.bb : stmt->switch_.break_bb);
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

inline void gen_case(Stmt *stmt) {
  set_curbb(stmt->case_.bb);
}

static void gen_while(Stmt *stmt) {
  BB *save_break, *save_cont;
  BB *loop_bb = new_bb();
  BB *cond_bb = push_continue_bb(&save_cont);
  BB *next_bb = push_break_bb(&save_break);

  new_ir_jmp(cond_bb);

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

  new_ir_jmp(cond_bb);

  set_curbb(loop_bb);
  gen_stmt(stmt->for_.body);

  set_curbb(continue_bb);
  if (stmt->for_.post != NULL)
    gen_expr_stmt(stmt->for_.post);

  set_curbb(cond_bb);
  if (stmt->for_.cond != NULL)
    gen_cond_jmp(stmt->for_.cond, true, loop_bb);
  else
    new_ir_jmp(loop_bb);

  set_curbb(next_bb);
  pop_continue_bb(save_cont);
  pop_break_bb(save_break);
}

static void gen_break(void) {
  assert(s_break_bb != NULL);
  BB *bb = new_bb();
  new_ir_jmp(s_break_bb);
  set_curbb(bb);
}

static void gen_continue(void) {
  assert(s_continue_bb != NULL);
  BB *bb = new_bb();
  new_ir_jmp(s_continue_bb);
  set_curbb(bb);
}

extern inline void gen_goto(Stmt *stmt) {
  assert(curfunc->label_table != NULL);
  Stmt *label = table_get(curfunc->label_table, stmt->goto_.label->ident);
  assert(label != NULL);
  new_ir_jmp(label->label.bb);
  set_curbb(new_bb());
}

extern inline void gen_label(Stmt *stmt) {
  assert(stmt->label.bb != NULL);
  set_curbb(stmt->label.bb);
  gen_stmt(stmt->label.stmt);
}

void gen_clear_local_var(const VarInfo *varinfo) {
  if (is_prim_type(varinfo->type))
    return;

  VReg *vreg = new_ir_bofs(varinfo->local.frameinfo);
  gen_clear(varinfo->type, vreg);
}

static void gen_vardecl(Vector *decls) {
  for (int i = 0; i < decls->len; ++i) {
    VarDecl *decl = decls->data[i];
    if (decl->init_stmt != NULL) {
      if (decl->ident != NULL) {
        VarInfo *varinfo = scope_find(curscope, decl->ident, NULL);
        gen_clear_local_var(varinfo);
      }
      gen_stmt(decl->init_stmt);
    }
  }
}

extern inline void gen_expr_stmt(Expr *expr) {
  gen_expr(expr);
}

void gen_stmt(Stmt *stmt) {
  if (stmt == NULL)
    return;

  switch (stmt->kind) {
  case ST_EMPTY: break;
  case ST_EXPR:  gen_expr_stmt(stmt->expr); break;
  case ST_RETURN:  gen_return(stmt); break;
  case ST_BLOCK:  gen_block(stmt); break;
  case ST_IF:  gen_if(stmt); break;
  case ST_SWITCH:  gen_switch(stmt); break;
  case ST_CASE: gen_case(stmt); break;
  case ST_WHILE:  gen_while(stmt); break;
  case ST_DO_WHILE:  gen_do_while(stmt); break;
  case ST_FOR:  gen_for(stmt); break;
  case ST_BREAK:  gen_break(); break;
  case ST_CONTINUE:  gen_continue(); break;
  case ST_GOTO:  gen_goto(stmt); break;
  case ST_LABEL:  gen_label(stmt); break;
  case ST_VARDECL:  gen_vardecl(stmt->vardecl.decls); break;
  case ST_ASM:  gen_asm(stmt); break;
  }
}

////////////////////////////////////////////////

void prepare_register_allocation(Function *func) {
  bool require_stack_frame = func->type->func.vaargs || (func->flag & FUNCF_STACK_MODIFIED) != 0;

  for (int i = 0; i < func->scopes->len; ++i) {
    Scope *scope = (Scope*)func->scopes->data[i];
    if (scope->vars == NULL)
      continue;

    for (int j = 0; j < scope->vars->len; ++j) {
      VarInfo *varinfo = scope->vars->data[j];
      if (!is_local_storage(varinfo))
        continue;
      VReg *vreg = varinfo->local.vreg;
      if (vreg == NULL) {
        assert(!is_prim_type(varinfo->type));
        // Whether it is a parameter or local variable defined in a function,
        // it is needed to access relative to base pointer.
        require_stack_frame = true;
        continue;
      }

      assert(is_prim_type(varinfo->type));
      if (vreg->flag & (VRF_REF | VRF_STACK_PARAM)) {
        spill_vreg(vreg);
        require_stack_frame = true;
      }
    }
  }

  if (require_stack_frame) {
    FuncBackend *fnbe = func->extra;
    fnbe->ra->flag |= RAF_STACK_FRAME;
  }
}

void map_virtual_to_physical_registers(RegAlloc *ra) {
  for (int i = 0, vreg_count = ra->vregs->len; i < vreg_count; ++i) {
    VReg *vreg = ra->vregs->data[i];
    if (vreg == NULL)
      continue;
    assert(!(vreg->flag & VRF_CONST) && vreg->virt >= 0);
    vreg->phys = ra->intervals[vreg->virt].phys;
  }
}

// Detect living registers for each instruction.
void detect_living_registers(RegAlloc *ra, BBContainer *bbcon) {
  int maxbit = ra->settings->phys_max + ra->settings->fphys_max;
  uint64_t living_pregs = 0;
  assert((int)sizeof(living_pregs) * CHAR_BIT >= maxbit);
  LiveInterval **livings = ALLOCA(sizeof(*livings) * maxbit);
  for (int i = 0; i < maxbit; ++i)
    livings[i] = NULL;

#define VREGFOR(li, ra)  ((VReg*)ra->vregs->data[li->virt])
#define BITNO(li, ra)    (li->phys + (VREGFOR(li, ra)->flag & VRF_FLONUM ? floreg_offset : 0))
  const int floreg_offset = ra->settings->phys_max;
  // Activate function parameters a priori.
  for (int i = 0; i < ra->vregs->len; ++i) {
    LiveInterval *li = ra->sorted_intervals[i];
    assert(li != NULL);
    if (li->start >= 0)
      break;
    if (li->state != LI_NORMAL || VREGFOR(li, ra) == NULL)
      continue;
    int bitno = BITNO(li, ra);
    living_pregs |= 1ULL << bitno;
    livings[bitno] = li;
  }

  int nip = 0, head = 0;
  for (int i = 0; i < bbcon->bbs->len; ++i) {
    BB *bb = bbcon->bbs->data[i];
    for (int j = 0; j < bb->irs->len; ++j, ++nip) {
      // Eliminate deactivated registers.
      for (int k = 0; k < maxbit; ++k) {
        LiveInterval *li = livings[k];
        if (li != NULL && nip == li->end) {
          assert(BITNO(li, ra) == k);
          living_pregs &= ~(1ULL << k);
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
        if (li->start > nip)
          break;
        if (li->state != LI_NORMAL)
          continue;
        if (nip == li->start) {
          assert(VREGFOR(li, ra) != NULL);
          int bitno = BITNO(li, ra);
          living_pregs |= 1ULL << bitno;
          livings[bitno] = li;
        }
      }
    }
  }
#undef BITNO
#undef VREGFOR
}

void alloc_stack_variables_onto_stack_frame(Function *func) {
  FuncBackend *fnbe = func->extra;
  assert(fnbe->frame_size == 0);
  size_t frame_size = 0;
  int param_offset = calculate_func_param_bottom(func);
  fnbe->vaarg_frame_info.offset = param_offset;

  if (func->type->func.vaargs) {
#if VAARG_FP_AS_GP
    // Register parameters are put below stack frame, so not added to frame_size.
#else
    frame_size = (MAX_REG_ARGS + MAX_FREG_ARGS) * POINTER_SIZE;
#endif
  }

  bool require_stack_frame = false;

  // Allocate stack variables onto stack frame.
  for (int i = 0; i < func->scopes->len; ++i) {
    Scope *scope = func->scopes->data[i];
    if (scope->vars == NULL)
      continue;

    for (int j = 0; j < scope->vars->len; ++j) {
      VarInfo *varinfo = scope->vars->data[j];
      if (!is_local_storage(varinfo))
        continue;

      if (varinfo->storage & VS_PARAM) {
        assert(is_stack_param(varinfo->type) || varinfo->local.vreg != NULL);
        if (is_stack_param(varinfo->type)) {
          FrameInfo *fi = varinfo->local.frameinfo;
          fi->offset = param_offset = ALIGN(param_offset, align_size(varinfo->type));
          param_offset += ALIGN(type_size(varinfo->type), POINTER_SIZE);
          require_stack_frame = true;
          continue;
        } else if (varinfo->local.vreg->flag & VRF_STACK_PARAM) {
          FrameInfo *fi = varinfo->local.frameinfo;
          fi->offset = param_offset = ALIGN(param_offset, POINTER_SIZE);
          param_offset += POINTER_SIZE;
          require_stack_frame = true;
          continue;
        }
      }

      if (is_prim_type(varinfo->type)) {
        // Primitive type variables are handled according to RegAlloc results in below.
        continue;
      }

      assert(varinfo->local.vreg == NULL);
      FrameInfo *fi = varinfo->local.frameinfo;
      assert(fi != NULL);

      Type *type = varinfo->type;
      size_t size = type_size(type);
      if (type->kind == TY_STRUCT && type->struct_.info->is_flexible) {
        Initializer *init = varinfo->local.init;
        if (init != NULL) {
          int m = type->struct_.info->member_count;
          assert(init->kind == IK_MULTI);
          assert(init->multi->len == m);
          Initializer *e = init->multi->data[m - 1];
          if (e != NULL) {
            assert(e->kind == IK_MULTI);
            MemberInfo *me = &type->struct_.info->members[m - 1];
            assert(me->type->kind == TY_ARRAY);
            size += type_size(me->type->pa.ptrof) * e->multi->len;
          }
        }
      }
      if (size < 1)
        size = 1;
      size_t align = align_size(type);

      frame_size = ALIGN(frame_size + size, align);
      fi->offset = -(int)frame_size;
    }
  }

  // Allocate spilled variables onto stack frame.
  RegAlloc *ra = fnbe->ra;
  for (int i = 0; i < ra->vregs->len; ++i) {
    LiveInterval *li = ra->sorted_intervals[i];
    if (li->state != LI_SPILL)
      continue;
    VReg *vreg = ra->vregs->data[li->virt];
    assert(vreg->flag & VRF_SPILLED);
    if (vreg->flag & VRF_STACK_PARAM)
      continue;  // Handled in above, so skip here.

    int size, align;
    size = align = 1 << vreg->vsize;

    frame_size = ALIGN(frame_size + size, align);
    vreg->frame.offset = -(int)frame_size;
  }

  fnbe->frame_size = frame_size;
  assert(!(require_stack_frame || frame_size > 0) || (fnbe->ra->flag & RAF_STACK_FRAME));
  UNUSED(require_stack_frame);
}

bool gen_defun(Function *func) {
  if (func->scopes == NULL)  // Prototype definition
    return false;

  VarInfo *funcvi = scope_find(global_scope, func->name, NULL);
  if (funcvi != NULL && satisfy_inline_criteria(funcvi) && !(funcvi->storage & VS_STATIC)) {
    // Omit inline function: func->extra preserves the value (NULL).
    return false;
  }

  curfunc = func;
  FuncBackend *fnbe = func->extra = calloc_or_die(sizeof(FuncBackend));
  fnbe->ra = NULL;
  fnbe->bbcon = NULL;
  fnbe->ret_bb = NULL;
  fnbe->retval = NULL;
  fnbe->result_dst = NULL;
  fnbe->frame_size = 0;
  fnbe->vaarg_frame_info.offset = 0;  // Calculated in later.

  fnbe->bbcon = new_func_blocks();
  set_curbb(new_bb());
  fnbe->ra = curra = new_reg_alloc(&kArchRegAllocSettings);

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
  detect_from_bbs(fnbe->bbcon);

  curfunc = NULL;
  curra = NULL;
  return true;
}

extern inline void gen_defun_after(Function *func) {
  FuncBackend *fnbe = func->extra;
  curfunc = func;

  optimize(fnbe->ra, fnbe->bbcon);

  prepare_register_allocation(func);
  tweak_irs(fnbe);
  analyze_reg_flow(fnbe->bbcon);

  alloc_physical_registers(fnbe->ra, fnbe->bbcon);
  map_virtual_to_physical_registers(fnbe->ra);
  detect_living_registers(fnbe->ra, fnbe->bbcon);

  alloc_stack_variables_onto_stack_frame(func);

  curfunc = NULL;
}

static void gen_decl(Declaration *decl) {
  if (decl == NULL)
    return;

  switch (decl->kind) {
  case DCL_DEFUN:
    {
      Function *func = decl->defun.func;
      if (gen_defun(func))
        gen_defun_after(func);
    }
    break;
  case DCL_VARDECL:
    break;
  case DCL_ASM:
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
