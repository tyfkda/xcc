#include "../../config.h"
#include "codegen.h"

#include <assert.h>
#include <inttypes.h>
#include <limits.h>  // CHAR_BIT
#include <stdbool.h>
#include <stdlib.h>  // qsort
#include <string.h>

#include "ast.h"
#include "be_aux.h"
#include "cc_misc.h"  // is_function_omitted
#include "fe_misc.h"  // curfunc, curscope
#include "ir.h"
#include "optimize.h"
#include "regalloc.h"
#include "table.h"
#include "type.h"
#include "util.h"
#include "var.h"

void set_curbb(BB *bb) {
  assert(bb != NULL);
  assert(curfunc != NULL);
  if (curbb != NULL)
    curbb->next = bb;
  curbb = bb;
  vec_push(((FuncBackend*)curfunc->extra)->bbcon, bb);
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
  const Token *retval_token = alloc_dummy_ident();
  Type *retptrtype = ptrof(rettype);
  Scope *top_scope = func->scopes->data[0];
  VarInfo *varinfo = scope_add(top_scope, retval_token, retptrtype, 0);
  VReg *vreg = add_new_vreg(varinfo->type);
  vreg->flag |= VRF_PARAM;
  vreg->reg_param_index = 0;
  varinfo->local.vreg = vreg;
  FuncBackend *fnbe = func->extra;
  fnbe->retvarinfo = varinfo;
  fnbe->retval = vreg;
  return varinfo;
}

static void alloc_variable_registers(Function *func) {
  assert(func->type->kind == TY_FUNC);

  for (int i = 0; i < func->scopes->len; ++i) {
    Scope *scope = func->scopes->data[i];
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
      Type *type = varinfo->type;
#if STRUCT_ARG_AS_POINTER
      if (varinfo->storage & VS_PARAM && type->kind == TY_STRUCT && !is_small_struct(type)) {
        varinfo->type = type = ptrof(type);  // Caution! Overwrite type as its pointer.
      }
#endif
      if (!is_prim_type(type)) {
        FrameInfo *fi = malloc_or_die(sizeof(*fi));
        fi->offset = 0;
        varinfo->local.frameinfo = fi;
        continue;
      }

      const int storage = varinfo->storage;
      VReg *vreg = add_new_vreg_with_storage(type, storage);
      if (storage & VS_REF_TAKEN)
        vreg->flag |= VRF_REF;
      if (type->qualifier & TQ_VOLATILE)
        vreg->flag |= (storage & VS_REGISTER) ? (VRF_VOLATILEREG | VRF_NO_SPILL) : VRF_VOLATILE;
      varinfo->local.vreg = vreg;
      varinfo->local.frameinfo = &vreg->frame;
    }
  }

  int regcount[2] = {0, 0};

  // Handle if return value is on the stack.
  if (func->type->func.ret->kind == TY_STRUCT) {
    prepare_retvar(func);
    ++regcount[GPREG];
  }

  // Count register parameters, or set flag.
  const Vector *params = func->params;
  if (params != NULL) {
    for (int i = 0; i < params->len; ++i) {
      VarInfo *varinfo = params->data[i];
      VReg *vreg = varinfo->local.vreg;
      if (vreg != NULL) {
        vreg->flag |= VRF_PARAM;
        bool is_flo = (vreg->flag & VRF_FLONUM) != 0;
        int *p = &regcount[is_flo];
        if (*p < kArchSetting.max_reg_args[is_flo])
          vreg->reg_param_index = (*p)++;
        else
          vreg->flag |= VRF_STACK_PARAM;
      }
    }
  }
}

int enumerate_register_params(Function *func, const int max_reg[2], RegParamInfo *args) {
  int arg_count[2] = {0, 0};
  int reg_index[2] = {0, 0};
  int total = 0;

  FuncBackend *fnbe = func->extra;
  VReg *retval = fnbe->retval;
  if (retval != NULL) {
    RegParamInfo *p = &args[total++];
    p->varinfo = fnbe->retvarinfo;
    p->vreg = retval;
    p->index = 0;
    ++arg_count[GPREG];
    ++reg_index[GPREG];
  }

  const Vector *params = func->params;
  if (params != NULL) {
    for (int i = 0, len = params->len; i < len; ++i) {
      const VarInfo *varinfo = params->data[i];
      const Type *type = varinfo->type;
      size_t n = 1;
      if (is_stack_param(type)) {
        if (type->kind != TY_STRUCT || !is_small_struct(type))
          continue;
        n = (type_size(type) + TARGET_POINTER_SIZE - 1) / TARGET_POINTER_SIZE;
      }
      bool is_flo = is_flonum(type);
      int regidx = reg_index[is_flo];
      if (regidx + (int)n > max_reg[is_flo])
        continue;
      reg_index[is_flo] += n;

      RegParamInfo *p = &args[total++];
      p->varinfo = varinfo;
      p->vreg = varinfo->local.vreg;  // Might be NULL (small struct).
      p->index = regidx;
      arg_count[is_flo] += 1;
    }
  }
  return arg_count[GPREG] + arg_count[FPREG];
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
  return most_significant_bit(s);
}

void gen_memcpy(const Type *type, VReg *dst, VReg *src) {
  size_t size = type_size(type);
  if (size == 0)
    return;
  enum VRegSize elem_vsize = get_elem_vtype(type);
  size_t count = size >> elem_vsize;
  assert(count > 0);
  if (count == 1) {
    VReg *tmp = new_ir_load(src, elem_vsize, to_vflag(type), 0)->dst;
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
    VReg *tmp = new_ir_load(srcp, elem_vsize, to_vflag(type), 0)->dst;
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

static inline void gen_asm(Stmt *stmt) {
  VReg *output = NULL;
  Vector *registers = new_vector();
  if (stmt->asm_.outputs != NULL) {
    assert(stmt->asm_.outputs->len == 1);  // TODO: Handle multiple outputs.
    const AsmArg *arg = stmt->asm_.outputs->data[0];
    assert(arg->expr->kind == EX_VAR);
    output = gen_expr(arg->expr);
    vec_push(registers, output);
  }
  if (stmt->asm_.inputs != NULL) {
    for (int i = 0; i < stmt->asm_.inputs->len; ++i) {
      const AsmArg *arg = stmt->asm_.inputs->data[i];
      VReg *vreg = gen_expr(arg->expr);
      vec_push(registers, vreg);
    }
  }

  new_ir_asm(stmt->asm_.templates, output, registers);
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

static inline void gen_expr_stmt(Expr *expr) {
  gen_expr(expr);
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

static inline void gen_return(Stmt *stmt) {
  assert(curfunc != NULL);
  BB *bb = new_bb();
  FuncBackend *fnbe = curfunc->extra;
  if (stmt->return_.val != NULL) {
    Expr *val = stmt->return_.val;
    VReg *vreg = gen_expr(val);
    if (is_prim_type(val->type)) {
      int flag = is_unsigned(val->type) ? IRF_UNSIGNED : 0;
      if (fnbe->result_dst == NULL)
        new_ir_result(vreg, flag);
      else
        new_ir_mov(fnbe->result_dst, vreg, flag);
    } else if (val->type->kind != TY_VOID) {
      VReg *retval = fnbe->retval;
      if (retval != NULL) {
        gen_memcpy(val->type, retval, vreg);
        if (fnbe->result_dst == NULL)
          new_ir_result(retval, IRF_UNSIGNED);  // Pointer is unsigned.
        else
          new_ir_mov(fnbe->result_dst, retval, IRF_UNSIGNED);  // Pointer is unsigned.
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

static inline void gen_if(Stmt *stmt) {
  BB *tbb = new_bb();
  BB *fbb = new_bb();
  gen_cond_jmp(stmt->if_.cond, tbb, fbb);
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

static void gen_switch_cond_table_jump(Stmt *swtch, VReg *vreg, Stmt **cases, int len,
                                       int cond_flag) {
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

static inline void gen_case(Stmt *stmt) {
  set_curbb(stmt->case_.bb);
  gen_stmt(stmt->case_.stmt);
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
  gen_cond_jmp(stmt->while_.cond, loop_bb, next_bb);

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
  gen_cond_jmp(stmt->while_.cond, loop_bb, next_bb);

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
    gen_cond_jmp(stmt->for_.cond, loop_bb, next_bb);
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

static inline void gen_goto(Stmt *stmt) {
  assert(curfunc->label_table != NULL);
  Stmt *label = table_get(curfunc->label_table, stmt->goto_.label->ident);
  assert(label != NULL);
  new_ir_jmp(label->label.bb);
  set_curbb(new_bb());
}

static inline void gen_label(Stmt *stmt) {
  if (stmt->label.bb != NULL)  // This case happens when the label is not used.
    set_curbb(stmt->label.bb);
  gen_stmt(stmt->label.stmt);
}

void gen_clear_local_var(const VarInfo *varinfo) {
  if (is_prim_type(varinfo->type))
    return;

  VReg *vreg = new_ir_bofs(varinfo->local.frameinfo)->dst;
  gen_clear(varinfo->type, vreg);
}

static void gen_vardecl(VarDecl *decl) {
  assert(decl->init_stmt != NULL);
  VarInfo *varinfo = decl->varinfo;
  assert(varinfo != NULL);
  gen_clear_local_var(varinfo);
  gen_stmt(decl->init_stmt);
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
  case ST_VARDECL:  gen_vardecl(stmt->vardecl); break;
  case ST_ASM:  gen_asm(stmt); break;
  }
}

////////////////////////////////////////////////

void prepare_register_allocation(Function *func) {
  FuncBackend *fnbe = func->extra;
  bool require_stack_frame = func->type->func.vaargs || fnbe->stack_work_size_vreg != NULL;

  for (int i = 0; i < func->scopes->len; ++i) {
    Scope *scope = (Scope*)func->scopes->data[i];
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
      if (vreg->flag & (VRF_FORCEMEMORY | VRF_STACK_PARAM)) {
        spill_vreg(vreg);
        require_stack_frame = true;
      }
    }
  }

  if (require_stack_frame)
    fnbe->ra->flag |= RAF_STACK_FRAME;
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
  int maxbit = ra->settings->regset[GPREG].phys_max + ra->settings->regset[FPREG].phys_max;
  unsigned long living_pregs = 0;
  assert((int)sizeof(living_pregs) * CHAR_BIT >= maxbit);
  LiveInterval **livings = ALLOCA(sizeof(*livings) * maxbit);
  for (int i = 0; i < maxbit; ++i)
    livings[i] = NULL;

#define VREGFOR(li, ra)  ((VReg*)ra->vregs->data[li->virt])
#define BITNO(li, ra)    (li->phys + (VREGFOR(li, ra)->flag & VRF_FLONUM ? floreg_offset : 0))
  const int floreg_offset = ra->settings->regset[GPREG].phys_max;
  // Activate function parameters a priori.
  for (int i = 0; i < ra->vregs->len; ++i) {
    LiveInterval *li = ra->sorted_intervals[i];
    assert(li != NULL);
    if (li->start >= 0)
      break;
    if (li->state != LI_NORMAL || VREGFOR(li, ra) == NULL)
      continue;
    int bitno = BITNO(li, ra);
    living_pregs |= 1UL << bitno;
    livings[bitno] = li;
  }

  int nip = 0, head = 0;
  for (int i = 0; i < bbcon->len; ++i) {
    BB *bb = bbcon->data[i];
    for (int j = 0; j < bb->irs->len; ++j, ++nip) {
      // Eliminate deactivated registers.
      for (int k = 0; k < maxbit; ++k) {
        LiveInterval *li = livings[k];
        if (li != NULL && nip == li->end) {
          assert(BITNO(li, ra) == k);
          living_pregs &= ~(1UL << k);
          livings[k] = NULL;
        }
      }

      // Store living vregs to IR_CALL.
      IR *ir = bb->irs->data[j];
      if (ir->kind == IR_CALL) {
        ir->call->living_pregs = living_pregs;
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
          living_pregs |= 1UL << bitno;
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
    frame_size = (kArchSetting.max_reg_args[GPREG] + kArchSetting.max_reg_args[FPREG]) * TARGET_POINTER_SIZE;
#endif
  }

  bool require_stack_frame = false;

  int arg_start = is_prim_type(func->type->func.ret) ? 0 : 1;
  int reg_index[2] = {arg_start, 0};  // [0]=gp-reg, [1]=fp-reg

  // Parameters.
  for (int i = 0; i < func->params->len; ++i) {
    VarInfo *varinfo = func->params->data[i];
    assert(is_local_storage(varinfo));
    const Type *type = varinfo->type;
    size_t size = type_size(type), align = align_size(type);
    bool is_flo = is_flonum(type);
    if (is_small_struct(type)) {
      size_t n = (size + TARGET_POINTER_SIZE - 1) / TARGET_POINTER_SIZE;
      if (reg_index[is_flo] + (int)n <= kArchSetting.max_reg_args[is_flo]) {
        // Small struct, passed by register.
        reg_index[GPREG] += n;

        // Allocate stack frame.
        FrameInfo *fi = varinfo->local.frameinfo;
        frame_size = ALIGN(frame_size + size, align);
        fi->offset = -(int)frame_size;
        continue;
      }
    } else if (!is_stack_param(type)) {
      assert(varinfo->local.vreg != NULL);
      if (!(varinfo->local.vreg->flag & VRF_STACK_PARAM)) {
        reg_index[is_flo] += 1;
        continue;  // Passed by register.
      }
      size = align = TARGET_POINTER_SIZE;
    }

    FrameInfo *fi = varinfo->local.frameinfo;
    fi->offset = param_offset = ALIGN(param_offset, align);
    param_offset += ALIGN(size, TARGET_POINTER_SIZE);
    require_stack_frame = true;
  }

  // Local variables.
  for (int i = 0; i < func->scopes->len; ++i) {
    Scope *scope = func->scopes->data[i];
    for (int j = 0; j < scope->vars->len; ++j) {
      VarInfo *varinfo = scope->vars->data[j];
      if (!is_local_storage(varinfo) || (varinfo->storage & VS_PARAM))
        continue;

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

  VarInfo *funcvi = scope_find(global_scope, func->ident->ident, NULL);
  if (is_function_omitted(funcvi)) {
    // Omit function: func->extra preserves the value (NULL).
    return false;
  }

  curfunc = func;
  static_vars = func->static_vars;
  FuncBackend *fnbe = func->extra = calloc_or_die(sizeof(FuncBackend));
  fnbe->ra = NULL;
  fnbe->bbcon = NULL;
  fnbe->ret_bb = NULL;
  fnbe->retvarinfo = NULL;
  fnbe->retval = NULL;
  fnbe->result_dst = NULL;
  fnbe->funcalls = NULL;
  fnbe->frame_size = 0;
  fnbe->vaarg_frame_info.offset = 0;  // Calculated in later.
  fnbe->stack_work_size = 0;
  fnbe->stack_work_size_vreg = NULL;

  fnbe->bbcon = new_func_blocks();
  set_curbb(new_bb());
  fnbe->ra = curra = new_reg_alloc(&kArchRegAllocSettings);

  // Allocate BBs for goto labels.
  if (func->label_table != NULL) {
    Table *label_table = func->label_table;
    const Name *name;
    Stmt *label;
    for (int it = 0; (it = table_iterate(label_table, it, &name, (void**)&label)) != -1; ) {
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
  static_vars = NULL;
  curra = NULL;
  return true;
}

static inline void gen_defun_after(Function *func) {
  FuncBackend *fnbe = func->extra;
  curfunc = func;
  curra = fnbe->ra;

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
