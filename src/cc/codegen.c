#include "codegen.h"

#include <assert.h>
#include <inttypes.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

#if defined(__linux) && !defined(__XV6)
#define USE_ALLOCA
#include <alloca.h>
#endif

#include "expr.h"
#include "ir.h"
#include "lexer.h"
#include "stmt.h"
#include "regalloc.h"
#include "sema.h"
#include "type.h"
#include "util.h"
#include "var.h"
#include "x86_64.h"

const int FRAME_ALIGN = 8;
const int STACK_PARAM_BASE_OFFSET = (2 - MAX_REG_ARGS) * 8;

static void gen_expr_stmt(Expr *expr);

void set_curbb(BB *bb) {
  assert(curdefun != NULL);
  curbb = bb;
  vec_push(curdefun->func->bbcon->bbs, bb);
}

size_t type_size(const Type *type) {
  switch (type->kind) {
  case TY_VOID:
    return 1;  // ?
  case TY_NUM:
    switch (type->num.kind) {
    case NUM_CHAR:
      return 1;
    case NUM_SHORT:
      return 2;
    case NUM_INT:
    case NUM_ENUM:
      return 4;
    case NUM_LONG:
      return 8;
    default:
      assert(!"Error");
      return 1;
    }
  case TY_PTR:
  case TY_FUNC:
    return 8;
  case TY_ARRAY:
    assert(type->pa.length != (size_t)-1);
    return type_size(type->pa.ptrof) * type->pa.length;
  case TY_STRUCT:
    ensure_struct((Type*)type, NULL);
    calc_struct_size(type->struct_.info);
    return type->struct_.info->size;
  default:
    assert(false);
    return 1;
  }
}

int align_size(const Type *type) {
  switch (type->kind) {
  case TY_VOID:
    return 1;  // ?
  case TY_NUM:
    switch (type->num.kind) {
    case NUM_CHAR:
      return 1;
    case NUM_SHORT:
      return 2;
    case NUM_INT:
    case NUM_ENUM:
      return 4;
    case NUM_LONG:
      return 8;
    default:
      assert(!"Error");
      return 1;
    }
  case TY_PTR:
  case TY_FUNC:
    return 8;
  case TY_ARRAY:
    return align_size(type->pa.ptrof);
  case TY_STRUCT:
    ensure_struct((Type*)type, NULL);
    calc_struct_size(type->struct_.info);
    return type->struct_.info->align;
  default:
    assert(false);
    return 1;
  }
}

void calc_struct_size(StructInfo *sinfo) {
  assert(sinfo != NULL);
  if (sinfo->size >= 0)
    return;

  size_t size = 0;
  size_t maxsize = 0;
  int max_align = 1;

  for (int i = 0, len = sinfo->members->len; i < len; ++i) {
    VarInfo *member = sinfo->members->data[i];
    size_t sz = type_size(member->type);
    int align = align_size(member->type);
    size = ALIGN(size, align);
    member->struct_.offset = size;
    if (!sinfo->is_union)
      size += sz;
    else
      if (maxsize < sz)
        maxsize = sz;
    if (max_align < align)
      max_align = align;
  }

  if (sinfo->is_union)
    size = maxsize;
  size = ALIGN(size, max_align);
  sinfo->size = size;
  sinfo->align = max_align;
}

static const char *escape(int c) {
  switch (c) {
  case '\0': return "\\0";
  case '\n': return "\\n";
  case '\r': return "\\r";
  case '\t': return "\\t";
  case '"': return "\\\"";
  case '\\': return "\\\\";
  default:   return NULL;
  }
}

static void escape_string(const char *str, size_t size, StringBuffer *sb) {
  const char *s, *p;
  const char *end = str + size;
  for (s = p = str; p < end ; ++p) {
    const char *e = escape(*p);
    if (e == NULL)
      continue;

    if (p > s)
      sb_append(sb, s, p);
    sb_append(sb, e, NULL);
    s = p + 1;
  }
  if (p > s)
    sb_append(sb, s, p);
}

void construct_initial_value(unsigned char *buf, const Type *type, Initializer *init, Vector **pptrinits) {
  assert(init == NULL || init->kind != vDot);

  switch (type->kind) {
  case TY_NUM:
    {
      intptr_t v = 0;
      if (init != NULL) {
        assert(init->kind == vSingle);
        Expr *value = init->single;
        if (!(is_const(value) && is_number(value->type->kind)))
          error("Illegal initializer: constant number expected");
        v = value->num.ival;
      }

      int size = type_size(type);
      for (int i = 0; i < size; ++i)
        buf[i] = v >> (i * 8);  // Little endian

      switch (type->num.kind) {
      case NUM_CHAR:  _BYTE(NUM(v)); break;
      case NUM_SHORT: _WORD(NUM(v)); break;
      case NUM_LONG:  _QUAD(NUM(v)); break;
      default:
      case NUM_INT: case NUM_ENUM:
        _LONG(NUM(v));
        break;
      }
    }
    break;
  case TY_PTR:
    if (init != NULL) {
      assert(init->kind == vSingle);
      Expr *value = init->single;
      while (value->kind == EX_CAST)
        value = value->unary.sub;
      if (value->kind == EX_REF || value->kind == EX_VARREF) {
        if (value->kind == EX_REF)
          value = value->unary.sub;
        // TODO: Type check.

        assert(value->kind == EX_VARREF);
        assert(value->varref.scope == NULL);

        void **init = malloc(sizeof(void*) * 2);
        init[0] = buf;
        init[1] = (void*)value->varref.ident;
        if (*pptrinits == NULL)
          *pptrinits = new_vector();
        vec_push(*pptrinits, init);

        const char *label = value->varref.ident;
        const VarInfo *varinfo = find_global(label);
        assert(varinfo != NULL);
        if ((varinfo->flag & VF_STATIC) == 0)
          label = MANGLE(label);
        _QUAD(label);
      } else if (value->kind == EX_STR) {
        assert(!"`char* s = \"...\"`; should be handled in parser");
      } else if (is_const(value) && value->kind == EX_NUM) {
        intptr_t x = value->num.ival;
        for (int i = 0; i < WORD_SIZE; ++i)
          buf[i] = x >> (i * 8);  // Little endian

        _QUAD(NUM(x));
      } else {
        assert(!"initializer type error");
      }
    } else {
      _QUAD(NUM(0));
    }
    break;
  case TY_ARRAY:
    if (init == NULL || init->kind == vMulti) {
      const Type *elem_type = type->pa.ptrof;
      size_t elem_size = type_size(elem_type);
      if (init != NULL) {
        Vector *init_array = init->multi;
        size_t index = 0;
        size_t len = init_array->len;
        for (size_t i = 0; i < len; ++i, ++index) {
          Initializer *init_elem = init_array->data[i];
          if (init_elem->kind == vArr) {
            size_t next = init_elem->arr.index->num.ival;
            for (size_t j = index; j < next; ++j)
              construct_initial_value(buf + (j * elem_size), elem_type, NULL, pptrinits);
            index = next;
            init_elem = init_elem->arr.value;
          }
          construct_initial_value(buf + (index * elem_size), elem_type, init_elem, pptrinits);
        }
        // Padding
        for (size_t i = index, n = type->pa.length; i < n; ++i)
          construct_initial_value(buf + (i * elem_size), elem_type, NULL, pptrinits);
      }
    } else {
      if (init->kind == vSingle &&
          is_char_type(type->pa.ptrof) && init->single->kind == EX_STR) {
        int src_size = init->single->str.size;
        size_t size = type_size(type);
        assert(size >= (size_t)src_size);
        memcpy(buf, init->single->str.buf, src_size);

        UNUSED(size);
        StringBuffer sb;
        sb_init(&sb);
        sb_append(&sb, "\"", NULL);
        escape_string((char*)buf, size, &sb);
        sb_append(&sb, "\"", NULL);
        _ASCII(sb_to_string(&sb));
      } else {
        error("Illegal initializer");
      }
    }
    break;
  case TY_STRUCT:
    {
      assert(init == NULL || init->kind == vMulti);

      const StructInfo *sinfo = type->struct_.info;
      int count = 0;
      int offset = 0;
      for (int i = 0, n = sinfo->members->len; i < n; ++i) {
        const VarInfo* member = sinfo->members->data[i];
        Initializer *mem_init;
        if (init == NULL) {
          if (sinfo->is_union)
            continue;
          mem_init = NULL;
        } else {
          mem_init = init->multi->data[i];
        }
        if (mem_init != NULL || !sinfo->is_union) {
          int align = align_size(member->type);
          if (offset % align != 0) {
            emit_align(align);
            offset = ALIGN(offset, align);
          }
          construct_initial_value(buf + member->struct_.offset, member->type, mem_init, pptrinits);
          ++count;
          offset = ALIGN(offset, align);
          offset += type_size(member->type);
        }
      }
      if (sinfo->is_union && count <= 0) {
        const VarInfo* member = sinfo->members->data[0];
        construct_initial_value(buf + member->struct_.offset, member->type, NULL, pptrinits);
        offset += type_size(member->type);
      }

      size_t size = type_size(type);
      if (size != (size_t)offset) {
        // Put padding.
        int d = size - offset;
        switch (d) {
        case 1:  _BYTE(NUM(0)); break;
        case 2:  _WORD(NUM(0)); break;
        case 4:  _LONG(NUM(0)); break;
        case 8:  _QUAD(NUM(0)); break;
        default:
          for (int i = 0; i < d; ++i)
            _BYTE(NUM(0));
          break;
        }
      }
    }
    break;
  default:
    fprintf(stderr, "Global initial value for type %d not implemented (yet)\n", type->kind);
    assert(false);
    break;
  }
}

static void put_data(const char *label, const VarInfo *varinfo) {
  size_t size = type_size(varinfo->type);
  unsigned char *buf = calloc(size, 1);
  if (buf == NULL)
    error("Out of memory");

  emit_align(align_size(varinfo->type));
  if ((varinfo->flag & VF_STATIC) == 0) {  // global
    label = MANGLE(label);
    _GLOBL(label);
  }
  EMIT_LABEL(label);

  Vector *ptrinits = NULL;  // <[ptr, label]>
  construct_initial_value(buf, varinfo->type, varinfo->global.init, &ptrinits);

  free(buf);
}

// Put RoData into code.
static void put_rodata(void) {
  for (int i = 0, len = map_count(gvar_map); i < len; ++i) {
    const VarInfo *varinfo = (const VarInfo*)gvar_map->vals->data[i];
    if (varinfo->type->kind == TY_FUNC ||
        (varinfo->flag & VF_EXTERN) != 0 || varinfo->global.init == NULL ||
        (varinfo->flag & VF_CONST) == 0)
      continue;

    const char *name = (const char *)gvar_map->keys->data[i];
    put_data(name, varinfo);
  }
}

// Put global with initial value (RwData).
static void put_rwdata(void) {
  for (int i = 0, len = map_count(gvar_map); i < len; ++i) {
    const VarInfo *varinfo = (const VarInfo*)gvar_map->vals->data[i];
    if (varinfo->type->kind == TY_FUNC ||
        (varinfo->flag & VF_EXTERN) != 0 || varinfo->global.init == NULL ||
        (varinfo->flag & VF_CONST) != 0)
      continue;

    const char *name = (const char *)gvar_map->keys->data[i];
    put_data(name, varinfo);
  }
}

// Put global without initial value (bss).
static void put_bss(void) {
  for (int i = 0, len = map_count(gvar_map); i < len; ++i) {
    const char *label = (const char *)gvar_map->keys->data[i];
    const VarInfo *varinfo = (const VarInfo*)gvar_map->vals->data[i];
    if (varinfo->type->kind == TY_FUNC || varinfo->global.init != NULL ||
        (varinfo->flag & VF_EXTERN) != 0)
      continue;

    emit_align(align_size(varinfo->type));
    size_t size = type_size(varinfo->type);
    if (size < 1)
      size = 1;
    if ((varinfo->flag & VF_STATIC) == 0) {  // global
      label = MANGLE(label);
      _GLOBL(label);
    }
    _COMM(label, NUM(size));
  }
}

//

static BB *s_break_bb;
static BB *s_continue_bb;
int stackpos = 8;

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

static void alloc_variable_registers(Function *func) {
  int param_index = 0;
  for (int i = 0; i < func->all_scopes->len; ++i) {
    Scope *scope = (Scope*)func->all_scopes->data[i];
    if (scope->vars == NULL)
      continue;

    for (int j = 0; j < scope->vars->len; ++j) {
      VarInfo *varinfo = scope->vars->data[j];
      if (varinfo->flag & (VF_STATIC | VF_EXTERN))
        continue;  // Static variable is not allocated on stack.

      VReg *vreg = add_new_reg(varinfo->type);
      bool spill = false;
      if (i == 0 && func->params != NULL && vec_contains(func->params, varinfo)) {
        vreg->param_index = param_index++;
        spill = true;
        if (func->type->func.vaargs) {  // Variadic function parameters.
          vreg->offset = (vreg->param_index - MAX_REG_ARGS) * WORD_SIZE;
        }
      }

      switch (varinfo->type->kind) {
      case TY_ARRAY:
      case TY_STRUCT:
        // Make non-primitive variable spilled.
        spill = true;
        break;
      default:
        break;
      }
      if (i == 0 && vreg->param_index >= MAX_REG_ARGS) {
        // Function argument passed through the stack.
        spill = true;
        vreg->offset = (vreg->param_index - MAX_REG_ARGS + 2) * WORD_SIZE;
      }

      if (spill)
        vreg_spill(vreg);

      varinfo->reg = vreg;
    }
  }
}

static void put_args_to_stack(Defun *defun) {
  static const char *kReg8s[] = {DIL, SIL, DL, CL, R8B, R9B};
  static const char *kReg16s[] = {DI, SI, DX, CX, R8W, R9W};
  static const char *kReg32s[] = {EDI, ESI, EDX, ECX, R8D, R9D};
  static const char *kReg64s[] = {RDI, RSI, RDX, RCX, R8, R9};

  // Store arguments into local frame.
  Vector *params = defun->func->params;
  int len = params != NULL ? params->len : 0;
  int n = defun->func->type->func.vaargs ? MAX_REG_ARGS : len;
  for (int i = 0; i < n; ++i) {
    const Type *type;
    int offset;
    if (i < len) {
      const VarInfo *varinfo = (const VarInfo*)params->data[i];
      type = varinfo->type;
      offset = varinfo->reg->offset;
    } else {  // vaargs
      type = &tyLong;
      offset = (i - MAX_REG_ARGS) * WORD_SIZE;
    }

    int size = 0;
    switch (type->kind) {
    case TY_NUM:
      switch (type->num.kind) {
      case NUM_CHAR:  size = 1; break;
      case NUM_SHORT: size = 2; break;
      case NUM_INT: case NUM_ENUM:
        size = 4;
        break;
      case NUM_LONG:  size = 8; break;
      default: assert(false); break;
      }
      break;
    case TY_PTR:  size = 8; break;
    default: assert(false); break;
    }

    if (i < MAX_REG_ARGS) {
      switch (size) {
      case 1:
        MOV(kReg8s[i], OFFSET_INDIRECT(offset, RBP));
        break;
      case 2:
        MOV(kReg16s[i], OFFSET_INDIRECT(offset, RBP));
        break;
      case 4:
        MOV(kReg32s[i], OFFSET_INDIRECT(offset, RBP));
        break;
      case 8:
        MOV(kReg64s[i], OFFSET_INDIRECT(offset, RBP));
        break;
      default:
        assert(false);
        break;
      }
    }
  }
}

static bool is_asm(Stmt *stmt) {
  return stmt->kind == ST_ASM;
}

static void gen_asm(Stmt *stmt) {
  assert(stmt->asm_.str->kind == EX_STR);
  new_ir_asm(stmt->asm_.str->str.buf);
}

static void gen_stmts(Vector *stmts) {
  if (stmts == NULL)
    return;

  for (int i = 0, len = stmts->len; i < len; ++i) {
    Stmt *stmt = stmts->data[i];
    if (stmt == NULL)
      continue;
    gen(stmt);
  }
}

static void gen_defun(Defun *defun) {
  Function *func = defun->func;
  if (func->top_scope == NULL)  // Prototype definition
    return;

  curdefun = defun;
  func->bbcon = new_func_blocks();
  set_curbb(new_bb());
  func->ra = curra = new_reg_alloc();

  // Allocate labels for goto.
  if (defun->label_map != NULL) {
    Map *label_map = defun->label_map;
    for (int i = 0, n = map_count(label_map); i < n; ++i)
      label_map->vals->data[i] = new_bb();
  }

  alloc_variable_registers(func);

  curscope = func->top_scope;
  func->ret_bb = bb_split(curbb);

  // Statements
  gen_stmts(defun->stmts);

  set_curbb(func->ret_bb);
  curbb = NULL;

  func->frame_size = alloc_real_registers(func);

  remove_unnecessary_bb(func->bbcon);

  curdefun = NULL;
  curscope = NULL;
  curra = NULL;
}

static void gen_block(Stmt *stmt) {
  if (stmt->block.stmts != NULL) {
    if (stmt->block.scope != NULL) {
      assert(curscope == stmt->block.scope->parent);
      curscope = stmt->block.scope;
    }
    gen_stmts(stmt->block.stmts);
    if (stmt->block.scope != NULL)
      curscope = curscope->parent;
  }
}

static void gen_return(Stmt *stmt) {
  BB *bb = bb_split(curbb);
  if (stmt->return_.val != NULL) {
    VReg *reg = gen_expr(stmt->return_.val);
    new_ir_result(reg, type_size(stmt->return_.val->type));
  }
  assert(curdefun != NULL);
  new_ir_jmp(COND_ANY, curdefun->func->ret_bb);
  set_curbb(bb);
}

static void gen_if(Stmt *stmt) {
  BB *tbb = bb_split(curbb);
  BB *fbb = bb_split(tbb);
  gen_cond_jmp(stmt->if_.cond, false, fbb);
  set_curbb(tbb);
  gen(stmt->if_.tblock);
  if (stmt->if_.fblock == NULL) {
    set_curbb(fbb);
  } else {
    BB *nbb = bb_split(fbb);
    new_ir_jmp(COND_ANY, nbb);
    set_curbb(fbb);
    gen(stmt->if_.fblock);
    set_curbb(nbb);
  }
}

static Vector *cur_case_values;
static Vector *cur_case_bbs;

static int compare_cases(const void *pa, const void *pb) {
  const int ia = *(int*)pa;
  const int ib = *(int*)pb;
  intptr_t d = (intptr_t)cur_case_values->data[ia] - (intptr_t)cur_case_values->data[ib];
  return d > 0 ? 1 : d < 0 ? -1 : 0;
}

static void gen_switch_cond_recur(Stmt *stmt, VReg *reg, const int *order, int len) {
  Vector *case_values = stmt->switch_.case_values;
  Expr *value = stmt->switch_.value;
  size_t size = type_size(value->type);

  if (len <= 2) {
    for (int i = 0; i < len; ++i) {
      BB *nextbb = bb_split(curbb);
      int index = order[i];
      intptr_t x = (intptr_t)case_values->data[index];
      VReg *num = new_ir_imm(x, value->type);
      new_ir_cmp(reg, num, size);
      new_ir_jmp(COND_EQ, cur_case_bbs->data[index]);
      set_curbb(nextbb);
    }
    new_ir_jmp(COND_ANY, cur_case_bbs->data[cur_case_bbs->len - 2]);  // Jump to default.
  } else {
    BB *bbne = bb_split(curbb);
    int m = len >> 1;
    int index = order[m];
    intptr_t x = (intptr_t)case_values->data[index];
    VReg *num = new_ir_imm(x, value->type);
    new_ir_cmp(reg, num, size);
    new_ir_jmp(COND_EQ, cur_case_bbs->data[index]);
    set_curbb(bbne);

    BB *bblt = bb_split(curbb);
    BB *bbgt = bb_split(bblt);
    new_ir_jmp(COND_GT, bbgt);
    set_curbb(bblt);
    gen_switch_cond_recur(stmt, reg, order, m);
    set_curbb(bbgt);
    gen_switch_cond_recur(stmt, reg, order + (m + 1), len - (m + 1));
  }
}

static void gen_switch_cond(Stmt *stmt) {
  Vector *case_values = stmt->switch_.case_values;
  int len = case_values->len;

  Expr *value = stmt->switch_.value;
  VReg *reg = gen_expr(value);

  // Sort cases in increasing order.
#if defined(USE_ALLOCA)
  int *order = alloca(sizeof(int) * len);
#else
  int *order = malloc(sizeof(int) * len);
#endif
  for (int i = 0; i < len; ++i)
    order[i] = i;
  myqsort(order, len, sizeof(int), compare_cases);

  gen_switch_cond_recur(stmt, reg, order, len);
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

  gen(stmt->switch_.body);

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
  intptr_t x = value->num.ival;
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
  gen(stmt->while_.body);

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
  gen(stmt->while_.body);

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
  gen(stmt->for_.body);

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
  assert(curdefun->label_map != NULL);
  BB *bb = map_get(curdefun->label_map, stmt->goto_.label->ident);
  assert(bb != NULL);
  new_ir_jmp(COND_ANY, bb);
}

static void gen_label(Stmt *stmt) {
  assert(curdefun->label_map != NULL);
  BB *bb = map_get(curdefun->label_map, stmt->token->ident);
  assert(bb != NULL);
  bb_insert(curbb, bb);
  set_curbb(bb);
  gen(stmt->label.stmt);
}

static void gen_clear_local_var(const VarInfo *varinfo) {
  // Fill with zeros regardless of variable type.
  VReg *reg = new_ir_bofs(varinfo->reg);
  new_ir_clear(reg, type_size(varinfo->type));
}

static void gen_vardecl(Stmt *stmt) {
  if (curdefun != NULL) {
    Vector *decls = stmt->vardecl.decls;
    for (int i = 0; i < decls->len; ++i) {
      VarDecl *decl = decls->data[i];
      if (decl->init == NULL)
        continue;
      Scope *scope = curscope;
      VarInfo *varinfo = scope_find(&scope, decl->ident->ident);
      if (varinfo == NULL || (varinfo->flag & (VF_STATIC | VF_EXTERN)) ||
          !(varinfo->type->kind == TY_STRUCT ||
            varinfo->type->kind == TY_ARRAY))
        continue;
      gen_clear_local_var(varinfo);
    }
  }
  gen_stmts(stmt->vardecl.inits);
}

static void gen_expr_stmt(Expr *expr) {
  gen_expr(expr);
}

static void gen_toplevel(Stmt *stmt) {
  gen_stmts(stmt->toplevel.stmts);
}

void gen(Stmt *stmt) {
  if (stmt == NULL)
    return;

  switch (stmt->kind) {
  case ST_EXPR:  gen_expr_stmt(stmt->expr); break;
  case ST_DEFUN:
    gen_defun(stmt->defun);
    break;
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
  case ST_VARDECL:  gen_vardecl(stmt); break;
  case ST_ASM:  gen_asm(stmt); break;
  case ST_TOPLEVEL:
    gen_toplevel(stmt);
    break;

  default:
    error("Unhandled stmt: %d", stmt->kind);
    break;
  }
}

////////////////////////////////////////////////

static void emit_defun(Defun *defun) {
  Function *func = defun->func;
  if (func->top_scope == NULL)  // Prototype definition
    return;

  assert(stackpos == 8);

  _TEXT();

  bool global = true;
  const VarInfo *varinfo = find_global(func->name);
  if (varinfo != NULL) {
    global = (varinfo->flag & VF_STATIC) == 0;
  }

  if (global) {
    const char *gl = MANGLE(func->name);
    _GLOBL(gl);
    EMIT_LABEL(gl);
  } else {
    emit_comment("%s: static func", func->name);
    EMIT_LABEL(func->name);
  }

  bool no_stmt = true;
  if (defun->stmts != NULL) {
    for (int i = 0; i < defun->stmts->len; ++i) {
      Stmt *stmt = defun->stmts->data[i];
      if (stmt == NULL)
        continue;
      if (!is_asm(stmt)) {
        no_stmt = false;
        break;
      }
    }
  }

  // Prologue
  // Allocate variable bufer.
  if (!no_stmt) {
    PUSH(RBP); PUSH_STACK_POS();
    MOV(RSP, RBP);
    if (func->frame_size > 0) {
      SUB(IM(func->frame_size), RSP);
      stackpos += func->frame_size;
    }

    put_args_to_stack(defun);

    // Callee save.
    push_callee_save_regs(func);
  }

  emit_bb_irs(func->bbcon);

  // Epilogue
  if (!no_stmt) {
    pop_callee_save_regs(func);

    MOV(RBP, RSP);
    stackpos -= func->frame_size;
    POP(RBP); POP_STACK_POS();
  }

  RET();
  emit_comment(NULL);
  assert(stackpos == 8);
}

static void emit_data(void) {
  _RODATA();
  put_rodata();

  emit_comment(NULL);
  _DATA();
  put_rwdata();

  emit_comment(NULL);
  emit_comment("bss");
  put_bss();
}

void emit_code(Stmt *stmt) {
  if (stmt == NULL)
    return;

  switch (stmt->kind) {
  case ST_DEFUN:
    emit_defun(stmt->defun);
    break;
  case ST_TOPLEVEL:
    for (int i = 0, len = stmt->toplevel.stmts->len; i < len; ++i) {
      Stmt *child = stmt->toplevel.stmts->data[i];
      if (child == NULL)
        continue;
      emit_code(child);
    }
    emit_data();
    break;
  case ST_VARDECL:
    break;

  default:
    error("Unhandled stmt in emit_code: %d", stmt->kind);
    break;
  }
}
