#include "codegen.h"

#include <assert.h>
#include <inttypes.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

#include "expr.h"
#include "ir.h"
#include "lexer.h"
#include "parser.h"
#include "sema.h"
#include "type.h"
#include "util.h"
#include "var.h"
#include "x86_64.h"

const int FRAME_ALIGN = 8;
const int STACK_PARAM_BASE_OFFSET = (2 - MAX_REG_ARGS) * 8;

static void gen_expr_stmt(Expr *expr);

void set_curbb(BB *bb) {
  assert(curfunc != NULL);
  curbb = bb;
  vec_push(curfunc->bbcon->bbs, bb);
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
if (sinfo == NULL) {
}
  assert(sinfo != NULL);
  if (sinfo->size >= 0)
    return;

  size_t size = 0;
  size_t maxsize = 0;
  int max_align = 1;

  for (int i = 0, len = sinfo->members->len; i < len; ++i) {
    VarInfo *varinfo = (VarInfo*)sinfo->members->data[i];
    size_t sz = type_size(varinfo->type);
    int align = align_size(varinfo->type);
    size = ALIGN(size, align);
    varinfo->offset = size;
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

static char *append_str(const char *str, const char *add, size_t size) {
  if (size == 0)
    size = strlen(add);
  size_t len = str == NULL ? 0 : strlen(str);
  char *newstr = malloc(len + size + 1);
  if (str != NULL)
    memcpy(newstr, str, len);
  memcpy(newstr + len, add, size);
  newstr[len + size] = '\0';
  return newstr;
}

static char *escape_string(const char *str, size_t size) {
  const char *s, *p;
  char *escaped = NULL;
  for (s = p = str; ; ++p) {
    bool is_end = (size_t)(p - str) >= size;
    const char *e = NULL;
    if (!is_end && (e = escape(*p)) == NULL)
      continue;

    if (p - s > 0) {
      char *newstr1 = append_str(escaped, s, p - s);
      free(escaped);
      escaped = newstr1;
    }
    if (is_end)
      return escaped;
    char *newstr2 = append_str(escaped, e, 0);
    free(escaped);
    escaped = newstr2;
    s = p + 1;
  }
}

void construct_initial_value(unsigned char *buf, const Type *type, Initializer *init, Vector **pptrinits) {
  assert(init == NULL || init->kind != vDot);

  emit_align(align_size(type));

  switch (type->kind) {
  case TY_NUM:
    {
      intptr_t v = 0;
      if (init != NULL) {
        assert(init->kind == vSingle);
        Expr *value = init->single;
        if (!(is_const(value) && is_number(value->valType->kind)))
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

        _QUAD(value->varref.ident);
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
        assert((size_t)len <= type->pa.length);
      }
    } else {
      if (init->kind == vSingle &&
          is_char_type(type->pa.ptrof) && init->single->kind == EX_STR) {
        int src_size = init->single->str.size;
        size_t size = type_size(type);
        assert(size >= (size_t)src_size);
        memcpy(buf, init->single->str.buf, src_size);

        UNUSED(size);
        _ASCII(fmt("\"%s\"", escape_string((char*)buf, size)));
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
      for (int i = 0, n = sinfo->members->len; i < n; ++i) {
        VarInfo* varinfo = sinfo->members->data[i];
        Initializer *mem_init;
        if (init == NULL) {
          if (sinfo->is_union)
            continue;
          mem_init = NULL;
        } else {
          mem_init = init->multi->data[i];
        }
        if (mem_init != NULL || !sinfo->is_union) {
          construct_initial_value(buf + varinfo->offset, varinfo->type, mem_init, pptrinits);
          ++count;
        }
      }
      if (sinfo->is_union && count <= 0) {
        VarInfo* varinfo = sinfo->members->data[0];
        construct_initial_value(buf + varinfo->offset, varinfo->type, NULL, pptrinits);
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
  if ((varinfo->flag & VF_STATIC) == 0)  // global
    _GLOBL(label);
  EMIT_LABEL(label);

  Vector *ptrinits = NULL;  // <[ptr, label]>
  construct_initial_value(buf, varinfo->type, varinfo->global.init, &ptrinits);
  //emit_section_data(sec, buf, size);

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
    const char *name = (const char *)gvar_map->keys->data[i];
    const VarInfo *varinfo = (const VarInfo*)gvar_map->vals->data[i];
    if (varinfo->type->kind == TY_FUNC || varinfo->global.init != NULL ||
        (varinfo->flag & VF_EXTERN) != 0)
      continue;

    emit_align(align_size(varinfo->type));
    size_t size = type_size(varinfo->type);
    if (size < 1)
      size = 1;
    _COMM(name, NUM(size));
  }
}

static void gen_data(void) {
  _SECTION(".rodata");
  put_rodata();

  emit_comment(NULL);
  _DATA();
  put_rwdata();

  emit_comment(NULL);
  emit_comment("bss");
  put_bss();
}

//

static BB *s_break_bb;
static BB *s_continue_bb;
int stackpos;

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

static int arrange_variadic_func_params(Scope *scope) {
  // Arrange parameters increasing order in stack,
  // and each parameter occupies sizeof(intptr_t).
  for (int i = 0; i < scope->vars->len; ++i) {
    VarInfo *varinfo = (VarInfo*)scope->vars->data[i];
    VReg *vreg = add_new_reg();
    vreg_spill(vreg);
    vreg->type = varinfo->type;
    vreg->offset = (i - MAX_REG_ARGS) * WORD_SIZE;
    varinfo->reg = vreg;
  }
  return MAX_REG_ARGS * WORD_SIZE;
}

static void alloc_variable_registers(Defun *defun) {
  for (int i = 0; i < defun->all_scopes->len; ++i) {
    Scope *scope = (Scope*)defun->all_scopes->data[i];
    if (scope->vars != NULL) {
      if (i == 0 && defun->type->func.vaargs) {  // Variadic function parameters.
        // Special arrangement for va_list.
        arrange_variadic_func_params(scope);
      } else {
        for (int j = 0; j < scope->vars->len; ++j) {
          VarInfo *varinfo = (VarInfo*)scope->vars->data[j];
          if (varinfo->flag & (VF_STATIC | VF_EXTERN))
            continue;  // Static variable is not allocated on stack.

          VReg *vreg = add_new_reg();
          bool spill = false;
          switch (varinfo->type->kind) {
          case TY_ARRAY:
          case TY_STRUCT:
            // Make non-primitive variable spilled.
            spill = true;
            break;
          default:
            break;
          }
          if (i == 0 && j >= MAX_REG_ARGS) {  // Function argument passed through the stack.
            spill = true;
            vreg->offset = (j - MAX_REG_ARGS + 2) * WORD_SIZE;
          }

          if (spill) {
            vreg_spill(vreg);
            vreg->type = varinfo->type;
          }
          varinfo->reg = vreg;
        }
      }
    }
  }
}

static void put_args_to_stack(Defun *defun) {
  static const char *kReg8s[] = {DIL, SIL, DL, CL, R8B, R9B};
  static const char *kReg16s[] = {DI, SI, DX, CX, R8W, R9W};
  static const char *kReg32s[] = {EDI, ESI, EDX, ECX, R8D, R9D};
  static const char *kReg64s[] = {RDI, RSI, RDX, RCX, R8, R9};

  // Store arguments into local frame.
  Vector *params = defun->params;
  int len = params != NULL ? params->len : 0;
  int n = defun->type->func.vaargs ? MAX_REG_ARGS : len;
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
      case NUM_INT:
      case NUM_ENUM:
        size = 4;
        break;
      case NUM_LONG:  size = 8; break;
      default: break;
      }
      break;
    case TY_PTR:  size = 8; break;
    default:
      break;
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

static bool is_funcall(Expr *expr, const char *funcname) {
  if (expr->kind == EX_FUNCALL) {
    Expr *func = expr->funcall.func;
    if (func->kind == EX_VARREF &&
        strcmp(func->varref.ident, funcname) == 0)
      return true;
  }
  return false;
}

static bool is_asm(Node *node) {
  return node->kind == ND_EXPR &&
    is_funcall(node->expr, "__asm");
}

static void out_asm(Node *node) {
  Expr *funcall = node->expr;
  Vector *args = funcall->funcall.args;
  int len = args->len;

  Expr *arg0;
  if (len != 1 || (arg0 = (Expr*)args->data[0])->kind != EX_STR)
    error("__asm takes string at 1st argument");
  else
    EMIT_ASM0(arg0->str.buf);
}

static void gen_nodes(Vector *nodes) {
  if (nodes == NULL)
    return;

  for (int i = 0, len = nodes->len; i < len; ++i) {
    Node *node = nodes->data[i];
    if (node == NULL)
      continue;
    if (is_asm(node))
      out_asm(node);
    else
      gen(node);
  }
}

static void gen_defun(Node *node) {
  assert(stackpos == 0);
  Defun *defun = node->defun;
  if (defun->top_scope == NULL)  // Prototype definition
    return;

  curfunc = defun;
  defun->bbcon = new_func_blocks();
  set_curbb(new_bb());
  init_reg_alloc();

  bool global = true;
  VarInfo *varinfo = find_global(defun->name);
  if (varinfo != NULL) {
    global = (varinfo->flag & VF_STATIC) == 0;
  }

  if (global)
    _GLOBL(defun->name);
  else
    emit_comment("%s: static func", defun->name);

  EMIT_LABEL(defun->name);

  // Allocate labels for goto.
  if (defun->label_map != NULL) {
    Map *label_map = defun->label_map;
    for (int i = 0, n = map_count(label_map); i < n; ++i)
      label_map->vals->data[i] = new_bb();
  }

  //size_t frame_size = arrange_scope_vars(defun);
  //UNUSED(frame_size);
  alloc_variable_registers(defun);

  bool no_stmt = true;
  if (defun->stmts != NULL) {
    for (int i = 0; i < defun->stmts->len; ++i) {
      Node *node = defun->stmts->data[i];
      if (node == NULL)
        continue;
      if (!is_asm(node)) {
        no_stmt = false;
        break;
      }
    }
  }

  curscope = defun->top_scope;
  defun->ret_bb = bb_split(curbb);

  // Statements
  gen_nodes(defun->stmts);

  set_curbb(defun->ret_bb);
  curbb = NULL;

  size_t frame_size = alloc_real_registers(defun);

  remove_unnecessary_bb(defun->bbcon);

  // Prologue
  // Allocate variable bufer.
  if (!no_stmt) {
    PUSH(RBP); PUSH_STACK_POS();
    MOV(RSP, RBP);
    if (frame_size > 0) {
      SUB(IM(frame_size), RSP);
      stackpos += frame_size;
    }

    put_args_to_stack(defun);

    // Callee save.
    push_callee_save_regs(defun);
  }

  emit_bb_irs(defun->bbcon);

  // Epilogue
  if (!no_stmt) {
    pop_callee_save_regs(defun);

    MOV(RBP, RSP);
    stackpos -= frame_size;
    POP(RBP); POP_STACK_POS();
  }

  RET();
  emit_comment(NULL);
  curfunc = NULL;
  curscope = NULL;
  assert(stackpos == 0);
}

static void gen_block(Node *node) {
  if (node->block.nodes != NULL) {
    if (node->block.scope != NULL) {
      assert(curscope == node->block.scope->parent);
      curscope = node->block.scope;
    }
    gen_nodes(node->block.nodes);
    if (node->block.scope != NULL)
      curscope = curscope->parent;
  }
}

static void gen_return(Node *node) {
  BB *bb = bb_split(curbb);
  if (node->return_.val != NULL) {
    VReg *reg = gen_expr(node->return_.val);
    new_ir_result(reg, type_size(node->return_.val->valType));
  }
  assert(curfunc != NULL);
  new_ir_jmp(COND_ANY, curfunc->ret_bb);
  set_curbb(bb);
}

static void gen_if(Node *node) {
  BB *tbb = bb_split(curbb);
  BB *fbb = bb_split(tbb);
  gen_cond_jmp(node->if_.cond, false, fbb);
  set_curbb(tbb);
  gen(node->if_.tblock);
  if (node->if_.fblock == NULL) {
    set_curbb(fbb);
  } else {
    BB *nbb = bb_split(fbb);
    new_ir_jmp(COND_ANY, nbb);
    set_curbb(fbb);
    gen(node->if_.fblock);
    set_curbb(nbb);
  }
}

static Vector *cur_case_values;
static Vector *cur_case_bbs;

static void gen_switch(Node *node) {
  BB *pbb = curbb;

  Vector *save_case_values = cur_case_values;
  Vector *save_case_bbs = cur_case_bbs;
  BB *save_break;
  BB *break_bb = push_break_bb(pbb, &save_break);

  Vector *bbs = new_vector();
  Vector *case_values = node->switch_.case_values;
  int len = case_values->len;
  for (int i = 0; i < len; ++i) {
    BB *bb = bb_split(pbb);
    vec_push(bbs, bb);
    pbb = bb;
  }
  vec_push(bbs, new_bb());  // len+0: Extra label for default.
  vec_push(bbs, break_bb);  // len+1: Extra label for break.

  Expr *value = node->switch_.value;
  VReg *reg = gen_expr(value);

  int size = type_size(value->valType);
  for (int i = 0; i < len; ++i) {
    BB *nextbb = bb_split(curbb);
    intptr_t x = (intptr_t)case_values->data[i];
    VReg *num = new_ir_imm(x, size);
    new_ir_cmp(reg, num, size);
    new_ir_jmp(COND_EQ, bbs->data[i]);
    set_curbb(nextbb);
  }
  new_ir_jmp(COND_ANY, bbs->data[len]);  // Jump to default.
  set_curbb(bb_split(curbb));

  // No bb setting.

  cur_case_values = case_values;
  cur_case_bbs = bbs;

  gen(node->switch_.body);

  if (!node->switch_.has_default) {
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

static void gen_case(Node *node) {
  assert(cur_case_values != NULL);
  assert(cur_case_bbs != NULL);
  Expr *valnode = node->case_.value;
  assert(is_const(valnode));
  intptr_t x = valnode->num.ival;
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

static void gen_while(Node *node) {
  BB *loop_bb = bb_split(curbb);

  BB *save_break, *save_cont;
  BB *cond_bb = push_continue_bb(loop_bb, &save_cont);
  BB *next_bb = push_break_bb(cond_bb, &save_break);

  new_ir_jmp(COND_ANY, cond_bb);

  set_curbb(loop_bb);
  gen(node->while_.body);

  set_curbb(cond_bb);
  gen_cond_jmp(node->while_.cond, true, loop_bb);

  set_curbb(next_bb);
  pop_continue_bb(save_cont);
  pop_break_bb(save_break);
}

static void gen_do_while(Node *node) {
  BB *loop_bb = bb_split(curbb);

  BB *save_break, *save_cont;
  BB *cond_bb = push_continue_bb(loop_bb, &save_cont);
  BB *next_bb = push_break_bb(cond_bb, &save_break);

  set_curbb(loop_bb);
  gen(node->while_.body);

  set_curbb(cond_bb);
  gen_cond_jmp(node->while_.cond, true, loop_bb);

  set_curbb(next_bb);
  pop_continue_bb(save_cont);
  pop_break_bb(save_break);
}

static void gen_for(Node *node) {
  BB *cond_bb = bb_split(curbb);
  BB *body_bb = bb_split(cond_bb);

  BB *save_break, *save_cont;
  BB *continue_bb = push_continue_bb(body_bb, &save_cont);
  BB *next_bb = push_break_bb(continue_bb, &save_break);

  if (node->for_.pre != NULL)
    gen_expr_stmt(node->for_.pre);

  set_curbb(cond_bb);

  if (node->for_.cond != NULL)
    gen_cond_jmp(node->for_.cond, false, next_bb);

  set_curbb(body_bb);
  gen(node->for_.body);

  set_curbb(continue_bb);
  if (node->for_.post != NULL)
    gen_expr_stmt(node->for_.post);
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

static void gen_goto(Node *node) {
  assert(curfunc->label_map != NULL);
  BB *bb = map_get(curfunc->label_map, node->goto_.ident);
  assert(bb != NULL);
  new_ir_jmp(COND_ANY, bb);
}

static void gen_label(Node *node) {
  assert(curfunc->label_map != NULL);
  BB *bb = map_get(curfunc->label_map, node->label.name);
  assert(bb != NULL);
  bb_insert(curbb, bb);
  set_curbb(bb);
  gen(node->label.stmt);
}

static void gen_clear_local_var(const VarInfo *varinfo) {
  // Fill with zeros regardless of variable type.
  VReg *reg = new_ir_bofs(varinfo->reg);
  new_ir_clear(reg, type_size(varinfo->type));
}

static void gen_vardecl(Node *node) {
  if (curfunc != NULL) {
    Vector *decls = node->vardecl.decls;
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
  gen_nodes(node->vardecl.inits);
}

static void gen_expr_stmt(Expr *expr) {
  gen_expr(expr);
}

static void gen_toplevel(Node *node) {
  _TEXT();
  gen_nodes(node->toplevel.nodes);
}

void gen(Node *node) {
  if (node == NULL)
    return;

  switch (node->kind) {
  case ND_EXPR:  gen_expr_stmt(node->expr); break;
  case ND_DEFUN:  gen_defun(node); break;
  case ND_RETURN:  gen_return(node); break;
  case ND_BLOCK:  gen_block(node); break;
  case ND_IF:  gen_if(node); break;
  case ND_SWITCH:  gen_switch(node); break;
  case ND_CASE:  gen_case(node); break;
  case ND_DEFAULT:  gen_default(); break;
  case ND_WHILE:  gen_while(node); break;
  case ND_DO_WHILE:  gen_do_while(node); break;
  case ND_FOR:  gen_for(node); break;
  case ND_BREAK:  gen_break(); break;
  case ND_CONTINUE:  gen_continue(); break;
  case ND_GOTO:  gen_goto(node); break;
  case ND_LABEL:  gen_label(node); break;
  case ND_VARDECL:  gen_vardecl(node); break;
  case ND_TOPLEVEL:
    gen_toplevel(node);
    gen_data();
    break;

  default:
    error("Unhandled node: %d", node->kind);
    break;
  }
}
