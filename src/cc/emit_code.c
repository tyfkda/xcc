#include "emit_code.h"

#include <assert.h>
#include <stdlib.h>
#include <string.h>

#include "ast.h"
#include "codegen.h"
#include "ir.h"
#include "regalloc.h"
#include "table.h"
#include "type.h"
#include "util.h"
#include "var.h"
#include "x86_64.h"

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
  for (s = p = str; p < end; ++p) {
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

static void construct_initial_value(unsigned char *buf, const Type *type, Initializer *init) {
  assert(init == NULL || init->kind != IK_DOT);

  switch (type->kind) {
  case TY_NUM:
    {
      intptr_t v = 0;
      if (init != NULL) {
        assert(init->kind == IK_SINGLE);
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
      assert(init->kind == IK_SINGLE);
      Expr *value = init->single;
      while (value->kind == EX_CAST)
        value = value->unary.sub;
      if (value->kind == EX_REF || value->kind == EX_VARIABLE) {
        if (value->kind == EX_REF)
          value = value->unary.sub;
        // TODO: Type check.

        assert(value->kind == EX_VARIABLE);
        assert(value->variable.scope == NULL);

        const Name *name = value->variable.name;
        const VarInfo *varinfo = find_global(name);
        assert(varinfo != NULL);
        const char *label = fmt_name(name);
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
    if (init == NULL || init->kind == IK_MULTI) {
      const Type *elem_type = type->pa.ptrof;
      size_t elem_size = type_size(elem_type);
      if (init != NULL) {
        Vector *init_array = init->multi;
        size_t index = 0;
        size_t len = init_array->len;
        for (size_t i = 0; i < len; ++i, ++index) {
          Initializer *init_elem = init_array->data[i];
          if (init_elem->kind == IK_ARR) {
            size_t next = init_elem->arr.index->num.ival;
            for (size_t j = index; j < next; ++j)
              construct_initial_value(buf + (j * elem_size), elem_type, NULL);
            index = next;
            init_elem = init_elem->arr.value;
          }
          construct_initial_value(buf + (index * elem_size), elem_type, init_elem);
        }
        // Padding
        for (size_t i = index, n = type->pa.length; i < n; ++i)
          construct_initial_value(buf + (i * elem_size), elem_type, NULL);
      }
    } else {
      if (init->kind == IK_SINGLE &&
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
      assert(init == NULL || init->kind == IK_MULTI);

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
          construct_initial_value(buf + member->struct_.offset, member->type, mem_init);
          ++count;
          offset = ALIGN(offset, align);
          offset += type_size(member->type);
        }
      }
      if (sinfo->is_union && count <= 0) {
        const VarInfo* member = sinfo->members->data[0];
        construct_initial_value(buf + member->struct_.offset, member->type, NULL);
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

static void put_data(const Name *name, const VarInfo *varinfo) {
  size_t size = type_size(varinfo->type);
  unsigned char *buf = calloc(size, 1);
  if (buf == NULL)
    error("Out of memory");

  emit_align(align_size(varinfo->type));
  const char *label = fmt_name(name);
  if ((varinfo->flag & VF_STATIC) == 0) {  // global
    label = MANGLE(label);
    _GLOBL(label);
  }
  EMIT_LABEL(label);

  construct_initial_value(buf, varinfo->type, varinfo->global.init);

  free(buf);
}

// Put RoData into code.
static void put_rodata(void) {
  for (int i = 0, len = gvar_names->len; i < len; ++i) {
    const Name *name = gvar_names->data[i];
    const VarInfo *varinfo = find_global(name);
    if (varinfo->type->kind == TY_FUNC ||
        (varinfo->flag & VF_EXTERN) != 0 || varinfo->global.init == NULL ||
        (varinfo->flag & VF_CONST) == 0)
      continue;

    put_data(name, varinfo);
  }
}

// Put global with initial value (RwData).
static void put_rwdata(void) {
  for (int i = 0, len = gvar_names->len; i < len; ++i) {
    const Name *name = gvar_names->data[i];
    const VarInfo *varinfo = find_global(name);
    if (varinfo->type->kind == TY_FUNC ||
        (varinfo->flag & VF_EXTERN) != 0 || varinfo->global.init == NULL ||
        (varinfo->flag & VF_CONST) != 0)
      continue;

    put_data(name, varinfo);
  }
}

// Put global without initial value (bss).
static void put_bss(void) {
  for (int i = 0, len = gvar_names->len; i < len; ++i) {
    const Name *name = gvar_names->data[i];
    const VarInfo *varinfo = find_global(name);
    if (varinfo->type->kind == TY_FUNC || varinfo->global.init != NULL ||
        (varinfo->flag & VF_EXTERN) != 0)
      continue;

    emit_align(align_size(varinfo->type));
    size_t size = type_size(varinfo->type);
    if (size < 1)
      size = 1;
    const char *label = fmt_name(name);
    if ((varinfo->flag & VF_STATIC) == 0) {  // global
      label = MANGLE(label);
      _GLOBL(label);
    }
    _COMM(label, NUM(size));
  }
}

////////////////////////////////////////////////

static bool is_asm(Stmt *stmt) {
  return stmt->kind == ST_ASM;
}

static void put_args_to_stack(Defun *defun) {
  static const char *kReg8s[] = {DIL, SIL, DL, CL, R8B, R9B};
  static const char *kReg16s[] = {DI, SI, DX, CX, R8W, R9W};
  static const char *kReg32s[] = {EDI, ESI, EDX, ECX, R8D, R9D};
  static const char *kReg64s[] = {RDI, RSI, RDX, RCX, R8, R9};
  static const char **kRegTable[] = {NULL, kReg8s, kReg16s, NULL, kReg32s, NULL, NULL, NULL, kReg64s};

  // Store arguments into local frame.
  Vector *params = defun->func->type->func.params;
  if (params == NULL)
    return;

  int len = params->len;
  int n = len;
  if (defun->func->type->func.vaargs && n < MAX_REG_ARGS)
    n = MAX_REG_ARGS;
  int arg_index = 0;
  for (int i = 0; i < n; ++i) {
    const Type *type;
    int offset;
    if (i < len) {
      const VarInfo *varinfo = params->data[i];
      type = varinfo->type;
      offset = varinfo->reg->offset;
    } else {  // vaargs
      type = &tyLong;
      offset = (i - MAX_REG_ARGS) * WORD_SIZE;
    }

    if (is_stack_param(type))
      continue;

    switch (type->kind) {
    case TY_NUM:
    case TY_PTR:
      break;
    default: assert(false); break;
    }

    int size = type_size(type);
    assert(size < (int)(sizeof(kRegTable) / sizeof(*kRegTable)) &&
           kRegTable[size] != NULL);
    MOV(kRegTable[size][arg_index], OFFSET_INDIRECT(offset, RBP, NULL, 1));

    ++arg_index;
    if (arg_index >= MAX_REG_ARGS)
      break;
  }
}

static void emit_defun(Defun *defun) {
  Function *func = defun->func;
  if (func->scopes == NULL)  // Prototype definition
    return;

  assert(stackpos == 8);

  _TEXT();

  bool global = true;
  const VarInfo *varinfo = find_global(func->name);
  if (varinfo != NULL) {
    global = (varinfo->flag & VF_STATIC) == 0;
  }

  const char *label = fmt_name(func->name);
  if (global) {
    const char *gl = MANGLE(label);
    _GLOBL(gl);
    EMIT_LABEL(gl);
  } else {
    emit_comment("%.*s: static func", func->name->bytes, func->name->chars);
    EMIT_LABEL(label);
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
    if (func->ra->frame_size > 0) {
      SUB(IM(func->ra->frame_size), RSP);
      stackpos += func->ra->frame_size;
    }

    put_args_to_stack(defun);

    // Callee save.
    push_callee_save_regs(func->ra->used_reg_bits);
  }

  emit_bb_irs(func->bbcon);

  // Epilogue
  if (!no_stmt) {
    pop_callee_save_regs(func->ra->used_reg_bits);

    MOV(RBP, RSP);
    stackpos -= func->ra->frame_size;
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

void emit_code(Vector *toplevel) {
  for (int i = 0, len = toplevel->len; i < len; ++i) {
    Declaration *decl = toplevel->data[i];
    if (decl == NULL)
      continue;

    switch (decl->kind) {
    case DCL_DEFUN:
      emit_defun(decl->defun);
      break;
    case DCL_VARDECL:
      break;

    default:
      error("Unhandled decl in emit_code: %d", decl->kind);
      break;
    }
  }
  emit_data();
}
