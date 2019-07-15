#if defined(__XV6)
#define NO_ASM_OUTPUT
#endif

#include "codegen.h"

#include <assert.h>
#include <inttypes.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

#include "expr.h"
#include "type.h"
#include "util.h"
#include "var.h"

#if defined(NO_ASM_OUTPUT)
#define ADD_ASM(...)  // ignore
#define add_asm(...)  // ignore
#endif

#define UNUSED(x)  ((void)(x))

const int FRAME_ALIGN = 8;
const int MAX_ARGS = 6;
const int WORD_SIZE = /*sizeof(void*)*/ 8;

#define CURIP(ofs)  (instruction_pointer + ofs)
#include "x86_64.h"

#define ALIGN_SECTION_SIZE(sec, align_)  do { int align = (int)(align_); add_asm_align(align); align_section_size(sec, align); } while (0)

size_t type_size(const Type *type) {
  switch (type->type) {
  case TY_VOID:
    return 1;  // ?
  case TY_NUM:
    switch (type->u.numtype) {
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
    assert(type->u.pa.length != (size_t)-1);
    return type_size(type->u.pa.ptrof) * type->u.pa.length;
  case TY_STRUCT:
  case TY_UNION:
    calc_struct_size(type->u.struct_.info, type->type == TY_UNION);
    return type->u.struct_.info->size;
  default:
    assert(false);
    return 1;
  }
}

static int align_size(const Type *type) {
  switch (type->type) {
  case TY_VOID:
    return 1;  // ?
  case TY_NUM:
    switch (type->u.numtype) {
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
    return align_size(type->u.pa.ptrof);
  case TY_STRUCT:
  case TY_UNION:
    calc_struct_size(type->u.struct_.info, type->type == TY_UNION);
    return type->u.struct_.info->align;
  default:
    assert(false);
    return 1;
  }
}

void calc_struct_size(StructInfo *sinfo, bool is_union) {
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
    if (!is_union)
      size += sz;
    else
      if (maxsize < sz)
        maxsize = sz;
    if (max_align < align)
      max_align = align;
  }

  if (is_union)
    size = maxsize;
  size = ALIGN(size, max_align);
  sinfo->size = size;
  sinfo->align = max_align;
}

static Map *label_map;  // <uintptr_t adr>

enum LocType {
  LOC_REL8,
  LOC_REL32,
  LOC_ABS64,
};

typedef struct {
  enum LocType type;
  enum SectionType section;
  uintptr_t adr;
  const char *label;
  union {
    struct {
      uintptr_t base;
    } rel;
  };
} LocInfo;

typedef struct {
  uintptr_t start;
  unsigned char* buf;
  size_t size;
} Section;

static Section sections[2];
static size_t instruction_pointer;
static FILE *asm_fp;

static void add_section_data(enum SectionType secno, const unsigned char* data, size_t bytes) {
  Section *sec = &sections[secno];
  size_t size = sec->size;
  size_t newsize = size + bytes;
  unsigned char *buf = realloc(sec->buf, newsize);
  if (buf == NULL)
    error("not enough memory");
  memcpy(buf + size, data, bytes);
  sec->buf = buf;
  sec->size = newsize;
  instruction_pointer += bytes;
}

void add_code(const unsigned char* buf, size_t bytes) {
  add_section_data(SEC_CODE, buf, bytes);
}

#if !defined(NO_ASM_OUTPUT)
void add_asm(const char *fmt, ...) {
  if (asm_fp == NULL)
    return;

  va_list ap;
  va_start(ap, fmt);
  fprintf(asm_fp, "\t");
  vfprintf(asm_fp, fmt, ap);
  fprintf(asm_fp, "\n");
  va_end(ap);
}
#endif

void add_asm_label(const char *label) {
  if (asm_fp == NULL)
    return;

  fprintf(asm_fp, "%s:\n", label);
}

static void add_asm_comment(const char *comment, ...) {
  if (asm_fp == NULL)
    return;

  if (comment == NULL) {
    fprintf(asm_fp, "\n");
    return;
  }

  va_list ap;
  va_start(ap, comment);
  fprintf(asm_fp, "// ");
  vfprintf(asm_fp, comment, ap);
  fprintf(asm_fp, "\n");
  va_end(ap);
}

static void add_asm_align(int align) {
  if ((align) > 1)
    add_asm(".align %d", (int)(align));
}

// Put label at the current.
void add_label(const char *label) {
  map_put(label_map, label, (void*)CURIP(0));
}

void add_bss(size_t size) {
  //codesize += size;
  instruction_pointer += size;
}

void align_section_size(int sec, int align) {
  size_t size = sections[sec].size;
  size_t aligned_size = ALIGN(size, align);
  size_t add = aligned_size - size;
  if (add <= 0)
    return;

  void* zero = calloc(add, 1);
  add_section_data(sec, zero, add);
  free(zero);

  assert(sections[sec].size == aligned_size);
}

uintptr_t label_adr(const char *label) {
  void *adr = map_get(label_map, label);
  return adr != NULL ? (uintptr_t)adr : (uintptr_t)-1;
}

static Vector *loc_vector;

static LocInfo *new_loc(enum LocType type, enum SectionType section, uintptr_t adr, const char *label) {
  LocInfo *loc = malloc(sizeof(*loc));
  loc->type = type;
  loc->section = section;
  loc->adr = adr;
  loc->label = label;
  vec_push(loc_vector, loc);
  return loc;
}

void add_loc_rel8(const char *label, int ofs, int baseofs) {
  uintptr_t adr = instruction_pointer + ofs;
  LocInfo *loc = new_loc(LOC_REL8, SEC_CODE, adr, label);
  loc->rel.base = CURIP(baseofs);
}

void add_loc_rel32(const char *label, int ofs, int baseofs) {
  uintptr_t adr = instruction_pointer + ofs;
  LocInfo *loc = new_loc(LOC_REL32, SEC_CODE, adr, label);
  loc->rel.base = CURIP(baseofs);
}

void add_loc_abs64(enum SectionType section, const char *label, uintptr_t pos) {
  new_loc(LOC_ABS64, section, pos, label);
}

#if !defined(NO_ASM_OUTPUT)
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
#endif

void construct_initial_value(unsigned char *buf, const Type *type, Initializer *init, Vector **pptrinits) {
  add_asm_align(align_size(type));

  switch (type->type) {
  case TY_NUM:
    {
      intptr_t v = 0;
      if (init != NULL) {
        assert(init->type == vSingle);
        Expr *value = init->u.single;
        if (!(is_const(value) && is_number(value->valType->type)))
          error("Illegal initializer: constant number expected");
        v = value->u.num.ival;
      }

      int size = type_size(type);
      for (int i = 0; i < size; ++i)
        buf[i] = v >> (i * 8);  // Little endian

      const char *fmt;
      switch (type->u.numtype) {
      case NUM_CHAR:  fmt = ".byte %"SCNdPTR; break;
      case NUM_SHORT: fmt = ".word %"SCNdPTR; break;
      case NUM_LONG:  fmt = ".quad %"SCNdPTR; break;
      default:
      case NUM_INT: case NUM_ENUM:
        fmt = ".long %"SCNdPTR;
        break;
      }
      UNUSED(fmt);
      add_asm(fmt, v);
    }
    break;
  case TY_PTR:
    if (init != NULL) {
      assert(init->type == vSingle);
      Expr *value = init->u.single;
      while (value->type == EX_CAST)
        value = value->u.unary.sub;
      if (value->type == EX_REF || value->type == EX_VARREF) {
        if (value->type == EX_REF)
          value = value->u.unary.sub;
        // TODO: Type check.

        assert(value->type == EX_VARREF);
        assert(value->u.varref.global);

        void **init = malloc(sizeof(void*) * 2);
        init[0] = buf;
        init[1] = (void*)value->u.varref.ident;
        if (*pptrinits == NULL)
          *pptrinits = new_vector();
        vec_push(*pptrinits, init);

        add_asm(".quad %s", value->u.varref.ident);
      } else if (value->type == EX_STR) {
        assert(!"`char* s = \"...\"`; should be handled in parser");
      } else if (is_const(value) && is_number(value->valType->type)) {
        intptr_t x = value->u.num.ival;
        for (int i = 0; i < WORD_SIZE; ++i)
          buf[i] = x >> (i * 8);  // Little endian

        add_asm(".quad 0x%"PRIxPTR, x);
      } else {
        assert(!"initializer type error");
      }
    } else {
      add_asm(".quad 0");
    }
    break;
  case TY_ARRAY:
    if (init == NULL || init->type == vMulti) {
      const Type *elem_type = type->u.pa.ptrof;
      size_t elem_size = type_size(elem_type);
      size_t elem_count = type->u.pa.length;
      int len = 0;
      if (init != NULL) {
        Vector *init_array = init->u.multi;
        len = init_array->len;
        for (int i = 0; i < len; ++i) {
          construct_initial_value(buf + (i * elem_size), elem_type, init_array->data[i], pptrinits);
        }
        assert((size_t)len <= elem_count);
      }
      for (size_t i = len; i < elem_count; ++i) {
        construct_initial_value(buf + (i * elem_size), elem_type, NULL, pptrinits);
      }
    } else {
      if (init->type == vSingle &&
          is_char_type(type->u.pa.ptrof) && init->u.single->type == EX_STR) {
        int src_size = init->u.single->u.str.size;
        size_t size = type_size(type);
        assert(size >= (size_t)src_size);
        memcpy(buf, init->u.single->u.str.buf, src_size);

        UNUSED(size);
        add_asm(".string \"%s\"", escape_string((char*)buf, size));
      } else {
        error("Illegal initializer");
      }
    }
    break;
  case TY_STRUCT:
  case TY_UNION:
    {
      Initializer **values = NULL;

      if (init != NULL) {
        if (init->type != vMulti)
          error("initializer type error");
        values = flatten_initializer(type, init);
      }

      ensure_struct((Type*)type, NULL);

      const StructInfo *sinfo = type->u.struct_.info;
      int count = 0;
      for (int i = 0, n = sinfo->members->len; i < n; ++i) {
        VarInfo* varinfo = sinfo->members->data[i];
        Initializer *mem_init;
        if (values == NULL) {
          if (type->type == TY_UNION)
            continue;
          mem_init = NULL;
        } else {
          mem_init = values[i];
        }
        if (mem_init != NULL || type->type != TY_UNION) {
          construct_initial_value(buf + varinfo->offset, varinfo->type, mem_init, pptrinits);
          ++count;
        }
      }
      if (type->type == TY_UNION && count <= 0) {
        VarInfo* varinfo = sinfo->members->data[0];
        construct_initial_value(buf + varinfo->offset, varinfo->type, NULL, pptrinits);
      }
    }
    break;
  default:
    fprintf(stderr, "Global initial value for type %d not implemented (yet)\n", type->type);
    assert(false);
    break;
  }
}

static void put_data(enum SectionType sec, const char *label, const VarInfo *varinfo) {
  size_t size = type_size(varinfo->type);
  unsigned char *buf = calloc(size, 1);
  if (buf == NULL)
    error("Out of memory");

  ALIGN_SECTION_SIZE(sec, align_size(varinfo->type));
  if ((varinfo->flag & VF_STATIC) == 0)  // global
    add_asm(".globl %s", label);
  size_t baseadr = instruction_pointer;
  ADD_LABEL(label);

  Vector *ptrinits = NULL;  // <[ptr, label]>
  construct_initial_value(buf, varinfo->type, varinfo->u.g.init, &ptrinits);
  add_section_data(sec, buf, size);

  if (ptrinits != NULL) {
    for (int i = 0; i < ptrinits->len; ++i) {
      void **pp = (void**)ptrinits->data[i];
      unsigned char *p = pp[0];
      const char *label = pp[1];
      add_loc_abs64(sec, label, p - buf + baseadr);
    }
  }

  free(buf);
}

// Put RoData into code.
static void put_rodata(void) {
  for (int i = 0, len = map_count(gvar_map); i < len; ++i) {
    const VarInfo *varinfo = (const VarInfo*)gvar_map->vals->data[i];
    if (varinfo->type->type == TY_FUNC ||
        (varinfo->type->type == TY_NUM && varinfo->type->u.numtype == NUM_ENUM) ||
        (varinfo->flag & VF_EXTERN) != 0 || varinfo->u.g.init == NULL ||
        (varinfo->flag & VF_CONST) == 0)
      continue;

    const char *name = (const char *)gvar_map->keys->data[i];
    put_data(SEC_CODE, name, varinfo);
  }
}

// Put global with initial value (RwData).
static void put_rwdata(void) {
  for (int i = 0, len = map_count(gvar_map); i < len; ++i) {
    const VarInfo *varinfo = (const VarInfo*)gvar_map->vals->data[i];
    if (varinfo->type->type == TY_FUNC ||
        (varinfo->type->type == TY_NUM && varinfo->type->u.numtype == NUM_ENUM) ||
        (varinfo->flag & VF_EXTERN) != 0 || varinfo->u.g.init == NULL ||
        (varinfo->flag & VF_CONST) != 0)
      continue;

    const char *name = (const char *)gvar_map->keys->data[i];
    put_data(SEC_DATA, name, varinfo);
  }
}

// Put global without initial value (bss).
static void put_bss(void) {
  for (int i = 0, len = map_count(gvar_map); i < len; ++i) {
    const char *name = (const char *)gvar_map->keys->data[i];
    const VarInfo *varinfo = (const VarInfo*)gvar_map->vals->data[i];
    if (varinfo->type->type == TY_FUNC || varinfo->u.g.init != NULL ||
        (varinfo->flag & VF_EXTERN) != 0)
      continue;
    //ALIGN_SECTION_SIZE(SEC_DATA, align_size(varinfo->type));
    int align = align_size(varinfo->type);
    add_asm_align(align);
    instruction_pointer = ALIGN(instruction_pointer, align);
    size_t size = type_size(varinfo->type);
    if (size < 1)
      size = 1;
    add_label(name);
    add_bss(size);
    add_asm(".comm %s, %d", name, size);
  }
}

// Resolve label locations.
static void resolve_label_locations(void) {
  Vector *unsolved_labels = NULL;
  for (int i = 0; i < loc_vector->len; ++i) {
    LocInfo *loc = loc_vector->data[i];
    void *val = map_get(label_map, loc->label);
    if (val == NULL) {
      if (unsolved_labels == NULL)
        unsolved_labels = new_vector();
      bool found = false;
      for (int j = 0; j < unsolved_labels->len; ++j) {
        if (strcmp(unsolved_labels->data[j], loc->label) == 0) {
          found = true;
          break;
        }
      }
      if (!found)
        vec_push(unsolved_labels, loc->label);
      continue;
    }

    intptr_t v = (intptr_t)val;
    Section *section = &sections[loc->section];
    unsigned char *code = section->buf;
    uintptr_t offset = loc->adr - section->start;
    switch (loc->type) {
    case LOC_REL8:
      {
        intptr_t d = v - loc->rel.base;
        // TODO: Check out of range
        code[offset] = d;
      }
      break;
    case LOC_REL32:
      {
        intptr_t d = v - loc->rel.base;
        // TODO: Check out of range
        for (int i = 0; i < 4; ++i)
          code[offset + i] = d >> (i * 8);
      }
      break;
    case LOC_ABS64:
      for (int i = 0; i < 8; ++i)
        code[offset + i] = v >> (i * 8);
      break;
    default:
      assert(false);
      break;
    }
  }

  if (unsolved_labels != NULL) {
    fprintf(stderr, "Link error:\n");
    for (int i = 0; i < unsolved_labels->len; ++i)
      fprintf(stderr, "  Cannot find label `%s'\n", (char*)unsolved_labels->data[i]);
    exit(1);
  }
}

static void dump_labels(void) {
  add_asm_comment(NULL);
  for (int i = 0, n = map_count(label_map); i < n; ++i) {
    const char *name = label_map->keys->data[i];
    uintptr_t adr = (uintptr_t)label_map->vals->data[i];
    add_asm_comment("%08x: %s", adr, name);
  }
}

void fixup_locations(void) {
  add_asm(".section .rodata");
  put_rodata();

  // Data section
  sections[SEC_DATA].start = instruction_pointer = ALIGN(instruction_pointer, 0x1000);  // Page size.

  add_asm_comment(NULL);
  add_asm(".data");
  put_rwdata();

  put_bss();

  resolve_label_locations();

  dump_labels();
}

void get_section_size(int section, size_t *pfilesz, size_t *pmemsz, uintptr_t *ploadadr) {
  *pfilesz = sections[section].size;
  *ploadadr = sections[section].start;
  switch (section) {
  case SEC_CODE:
    *pmemsz = *pfilesz;
    break;
  case SEC_DATA:
    *pmemsz = instruction_pointer - sections[SEC_DATA].start;  // Include bss.
    break;
  default:
    assert(!"Illegal");
    break;
  }
}

//

#ifndef __XCC
static Defun *curfunc;
#endif
static const char *s_break_label;
static const char *s_continue_label;
int stackpos;

static const char *push_break_label(const char **save) {
  *save = s_break_label;
  return s_break_label = alloc_label();
}

static void pop_break_label(const char *save) {
  s_break_label = save;
}

static const char *push_continue_label(const char **save) {
  *save = s_continue_label;
  return s_continue_label = alloc_label();
}

static void pop_continue_label(const char *save) {
  s_continue_label = save;
}

void gen_cond_jmp(Expr *cond, bool tf, const char *label) {
  gen_expr(cond);

  switch (cond->valType->type) {
  case TY_NUM:
    switch (cond->valType->u.numtype) {
    case NUM_CHAR:  TEST_AL_AL(); break;
    case NUM_SHORT: TEST_AX_AX(); break;
    case NUM_INT: case NUM_ENUM:
      TEST_EAX_EAX();
      break;
    case NUM_LONG:
      TEST_RAX_RAX();
      break;
    default: assert(false); break;
    }
    break;
  case TY_PTR:
    TEST_RAX_RAX();
    break;
  default: assert(false); break;
  }

  if (tf)
    JNE32(label);
  else
    JE32(label);
}

static int arrange_func_params(Scope *scope) {
  // Arrange parameters increasing order in stack,
  // and each parameter occupies sizeof(intptr_t).
  for (int i = 0; i < scope->vars->len; ++i) {
    VarInfo *varinfo = (VarInfo*)scope->vars->data[i];
    varinfo->offset = (i - MAX_ARGS) * WORD_SIZE;
  }
  return MAX_ARGS * WORD_SIZE;
}

static size_t arrange_scope_vars(Defun *defun) {
  // Calc local variable offsets.
  // Map parameters from the bottom (to reduce offsets).
  size_t frame_size = 0;
  for (int i = 0; i < defun->all_scopes->len; ++i) {
    Scope *scope = (Scope*)defun->all_scopes->data[i];
    size_t scope_size = scope->parent != NULL ? scope->parent->size : 0;
    if (scope->vars != NULL) {
      if (defun->type->u.func.vaargs && i == 0) {
        // Special arrangement for function parameters to work va_list.
        scope_size = arrange_func_params(scope);
      } else {
        for (int j = 0; j < scope->vars->len; ++j) {
          VarInfo *varinfo = (VarInfo*)scope->vars->data[j];
          if (varinfo->flag & VF_STATIC)
            continue;  // Static variable is not allocated on stack.
          size_t size = type_size(varinfo->type);
          int align = align_size(varinfo->type);
          if (size < 1)
            size = 1;
          scope_size = ALIGN(scope_size + size, align);
          varinfo->offset = -scope_size;
        }
      }
    }
    scope->size = scope_size;
    if (frame_size < scope_size)
      frame_size = scope_size;
  }
  return ALIGN(frame_size, FRAME_ALIGN);
}

static void put_args_to_stack(Defun *defun) {
  // Store arguments into local frame.
  Vector *params = defun->params;
  int len = params != NULL ? params->len : 0;
  if (len > MAX_ARGS)
    error("Parameter count %d exceeds %d in function `%s'", len, MAX_ARGS, defun->name);
  int n = defun->type->u.func.vaargs ? MAX_ARGS : len;
  for (int i = 0; i < n; ++i) {
    const Type *type;
    int offset;
    if (i < len) {
      const VarInfo *varinfo = (const VarInfo*)params->data[i];
      type = varinfo->type;
      offset = varinfo->offset;
    } else {  // vaargs
      type = &tyLong;
      offset = (i - MAX_ARGS) * WORD_SIZE;
    }

    int size = 0;
    switch (type->type) {
    case TY_NUM:
      switch (type->u.numtype) {
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
    switch (size) {
    case 1:
      switch (i) {
      case 0:  MOV_DIL_IND8_RBP(offset); break;
      case 1:  MOV_SIL_IND8_RBP(offset); break;
      case 2:  MOV_DL_IND8_RBP(offset); break;
      case 3:  MOV_CL_IND8_RBP(offset); break;
      case 4:  MOV_R8B_IND8_RBP(offset); break;
      case 5:  MOV_R9B_IND8_RBP(offset); break;
      default: break;
      }
      break;
    case 4:
      switch (i) {
      case 0:  MOV_EDI_IND8_RBP(offset); break;
      case 1:  MOV_ESI_IND8_RBP(offset); break;
      case 2:  MOV_EDX_IND8_RBP(offset); break;
      case 3:  MOV_ECX_IND8_RBP(offset); break;
      case 4:  MOV_R8D_IND8_RBP(offset); break;
      case 5:  MOV_R9D_IND8_RBP(offset); break;
      default: break;
      }
      break;
    case 8:
      switch (i) {
      case 0:  MOV_RDI_IND8_RBP(offset); break;
      case 1:  MOV_RSI_IND8_RBP(offset); break;
      case 2:  MOV_RDX_IND8_RBP(offset); break;
      case 3:  MOV_RCX_IND8_RBP(offset); break;
      case 4:  MOV_R8_IND8_RBP(offset); break;
      case 5:  MOV_R9_IND8_RBP(offset); break;
      default: break;
      }
      break;
    default:
      assert(false);
      break;
    }
  }
}

static bool is_funcall(Expr *expr, const char *funcname) {
  if (expr->type == EX_FUNCALL) {
    Expr *func = expr->u.funcall.func;
    if (func->type == EX_VARREF &&
        strcmp(func->u.varref.ident, funcname) == 0)
      return true;
  }
  return false;
}

static bool is_asm(Node *node) {
  return node->type == ND_EXPR &&
    is_funcall(node->u.expr, "__asm");
}

static void out_asm(Node *node) {
  Expr *funcall = node->u.expr;
  Vector *args = funcall->u.funcall.args;
  int len = args->len;

  Expr *arg0 = (Expr*)args->data[0];
  if (arg0->type != EX_STR)
    error("__asm takes string at 1st argument");
  add_asm("%s", arg0->u.str.buf);

  for (int i = 1; i < len; ++i) {
    Expr *arg = (Expr*)args->data[i];
    switch (arg->type) {
    case EX_NUM:
      switch (arg->valType->u.numtype) {
      case NUM_CHAR:
      case NUM_SHORT:
      case NUM_INT:
      case NUM_LONG:
        {
          unsigned char buf[1] = {arg->u.num.ival};
          add_section_data(SEC_CODE, buf, sizeof(buf));
        }
        break;
      default:
        assert(false);
        break;
      }
      break;
    case EX_FUNCALL:
      if (is_funcall(arg, "__rel32")) {
        if (arg->u.funcall.args->len == 1 &&
            ((Expr*)arg->u.funcall.args->data[0])->type == EX_STR) {
          const char *label = ((Expr*)arg->u.funcall.args->data[0])->u.str.buf;
          ADD_LOC_REL32(label, 0, 4);
          unsigned char buf[4] = {0};
          add_section_data(SEC_CODE, buf, sizeof(buf));
          break;
        }
      }
      // Fallthrough
    default:
      error("num literal expected");
      break;
    }
  }
}

static void gen_defun(Node *node) {
  assert(stackpos == 0);
  Defun *defun = node->u.defun;

  bool global = true;
  VarInfo *varinfo = find_global(defun->name);
  if (varinfo != NULL) {
    global = (varinfo->flag & VF_STATIC) == 0;
  }
  if (global)
    add_asm(".globl %s", defun->name);
  else
    add_asm_comment("%s: static func", defun->name);

  ADD_LABEL(defun->name);

  // Allocate labels for goto.
  if (defun->labels != NULL) {
    Map *labels = defun->labels;
    for (int i = 0, n = map_count(labels); i < n; ++i)
      labels->vals->data[i] = alloc_label();
  }

  size_t frame_size = arrange_scope_vars(defun);

  bool no_stmt = true;
  if (defun->stmts != NULL) {
    for (int i = 0; i < defun->stmts->len; ++i) {
      Node *node = defun->stmts->data[i];
      if (!is_asm(node)) {
        no_stmt = false;
        break;
      }
    }
  }

  curfunc = defun;
  curscope = defun->top_scope;
  defun->ret_label = alloc_label();

  // Prologue
  // Allocate variable bufer.
  if (!no_stmt) {
    PUSH_RBP(); PUSH_STACK_POS();
    MOV_RSP_RBP();
    if (frame_size > 0) {
      SUB_IM32_RSP(frame_size);
      stackpos += frame_size;
    }

    put_args_to_stack(defun);
  }

  // Statements
  if (defun->stmts != NULL) {
    for (int i = 0; i < defun->stmts->len; ++i) {
      Node *node = defun->stmts->data[i];
      if (is_asm(node))
        out_asm(node);
      else
        gen(node);
    }
  }

  // Epilogue
  if (!no_stmt) {
    ADD_LABEL(defun->ret_label);
    MOV_RBP_RSP();
    stackpos -= frame_size;
    POP_RBP(); POP_STACK_POS();
  }
  RET();
  add_asm_comment(NULL);
  curfunc = NULL;
  curscope = NULL;
  assert(stackpos == 0);
}

static void gen_block(Node *node) {
  if (node->u.block.nodes != NULL) {
    if (node->u.block.scope != NULL) {
      assert(curscope == node->u.block.scope->parent);
      curscope = node->u.block.scope;
    }
    for (int i = 0, len = node->u.block.nodes->len; i < len; ++i)
      gen((Node*)node->u.block.nodes->data[i]);
    if (node->u.block.scope != NULL)
      curscope = curscope->parent;
  }
}

static void gen_return(Node *node) {
  if (node->u.return_.val != NULL)
    gen_expr(node->u.return_.val);
  assert(curfunc != NULL);
  JMP32(curfunc->ret_label);
}

static void gen_if(Node *node) {
  const char *flabel = alloc_label();
  gen_cond_jmp(node->u.if_.cond, false, flabel);
  gen(node->u.if_.tblock);
  if (node->u.if_.fblock == NULL) {
    ADD_LABEL(flabel);
  } else {
    const char *nlabel = alloc_label();
    JMP32(nlabel);
    ADD_LABEL(flabel);
    gen(node->u.if_.fblock);
    ADD_LABEL(nlabel);
  }
}

static Vector *cur_case_values;
static Vector *cur_case_labels;

static void gen_switch(Node *node) {
  Vector *save_case_values = cur_case_values;
  Vector *save_case_labels = cur_case_labels;
  const char *save_break;
  const char *l_break = push_break_label(&save_break);

  Vector *labels = new_vector();
  Vector *case_values = node->u.switch_.case_values;
  int len = case_values->len;
  for (int i = 0; i < len; ++i) {
    const char *label = alloc_label();
    vec_push(labels, label);
  }
  vec_push(labels, alloc_label());  // len+0: Extra label for default.
  vec_push(labels, l_break);  // len+1: Extra label for break.

  Expr *value = node->u.switch_.value;
  gen_expr(value);

  enum NumType valtype = value->valType->u.numtype;
  for (int i = 0; i < len; ++i) {
    intptr_t x = (intptr_t)case_values->data[i];
    switch (valtype) {
    case NUM_CHAR:
      CMP_IM8_AL(x);
      break;
    case NUM_INT: case NUM_ENUM:
      CMP_IM32_EAX(x);
      break;
    case NUM_LONG:
      if (x <= 0x7fL && x >= -0x80L) {
        CMP_IM8_RAX(x);
      } else if (x <= 0x7fffffffL && x >= -0x80000000L) {
        CMP_IM32_RAX(x);
      } else {
        MOV_IM64_RDI(x);
        CMP_RDI_RAX();
      }
      break;
    default: assert(false); break;
    }
    JE32(labels->data[i]);
  }
  JMP32(labels->data[len]);

  cur_case_values = case_values;
  cur_case_labels = labels;

  gen(node->u.switch_.body);

  if (!node->u.switch_.has_default)
    ADD_LABEL(labels->data[len]);  // No default: Locate at the end of switch statement.
  ADD_LABEL(l_break);

  cur_case_values = save_case_values;
  cur_case_labels = save_case_labels;
  pop_break_label(save_break);
}

static void gen_case(Node *node) {
  assert(cur_case_values != NULL);
  assert(cur_case_labels != NULL);
  intptr_t x = node->u.case_.value;
  int i, len = cur_case_values->len;
  for (i = 0; i < len; ++i) {
    if ((intptr_t)cur_case_values->data[i] == x)
      break;
  }
  assert(i < len);
  assert(i < cur_case_labels->len);
  ADD_LABEL(cur_case_labels->data[i]);
}

static void gen_default(void) {
  assert(cur_case_values != NULL);
  assert(cur_case_labels != NULL);
  int i = cur_case_values->len;  // Label for default is stored at the size of values.
  assert(i < cur_case_labels->len);
  ADD_LABEL(cur_case_labels->data[i]);
}

static void gen_while(Node *node) {
  const char *save_break, *save_cont;
  const char *l_cond = push_continue_label(&save_cont);
  const char *l_break = push_break_label(&save_break);
  const char *l_loop = alloc_label();
  JMP32(l_cond);
  ADD_LABEL(l_loop);
  gen(node->u.while_.body);
  ADD_LABEL(l_cond);
  gen_cond_jmp(node->u.while_.cond, true, l_loop);
  ADD_LABEL(l_break);
  pop_continue_label(save_cont);
  pop_break_label(save_break);
}

static void gen_do_while(Node *node) {
  const char *save_break, *save_cont;
  const char *l_cond = push_continue_label(&save_cont);
  const char *l_break = push_break_label(&save_break);
  const char * l_loop = alloc_label();
  ADD_LABEL(l_loop);
  gen(node->u.do_while.body);
  ADD_LABEL(l_cond);
  gen_cond_jmp(node->u.do_while.cond, true, l_loop);
  ADD_LABEL(l_break);
  pop_continue_label(save_cont);
  pop_break_label(save_break);
}

static void gen_for(Node *node) {
  const char *save_break, *save_cont;
  const char *l_continue = push_continue_label(&save_cont);
  const char *l_break = push_break_label(&save_break);
  const char * l_cond = alloc_label();
  if (node->u.for_.pre != NULL)
    gen_expr(node->u.for_.pre);
  ADD_LABEL(l_cond);
  if (node->u.for_.cond != NULL) {
    gen_cond_jmp(node->u.for_.cond, false, l_break);
  }
  gen(node->u.for_.body);
  ADD_LABEL(l_continue);
  if (node->u.for_.post != NULL)
    gen_expr(node->u.for_.post);
  JMP32(l_cond);
  ADD_LABEL(l_break);
  pop_continue_label(save_cont);
  pop_break_label(save_break);
}

static void gen_break(void) {
  assert(s_break_label != NULL);
  JMP32(s_break_label);
}

static void gen_continue(void) {
  assert(s_continue_label != NULL);
  JMP32(s_continue_label);
}

static void gen_goto(Node *node) {
  assert(curfunc->labels != NULL);
  const char *label = map_get(curfunc->labels, node->u.goto_.ident);
  assert(label != NULL);
  JMP32(label);
}

static void gen_label(Node *node) {
  assert(curfunc->labels != NULL);
  const char *label = map_get(curfunc->labels, node->u.label.name);
  assert(label != NULL);
  ADD_LABEL(label);
  gen(node->u.label.stmt);
}

void gen(Node *node) {
  switch (node->type) {
  case ND_EXPR:  gen_expr(node->u.expr); break;
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

  default:
    error("Unhandled node: %d", node->type);
    break;
  }
}

void init_gen(uintptr_t start_address_) {
  sections[SEC_CODE].start = instruction_pointer = start_address_;
  label_map = new_map();
  loc_vector = new_vector();
}

void set_asm_fp(FILE *fp) {
#if !defined(NO_ASM_OUTPUT)
  asm_fp = fp;
#else
  (void)fp;
#endif
}

void output_section(FILE* fp, int section) {
  Section *p = &sections[section];
  unsigned char *buf = p->buf;
  fwrite(buf, p->size, 1, fp);
}
