#include <assert.h>
#include <inttypes.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

#include "xcc.h"
#include "expr.h"
#include "util.h"

const int FRAME_ALIGN = 8;
const int MAX_ARGS = 6;
const int WORD_SIZE = /*sizeof(void*)*/ 8;

#define CURIP(ofs)  (instruction_pointer + ofs)
#include "x86_64.h"

#define ALIGN_CODESIZE(sec, align_)  do { int align = (int)(align_); add_asm_align(align); align_codesize(sec, align); } while (0)

static void calc_struct_size(StructInfo *sinfo, bool is_union);

static size_t type_size(const Type *type) {
  switch (type->type) {
  case TY_VOID:
    return 1;  // ?
  case TY_CHAR:
    return 1;
  case TY_SHORT:
    return 2;
  case TY_INT:
  case TY_ENUM:
    return 4;
  case TY_LONG:
    return 8;
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
  case TY_CHAR:
    return 1;
  case TY_SHORT:
    return 2;
  case TY_INT:
  case TY_ENUM:
    return 4;
  case TY_LONG:
    return 8;
  case TY_PTR:
  case TY_FUNC:
    return 8;
  case TY_ARRAY:
    return align_size(type->u.pa.ptrof);
  case TY_STRUCT:
  case TY_UNION:
    ensure_struct((Type*)type, NULL);
    calc_struct_size(type->u.struct_.info, type->type == TY_UNION);
    return type->u.struct_.info->align;
  default:
    assert(false);
    return 1;
  }
}

static void calc_struct_size(StructInfo *sinfo, bool is_union) {
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

static Map *label_map;

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

static void add_section_data(enum SectionType secno, const unsigned char* buf, size_t size) {
  Section *sec = &sections[secno];
  size_t codesize = sec->size;
  size_t newsize = codesize + size;
  unsigned char *code = realloc(sec->buf, newsize);
  if (code == NULL)
    error("not enough memory");
  memcpy(code + codesize, buf, size);
  sec->buf = code;
  sec->size = newsize;
  instruction_pointer += size;
}

void add_code(const unsigned char* buf, size_t size) {
  add_section_data(SEC_CODE, buf, size);
}

static void add_asm(const char *fmt, ...) {
  if (asm_fp == NULL)
    return;

  va_list ap;
  va_start(ap, fmt);
  fprintf(asm_fp, "\t");
  vfprintf(asm_fp, fmt, ap);
  fprintf(asm_fp, "\n");
  va_end(ap);
}

static void add_asm_label(const char *label) {
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

void align_codesize(int sec, int align) {
  sections[sec].size = ALIGN(sections[sec].size, align);
  instruction_pointer = ALIGN(instruction_pointer, align);
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
  add_asm_align(align_size(type));

  switch (type->type) {
  case TY_CHAR:
  case TY_SHORT:
  case TY_INT:
  case TY_LONG:
  case TY_ENUM:
    {
      intptr_t v = 0;
      if (init != NULL) {
        assert(init->type == vSingle);
        Expr *value = init->u.single;
        if (!(is_const(value) && is_number(value->valType->type)))
          error("Illegal initializer: constant number expected");
        v = value->u.value;
      }

      int size = type_size(type);
      for (int i = 0; i < size; ++i)
        buf[i] = v >> (i * 8);  // Little endian

      const char *fmt;
      switch (type->type) {
      case TY_CHAR:  fmt = ".byte %"SCNdPTR; break;
      case TY_SHORT: fmt = ".word %"SCNdPTR; break;
      case TY_INT: case TY_ENUM:
        fmt = ".long %"SCNdPTR;
        break;
      case TY_LONG:  fmt = ".quad %"SCNdPTR; break;
      default: break;
      }
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

        memset(buf, 0, type_size(type));  // Just in case.
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
        intptr_t x = value->u.value;
        for (int i = 0; i < WORD_SIZE; ++i)
          buf[i] = x >> (i * 8);  // Little endian

        add_asm(".quad 0x%"PRIxPTR, x);
      } else {
        assert(!"initializer type error");
      }
    } else {
      memset(buf, 0x00, WORD_SIZE);
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
        memset(buf, 0, elem_size * len);
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
          type->u.pa.ptrof->type == TY_CHAR && init->u.single->type == EX_STR) {
        int src_size = init->u.single->u.str.size;
        size_t size = type_size(type);
        int d = size - src_size;
        assert(d >= 0);
        memcpy(buf, init->u.single->u.str.buf, src_size);
        if (d > 0) {
          memset(buf + src_size, 0x00, d);
        }

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
      memset(buf, 0x00, type_size(type));

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
        construct_initial_value(buf + varinfo->offset, varinfo->type, mem_init, pptrinits);
        ++count;
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
  unsigned char *buf = malloc(size);
  if (buf == NULL)
    error("Memory alloc failed: %zu", size);

  ALIGN_CODESIZE(sec, align_size(varinfo->type));
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
    if (varinfo->type->type == TY_FUNC || varinfo->type->type == TY_ENUM ||
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
    if (varinfo->type->type == TY_FUNC || varinfo->type->type == TY_ENUM ||
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
    ALIGN_CODESIZE(SEC_CODE, align_size(varinfo->type));
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

void fixup_locations(void) {
  add_asm(".section .rodata");
  put_rodata();

  // Data section
  sections[SEC_DATA].start = instruction_pointer = ALIGN(instruction_pointer, 0x1000);  // Page size.

  add_asm("");
  add_asm(".data");
  put_rwdata();

  put_bss();

  resolve_label_locations();
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

typedef struct LoopInfo {
  struct LoopInfo *outer;
  const char *l_break;
  const char *l_continue;
} LoopInfo;

#ifndef __XCC
static Defun *curfunc;
static Scope *curscope;
#endif
static const char *s_break_label;
static const char *s_continue_label;

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

static void gen_expr(Expr *expr);
static void gen_lval(Expr *expr);

static void cast(const enum eType ltype, const enum eType rtype) {
  if (ltype == rtype)
    return;

  switch (ltype) {
  case TY_VOID:
    return;
  case TY_CHAR:
    switch (rtype) {
    case TY_SHORT: return;
    case TY_INT:   return;
    case TY_LONG:  return;
    default: assert(false); break;
    }
    break;
  case TY_SHORT:
    switch (rtype) {
    case TY_CHAR: MOVSX_AL_AX(); return;
    case TY_INT:  return;
    case TY_LONG: return;
    default: assert(false); break;
    }
    break;
  case TY_INT: case TY_ENUM:
    switch (rtype) {
    case TY_CHAR:  MOVSX_AL_EAX(); return;
    case TY_SHORT: MOVSX_AX_EAX(); return;
    case TY_INT:   return;
    case TY_LONG:  return;
    case TY_ENUM:  return;
    default: assert(false); break;
    }
    break;
  case TY_LONG:
    switch (rtype) {
    case TY_CHAR:  MOVSX_AL_RAX(); return;
    case TY_SHORT: MOVSX_AX_RAX(); return;
    case TY_INT:   MOVSX_EAX_RAX(); return;
    case TY_PTR:
    case TY_ARRAY:
      return;
    default: assert(false); break;
    }
    break;
  case TY_PTR:
    switch (rtype) {
    case TY_INT:   MOVSX_EAX_RAX(); return;
    case TY_LONG: case TY_ARRAY: case TY_FUNC:
      return;
    default: assert(false); break;
    }
    break;
  default:
    assert(false); break;
    break;
  }

  fprintf(stderr, "ltype=%d, rtype=%d\n", ltype, rtype);
  assert(!"Cast failed");
}

static void gen_rval(Expr *expr) {
  gen_expr(expr);  // ?
}

static void gen_ref(Expr *expr) {
  gen_lval(expr);
}

static void gen_lval(Expr *expr) {
  switch (expr->type) {
  case EX_VARREF:
    if (expr->u.varref.global) {
      LEA_LABEL32_RIP_RAX(expr->u.varref.ident);
    } else {
      VarInfo *varinfo = scope_find(curscope, expr->u.varref.ident);
      assert(varinfo != NULL);
      assert(!(varinfo->flag & VF_STATIC));
      int offset = varinfo->offset;
      LEA_OFS32_RBP_RAX(offset);
    }
    break;
  case EX_DEREF:
    gen_rval(expr->u.unary.sub);
    break;
  case EX_MEMBER:
    {
      const Type *type = expr->u.member.target->valType;
      if (type->type == TY_PTR || type->type == TY_ARRAY)
        type = type->u.pa.ptrof;
      assert(type->type == TY_STRUCT || type->type == TY_UNION);
      calc_struct_size(type->u.struct_.info, type->type == TY_UNION);
      Vector *members = type->u.struct_.info->members;
      VarInfo *varinfo = (VarInfo*)members->data[expr->u.member.index];

      if (expr->u.member.target->valType->type == TY_PTR)
        gen_expr(expr->u.member.target);
      else
        gen_ref(expr->u.member.target);
      if (varinfo->offset != 0)
        ADD_IM32_RAX(varinfo->offset);
    }
    break;
  default:
    error("No lvalue: %d", expr->type);
    break;
  }
}

static void gen_cond_jmp(Expr *cond, bool tf, const char *label) {
  gen_expr(cond);

  switch (cond->valType->type) {
  case TY_CHAR:  TEST_AL_AL(); break;
  case TY_SHORT: TEST_AX_AX(); break;
  case TY_INT:   TEST_EAX_EAX(); break;
  case TY_LONG: case TY_PTR:
    TEST_RAX_RAX();
    break;
  default: assert(false); break;
  }

  if (tf)
    JNE32(label);
  else
    JE32(label);
}

static void gen_varref(Expr *expr) {
  gen_lval(expr);
  switch (expr->valType->type) {
  case TY_CHAR:  MOV_IND_RAX_AL(); break;
  case TY_SHORT: MOV_IND_RAX_AX(); break;
  case TY_INT:   MOV_IND_RAX_EAX(); break;
  case TY_LONG:  MOV_IND_RAX_RAX(); break;
  case TY_ENUM:  MOV_IND_RAX_EAX(); break;
  case TY_PTR:   MOV_IND_RAX_RAX(); break;
  case TY_ARRAY: break;  // Use variable address as a pointer.
  case TY_FUNC:  break;
  default: assert(false); break;
  }
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
  Vector *params = defun->type->u.func.params;
  int len = params != NULL ? params->len : 0;
  if (len > MAX_ARGS)
    error("Parameter count %d exceeds %d in function `%s'", len, MAX_ARGS, defun->name);
  int n = defun->type->u.func.vaargs ? MAX_ARGS : len;
  for (int i = 0; i < n; ++i) {
    enum eType type;
    int offset;
    if (i < len) {
      const VarInfo *varinfo = (const VarInfo*)params->data[i];
      type = varinfo->type->type;
      offset = varinfo->offset;
    } else {  // vaargs
      type = TY_PTR;
      offset = (i - MAX_ARGS) * WORD_SIZE;
    }

    switch (type) {
    case TY_CHAR:  // 1
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
    case TY_INT:
    case TY_ENUM:
      // 4
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
    case TY_LONG:
    case TY_PTR:
      // 8
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
    case EX_CHAR:
    case EX_SHORT:
    case EX_INT:
    case EX_LONG:
      {
        unsigned char buf[1] = {arg->u.value};
        add_section_data(SEC_CODE, buf, sizeof(buf));
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
    PUSH_RBP();
    MOV_RSP_RBP();
    if (frame_size > 0)
      SUB_IM32_RSP(frame_size);

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
    POP_RBP();
  }
  RET();
  add_asm_comment(NULL);
  curfunc = NULL;
  curscope = NULL;
}

static void gen_return(Node *node) {
  if (node->u.return_.val != NULL)
    gen_expr(node->u.return_.val);
  assert(curfunc != NULL);
  JMP32(curfunc->ret_label);
}

static void gen_funcall(Expr *expr) {
  Vector *args = expr->u.funcall.args;
  if (args != NULL) {
    int len = args->len;
    if (len > 6)
      error("Param count exceeds 6 (%d)", len);

    for (int i = 0; i < len; ++i) {
      gen_expr((Expr*)args->data[i]);
      PUSH_RAX();
    }

    switch (len) {
    case 6:  POP_R9();  // Fallthrough
    case 5:  POP_R8();  // Fallthrough
    case 4:  POP_RCX();  // Fallthrough
    case 3:  POP_RDX();  // Fallthrough
    case 2:  POP_RSI();  // Fallthrough
    case 1:  POP_RDI();  // Fallthrough
    default: break;
    }
  }
  Expr *func = expr->u.funcall.func;
  if (func->type == EX_VARREF && func->u.varref.global) {
    CALL(func->u.varref.ident);
  } else {
    gen_expr(func);
    CALL_IND_RAX();
  }
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

static void gen_ternary(Expr *expr) {
  const char *nlabel = alloc_label();
  const char *flabel = alloc_label();
  gen_cond_jmp(expr->u.ternary.cond, false, flabel);
  gen_expr(expr->u.ternary.tval);
  JMP32(nlabel);
  ADD_LABEL(flabel);
  gen_expr(expr->u.ternary.fval);
  ADD_LABEL(nlabel);
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

  enum eType valtype = value->valType->type;
  for (int i = 0; i < len; ++i) {
    intptr_t x = (intptr_t)case_values->data[i];
    switch (valtype) {
    case TY_CHAR:
      CMP_IM8_AL(x);
      break;
    case TY_INT: case TY_ENUM:
      CMP_IM32_EAX(x);
      break;
    case TY_LONG:
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

static void gen_arith(enum ExprType exprType, enum eType valType, enum eType rhsType) {
  // lhs=rax, rhs=rdi, result=rax

  switch (exprType) {
  case EX_ADD:
    switch (valType) {
    case TY_CHAR:  ADD_DIL_AL(); break;
    case TY_SHORT: ADD_DI_AX(); break;
    case TY_INT:   ADD_EDI_EAX(); break;
    case TY_LONG: case TY_PTR:
      ADD_RDI_RAX();
      break;
    default: assert(false); break;
    }
    break;

  case EX_SUB:
    switch (valType) {
    case TY_CHAR:  SUB_DIL_AL(); break;
    case TY_SHORT: SUB_DI_AX(); break;
    case TY_INT:   SUB_EDI_EAX(); break;
    case TY_LONG: case TY_PTR:
      SUB_RDI_RAX();
      break;
    default: assert(false); break;
    }
    break;

  case EX_MUL:
    switch (valType) {
    case TY_CHAR:  MUL_DIL(); break;
    case TY_SHORT: MUL_DI(); break;
    case TY_INT:   MUL_EDI(); break;
    case TY_LONG:  MUL_RDI(); break;
    default: assert(false); break;
    }

    break;

  case EX_DIV:
    XOR_EDX_EDX();  // MOV_IM32_RDX(0);
    switch (valType) {
    case TY_CHAR:  DIV_DIL(); break;
    case TY_SHORT: DIV_DI(); break;
    case TY_INT:   DIV_EDI(); break;
    case TY_LONG:  DIV_RDI(); break;
    default: assert(false); break;
    }
    break;

  case EX_MOD:
    XOR_EDX_EDX();  // MOV_IM32_RDX(0);
    switch (valType) {
    case TY_CHAR:  DIV_DIL(); MOV_DL_AL(); break;
    case TY_SHORT: DIV_DI();  MOV_DX_AX(); break;
    case TY_INT:   DIV_EDI(); MOV_EDX_EAX(); break;
    case TY_LONG:  DIV_RDI(); MOV_RDX_RAX(); break;
    default: assert(false); break;
    }
    break;

  case EX_BITAND:
    switch (valType) {
    case TY_CHAR:  AND_DIL_AL(); break;
    case TY_SHORT: AND_DI_AX(); break;
    case TY_INT:   AND_EDI_EAX(); break;
    case TY_LONG:  AND_RDI_RAX(); break;
    default: assert(false); break;
    }
    break;

  case EX_BITOR:
    switch (valType) {
    case TY_CHAR:  OR_DIL_AL(); break;
    case TY_SHORT: OR_DI_AX(); break;
    case TY_INT: case TY_ENUM:
      OR_EDI_EAX();
      break;
    case TY_LONG:  OR_RDI_RAX(); break;
    default: assert(false); break;
    }
    break;

  case EX_BITXOR:
    switch (valType) {
    case TY_CHAR:  XOR_DIL_AL(); break;
    case TY_SHORT: XOR_DI_AX(); break;
    case TY_INT:   XOR_EDI_EAX(); break;
    case TY_LONG:  XOR_RDI_RAX(); break;
    default: assert(false); break;
    }
    break;

  case EX_LSHIFT:
  case EX_RSHIFT:
    switch (rhsType) {
    case TY_CHAR:  MOV_DIL_CL(); break;
    case TY_SHORT: MOV_DI_CX(); break;
    case TY_INT:   MOV_EDI_ECX(); break;
    case TY_LONG:  MOV_RDI_RCX(); break;
    default: assert(false); break;
    }
    if (exprType == EX_LSHIFT) {
      switch (valType) {
      case TY_CHAR:  SHL_CL_AL(); break;
      case TY_SHORT: SHL_CL_AX(); break;
      case TY_INT:   SHL_CL_EAX(); break;
      case TY_LONG:  SHL_CL_RAX(); break;
      default: assert(false); break;
      }
    } else {
      switch (valType) {
      case TY_CHAR:  SHR_CL_AL(); break;
      case TY_SHORT: SHR_CL_AX(); break;
      case TY_INT:   SHR_CL_EAX(); break;
      case TY_LONG:  SHR_CL_RAX(); break;
      default: assert(false); break;
      }
    }
    break;

  default:
    assert(false);
    break;
  }
}

void gen_expr(Expr *expr) {
  switch (expr->type) {
  case EX_CHAR:
    if (expr->u.value == 0)
      XOR_AL_AL();
    else
      MOV_IM8_AL(expr->u.value);
    return;

  case EX_INT:
    if (expr->u.value == 0)
      XOR_EAX_EAX();
    else
      MOV_IM32_EAX(expr->u.value);
    return;

  case EX_LONG:
    if (expr->u.value == 0)
      XOR_EAX_EAX();  // upper 32bit is also cleared.
    else if (expr->u.value <= 0x7fffffffL && expr->u.value >= -0x80000000L)
      MOV_IM32_RAX(expr->u.value);
    else
      MOV_IM64_RAX(expr->u.value);
    return;

  case EX_STR:
    {
      Initializer *init = malloc(sizeof(*init));
      init->type = vSingle;
      init->u.single = expr;

      // Create string and point to it.
      const char * label = alloc_label();
      Type* strtype = arrayof(&tyChar, expr->u.str.size);
      VarInfo *varinfo = define_global(strtype, VF_CONST | VF_STATIC, NULL, label);

      varinfo->u.g.init = init;

      LEA_LABEL32_RIP_RAX(label);
    }
    return;

  case EX_SIZEOF:
    {
      size_t size = type_size(expr->u.sizeof_.type);
      if (size <= 0x7fffffffL)
        MOV_IM32_RAX(size);
      else
        MOV_IM64_RAX(size);
    }
    return;

  case EX_VARREF:
    gen_varref(expr);
    return;

  case EX_REF:
    gen_ref(expr->u.unary.sub);
    return;

  case EX_DEREF:
    gen_rval(expr->u.unary.sub);
    switch (expr->valType->type) {
    case TY_CHAR:  MOV_IND_RAX_AL(); break;
    case TY_SHORT: MOV_IND_RAX_AX(); break;
    case TY_INT: case TY_ENUM:
      MOV_IND_RAX_EAX();
      break;
    case TY_LONG: case TY_PTR:
      MOV_IND_RAX_RAX();
      break;
    case TY_ARRAY: break;
    default: assert(false); break;
    }
    return;

  case EX_MEMBER:
    gen_lval(expr);
    switch (expr->valType->type) {
    case TY_CHAR:  MOV_IND_RAX_AL(); break;
    case TY_SHORT: MOV_IND_RAX_AX(); break;
    case TY_INT: case TY_ENUM:
      MOV_IND_RAX_EAX();
      break;
    case TY_LONG: case TY_PTR:
      MOV_IND_RAX_RAX();
      break;
    case TY_ARRAY:
      break;
    default:
      assert(false);
      break;
    }
    return;

  case EX_COMMA:
    {
      Vector *list = expr->u.comma.list;
      for (int i = 0, len = list->len; i < len; ++i)
        gen_expr(list->data[i]);
    }
    break;

  case EX_TERNARY:
    gen_ternary(expr);
    break;

  case EX_CAST:
    gen_expr(expr->u.cast.sub);
    cast(expr->valType->type, expr->u.cast.sub->valType->type);
    break;

  case EX_ASSIGN:
    gen_lval(expr->u.bop.lhs);
    PUSH_RAX();
    gen_expr(expr->u.bop.rhs);

    POP_RDI();
    switch (expr->u.bop.lhs->valType->type) {
    case TY_CHAR:  MOV_AL_IND_RDI(); break;
    case TY_SHORT: MOV_AX_IND_RDI(); break;
    case TY_INT: case TY_ENUM:
      MOV_EAX_IND_RDI();
      break;
    case TY_LONG: case TY_PTR:
      MOV_RAX_IND_RDI();
      break;
    default: assert(false); break;
    }
    return;

  case EX_ASSIGN_WITH:
    {
      Expr *sub = expr->u.unary.sub;
      gen_expr(sub->u.bop.rhs);
      PUSH_RAX();
      gen_lval(sub->u.bop.lhs);
      MOV_RAX_RSI();  // Save lhs address to %rsi.

      // Move lhs to %?ax
      switch (expr->u.bop.lhs->valType->type) {
      case TY_CHAR:  MOV_IND_RAX_AL(); break;
      case TY_SHORT: MOV_IND_RAX_AX(); break;
      case TY_INT:   MOV_IND_RAX_EAX(); break;
      case TY_LONG: case TY_PTR:
        MOV_IND_RAX_RAX();
        break;
      default: assert(false); break;
      }

      POP_RDI();  // %rdi=rhs
      gen_arith(sub->type, sub->valType->type, sub->u.bop.rhs->valType->type);
      cast(expr->valType->type, sub->valType->type);

      switch (expr->valType->type) {
      case TY_CHAR:  MOV_AL_IND_RSI(); break;
      case TY_SHORT: MOV_AX_IND_RSI(); break;
      case TY_INT:   MOV_EAX_IND_RSI(); break;
      case TY_LONG: case TY_PTR:
        MOV_RAX_IND_RSI();
        break;
      default: assert(false); break;
      }
    }
    return;

  case EX_PREINC:
  case EX_PREDEC:
    gen_lval(expr->u.unary.sub);
    switch (expr->valType->type) {
    case TY_CHAR:
      if (expr->type == EX_PREINC)  INCB_IND_RAX();
      else                          DECB_IND_RAX();
      MOV_IND_RAX_AL();
      break;
    case TY_SHORT:
      if (expr->type == EX_PREINC)  INCW_IND_RAX();
      else                          DECW_IND_RAX();
      MOV_IND_RAX_AX();
      break;
    case TY_INT:
      if (expr->type == EX_PREINC)  INCL_IND_RAX();
      else                          DECL_IND_RAX();
      MOV_IND_RAX_EAX();
      break;
    case TY_LONG:
      if (expr->type == EX_PREINC)  INCQ_IND_RAX();
      else                          DECQ_IND_RAX();
      MOV_IND_RAX_RAX();
      break;
    case TY_PTR:
      {
        MOV_RAX_RDI();
        size_t size = type_size(expr->valType->u.pa.ptrof);
        MOV_IM32_RAX(expr->type == EX_PREINC ? size : -size);
        ADD_IND_RDI_RAX();
        MOV_RAX_IND_RDI();
      }
      break;
    default:
      assert(false);
      break;
    }
    return;

  case EX_POSTINC:
  case EX_POSTDEC:
    gen_lval(expr->u.unary.sub);
    MOV_IND_RAX_RDI();
    switch (expr->valType->type) {
    case TY_CHAR:
      if (expr->type == EX_POSTINC)  INCB_IND_RAX();
      else                           DECB_IND_RAX();
      break;
    case TY_SHORT:
      if (expr->type == EX_POSTINC)  INCW_IND_RAX();
      else                           DECW_IND_RAX();
      break;
    case TY_INT:
      if (expr->type == EX_POSTINC)  INCL_IND_RAX();
      else                           DECL_IND_RAX();
      break;
    case TY_LONG:
      if (expr->type == EX_POSTINC)  INCQ_IND_RAX();
      else                           DECQ_IND_RAX();
      break;
    case TY_PTR:
      {
        size_t size = type_size(expr->valType->u.pa.ptrof);
        assert(size < (1 << 15));  // TODO:
        if (expr->type == EX_POSTINC)  ADD_IM16_IND_RAX(size);
        else                           SUB_IM16_IND_RAX(size);
      }
      break;
    default:
      assert(false);
      break;
    }
    MOV_RDI_RAX();
    return;

  case EX_FUNCALL:
    gen_funcall(expr);
    return;

  case EX_NEG:
    gen_expr(expr->u.unary.sub);
    switch (expr->valType->type) {
    case TY_CHAR: NEG_AL(); break;
    case TY_INT:  NEG_EAX(); break;
    case TY_LONG: NEG_RAX(); break;
    default:  assert(false); break;
    }
    break;

  case EX_NOT:
    gen_expr(expr->u.unary.sub);
    switch (expr->valType->type) {
    case TY_CHAR: TEST_AL_AL(); break;
    case TY_INT:  TEST_EAX_EAX(); break;
    case TY_PTR:  TEST_RAX_RAX(); break;
    default:  assert(false); break;
    }
    SETE_AL();
    MOVZX_AL_EAX();
    break;

  case EX_EQ:
  case EX_NE:
  case EX_LT:
  case EX_GT:
  case EX_LE:
  case EX_GE:
    {
      enum ExprType type = expr->type;
      Expr *lhs = expr->u.bop.lhs;
      Expr *rhs = expr->u.bop.rhs;
      enum eType ltype = lhs->valType->type, rtype = rhs->valType->type;
      if (ltype == TY_ENUM)
        ltype = TY_INT;
      if (rtype == TY_ENUM)
        rtype = TY_INT;
      assert(ltype == rtype);
      if (type == EX_LE || type == EX_GT) {
        Expr *tmp = lhs; lhs = rhs; rhs = tmp;
        type = type == EX_LE ? EX_GE : EX_LT;
      }

      gen_expr(lhs);
      PUSH_RAX();
      gen_expr(rhs);

      POP_RDI();
      switch (ltype) {
      case TY_CHAR: CMP_AL_DIL(); break;
      case TY_INT:  CMP_EAX_EDI(); break;
      case TY_LONG: CMP_RAX_RDI(); break;
      case TY_PTR:  CMP_RAX_RDI(); break;
      default: assert(false); break;
      }

      switch (type) {
      case EX_EQ:  SETE_AL(); break;
      case EX_NE:  SETNE_AL(); break;
      case EX_LT:  SETS_AL(); break;
      case EX_GE:  SETNS_AL(); break;
      default: assert(false); break;
      }
    }
    MOVZX_AL_EAX();
    return;

  case EX_LOGAND:
    {
      const char * l_false = alloc_label();
      const char * l_true = alloc_label();
      const char * l_next = alloc_label();
      gen_cond_jmp(expr->u.bop.lhs, false, l_false);
      gen_cond_jmp(expr->u.bop.rhs, true, l_true);
      ADD_LABEL(l_false);
      XOR_EAX_EAX();  // 0
      JMP8(l_next);
      ADD_LABEL(l_true);
      MOV_IM32_EAX(1);
      ADD_LABEL(l_next);
    }
    return;

  case EX_LOGIOR:
    {
      const char * l_false = alloc_label();
      const char * l_true = alloc_label();
      const char * l_next = alloc_label();
      gen_cond_jmp(expr->u.bop.lhs, true, l_true);
      gen_cond_jmp(expr->u.bop.rhs, false, l_false);
      ADD_LABEL(l_true);
      MOV_IM32_EAX(1);
      JMP8(l_next);
      ADD_LABEL(l_false);
      XOR_EAX_EAX();  // 0
      ADD_LABEL(l_next);
    }
    return;

  case EX_ADD:
  case EX_SUB:
  case EX_MUL:
  case EX_DIV:
  case EX_MOD:
  case EX_LSHIFT:
  case EX_RSHIFT:
  case EX_BITAND:
  case EX_BITOR:
  case EX_BITXOR:
    gen_expr(expr->u.bop.rhs);
    PUSH_RAX();
    gen_expr(expr->u.bop.lhs);

    POP_RDI();

    gen_arith(expr->type, expr->valType->type, expr->u.bop.rhs->valType->type);
    return;

  default:
    fprintf(stderr, "Expr type=%d, ", expr->type);
    assert(!"Unhandled in gen_expr");
    break;
  }
}

void gen(Node *node) {
  switch (node->type) {
  case ND_EXPR:
    gen_expr(node->u.expr);
    return;

  case ND_DEFUN:
    gen_defun(node);
    return;

  case ND_RETURN:
    gen_return(node);
    return;

  case ND_BLOCK:
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
    break;

  case ND_IF:
    gen_if(node);
    break;

  case ND_SWITCH:
    gen_switch(node);
    break;

  case ND_CASE:
    gen_case(node);
    break;

  case ND_DEFAULT:
    gen_default();
    break;

  case ND_WHILE:
    gen_while(node);
    break;

  case ND_DO_WHILE:
    gen_do_while(node);
    break;

  case ND_FOR:
    gen_for(node);
    break;

  case ND_BREAK:
    gen_break();
    break;

  case ND_CONTINUE:
    gen_continue();
    break;

  case ND_GOTO:
    gen_goto(node);
    break;

  case ND_LABEL:
    gen_label(node);
    break;

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
  asm_fp = fp;
}

void output_section(FILE* fp, int section) {
  Section *p = &sections[section];
  unsigned char *buf = p->buf;
  fwrite(buf, p->size, 1, fp);
}
