#include <assert.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

#include "xcc.h"

#define CURIP(ofs)  (start_address + codesize + ofs)
#define ADD_CODE(...)  do { unsigned char buf[] = {__VA_ARGS__}; add_code(buf, sizeof(buf)); } while (0)
#include "x86_64.h"

char *strdup_(const char *str) {
  size_t len = strlen(str);
  char *dup = malloc(len + 1);
  strcpy(dup, str);
  return dup;
}

static void calc_struct_size(StructInfo *sinfo);

static int type_size(const Type *type) {
  switch (type->type) {
  case TY_VOID:
    return 1;  // ?
  case TY_CHAR:
    return 1;
  case TY_INT:
    return 4;
  case TY_PTR:
  case TY_FUNC:
    return 8;
  case TY_ARRAY:
    return type_size(type->ptrof) * type->array_size;
  case TY_STRUCT:
    if (type->struct_->size == 0)
      calc_struct_size(type->struct_);
    return type->struct_->size;
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
  case TY_INT:
    return 4;
  case TY_PTR:
  case TY_FUNC:
    return 8;
  case TY_ARRAY:
    return align_size(type->ptrof);
  case TY_STRUCT:
    calc_struct_size(type->struct_);
    return type->struct_->align;
  default:
    assert(false);
    return 1;
  }
}

static void calc_struct_size(StructInfo *sinfo) {
  int size = 0;
  int max_align = 1;
  for (int i = 0, len = sinfo->members->len; i < len; ++i) {
    VarInfo *varinfo = (VarInfo*)sinfo->members->data[i];
    int sz = type_size(varinfo->type);
    int align = align_size(varinfo->type);
    size = (size + align - 1) & -align;
    varinfo->offset = (int)size;
    size += sz;
    if (max_align < align)
      max_align = align;
  }
  size = (size + max_align - 1) & -max_align;
  if (size == 0)
    size = 1;
  sinfo->size = size;
  sinfo->align = max_align;
}

static void cast(const enum eType ltype, const enum eType rtype) {
  if (ltype == rtype)
    return;

  switch (ltype) {
  case TY_CHAR:
    switch (rtype) {
    case TY_INT:  return;
    case TY_LONG:  return;
    default: break;
    }
    break;
  case TY_INT:
    switch (rtype) {
    case TY_CHAR: MOVSX_AL_EAX(); return;
    case TY_LONG: return;
    default: break;
    }
    break;
  case TY_LONG:
    switch (rtype) {
    case TY_CHAR: MOVSX_AL_RAX(); return;
    case TY_INT:  MOVSX_EAX_RAX(); return;
    default: break;
    }
    break;
  case TY_PTR:
    switch (rtype) {
    default: break;
    }
    break;
  default:
    break;
  }

  assert(false);
}

static Map *label_map;

enum LocType {
  LOC_REL8,
  LOC_REL32,
};

typedef struct {
  enum LocType type;
  uintptr_t ip;
  const char *label;
  union {
    struct {
      uintptr_t base;
    } rel;
  };
} LocInfo;

static Vector *rodata_vector;

void add_rodata(const char *label, const void *data, size_t size) {
  RoData *ro = malloc(sizeof(*ro));
  ro->label = label;
  ro->data = data;
  ro->size = size;
  vec_push(rodata_vector, ro);
}

static uintptr_t start_address;
static unsigned char* code;
static size_t codesize;

void add_code(const unsigned char* buf, size_t size) {
  size_t newsize = codesize + size;
  code = realloc(code, newsize);
  if (code == NULL)
    error("not enough memory");
  memcpy(code + codesize, buf, size);
  codesize = newsize;
}

// Put label at the current.
void add_label(const char *label) {
  map_put(label_map, label, (void*)CURIP(0));
}

uintptr_t label_adr(const char *label) {
  void *adr = map_get(label_map, label);
  return adr != NULL ? (uintptr_t)adr : (uintptr_t)-1;
}

static char *alloc_label(void) {
  static int label_no;
  ++label_no;
  char buf[sizeof(int) * 3 + 1];
  snprintf(buf, sizeof(buf), ".L%d", label_no);
  char *dup = strdup_(buf);
  add_label(dup);
  return dup;
}

Vector *loc_vector;

static LocInfo *new_loc(enum LocType type, uintptr_t ip, const char *label) {
  LocInfo *loc = malloc(sizeof(*loc));
  loc->type = type;
  loc->ip = ip;
  loc->label = label;
  vec_push(loc_vector, loc);
  return loc;
}

void add_loc_rel8(const char *label, int ofs, int baseofs) {
  uintptr_t ip = codesize + ofs;
  uintptr_t base = CURIP(baseofs);
  LocInfo *loc = new_loc(LOC_REL8, ip, label);
  loc->rel.base = base;
}

void add_loc_rel32(const char *label, int ofs, int baseofs) {
  uintptr_t ip = codesize + ofs;
  uintptr_t base = CURIP(baseofs);
  LocInfo *loc = new_loc(LOC_REL32, ip, label);
  loc->rel.base = base;
}

size_t fixup_locations(void) {
  // Output RoData
  for (int i = 0, len = rodata_vector->len; i < len; ++i) {
    const RoData *ro = (const RoData*)rodata_vector->data[i];
    add_label(ro->label);
    add_code(ro->data, ro->size);
  }

  // Global
  for (int i = 0, len = map_count(global); i < len; ++i) {
    const char *name = (const char *)global->keys->data[i];
    const VarInfo *varinfo = (const VarInfo*)global->vals->data[i];
    if (varinfo->type->type == TY_FUNC)
      continue;
    add_label(name);
    int size = type_size(varinfo->type);
    unsigned char *buf = calloc(size, 1);
    add_code(buf, size);
  }

  // Resolve label locations.
  for (int i = 0; i < loc_vector->len; ++i) {
    LocInfo *loc = loc_vector->data[i];
    void *val = map_get(label_map, loc->label);
    if (val == NULL) {
      error("Cannot find label: `%s'", loc->label);
    }

    intptr_t v = (intptr_t)val;
    switch (loc->type) {
    case LOC_REL8:
      {
        intptr_t d = v - loc->rel.base;
        // TODO: Check out of range
        code[loc->ip] = d;
      }
      break;
    case LOC_REL32:
      {
        intptr_t d = v - loc->rel.base;
        // TODO: Check out of range
        code[loc->ip    ] = d;
        code[loc->ip + 1] = d >> 8;
        code[loc->ip + 2] = d >> 16;
        code[loc->ip + 3] = d >> 24;
      }
      break;
    default:
      assert(false);
      break;
    }
  }

  return codesize;
}

//

typedef struct LoopInfo {
  struct LoopInfo *outer;
  const char *l_break;
  const char *l_continue;
} LoopInfo;

static Node *curfunc;
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

static void gen_lval(Node *node);

static void gen_rval(Node *node) {
  gen(node);  // ?
}

static void gen_ref(Node *node) {
  gen_lval(node);
}

static void gen_lval(Node *node) {
  switch (node->type) {
  case ND_VARREF:
    if (node->varref.global) {
      LEA_OFS32_RIP_RAX(node->varref.ident);
    } else {
      int varidx = var_find(curfunc->defun.lvars, node->varref.ident);
      assert(varidx >= 0);
      int offset = ((VarInfo*)curfunc->defun.lvars->data[varidx])->offset;
      MOV_RBP_RAX();
      ADD_IM32_RAX(offset);
    }
    break;
  case ND_DEREF:
    gen_rval(node->unary.sub);
    break;
  case ND_MEMBER:
    {
      const Type *type = node->member.target->expType;
      if (type->type == TY_PTR)
        type = type->ptrof;
      assert(type->type == TY_STRUCT);
      Vector *members = type->struct_->members;
      int varidx = var_find(members, node->member.name);
      assert(varidx >= 0);
      VarInfo *varinfo = (VarInfo*)members->data[varidx];

      if (node->member.target->expType->type == TY_PTR)
        gen(node->member.target);
      else
        gen_ref(node->member.target);
      if (varinfo->offset != 0)
        ADD_IM32_RAX(varinfo->offset);
    }
    break;
  default:
    error("No lvalue: %d", node->type);
    break;
  }
}

static void gen_cond_jmp(Node *cond, bool tf, const char *label) {
  gen(cond);

  switch (cond->expType->type) {
  case TY_CHAR: CMP_IM8_AL(0); break;
  case TY_INT:  CMP_IM8_EAX(0); break;
  case TY_PTR:  CMP_IM8_RAX(0); break;
  default: assert(false); break;
  }

  if (tf)
    JNE32(label);
  else
    JE32(label);
}

static void gen_varref(Node *node) {
  gen_lval(node);
  VarInfo *varinfo;
  if (node->varref.global) {
    varinfo = find_global(node->varref.ident);
  } else {
    int varidx = var_find(curfunc->defun.lvars, node->varref.ident);
    assert(varidx >= 0);
    varinfo = (VarInfo*)curfunc->defun.lvars->data[varidx];
  }
  switch (node->expType->type) {
  case TY_CHAR:  MOV_IND_RAX_AL(); break;
  case TY_INT:   MOV_IND_RAX_EAX(); break;
  case TY_LONG:  MOV_IND_RAX_RAX(); break;
  case TY_PTR:
    if (varinfo->type->type != TY_ARRAY)  // If the variable is array, use variable address as a pointer.
      MOV_IND_RAX_RAX();
    break;
  default: assert(false); break;
  }
}

static void gen_defun(Node *node) {
  curfunc = node;
  add_label(node->defun.name);
  node->defun.ret_label = alloc_label();

  // Calc local variable offsets.
  // Map parameters from the bottom (to reduce offsets).
  int frame_size = 0;
  for (int i = 0; i < node->defun.lvars->len; ++i) {
    VarInfo *lvar = (VarInfo*)node->defun.lvars->data[i];
    int size = type_size(lvar->type);
    int align = align_size(lvar->type);
    frame_size = (frame_size + size + align - 1) & -align;
    lvar->offset = -frame_size;
  }

  // Prologue
  // Allocate variable bufer.
  Vector *lvars = node->defun.lvars;
  PUSH_RBP();
  MOV_RSP_RBP();
  if (frame_size > 0) {
    SUB_IM32_RSP(frame_size);
    // Store parameters into local frame.
    int len = len = node->defun.param_count;
    if (len > 6)
      error("Parameter count exceeds 6 (%d)", len);
    for (int i = 0; i < len; ++i) {
      const VarInfo *varinfo = (const VarInfo*)lvars->data[i];
      int offset = varinfo->offset;
      switch (varinfo->type->type) {
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
      case TY_INT:  // 4
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
      default:  // 8
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
      }
    }
  }

  // Statements
  for (int i = 0; i < node->defun.stmts->len; ++i) {
    gen((Node*)node->defun.stmts->data[i]);
  }

  // Epilogue
  add_label(node->defun.ret_label);
  MOV_RBP_RSP();
  POP_RBP();
  RET();
  curfunc = NULL;
}

static void gen_return(Node *node) {
  if (node->return_.val != NULL)
    gen(node->return_.val);
  assert(curfunc != NULL);
  JMP32(curfunc->defun.ret_label);
}

static void gen_funcall(Node *node) {
  Vector *args = node->funcall.args;
  if (args != NULL) {
    int len = args->len;
    if (len > 6)
      error("Param count exceeds 6 (%d)", len);

    for (int i = 0; i < len; ++i) {
      gen((Node*)args->data[i]);
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
  Node *func = node->funcall.func;
  if (func->type == ND_VARREF && func->varref.global) {
    CALL(func->varref.ident);
  } else {
    gen(func);
    CALL_IND_RAX();
  }
}

static void gen_if(Node *node) {
  const char * flabel = alloc_label();
  gen_cond_jmp(node->if_.cond, false, flabel);
  gen(node->if_.tblock);
  if (node->if_.fblock == NULL) {
    add_label(flabel);
  } else {
    const char * nlabel = alloc_label();
    JMP32(nlabel);
    add_label(flabel);
    gen(node->if_.fblock);
    add_label(nlabel);
  }
}

static void gen_while(Node *node) {
  const char *save_break, *save_cont;
  const char *l_cond = push_continue_label(&save_cont);
  const char *l_break = push_break_label(&save_break);
  const char *l_loop = alloc_label();
  JMP32(l_cond);
  add_label(l_loop);
  gen(node->while_.body);
  add_label(l_cond);
  gen_cond_jmp(node->while_.cond, true, l_loop);
  add_label(l_break);
  pop_continue_label(save_cont);
  pop_break_label(save_break);
}

static void gen_do_while(Node *node) {
  const char *save_break, *save_cont;
  const char *l_cond = push_continue_label(&save_cont);
  const char *l_break = push_break_label(&save_break);
  const char * l_loop = alloc_label();
  add_label(l_loop);
  gen(node->do_while.body);
  add_label(l_cond);
  gen_cond_jmp(node->do_while.cond, true, l_loop);
  add_label(l_break);
  pop_continue_label(save_cont);
  pop_break_label(save_break);
}

static void gen_for(Node *node) {
  const char *save_break, *save_cont;
  const char *l_continue = push_continue_label(&save_cont);
  const char *l_break = push_break_label(&save_break);
  const char * l_cond = alloc_label();
  if (node->for_.pre != NULL)
    gen(node->for_.pre);
  add_label(l_cond);
  if (node->for_.cond != NULL) {
    gen_cond_jmp(node->for_.cond, false, l_break);
  }
  gen(node->for_.body);
  add_label(l_continue);
  if (node->for_.post != NULL)
    gen(node->for_.post);
  JMP32(l_cond);
  add_label(l_break);
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

static void gen_arith(enum NodeType nodeType, enum eType expType) {
  // lhs=rax, rhs=rdi, result=rax

  switch (nodeType) {
  case ND_ADD:
    switch (expType) {
    case TY_INT:  ADD_EDI_EAX(); break;
    case TY_CHAR: ADD_DIL_AL(); break;
    default: assert(false); break;
    }
    break;

  case ND_SUB:
    switch (expType) {
    case TY_INT:  SUB_EDI_EAX(); break;
    case TY_CHAR: SUB_DIL_AL(); break;
    default: assert(false); break;
    }
    break;

  case ND_MUL:
    switch (expType) {
    case TY_INT:  MUL_EDI(); break;
    case TY_CHAR: MUL_DIL(); break;
    default: assert(false); break;
    }

    break;

  case ND_DIV:
    MOV_IM32_RDX(0);
    switch (expType) {
    case TY_INT:  DIV_EDI(); break;
    case TY_CHAR: DIV_DIL(); break;
    default: assert(false); break;
    }
    break;

  case ND_MOD:
    MOV_IM32_RDX(0);
    switch (expType) {
    case TY_INT:  DIV_EDI(); MOV_EDX_EAX(); break;
    case TY_CHAR: DIV_DIL(); MOV_DL_AL(); break;
    default: assert(false); break;
    }
    break;
  default:
    assert(false);
    break;
  }
}

void gen(Node *node) {
  switch (node->type) {
  case ND_INT:
    MOV_IM32_EAX(node->intval);
    return;

  case ND_CHAR:
    MOV_IM8_AL(node->charval);
    return;

  case ND_LONG:
    if (node->longval < 0x7fffffffL && node->longval >= -0x80000000L)
      MOV_IM32_RAX(node->longval);
    else
      MOV_IM64_RAX(node->longval);
    return;

  case ND_STR:
    {
      const char * label = alloc_label();
      add_rodata(label, node->str, strlen(node->str) + 1);
      LEA_OFS32_RIP_RAX(label);
    }
    return;

  case ND_VARREF:
    gen_varref(node);
    return;

  case ND_REF:
    gen_ref(node->unary.sub);
    return;

  case ND_DEREF:
    gen_rval(node->unary.sub);
    switch (node->expType->type) {
    case TY_CHAR:  MOV_IND_RAX_AL(); break;
    case TY_INT:   MOV_IND_RAX_EAX(); break;
    case TY_PTR:   MOV_IND_RAX_RAX(); break;
    default: assert(false); break;
    }
    return;

  case ND_MEMBER:
    gen_lval(node);
    switch (node->expType->type) {
    case TY_CHAR:
      MOV_IND_RAX_AL();
      break;
    case TY_INT:
      MOV_IND_RAX_EAX();
      break;
    case TY_PTR:
      MOV_IND_RAX_RAX();
      break;
    default:
      assert(false);
      break;
    }
    return;

  case ND_CAST:
    gen(node->cast.sub);
    cast(node->expType->type, node->cast.sub->expType->type);
    break;

  case ND_ASSIGN:
    gen_lval(node->bop.lhs);
    PUSH_RAX();
    gen(node->bop.rhs);

    POP_RDI();
    switch (node->bop.lhs->expType->type) {
    case TY_CHAR:  MOV_AL_IND_RDI(); break;
    case TY_INT:   MOV_EAX_IND_RDI(); break;
    default:
    case TY_PTR:   MOV_RAX_IND_RDI(); break;
    }
    return;

  case ND_ASSIGN_WITH:
    {
      Node *sub = node->unary.sub;
      gen(sub->bop.rhs);
      PUSH_RAX();
      gen_lval(sub->bop.lhs);
      MOV_RAX_RSI();  // Save lhs address to %rsi.

      // Move lhs to %?ax
      switch (node->bop.lhs->expType->type) {
      case TY_CHAR:  MOV_IND_RAX_AL(); break;
      case TY_INT:   MOV_IND_RAX_EAX(); break;
      default:
      case TY_PTR:   MOV_IND_RAX_RAX(); break;
      }

      POP_RDI();  // %rdi=rhs
      gen_arith(sub->type, sub->expType->type);
      cast(node->expType->type, sub->expType->type);

      switch (node->expType->type) {
      case TY_CHAR:  MOV_AL_IND_RSI(); break;
      case TY_INT:   MOV_EAX_IND_RSI(); break;
      default:
      case TY_PTR:   MOV_RAX_IND_RSI(); break;
      }
    }
    return;

  case ND_PREINC:
  case ND_PREDEC:
    gen_lval(node->unary.sub);
    switch (node->expType->type) {
    case TY_CHAR:
      if (node->type == ND_PREINC)  INCB_IND_RAX();
      else                          DECB_IND_RAX();
      MOV_IND_RAX_RAX();
      break;
    case TY_INT:
      if (node->type == ND_PREINC)  INCL_IND_RAX();
      else                          DECL_IND_RAX();
      MOV_IND_RAX_RAX();
      break;
    case TY_PTR:
      {
        MOV_RAX_RDI();
        int size = type_size(node->expType->ptrof);
        MOV_IM32_RAX(node->type == ND_PREINC ? size : -size);
        ADD_IND_RDI_RAX();
        MOV_RAX_IND_RDI();
      }
      break;
    default:
      assert(false);
      break;
    }
    return;

  case ND_POSTINC:
  case ND_POSTDEC:
    gen_lval(node->unary.sub);
    MOV_IND_RAX_RDI();
    switch (node->expType->type) {
    case TY_CHAR:
      if (node->type == ND_POSTINC)  INCB_IND_RAX();
      else                           DECB_IND_RAX();
      break;
    case TY_INT:
      if (node->type == ND_POSTINC)  INCL_IND_RAX();
      else                           DECL_IND_RAX();
      break;
    case TY_PTR:
      {
        int size = type_size(node->expType->ptrof);
        if (node->type == ND_POSTINC)  ADD_IM32_RAX(size);
        else                           SUB_IM32_RAX(size);
      }
      break;
    default:
      assert(false);
      break;
    }
    MOV_RDI_RAX();
    return;

  case ND_DEFUN:
    gen_defun(node);
    return;

  case ND_RETURN:
    gen_return(node);
    return;

  case ND_FUNCALL:
    gen_funcall(node);
    return;

  case ND_BLOCK:
    if (node->block.nodes != NULL) {
      for (int i = 0, len = node->block.nodes->len; i < len; ++i)
        gen((Node*)node->block.nodes->data[i]);
    }
    break;

  case ND_IF:
    gen_if(node);
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

  case ND_NEG:
    gen(node->unary.sub);
    switch (node->expType->type) {
    case TY_INT:  NEG_EAX(); break;
    case TY_CHAR: NEG_AL(); break;
    default:  assert(false); break;
    }
    break;

  case ND_NOT:
    gen(node->unary.sub);
    switch (node->expType->type) {
    case TY_INT:  CMP_IM8_EAX(0); break;
    case TY_CHAR: CMP_IM8_AL(0); break;
    case TY_PTR:  CMP_IM8_RAX(0); break;
    default:  assert(false); break;
    }
    SETE_AL();
    MOVZX_AL_EAX();
    break;

  case ND_EQ:
  case ND_NE:
  case ND_LT:
  case ND_GT:
  case ND_LE:
  case ND_GE:
    {
      enum NodeType type = node->type;
      Node *lhs = node->bop.lhs;
      Node *rhs = node->bop.rhs;
      if (type == ND_LE || type == ND_GT) {
        Node *tmp = lhs; lhs = rhs; rhs = tmp;
        type = type == ND_LE ? ND_GE : ND_LT;
      }

      gen(lhs);
      PUSH_RAX();
      gen(rhs);

      POP_RDI();
      switch (lhs->expType->type) {
      case TY_INT:  CMP_EAX_EDI(); break;
      case TY_CHAR: CMP_AL_DIL(); break;
      case TY_PTR:  CMP_RAX_RDI(); break;
      default: assert(false); break;
      }

      switch (type) {
      case ND_EQ:  SETE_AL(); break;
      case ND_NE:  SETNE_AL(); break;
      case ND_LT:  SETS_AL(); break;
      case ND_GE:  SETNS_AL(); break;
      default: assert(false); break;
      }
    }
    MOVZX_AL_EAX();
    return;

  case ND_LOGAND:
    {
      const char * l_false = alloc_label();
      const char * l_true = alloc_label();
      const char * l_next = alloc_label();
      gen_cond_jmp(node->bop.lhs, false, l_false);
      gen_cond_jmp(node->bop.rhs, true, l_true);
      add_label(l_false);
      MOV_IM32_EAX(0);
      JMP8(l_next);
      add_label(l_true);
      MOV_IM32_EAX(1);
      add_label(l_next);
    }
    return;

  case ND_LOGIOR:
    {
      const char * l_false = alloc_label();
      const char * l_true = alloc_label();
      const char * l_next = alloc_label();
      gen_cond_jmp(node->bop.lhs, true, l_true);
      gen_cond_jmp(node->bop.rhs, false, l_false);
      add_label(l_true);
      MOV_IM32_EAX(1);
      JMP8(l_next);
      add_label(l_false);
      MOV_IM32_EAX(0);
      add_label(l_next);
    }
    return;

  case ND_PTRADD:
    {
      Node *lhs = node->bop.lhs, *rhs = node->bop.rhs;
      gen(rhs);
      cast(TY_INT, rhs->expType->type);  // TODO: Fix
      long size = type_size(lhs->expType->ptrof);
      if (size != 1) {
        MOV_IM32_EDI(size);
        MUL_EDI();
      }
      PUSH_RAX();
      gen(lhs);
      POP_RDI();
      ADD_RDI_RAX();
      break;
    }
    return;

  case ND_PTRSUB:
    {
      Node *lhs = node->bop.lhs, *rhs = node->bop.rhs;
      gen(rhs);
      cast(TY_INT, rhs->expType->type);  // TODO: Fix
      int size = type_size(node->bop.lhs->expType->ptrof);
      if (size != 1) {
        MOV_IM64_RDI((long)size);
        MUL_RDI();
      }
      PUSH_RAX();
      gen(lhs);
      POP_RDI();
      SUB_RDI_RAX();
    }
    return;

  case ND_PTRDIFF:
    {
      gen(node->bop.rhs);
      PUSH_RAX();
      gen(node->bop.lhs);
      POP_RDI();
      SUB_RDI_RAX();

      int size = type_size(node->bop.lhs->expType->ptrof);
      switch (size) {
      case 1:  break;
      case 2:  SAR_RAX(); break;
      case 4:  SAR_IM8_RAX(2); break;
      case 8:  SAR_IM8_RAX(3); break;
      default:
        MOV_IM64_RDI((long)size);
        MOV_IM32_RDX(0);
        DIV_RDI();
        break;
      }
    }
    return;

  case ND_ADD:
  case ND_SUB:
  case ND_MUL:
  case ND_DIV:
  case ND_MOD:
    gen(node->bop.rhs);
    PUSH_RAX();
    gen(node->bop.lhs);

    POP_RDI();

    gen_arith(node->type, node->expType->type);
    return;

  default:
    error("Unhandled node: %d", node->type);
    break;
  }
}

void init_gen(uintptr_t start_address_) {
  start_address = start_address_;
  label_map = new_map();
  rodata_vector = new_vector();
}

void output_code(FILE* fp) {
  fwrite(code, codesize, 1, fp);
}
