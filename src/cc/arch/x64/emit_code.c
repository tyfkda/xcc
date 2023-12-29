#include "../../../config.h"
#include "./arch_config.h"
#include "emit_code.h"

#include <assert.h>
#include <inttypes.h>  // PRId64
#include <stdlib.h>
#include <string.h>

#include "ast.h"
#include "codegen.h"
#include "initializer.h"  // calc_bitfield_initial_value
#include "ir.h"
#include "lexer.h"
#include "regalloc.h"
#include "table.h"
#include "type.h"
#include "util.h"
#include "var.h"
#include "x64.h"

int count_callee_save_regs(unsigned long used, unsigned long fused);

char *im(int64_t x) {
  return fmt("$%" PRId64, x);
}

char *indirect(const char *base, const char *index, int scale) {
  if (index == NULL) {
    return fmt("(%s)", base);
  } else {
    if (scale == 1)
      return fmt("(%s,%s)", base, index);
    else
      return fmt("(%s,%s,%d)", base, index, scale);
  }
}

char *offset_indirect(int offset, const char *base, const char *index, int scale) {
  if (offset == 0)
    return indirect(base, index, scale);

  if (index == NULL) {
    return fmt("%d(%s)", offset, base);
  } else {
    if (scale == 1)
      return fmt("%d(%s,%s)", offset, base, index);
    else
      return fmt("%d(%s,%s,%d)", offset, base, index, scale);
  }
}

char *label_indirect(const char *label, const char *reg) {
  return fmt("%s(%s)", label, reg);
}

char *gotpcrel(char *label) {
  return fmt("%s@GOTPCREL", label);
}

////////

static void eval_initial_value(Expr *expr, Expr **pvar, Fixnum *poffset) {
  switch (expr->kind) {
  case EX_FIXNUM:
    *poffset = expr->fixnum;
    break;
  case EX_VAR:
    assert(*pvar == NULL);
    *pvar = expr;
    break;
  case EX_ADD:
  case EX_SUB:
    {
      Expr *var1 = NULL, *var2 = NULL;
      Fixnum offset1 = 0, offset2 = 0;
      eval_initial_value(expr->bop.lhs, &var1, &offset1);
      eval_initial_value(expr->bop.rhs, &var2, &offset2);
      if (var1 != NULL) {
        assert(var2 == NULL);
        *pvar = var1;
      } else if (var2 != NULL) {
        assert(expr->kind == EX_ADD);
        *pvar = var2;
      }
      if (expr->kind == EX_SUB)
        offset2 = -offset2;
      *poffset = offset1 + offset2;
    }
    break;
  case EX_REF:
  case EX_DEREF:
  case EX_CAST:
    eval_initial_value(expr->unary.sub, pvar, poffset);
    break;
  case EX_MEMBER:
    {
      eval_initial_value(expr->member.target, pvar, poffset);
      const MemberInfo *minfo = expr->member.info;
      *poffset += minfo->offset;
    }
    break;
  case EX_COMPLIT:
    assert(expr->complit.var->kind == EX_VAR);
    eval_initial_value(expr->complit.var, pvar, poffset);
    break;
  // case EX_STR:  // should be handled in parser.
  default: assert(!"illegal"); break;
  }
}

#ifndef __NO_BITFIELD
static int construct_initial_value_bitfield(const StructInfo *sinfo, const Initializer *init,
                                            int start, int *poffset) {
  const MemberInfo *member = &sinfo->members[start];
  if (member->bitfield.width == 0)
    return start;

  const Type *et = get_fixnum_type(member->bitfield.base_kind, false, 0);
  int offset = *poffset;
  int align = align_size(et);
  if (offset % align != 0) {
    EMIT_ALIGN(align);
    offset = ALIGN(offset, align);
  }

  int i = start;
  Fixnum x = calc_bitfield_initial_value(sinfo, init, &i);

  const char *output = NUM(x);
  switch (et->fixnum.kind) {
  case FX_CHAR:  _BYTE(output); break;
  case FX_SHORT: _SHORT(output); break;
  case FX_LONG: case FX_LLONG:
    _QUAD(output);
    break;
  case FX_INT: case FX_ENUM:
    _LONG(output);
    break;
  }
  *poffset = offset += type_size(et);

  return i;
}
#endif

static void construct_initial_value(const Type *type, const Initializer *init) {
  assert(init == NULL || init->kind != IK_DOT);

  switch (type->kind) {
  case TY_FLONUM:
#ifndef __NO_FLONUM
    switch (type->flonum.kind) {
    case FL_DOUBLE:
    case FL_LDOUBLE:  // long-double in XCC is same as double.
      {
        union {double f; uint64_t h;} v;
        v.f = 0;
        if (init != NULL) {
          assert(init->kind == IK_SINGLE);
          Expr *value = init->single;
          if (!(is_const(value) && is_flonum(value->type)))
            error("Illegal initializer: constant number expected");
          v.f = value->flonum;
        }
#if 0
        _DOUBLE(FLONUM(v.d));
#else
        _QUAD(HEXNUM(v.h));
#endif
      }
      break;
    case FL_FLOAT:
      {
        union {float f; uint32_t h;} v;
        v.f = 0;
        if (init != NULL) {
          assert(init->kind == IK_SINGLE);
          Expr *value = init->single;
          if (!(is_const(value) && is_flonum(value->type)))
            error("Illegal initializer: constant number expected");
          v.f = value->flonum;
        }
#if 0
        _FLOAT(FLONUM(v.f));
#else
        _LONG(HEXNUM(v.h));
#endif
      }
      break;
    }
#else
    assert(false);
#endif
    break;
  case TY_FIXNUM:
  case TY_PTR:
    {
      Expr *var = NULL;
      Fixnum offset = 0;
      if (init != NULL) {
        assert(init->kind == IK_SINGLE);
        eval_initial_value(init->single, &var, &offset);
      }
      const char *output;
      if (var == NULL) {
        output = NUM(offset);
      } else {
        const Name *name = var->var.name;
        Scope *scope;
        VarInfo *varinfo = scope_find(var->var.scope, name, &scope);
        assert(varinfo != NULL);
        if (!is_global_scope(scope) && varinfo->storage & VS_STATIC) {
          varinfo = varinfo->static_.gvar;
          assert(varinfo != NULL);
          name = varinfo->name;
        }

        char *label = fmt_name(name);
        if ((varinfo->storage & VS_STATIC) == 0)
          label = MANGLE(label);
        label = quote_label(label);

        if (offset == 0) {
          output = label;
        } else {
          output = fmt("%s + %" PRId64, label, offset);
        }
      }
      if (type->kind == TY_PTR) {
        _QUAD(output);
      } else {
        switch (type->fixnum.kind) {
        case FX_CHAR:  _BYTE(output); break;
        case FX_SHORT: _SHORT(output); break;
        case FX_LONG: case FX_LLONG:
          _QUAD(output);
          break;
        case FX_INT: case FX_ENUM:
          _LONG(output);
          break;
        }
      }
    }
    break;
  case TY_ARRAY:
    if (init == NULL || init->kind == IK_MULTI) {
      const Type *elem_type = type->pa.ptrof;
      ssize_t index = 0;
      if (init != NULL) {
        Vector *init_array = init->multi;
        for (ssize_t i = 0; i < init_array->len; ++i, ++index) {
          const Initializer *init_elem = init_array->data[i];
          construct_initial_value(elem_type, init_elem);
        }
      }
      // Padding
      for (ssize_t i = index, n = type->pa.length; i < n; ++i)
        construct_initial_value(elem_type, NULL);
      break;
    }
    if (init->kind == IK_SINGLE) {
      Expr *e = strip_cast(init->single);
      if (e->kind == EX_STR && is_char_type(type->pa.ptrof, e->str.kind)) {
        size_t src_size = e->str.len * type_size(e->type->pa.ptrof);
        size_t size = type_size(type);
        if (src_size > size)
          src_size = size;

        UNUSED(size);
        StringBuffer sb;
        sb_init(&sb);
        sb_append(&sb, "\"", NULL);
        escape_string(e->str.buf, src_size, &sb);
        if (size > src_size) {
          const char NULCHR[] = "\\0";
          for (size_t i = 0, n = size - src_size; i < n; ++i)
            sb_append(&sb, NULCHR, NULL);
        }
        sb_append(&sb, "\"", NULL);
        _ASCII(sb_to_string(&sb));
        break;
      }
    }
    error("Illegal initializer");
    break;
  case TY_STRUCT:
    {
      const StructInfo *sinfo = type->struct_.info;
      assert(init == NULL || (init->kind == IK_MULTI && init->multi->len == sinfo->member_count));
      int count = 0;
      int offset = 0;
      for (int i = 0, n = sinfo->member_count; i < n; ++i) {
        const MemberInfo *member = &sinfo->members[i];
#ifndef __NO_BITFIELD
        if (member->bitfield.width >= 0) {
          i = construct_initial_value_bitfield(sinfo, init, i, &offset);
          ++count;
          continue;
        }
#endif
        const Initializer *mem_init;
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
            EMIT_ALIGN(align);
            offset = ALIGN(offset, align);
          }
          construct_initial_value(member->type, mem_init);
          ++count;
          offset += type_size(member->type);
        }
      }
      if (sinfo->is_union && count <= 0) {
        const MemberInfo *member = &sinfo->members[0];
        construct_initial_value(member->type, NULL);
        offset += type_size(member->type);
      }

      size_t size = type_size(type);
      if (size != (size_t)offset) {
        // Put padding.
        int d = size - offset;
        switch (d) {
        case 1:  _BYTE(NUM(0)); break;
        case 2:  _SHORT(NUM(0)); break;
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
  case TY_FUNC: case TY_VOID: assert(false); break;
  }
}

static void emit_varinfo(const VarInfo *varinfo, const Initializer *init) {
  const Name *name = varinfo->name;
  if (init != NULL) {
    if (varinfo->type->qualifier & TQ_CONST)
      _RODATA();
    else
      _DATA();
  }

  char *label = fmt_name(name);
  if ((varinfo->storage & VS_STATIC) == 0) {  // global
    label = quote_label(MANGLE(label));
    _GLOBL(label);
  } else {
    label = quote_label(label);
    _LOCAL(label);
  }

  if (init != NULL) {
    EMIT_ALIGN(align_size(varinfo->type));
    EMIT_LABEL(label);
    construct_initial_value(varinfo->type, init);
  } else {
    size_t size = type_size(varinfo->type);
    if (size < 1)
      size = 1;

    size_t align = align_size(varinfo->type);
    _BSS(label, size, align);
  }
}

////////////////////////////////////////////////

static bool is_asm(Stmt *stmt) {
  return stmt->kind == ST_ASM;
}

static void move_params_to_assigned(Function *func) {
  extern const char *kRegSizeTable[][PHYSICAL_REG_MAX];
  extern const int ArchRegParamMapping[];
  extern const char *kFReg64s[];

  static const char *kRegParam8s[] = {DIL, SIL, DL, CL, R8B, R9B};
  static const char *kRegParam16s[] = {DI, SI, DX, CX, R8W, R9W};
  static const char *kRegParam32s[] = {EDI, ESI, EDX, ECX, R8D, R9D};
  static const char *kRegParam64s[] = {RDI, RSI, RDX, RCX, R8, R9};
  static const char **kRegParamTable[] = {kRegParam8s, kRegParam16s, kRegParam32s, kRegParam64s};
  static const char *kFRegParam64s[] = {XMM0, XMM1, XMM2, XMM3, XMM4, XMM5, XMM6, XMM7};
  static const int kPow2Table[] = {-1, 0, 1, -1, 2, -1, -1, -1, 3};
#define kPow2TableSize ((int)(sizeof(kPow2Table) / sizeof(*kPow2Table)))

  RegParamInfo iparams[MAX_REG_ARGS];
  RegParamInfo fparams[MAX_FREG_ARGS];
  int iparam_count = 0;
  int fparam_count = 0;
  enumerate_register_params(func, iparams, MAX_REG_ARGS, fparams, MAX_FREG_ARGS,
                            &iparam_count, &fparam_count);

  // Generate code to store parameters to the destination.
  for (int i = 0; i < iparam_count; ++i) {
    RegParamInfo *p = &iparams[i];
    VReg *vreg = p->vreg;
    size_t size = type_size(p->type);
    assert(0 < size && size < kPow2TableSize && kPow2Table[size] >= 0);
    int pow = kPow2Table[size];
    const char *src = kRegParamTable[pow][p->index];
    if (vreg->flag & VRF_SPILLED) {
      int offset = vreg->frame.offset;
      assert(offset != 0);
      MOV(src, OFFSET_INDIRECT(offset, RBP, NULL, 1));
    } else if (ArchRegParamMapping[p->index] != vreg->phys) {
      const char *dst = kRegSizeTable[pow][vreg->phys];
      MOV(src, dst);
    }
  }
  for (int i = 0; i < fparam_count; ++i) {
    RegParamInfo *p = &fparams[i];
    VReg *vreg = p->vreg;
    const char *src = kFRegParam64s[p->index];
    if (vreg->flag & VRF_SPILLED) {
      int offset = vreg->frame.offset;
      assert(offset != 0);
      const char *dst = OFFSET_INDIRECT(offset, RBP, NULL, 1);
      switch (p->type->flonum.kind) {
      case FL_FLOAT:   MOVSS(src, dst); break;
      case FL_DOUBLE: case FL_LDOUBLE:
        MOVSD(src, dst);
        break;
      }
    } else {
      if (p->index != vreg->phys) {
        const char *dst = kFReg64s[vreg->phys];
        switch (p->type->flonum.kind) {
        case FL_FLOAT:   MOVSS(src, dst); break;
        case FL_DOUBLE: case FL_LDOUBLE:
          MOVSD(src, dst);
          break;
        }
      }
    }
  }

  if (func->type->func.vaargs) {
    for (int i = iparam_count; i < MAX_REG_ARGS; ++i) {
      int offset = (i - MAX_REG_ARGS - MAX_FREG_ARGS) * POINTER_SIZE;
      MOV(kRegParam64s[i], OFFSET_INDIRECT(offset, RBP, NULL, 1));
    }
    for (int i = fparam_count; i < MAX_FREG_ARGS; ++i) {
      int offset = (i - MAX_FREG_ARGS) * POINTER_SIZE;
      MOVSD(kFRegParam64s[i], OFFSET_INDIRECT(offset, RBP, NULL, 1));
    }
  }
}

static void emit_defun(Function *func) {
  if (func->scopes == NULL ||  // Prototype definition.
      func->extra == NULL)     // Code emission is omitted.
    return;

  assert(stackpos == 8);

  emit_comment(NULL);
  _TEXT();

  bool global = true;
  const VarInfo *varinfo = scope_find(global_scope, func->name, NULL);
  if (varinfo != NULL) {
    global = (varinfo->storage & VS_STATIC) == 0;
  }

  char *label = fmt_name(func->name);
  if (global) {
    label = quote_label(MANGLE(label));
    _GLOBL(label);
  } else {
    label = quote_label(label);
    _LOCAL(label);
  }
  EMIT_LABEL(label);

  bool no_stmt = true;
  if (func->body_block != NULL) {
    Vector *stmts = func->body_block->block.stmts;
    for (int i = 0; i < stmts->len; ++i) {
      Stmt *stmt = stmts->data[i];
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
  FuncBackend *fnbe = func->extra;
  size_t frame_size = 0;
  bool rbp_saved = false;
  if (!no_stmt) {
    // Callee save.
    int callee_saved_count = push_callee_save_regs(fnbe->ra->used_reg_bits, fnbe->ra->used_freg_bits);

    // When function is called, return address is pused onto the stack by caller,
    // so default offset is 8.
    size_t frame_offset = 8;

    if (fnbe->frame_size > 0 || fnbe->ra->flag & RAF_STACK_FRAME) {
      PUSH(RBP); PUSH_STACK_POS();
      MOV(RSP, RBP);
      rbp_saved = true;
      // RBP is pushed so the 16-bytes-align offset becomes 0.
      frame_offset = 0;
    }

    size_t callee_saved_size = callee_saved_count * POINTER_SIZE;
    frame_size = fnbe->frame_size;
    if (func->flag & FUNCF_HAS_FUNCALL) {
      // Align frame size to 16 only it contains funcall.
      frame_size += -(fnbe->frame_size + callee_saved_size + frame_offset) & 15;
    }
    if (frame_size > 0) {
      SUB(IM(frame_size), RSP);
      stackpos += frame_size;
    }

    move_params_to_assigned(func);
  }

  emit_bb_irs(fnbe->bbcon);

  if (!function_not_returned(fnbe)) {
    // Epilogue
    if (!no_stmt) {
      if (rbp_saved) {
        MOV(RBP, RSP);
        stackpos -= frame_size;
        POP(RBP); POP_STACK_POS();
      } else if (frame_size > 0) {
        ADD(IM(frame_size), RSP);
        stackpos -= frame_size;
      }

      pop_callee_save_regs(fnbe->ra->used_reg_bits, fnbe->ra->used_freg_bits);
    }

    RET();

    assert(stackpos == 8);
  } else {
    stackpos = 8;
  }

  // Static variables are emitted through global variables.
}

static void emit_asm(Expr *asmstr) {
  assert(asmstr->kind == EX_STR);
  EMIT_ASM(asmstr->str.buf);
}

void emit_code(Vector *decls) {
  for (int i = 0, len = decls->len; i < len; ++i) {
    Declaration *decl = decls->data[i];
    if (decl == NULL)
      continue;

    switch (decl->kind) {
    case DCL_DEFUN:
      emit_defun(decl->defun.func);
      break;
    case DCL_VARDECL:
      break;
    case DCL_ASM:
      emit_asm(decl->asmstr);
      break;
    }
  }

  emit_comment(NULL);
  for (int i = 0; i < global_scope->vars->len; ++i) {
    VarInfo *varinfo = global_scope->vars->data[i];
    if ((varinfo->storage & (VS_EXTERN | VS_ENUM_MEMBER)) || varinfo->type->kind == TY_FUNC)
      continue;
    emit_varinfo(varinfo, varinfo->global.init);
  }
}
