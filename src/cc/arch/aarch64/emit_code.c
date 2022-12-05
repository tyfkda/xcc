#include "../config.h"
#include "emit_code.h"

#include <assert.h>
#include <inttypes.h>  // PRId64
#include <stdlib.h>
#include <string.h>

#include "aarch64.h"
#include "ast.h"
#include "codegen.h"
#include "ir.h"
#include "lexer.h"
#include "regalloc.h"
#include "table.h"
#include "type.h"
#include "util.h"
#include "var.h"

char *im(int64_t x) {
  return fmt("#%" PRId64, x);
}

char *immediate_offset(const char *reg, int offset) {
  return offset != 0 ? fmt("[%s,#%d]", reg, offset) : fmt("[%s]", reg);
}

char *pre_index(const char *reg, int offset) {
  return fmt("[%s,#%d]!", reg, offset);
}

char *post_index(const char *reg, int offset) {
  return fmt("[%s],#%d", reg, offset);
}

char *reg_offset(const char *base, const char *reg, const char *shift) {
  if (shift != NULL)
    return fmt("[%s,%s,%s]", base, reg, shift);
  return fmt("[%s,%s]", base, reg);
}

char *label_at_page(char *label, int flag) {
#ifdef __APPLE__
  static const char *s[] = {
    "%s@PAGE", "%s@PAGEOFF",
    "%s@GOTPAGE", "%s@GOTPAGEOFF",
  };
#else
  static const char *s[] = {
    "%s", ":lo12:%s",
    ":got:%s", ":got_lo12:%s",
  };
#endif
  return fmt(s[flag], label);
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

      const Type *type = expr->member.target->type;
      if (ptr_or_array(type))
        type = type->pa.ptrof;
      assert(type->kind == TY_STRUCT);
      const Vector *members = type->struct_.info->members;
      const MemberInfo *member = members->data[expr->member.index];
      *poffset += member->offset;
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

static void construct_initial_value(const Type *type, const Initializer *init) {
  assert(init == NULL || init->kind != IK_DOT);

  switch (type->kind) {
#ifndef __NO_FLONUM
  case TY_FLONUM:
    switch (type->flonum.kind) {
    case FL_DOUBLE:
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
    break;
#endif
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
        case FX_SHORT: _WORD(output); break;
        case FX_LONG:  _QUAD(output); break;
        case FX_LLONG: _QUAD(output); break;
        default:
          assert(false);
          // Fallthrough
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
          if (init_elem->kind == IK_ARR) {
            ssize_t next = init_elem->arr.index->fixnum;
            for (ssize_t j = index; j < next; ++j)
              construct_initial_value(elem_type, NULL);
            index = next;
            init_elem = init_elem->arr.value;
          }
          construct_initial_value(elem_type, init_elem);
        }
      }
      // Padding
      for (ssize_t i = index, n = type->pa.length; i < n; ++i)
        construct_initial_value(elem_type, NULL);
      break;
    }
    if (init->kind == IK_SINGLE && is_char_type(type->pa.ptrof)) {
      Expr *e = strip_cast(init->single);
      if (e->kind == EX_STR) {
        size_t src_size = e->str.size;
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
      assert(init == NULL || (init->kind == IK_MULTI && init->multi->len == sinfo->members->len));
      int count = 0;
      int offset = 0;
      for (int i = 0, n = sinfo->members->len; i < n; ++i) {
        const MemberInfo *member = sinfo->members->data[i];
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
          offset = ALIGN(offset, align);
          offset += type_size(member->type);
        }
      }
      if (sinfo->is_union && count <= 0) {
        const MemberInfo *member = sinfo->members->data[0];
        construct_initial_value(member->type, NULL);
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
    //size_t size = type_size(varinfo->type);
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

static VarInfo *find_ret_var(Scope *scope) {
  const Name *retval_name = alloc_name(RET_VAR_NAME, NULL, false);
  return scope_find(scope, retval_name, NULL);
}

static void put_args_to_stack(Function *func) {
  static const char *kReg32s[] = {W0, W1, W2, W3, W4, W5, W6, W7};
  static const char *kReg64s[] = {X0, X1, X2, X3, X4, X5, X6, X7};
  static const char **kRegTable[] = {kReg32s, kReg32s, kReg32s, kReg64s};
#ifndef __NO_FLONUM
  const char *kFReg32s[] = {S0, S1, S2, S3, S4, S5, S6, S7};
  const char *kFReg64s[] = {D0, D1, D2, D3, D4, D5, D6, D7};
#endif
  static const int kPow2Table[] = {-1, 0, 1, -1, 2, -1, -1, -1, 3};
#define kPow2TableSize ((int)(sizeof(kPow2Table) / sizeof(*kPow2Table)))

  int arg_index = 0;
  if (is_stack_param(func->type->func.ret)) {
    Scope *top_scope = func->scopes->data[0];
    VarInfo *varinfo = find_ret_var(top_scope);
    assert(varinfo != NULL);
    const Type *type = varinfo->type;
    int size = type_size(type);
    assert(0 <= size && size < kPow2TableSize);
    int pow = kPow2Table[size];
    const char *src = kRegTable[pow][0];
    int offset = varinfo->local.reg->offset;
    const char *dst = IMMEDIATE_OFFSET(FP, offset);
    switch (pow) {
    case 0:          STRB(src, dst); break;
    case 1:          STRH(src, dst); break;
    case 2: case 3:  STR(src, dst); break;
    default: assert(false); break;
    }
    ++arg_index;
  }

  // Store arguments into local frame.
  const Vector *params = func->type->func.params;
  if (params == NULL)
    return;

  int len = params->len;
#ifdef VAARG_ON_STACK
  bool vaargs = false;
#else
  bool vaargs = func->type->func.vaargs;
#endif
  if (!vaargs) {
#ifndef __NO_FLONUM
    int farg_index = 0;
#endif
    for (int i = 0; i < len; ++i) {
      const VarInfo *varinfo = params->data[i];
      const Type *type = varinfo->type;
      int offset = varinfo->local.reg->offset;

      if (is_stack_param(type))
        continue;

#ifndef __NO_FLONUM
      if (is_flonum(type)) {
        if (farg_index < MAX_FREG_ARGS) {
          switch (type->flonum.kind) {
          case FL_FLOAT:   STR(kFReg32s[farg_index], IMMEDIATE_OFFSET(FP, offset)); break;
          case FL_DOUBLE:  STR(kFReg64s[farg_index], IMMEDIATE_OFFSET(FP, offset)); break;
          default: assert(false); break;
          }
          ++farg_index;
        }
        continue;
      }
#endif

      switch (type->kind) {
      case TY_FIXNUM:
      case TY_PTR:
        break;
      default: assert(false); break;
      }

      if (arg_index < MAX_REG_ARGS) {
        int size = type_size(type);
        assert(0 <= size && size < kPow2TableSize);
        int pow = kPow2Table[size];
        const char *src = kRegTable[pow][arg_index];
        assert(offset < 0);
        const char *dst;
        if (offset >= -256) {
          dst = IMMEDIATE_OFFSET(FP, offset);
        } else {
          mov_immediate(X9, offset, true);  // x9 broken.
          dst = REG_OFFSET(FP, X9, NULL);
        }
        switch (pow) {
        case 0:          STRB(src, dst); break;
        case 1:          STRH(src, dst); break;
        case 2: case 3:  STR(src, dst); break;
        default: assert(false); break;
        }
        ++arg_index;
      }
    }
  } else {  // vaargs
    int ip = 0;
    for (int i = arg_index; i < MAX_REG_ARGS; ++i) {
      const VarInfo *varinfo = NULL;
      while (ip < len) {
        const VarInfo *p = params->data[ip++];
        const Type *type = p->type;
        if (!is_stack_param(type)
#ifndef __NO_FLONUM
            && !is_flonum(type)
#endif
        ) {
          varinfo = p;
          break;
        }
      }
      if (varinfo != NULL) {
        const Type *type = varinfo->type;
        assert(type->kind == TY_FIXNUM || type->kind == TY_PTR);
        int size = type_size(type);
        assert(0 <= size && size < kPow2TableSize);
        int pow = kPow2Table[size];
        const char *src = kRegTable[pow][i];
        int offset = varinfo->local.reg->offset;
        const char *dst = IMMEDIATE_OFFSET(FP, offset);
        switch (pow) {
        case 0:          STRB(src, dst); break;
        case 1:          STRH(src, dst); break;
        case 2: case 3:  STR(src, dst); break;
        default: assert(false); break;
        }
      } else {
        const char *src = kReg64s[i];
        int offset = (i - MAX_REG_ARGS - MAX_FREG_ARGS) * WORD_SIZE;
        const char *dst = IMMEDIATE_OFFSET(FP, offset);
	      STR(src, dst);
      }
    }

#ifndef __NO_FLONUM
    ip = 0;
    for (int i = 0; i < MAX_FREG_ARGS; ++i) {
      const VarInfo *varinfo = NULL;
      while (ip < len) {
        const VarInfo *p = params->data[ip++];
        const Type *type = p->type;
        if (!is_stack_param(type)
            && is_flonum(type)
        ) {
          varinfo = p;
          break;
        }
      }
      if (varinfo != NULL) {
        const Type *type = varinfo->type;
        assert(type->kind == TY_FLONUM);
        int offset = varinfo->local.reg->offset;
        switch (type->flonum.kind) {
        case FL_FLOAT:   STR(kFReg32s[i], IMMEDIATE_OFFSET(FP, offset)); break;
        case FL_DOUBLE:  STR(kFReg64s[i], IMMEDIATE_OFFSET(FP, offset)); break;
        default: assert(false); break;
        }
      } else {
        int offset = (i - MAX_FREG_ARGS) * WORD_SIZE;
        STR(kFReg64s[i], IMMEDIATE_OFFSET(FP, offset));
      }
    }
#endif
  }
}

static void emit_defun(Function *func) {
  if (func->scopes == NULL)  // Prototype definition
    return;

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
    emit_comment("%.*s: static func", func->name->bytes, func->name->chars);
    label = quote_label(label);
    _LOCAL(label);
  }
  EMIT_ALIGN(4);
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
  size_t frame_size = ALIGN(fnbe->frame_size, 16);
  int callee_saved_count = 0;
  if (!no_stmt) {
    STP(FP, LR, PRE_INDEX(SP, -16));
    MOV(FP, SP);
    if (frame_size > 0) {
      const char *value;
      if (frame_size <= 0x0fff)
        value = IM(frame_size);
      else
        mov_immediate(value = X9, frame_size, true);  // x9 broken
      SUB(SP, SP, value);
    }
    put_args_to_stack(func);

    // Callee save.
    callee_saved_count = push_callee_save_regs(fnbe->ra->used_reg_bits, fnbe->ra->used_freg_bits);
  }

  emit_bb_irs(fnbe->bbcon);

  // Epilogue
  if (!no_stmt) {
    if (func->flag & FUNCF_STACK_MODIFIED) {
      // Stack pointer might be changed if alloca is used, so it need to be recalculated.
      size_t size = frame_size + ALIGN(callee_saved_count * WORD_SIZE, 16);
      const char *value;
      if (size <= 0x0fff)
        value = IM(size);
      else
        mov_immediate(value = SP, size, true);
      SUB(SP, FP, value);
    }
    pop_callee_save_regs(fnbe->ra->used_reg_bits, fnbe->ra->used_freg_bits);
    if (frame_size > 0) {
      const char *value;
      if (frame_size <= 0x0fff)
        value = IM(frame_size);
      else
        mov_immediate(value = X8, frame_size, true);  // x9 broken
      ADD(SP, SP, value);
    }
    LDP(FP, LR, POST_INDEX(SP, 16));
  }

  RET();

  // Output static local variables.
  for (int i = 0; i < func->scopes->len; ++i) {
    Scope *scope = func->scopes->data[i];
    if (scope->vars == NULL)
      continue;
    for (int j = 0; j < scope->vars->len; ++j) {
      VarInfo *varinfo = scope->vars->data[j];
      if (!(varinfo->storage & VS_STATIC))
        continue;
      VarInfo *gvarinfo = varinfo->static_.gvar;
      assert(gvarinfo != NULL);
      emit_varinfo(gvarinfo, gvarinfo->global.init);
    }
  }
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
      {
        bool first = true;
        Vector *decls = decl->vardecl.decls;
        for (int i = 0; i < decls->len; ++i) {
          VarDecl *vd = decls->data[i];
          if ((vd->storage & VS_EXTERN) != 0)
            continue;
          const Name *name = vd->ident->ident;
          const VarInfo *varinfo = scope_find(global_scope, name, NULL);
          assert(varinfo != NULL);
          if (first) {
            emit_comment(NULL);
            first = false;
          }
          emit_varinfo(varinfo, varinfo->global.init);
        }
      }
      break;

    default:
      error("Unhandled decl in emit_code: %d", decl->kind);
      break;
    }
  }
}
