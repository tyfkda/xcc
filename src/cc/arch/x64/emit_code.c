#include "../../../config.h"
#include "./arch_config.h"
#include "emit_code.h"

#include <assert.h>
#include <inttypes.h>  // PRId64
#include <stdlib.h>
#include <string.h>

#include "ast.h"
#include "cc_misc.h"
#include "codegen.h"
#include "ir.h"
#include "lexer.h"
#include "regalloc.h"
#include "table.h"
#include "type.h"
#include "util.h"
#include "var.h"
#include "x64.h"

int count_callee_save_regs(uint64_t used, uint64_t fused);

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

char *label_indirect(const char *label, int64_t offset, const char *reg) {
  if (offset > 0) {
    return fmt("%s+%" PRId64 "(%s)", label, offset, reg);
  } else if (offset < 0) {
    return fmt("%s-%" PRId64 "(%s)", label, -offset, reg);
  }
  return fmt("%s(%s)", label, reg);
}

char *gotpcrel(char *label) {
  return fmt("%s@GOTPCREL", label);
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
#define kPow2TableSize ((int)ARRAY_SIZE(kPow2Table))

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
#if XCC_TARGET_PLATFORM != XCC_PLATFORM_APPLE
  EMIT_ASM(".type", quote_label(fmt_name(func->name)), "@function");
#endif
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
