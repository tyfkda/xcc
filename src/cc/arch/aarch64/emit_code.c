#include "../../../config.h"
#include "./arch_config.h"
#include "emit_code.h"

#include <assert.h>
#include <inttypes.h>  // PRId64
#include <stdlib.h>
#include <string.h>

#include "aarch64.h"
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

char *label_at_page(char *label, int flag, int64_t offset) {
  if (offset != 0) {
#if XCC_TARGET_PLATFORM == XCC_PLATFORM_APPLE
    static const char *s[] = {
      "%s+%" PRId64 "@PAGE", "%s+%" PRId64 "@PAGEOFF",
      "%s+%" PRId64 "@GOTPAGE", "%s+%" PRId64 "@GOTPAGEOFF",
    };
#else
    static const char *s[] = {
      "%s+%" PRId64, ":lo12:%s+%" PRId64,
      ":got:%s+%" PRId64, ":got_lo12:%s+%" PRId64,
    };
#endif
    return fmt(s[flag], label, offset);
  }

#if XCC_TARGET_PLATFORM == XCC_PLATFORM_APPLE
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

////////////////////////////////////////////////

static bool is_asm(Stmt *stmt) {
  return stmt->kind == ST_ASM;
}

static void move_params_to_assigned(Function *func) {
  extern const char **kRegSizeTable[];
  extern const int ArchRegParamMapping[];
  extern const char *kFReg32s[], *kFReg64s[];

  static const char *kRegParam32s[] = {W0, W1, W2, W3, W4, W5, W6, W7};
  static const char *kRegParam64s[] = {X0, X1, X2, X3, X4, X5, X6, X7};
  static const char **kRegParamTable[] = {kRegParam32s, kRegParam32s, kRegParam32s, kRegParam64s};
  const char *kFRegParam32s[] = {S0, S1, S2, S3, S4, S5, S6, S7};
  const char *kFRegParam64s[] = {D0, D1, D2, D3, D4, D5, D6, D7};
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
      const char *dst;
      if (offset >= -256) {
        dst = IMMEDIATE_OFFSET(FP, offset);
      } else {
        mov_immediate(X9, offset, true, false);  // x9 broken.
        dst = REG_OFFSET(FP, X9, NULL);
      }
      switch (pow) {
      case 0:          STRB(src, dst); break;
      case 1:          STRH(src, dst); break;
      case 2: case 3:  STR(src, dst); break;
      default: assert(false); break;
      }
    } else if (ArchRegParamMapping[p->index] != vreg->phys) {
      const char *dst = kRegSizeTable[pow][vreg->phys];
      MOV(dst, src);
    }
  }
  for (int i = 0; i < fparam_count; ++i) {
    RegParamInfo *p = &fparams[i];
    VReg *vreg = p->vreg;
    const char *src = (p->type->flonum.kind >= FL_DOUBLE ? kFRegParam64s : kFRegParam32s)[p->index];
    if (vreg->flag & VRF_SPILLED) {
      int offset = vreg->frame.offset;
      assert(offset != 0);
      STR(src, IMMEDIATE_OFFSET(FP, offset));
    } else {
      if (p->index != vreg->phys) {
        const char *dst = (p->type->flonum.kind >= FL_DOUBLE ? kFReg64s : kFReg32s)[vreg->phys];
        FMOV(dst, src);
      }
    }
  }

#if VAARG_ON_STACK
  bool vaargs = false;
#else
  bool vaargs = func->type->func.vaargs;
#endif
  if (vaargs) {
    for (int i = iparam_count; i < MAX_REG_ARGS; ++i) {
      int offset = (i - MAX_REG_ARGS - MAX_FREG_ARGS) * POINTER_SIZE;
      STR(kRegParam64s[i], IMMEDIATE_OFFSET(FP, offset));
    }
    for (int i = fparam_count; i < MAX_FREG_ARGS; ++i) {
      int offset = (i - MAX_FREG_ARGS) * POINTER_SIZE;
      STR(kFRegParam64s[i], IMMEDIATE_OFFSET(FP, offset));
    }
  }
}

static void emit_defun(Function *func) {
  if (func->scopes == NULL ||  // Prototype definition.
      func->extra == NULL)     // Code emission is omitted.
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
    label = quote_label(label);
    _LOCAL(label);
  }
  EMIT_ALIGN(4);
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
  size_t frame_size = ALIGN(fnbe->frame_size, 16);
  bool fp_saved = false;  // Frame pointer saved?
  bool lr_saved = false;  // Link register saved?
  uint64_t used_reg_bits = fnbe->ra->used_reg_bits;
  if (!no_stmt) {
    fp_saved = frame_size > 0 || fnbe->ra->flag & RAF_STACK_FRAME;
    lr_saved = (func->flag & FUNCF_HAS_FUNCALL) != 0;

    // TODO: Handle fp_saved and lr_saved individually.
    if (fp_saved || lr_saved) {
      STP(FP, LR, PRE_INDEX(SP, -16));

      // FP is saved, so omit from callee save.
      used_reg_bits &= ~(1ULL << GET_FPREG_INDEX());
    }

    // Callee save.
    push_callee_save_regs(used_reg_bits, fnbe->ra->used_freg_bits);

    if (fp_saved) {
      MOV(FP, SP);
      if (frame_size > 0) {
        const char *value;
        if (frame_size <= 0x0fff) {
          value = IM(frame_size);
        } else {
          // Break x17
          mov_immediate(value = X17, frame_size, true, false);
        }
        SUB(SP, SP, value);
      }
    }

    move_params_to_assigned(func);
  }

  emit_bb_irs(fnbe->bbcon);

  if (!function_not_returned(fnbe)) {
    // Epilogue
    if (!no_stmt) {
      if (fp_saved)
        MOV(SP, FP);

      pop_callee_save_regs(used_reg_bits, fnbe->ra->used_freg_bits);

      if (fp_saved || lr_saved)
        LDP(FP, LR, POST_INDEX(SP, 16));
    }

    RET();
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
