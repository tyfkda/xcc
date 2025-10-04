#include "../../../config.h"

#include <assert.h>
#include <inttypes.h>  // PRId64
#include <stdlib.h>
#include <string.h>

#include "aarch64.h"
#include "ast.h"
#include "be_aux.h"
#include "cc_misc.h"
#include "codegen.h"
#include "ir.h"
#include "regalloc.h"
#include "table.h"
#include "type.h"
#include "util.h"
#include "var.h"

#define MAX_REG_ARGS   (8)
#define MAX_FREG_ARGS  (8)

const ArchSetting kArchSetting = {
  .max_reg_args = {MAX_REG_ARGS, MAX_FREG_ARGS},
};

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

  // Assume fp-parameters are arranged from index 0.
#define kFRegParam32s  kFReg32s
#define kFRegParam64s  kFReg64s

  RegParamInfo params[MAX_REG_ARGS + MAX_FREG_ARGS];
  const int max_reg_args[2] = {MAX_REG_ARGS, MAX_FREG_ARGS};
  int param_count = enumerate_register_params(func, max_reg_args, params);
  int reg_index[2] = {0, 0};

  // Generate code to store parameters to the destination.
  for (int i = 0; i < param_count; ++i) {
    RegParamInfo *p = &params[i];
    VReg *vreg = p->vreg;
    const Type *type = p->varinfo->type;
    if (vreg == NULL) {
      // Small struct passed by value: Store to the stack frame.
      size_t size = type_size(type);
      if (size <= 0)
        continue;
      FrameInfo *fi = p->varinfo->local.frameinfo;
      int offset = fi->offset;
      assert(offset < 0);
      int index = p->index;
      for (;;) {
        size_t s;
        for (int i = VRegSize8; i >= VRegSize1; --i) {
          s = 1U << i;
          if (s <= size)
            break;
        }

        int pow = most_significant_bit(s);
        const char *src = kRegSizeTable[pow][ArchRegParamMapping[index]];
        const char *dst = IMMEDIATE_OFFSET(FP, offset);
        // TODO: Check alignment?
        switch (pow) {
        case 0:          STRB(src, dst); break;
        case 1:          STRH(src, dst); break;
        case 2: case 3:  STR(src, dst); break;
        default: assert(false); break;
        }
        size -= s;
        offset += s;
        if (size <= 0)
          break;
        if (s >= TARGET_POINTER_SIZE) {
          ++index;
        } else {
          const char *opr2 = IM(s * TARGET_CHAR_BIT);
          const char *s64 = kRegSizeTable[VRegSize8][ArchRegParamMapping[index]];
          LSR(s64, s64, opr2);
        }
      }

      size_t n = (size + TARGET_POINTER_SIZE - 1) / TARGET_POINTER_SIZE;
      reg_index[GPREG] += n;
      continue;
    }

    if (vreg->flag & VRF_FLONUM) {
      const char *src = (type->flonum.kind >= FL_DOUBLE ? kFRegParam64s : kFRegParam32s)[p->index];
      if (vreg->flag & VRF_SPILLED) {
        int offset = vreg->frame.offset;
        assert(offset != 0);
        STR(src, IMMEDIATE_OFFSET(FP, offset));
      } else {
        if (p->index != vreg->phys) {
          const char *dst = (type->flonum.kind >= FL_DOUBLE ? kFReg64s : kFReg32s)[vreg->phys];
          FMOV(dst, src);
        }
      }
      ++reg_index[FPREG];
    } else {
      size_t size = type_size(type);
      int pow = most_significant_bit(size);
      assert(IS_POWER_OF_2(size) && pow < 4);
      const char *src = kRegSizeTable[pow][ArchRegParamMapping[p->index]];
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
      ++reg_index[GPREG];
    }
  }

#if VAARG_ON_STACK
  bool vaargs = false;
#else
  bool vaargs = func->type->func.vaargs;
#endif
  if (vaargs) {
    for (int i = reg_index[GPREG]; i < MAX_REG_ARGS; ++i) {
      int offset = (i - MAX_REG_ARGS - MAX_FREG_ARGS) * TARGET_POINTER_SIZE;
      STR(kRegSizeTable[3][ArchRegParamMapping[i]], IMMEDIATE_OFFSET(FP, offset));
    }
#ifndef __NO_FLONUM
    for (int i = reg_index[FPREG]; i < MAX_FREG_ARGS; ++i) {
      int offset = (i - MAX_FREG_ARGS) * TARGET_POINTER_SIZE;
      STR(kFRegParam64s[i], IMMEDIATE_OFFSET(FP, offset));
    }
#endif
  }

#undef kFRegParam32s
#undef kFRegParam64s
}

static size_t detect_funcall_work_size(Function *func) {
  extern Vector *collect_caller_save_regs(unsigned long living);

  FuncBackend *fnbe = func->extra;
  Vector *funcalls = fnbe->funcalls;
  size_t max = 0;
  if (funcalls != NULL) {
    for (int i = 0; i < funcalls->len; ++i) {
      Expr *funcall = funcalls->data[i];
      FuncallInfo *funcall_info = funcall->funcall.info;

      // Caller save registers.
      IR *ir = funcall_info->call;
      Vector *saves = collect_caller_save_regs(ir->call->living_pregs);
      ir->call->caller_saves = saves;

      size_t total = ir->call->stack_args_size + saves->len * TARGET_POINTER_SIZE;
      max = MAX(max, total);
    }
  }
  return max;
}

void emit_defun_body(Function *func) {
  emit_comment(NULL);
  _TEXT();

  bool global = true;
  const Name *name = func->ident->ident;
  const VarInfo *varinfo = scope_find(global_scope, name, NULL);
  if (varinfo != NULL) {
    global = (varinfo->storage & VS_STATIC) == 0;
  }

  {
    char *label = format_func_name(name, global);
    if (is_weak_attr(func->attributes))
      _WEAK(label);
#if XCC_TARGET_PLATFORM != XCC_PLATFORM_APPLE  // Specify weak and global/local on Apple/Mach-O, but not on other platforms.
    else
#endif
    if (global)
      _GLOBL(label);
    else
      _LOCAL(label);
    EMIT_ALIGN(4);
#if XCC_TARGET_PLATFORM != XCC_PLATFORM_APPLE
    EMIT_ASM(".type", quote_label(fmt_name(name)), "@function");
#endif
    EMIT_LABEL(label);
  }

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

  FuncBackend *fnbe = func->extra;
  size_t funcall_work_size = detect_funcall_work_size(func);
  fnbe->stack_work_size = funcall_work_size;
  {
    VReg *vreg = fnbe->stack_work_size_vreg;
    if (vreg != NULL) {
      assert(vreg->flag & VRF_CONST);
      vreg->fixnum = funcall_work_size;
    }
  }

  // Prologue
  // Allocate variable bufer.
  size_t frame_size = ALIGN(fnbe->frame_size + funcall_work_size, 16);
  bool fp_saved = false;  // Frame pointer saved?
  bool lr_saved = false;  // Link register saved?
  if (!no_stmt) {
    fp_saved = fnbe->frame_size > 0 || fnbe->ra->flag & RAF_STACK_FRAME;
    assert(fnbe->funcalls == NULL || fnbe->funcalls->len > 0);
    lr_saved = fnbe->funcalls != NULL;

    // TODO: Handle fp_saved and lr_saved individually.
    if (fp_saved || lr_saved) {
      STP(FP, LR, PRE_INDEX(SP, -16));

      // FP is saved, so omit from callee save.
      fnbe->ra->used_reg_bits[GPREG] &= ~(1UL << GET_FPREG_INDEX());
    }

    // Callee save.
    push_callee_save_regs(fnbe->ra->used_reg_bits[GPREG], fnbe->ra->used_reg_bits[1]);

    if (fp_saved)
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

    move_params_to_assigned(func);
  }

  emit_bb_irs(fnbe->bbcon);

  if (!function_not_returned(fnbe)) {
    // Epilogue
    if (!no_stmt) {
      if (fp_saved)
        MOV(SP, FP);
      else if (frame_size > 0)
        ADD(SP, SP, IM(frame_size));

      pop_callee_save_regs(fnbe->ra->used_reg_bits[GPREG], fnbe->ra->used_reg_bits[FPREG]);

      if (fp_saved || lr_saved)
        LDP(FP, LR, POST_INDEX(SP, 16));
    }

    RET();
  }
}
