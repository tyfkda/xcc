#include "../../../config.h"

#include <assert.h>
#include <inttypes.h>  // PRId64
#include <stdlib.h>
#include <string.h>

#include "ast.h"
#include "be_aux.h"
#include "cc_misc.h"
#include "codegen.h"
#include "ir.h"
#include "regalloc.h"
#include "riscv64.h"
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
  return fmt("%" PRId64, x);
}

char *immediate_offset(int offset, const char *reg) {
  return offset != 0 ? fmt("%d(%s)", offset, reg) : fmt("(%s)", reg);
}

////////////////////////////////////////////////

static const char *kRegParam64s[] = {A0, A1, A2, A3, A4, A5, A6, A7};

static bool is_asm(Stmt *stmt) {
  return stmt->kind == ST_ASM;
}

static int put_vaarg_params(Function *func) {
  assert(func->type->func.vaargs);
#if VAARG_ON_STACK
  return 0;
#else
  RegParamInfo params[MAX_REG_ARGS + MAX_FREG_ARGS];
  const int max_reg_args[2] = {MAX_REG_ARGS, MAX_FREG_ARGS};
  int param_count = enumerate_register_params(func, max_reg_args, params);

  int size = 0;
  int ngp = 0;
  for (int i = 0; i < param_count; ++i) {
    VReg *vreg = params[i].vreg;
    if (!(vreg->flag & VRF_FLONUM))
      ++ngp;
  }
  int n = MAX_REG_ARGS - ngp;
  if (n > 0) {
    int size_org = n * TARGET_POINTER_SIZE;
    size = ALIGN(n, 2) * TARGET_POINTER_SIZE;
    int offset = size - size_org;
    ADDI(SP, SP, IM(-size));
    for (int i = ngp; i < MAX_REG_ARGS; ++i, offset += TARGET_POINTER_SIZE)
      SD(kRegParam64s[i], IMMEDIATE_OFFSET(offset, SP));
  }
  return size;
#endif
}

static void move_params_to_assigned(Function *func) {
  extern const char *kReg64s[];
  extern const int ArchRegParamMapping[];
  extern const char *kFReg64s[];

  // Assume fp-parameters are arranged from index 0.
#define kFRegParam64s  kFReg64s

  RegParamInfo params[MAX_REG_ARGS + MAX_FREG_ARGS];
  const int max_reg_args[2] = {MAX_REG_ARGS, MAX_FREG_ARGS};
  int param_count = enumerate_register_params(func, max_reg_args, params);

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
        const char *src = kReg64s[ArchRegParamMapping[index]];
        const char *dst;
        if (offset >= -2048) {
          dst = IMMEDIATE_OFFSET(offset, FP);
        } else {
          LI(T0, IM(offset));
          ADD(T0, T0, FP);
          dst = IMMEDIATE_OFFSET0(T0);
        }
        // TODO: Check alignment?
        switch (pow) {
        case 0:  SB(src, dst); break;
        case 1:  SH(src, dst); break;
        case 2:  SW(src, dst); break;
        case 3:  SD(src, dst); break;
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
          SRLI(src, src, opr2);
        }
      }

      continue;
    }

    if (vreg->flag & VRF_FLONUM) {
      const char *src = kFRegParam64s[p->index];
      if (vreg->flag & VRF_SPILLED) {
        int offset = vreg->frame.offset;
        assert(offset != 0);
        assert(offset != 0);
        FSD(src, IMMEDIATE_OFFSET(offset, FP));
      } else {
        if (p->index != vreg->phys) {
          const char *dst = kFReg64s[vreg->phys];
          FMV_D(dst, src);
        }
      }
    } else {
      size_t size = type_size(type);
      int pow = most_significant_bit(size);
      assert(IS_POWER_OF_2(size) && pow < 4);
      const char *src = kReg64s[ArchRegParamMapping[p->index]];
      if (vreg->flag & VRF_SPILLED) {
        int offset = vreg->frame.offset;
        assert(offset != 0);
        const char *dst;
        if (offset >= -2048) {
          dst = IMMEDIATE_OFFSET(offset, FP);
        } else {
          LI(T0, IM(offset));
          ADD(T0, T0, FP);
          dst = IMMEDIATE_OFFSET0(T0);
        }
        switch (pow) {
        case 0:  SB(src, dst); break;
        case 1:  SH(src, dst); break;
        case 2:  SW(src, dst); break;
        case 3:  SD(src, dst); break;
        default: assert(false); break;
        }
      } else if (ArchRegParamMapping[p->index] != vreg->phys) {
        const char *dst = kReg64s[vreg->phys];
        MV(dst, src);
      }
    }
  }
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
    else if (global)
      _GLOBL(label);
    else
      _LOCAL(label);
    EMIT_ALIGN(2);
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
  bool ra_saved = false;  // Return Address register saved?
  int vaarg_params_saved = 0;
  if (!no_stmt) {
    if (func->type->func.vaargs) {
      vaarg_params_saved = put_vaarg_params(func);

      // Re-align frame size.
      frame_size = ALIGN(fnbe->frame_size + funcall_work_size + vaarg_params_saved, 16) -
                   vaarg_params_saved;
    }

    fp_saved = fnbe->frame_size > 0 || fnbe->ra->flag & RAF_STACK_FRAME;
    assert(fnbe->funcalls == NULL || fnbe->funcalls->len > 0);
    ra_saved = fnbe->funcalls != NULL;

    // TODO: Handle fp_saved and ra_saved individually.
    if (fp_saved || ra_saved) {
      ADDI(SP, SP, IM(-16));
      SD(RA, IMMEDIATE_OFFSET(8, SP));
      SD(FP, IMMEDIATE_OFFSET0(SP));

      // FP is saved, so omit from callee save.
      fnbe->ra->used_reg_bits[GPREG] &= ~(1UL << GET_FPREG_INDEX());
    }

    // Callee save.
    push_callee_save_regs(fnbe->ra->used_reg_bits[GPREG], fnbe->ra->used_reg_bits[1]);

    if (fp_saved)
      MV(FP, SP);
    if (frame_size > 0) {
      if (frame_size < 2048) {
        ADDI(SP, SP, IM(-frame_size));
      } else {
        LI(T1, IM(frame_size));
        SUB(SP, SP, T1);
      }
    }

    move_params_to_assigned(func);
  }

  emit_bb_irs(fnbe->bbcon);

  if (!function_not_returned(fnbe)) {
    // Epilogue
    if (!no_stmt) {
      if (fp_saved)
        MV(SP, FP);
      else if (frame_size > 0)
        ADDI(SP, SP, IM(frame_size));

      pop_callee_save_regs(fnbe->ra->used_reg_bits[GPREG], fnbe->ra->used_reg_bits[FPREG]);

      if (fp_saved || ra_saved) {
        LD(FP, IMMEDIATE_OFFSET0(SP));
        LD(RA, IMMEDIATE_OFFSET(8, SP));
        ADDI(SP, SP, IM(16));
      }
    }
    if (vaarg_params_saved > 0) {
      if (vaarg_params_saved < 2048) {
        ADDI(SP, SP, IM(vaarg_params_saved));
      } else {
        LI(T1, IM(vaarg_params_saved));
        ADD(SP, SP, T1);
      }
    }

    RET();
  }
}
