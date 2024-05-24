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
#include "riscv64.h"
#include "table.h"
#include "type.h"
#include "util.h"
#include "var.h"

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
  return;
#else
  RegParamInfo iparams[MAX_REG_ARGS];
  RegParamInfo fparams[MAX_FREG_ARGS];
  int iparam_count = 0;
  int fparam_count = 0;
  enumerate_register_params(func, iparams, MAX_REG_ARGS, fparams, MAX_FREG_ARGS,
                            &iparam_count, &fparam_count);

  int size = 0;
  int n = MAX_REG_ARGS - iparam_count;
  if (n > 0) {
    int size_org = n * POINTER_SIZE;
    size = ALIGN(n, 2) * POINTER_SIZE;
    int offset = size - size_org;
    ADDI(SP, SP, IM(-size));
    for (int i = iparam_count; i < MAX_REG_ARGS; ++i, offset += POINTER_SIZE)
      SD(kRegParam64s[i], IMMEDIATE_OFFSET(offset, SP));
  }
  return size;
#endif
}

static void move_params_to_assigned(Function *func) {
  extern const char *kReg64s[];
  extern const int ArchRegParamMapping[];
  extern const char *kFReg64s[];

  const char *kFRegParam64s[] = {FA0, FA1, FA2, FA3, FA4, FA5, FA6, FA7};
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
    const char *src = kReg64s[p->index];
    if (vreg->flag & VRF_SPILLED) {
      int offset = vreg->frame.offset;
      assert(offset != 0);
      const char *dst = IMMEDIATE_OFFSET(offset, FP);
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
  for (int i = 0; i < fparam_count; ++i) {
    RegParamInfo *p = &fparams[i];
    VReg *vreg = p->vreg;
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
  EMIT_ALIGN(2);
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
  bool ra_saved = false;  // Return Address register saved?
  uint64_t used_reg_bits = fnbe->ra->used_reg_bits;
  int vaarg_params_saved = 0;
  if (!no_stmt) {
    if (func->type->func.vaargs) {
      vaarg_params_saved = put_vaarg_params(func);

      // Re-align frame size.
      frame_size = ALIGN(fnbe->frame_size + vaarg_params_saved, 16) - vaarg_params_saved;
    }

    fp_saved = frame_size > 0 || fnbe->ra->flag & RAF_STACK_FRAME;
    ra_saved = (func->flag & FUNCF_HAS_FUNCALL) != 0;

    // TODO: Handle fp_saved and ra_saved individually.
    if (fp_saved || ra_saved) {
      ADDI(SP, SP, IM(-16));
      SD(RA, IMMEDIATE_OFFSET(8, SP));
      SD(FP, IMMEDIATE_OFFSET0(SP));

      // FP is saved, so omit from callee save.
      used_reg_bits &= ~(1ULL << GET_FPREG_INDEX());
    }

    // Callee save.
    push_callee_save_regs(used_reg_bits, fnbe->ra->used_freg_bits);

    if (fp_saved) {
      MV(FP, SP);
      if (frame_size > 0) {
        ADDI(SP, SP, IM(-frame_size));
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

      pop_callee_save_regs(used_reg_bits, fnbe->ra->used_freg_bits);

      if (fp_saved || ra_saved) {
        LD(FP, IMMEDIATE_OFFSET0(SP));
        LD(RA, IMMEDIATE_OFFSET(8, SP));
        ADDI(SP, SP, IM(16));
      }
    }
    if (vaarg_params_saved > 0)
      ADDI(SP, SP, IM(vaarg_params_saved));

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
