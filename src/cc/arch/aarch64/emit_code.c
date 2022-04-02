#include "../config.h"
#include "emit_code.h"

#include <assert.h>
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

////////////////////////////////////////////////

static VarInfo *find_ret_var(Scope *scope) {
  const Name *retval_name = alloc_name(RET_VAR_NAME, NULL, false);
  return scope_find(scope, retval_name, NULL);
}

static void put_args_to_stack(Function *func) {
  static const char *kReg8s[] = {W0, W1, W2, W3, W4, W5, W6, W7};
  static const char *kReg16s[] = {W0, W1, W2, W3, W4, W5, W6, W7};
  static const char *kReg32s[] = {W0, W1, W2, W3, W4, W5, W6, W7};
  static const char *kReg64s[] = {X0, X1, X2, X3, X4, X5, X6, X7};
  static const char **kRegTable[] = {NULL, kReg8s, kReg16s, NULL, kReg32s, NULL, NULL, NULL, kReg64s};

  int arg_index = 0;
  if (is_stack_param(func->type->func.ret)) {
    Scope *top_scope = func->scopes->data[0];
    VarInfo *varinfo = find_ret_var(top_scope);
    assert(varinfo != NULL);
    const Type *type = varinfo->type;
    int size = type_size(type);
    int offset = varinfo->local.reg->offset;
    assert(size < (int)(sizeof(kRegTable) / sizeof(*kRegTable)) &&
           kRegTable[size] != NULL);
    STR(kRegTable[size][0], IMMEDIATE_OFFSET(FP, offset));
    ++arg_index;
  }

  // Store arguments into local frame.
  const Vector *params = func->type->func.params;
  if (params == NULL)
    return;

  int len = params->len;
  if (!func->type->func.vaargs) {
    for (int i = 0; i < len; ++i) {
      const VarInfo *varinfo = params->data[i];
      const Type *type = varinfo->type;
      int offset = varinfo->local.reg->offset;

      if (is_stack_param(type))
        continue;

#ifndef __NO_FLONUM
      assert(!is_flonum(type));
#endif

      switch (type->kind) {
      case TY_FIXNUM:
      case TY_PTR:
        break;
      default: assert(false); break;
      }

      if (arg_index < MAX_REG_ARGS) {
        int size = type_size(type);
        assert(size < (int)(sizeof(kRegTable) / sizeof(*kRegTable)) &&
               kRegTable[size] != NULL);
        STR(kRegTable[size][arg_index], IMMEDIATE_OFFSET(FP, offset));
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
        assert(size < (int)(sizeof(kRegTable) / sizeof(*kRegTable)) &&
               kRegTable[size] != NULL);
        int offset = varinfo->local.reg->offset;
        STR(kRegTable[size][i], IMMEDIATE_OFFSET(FP, offset));
      } else {
        int size = type_size(&tyVoidPtr);
        assert(size < (int)(sizeof(kRegTable) / sizeof(*kRegTable)) &&
               kRegTable[size] != NULL);
        int offset = (i - MAX_REG_ARGS - MAX_FREG_ARGS) * WORD_SIZE;
        STR(kRegTable[size][i], IMMEDIATE_OFFSET(FP, offset));
      }
    }
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

  // Prologue
  // Allocate variable bufer.
  FuncBackend *fnbe = func->extra;
  size_t frame_size = ALIGN(fnbe->ra->frame_size, 16);
  STP(FP, LR, PRE_INDEX(SP, -16));
  MOV(FP, SP);
  if (frame_size > 0)
    SUB(SP, SP, IM(frame_size));
  put_args_to_stack(func);

  // Callee save.
  push_callee_save_regs(fnbe->ra->used_reg_bits);

  emit_bb_irs(fnbe->bbcon);

  // Epilogue
  if (frame_size > 0)
    ADD(SP, SP, IM(frame_size));
  LDP(FP, LR, POST_INDEX(SP, 16));

  RET();
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
        emit_comment(NULL);
        Vector *decls = decl->vardecl.decls;
        for (int i = 0; i < decls->len; ++i) {
          VarDecl *vd = decls->data[i];
          if ((vd->storage & VS_EXTERN) != 0)
            continue;
          const Name *name = vd->ident->ident;
          const VarInfo *varinfo = scope_find(global_scope, name, NULL);
          assert(varinfo != NULL);

          // emit_varinfo(varinfo, varinfo->global.init);
        }
      }
      break;

    default:
      error("Unhandled decl in emit_code: %d", decl->kind);
      break;
    }
  }
}
