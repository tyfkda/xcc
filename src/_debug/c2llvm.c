#include <assert.h>
#include <inttypes.h>  // PRId64, PRIdPTR
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "codegen.h"
#include "fe_misc.h"
#include "ir.h"
#include "lexer.h"
#include "optimize.h"
#include "parser.h"
#include "regalloc.h"
#include "table.h"
#include "type.h"
#include "util.h"
#include "var.h"

#define _VRF_ALLOCATED     (1 << 30)

////////////////////////////////////////////////

extern void install_builtins(Vector *decls);

static char *kFxTypes[4] = {"i8", "i16", "i32", "i64"};

char *get_llvm_type(const Type *type) {
  switch (type->kind) {
  case TY_VOID:  return "void";
  case TY_PTR:   return "ptr";
  case TY_FIXNUM:
    {
      size_t size = type_size(type);
      return kFxTypes[most_significant_bit(size)];
    }
  default:  assert(false); break;
  }
}

char *get_llvm_type_from_vreg(const VReg *vreg) {
  if (vreg->flag & VRF_FLONUM) {
    assert(false);
  } else {
    return kFxTypes[vreg->vsize];
  }
}

static void dump_vvreg(FILE *fp, VReg *vreg, RegAlloc *ra) {
  UNUSED(ra);

  if (!(vreg->flag & _VRF_ALLOCATED)) {
    fprintf(fp, "%%%d", vreg->phys + 1);  // Stored in phys.
    return;
  }

  // if (vreg->original == vreg) {
    fprintf(fp, "%%V%d", vreg->virt + 1);
  // } else {
  //   Vector *versions = ra->vreg_table[vreg->original->virt];
  //   int version;
  //   for (version = 0; version < versions->len; ++version) {
  //     if (versions->data[version] == vreg)
  //       break;
  //   }
  //   assert(version < versions->len);

  //   char buf[16], *p = buf + sizeof(buf);
  //   *(--p) = '\0';
  //   do {
  //     --version;
  //     *(--p) = 'a' + (version % 26);
  //     version /= 26;
  //   } while (version > 0);
  //   fprintf(fp, "v%d%s%s", vreg->original->virt, p, kSize[vreg->vsize]);
  // }
}

static void dump_vreg(FILE *fp, VReg *vreg, RegAlloc *ra) {
  assert(vreg != NULL);
  if (vreg->flag & VRF_CONST) {
#ifndef __NO_FLONUM
    if (vreg->flag & VRF_FLONUM) {
      fprintf(fp, "(%gf)", vreg->flonum.value);
      return;
    }
#endif
    fprintf(fp, "%" PRId64 "", vreg->fixnum);
  // } else if (vreg->phys >= 0) {
  //   assert(!(vreg->flag & VRF_SPILLED));
  //   char regtype = 'R';
  //   if (vreg->flag & VRF_FLONUM)
  //     regtype = 'F';
  //   fprintf(fp, "%c%d%s<v%d>", regtype, vreg->phys, kSize[vreg->vsize], vreg->virt);
  } else {
    dump_vvreg(fp, vreg, ra);
  }
}

// static void dump_vreg2(FILE *fp, VReg *vreg, RegAlloc *ra) {
//   if (vreg->flag & VRF_SPILLED) {
//     fprintf(fp, "spilled(v%d)", vreg->virt);
//   } else {
//     dump_vreg(fp, vreg, ra);
//   }
// }

static void dump_vregs(FILE *fp, const char *title, Vector *regs, bool newline) {
  fprintf(fp, "%s=[", title);
  for (int i = 0; i < regs->len; ++i) {
    VReg *vreg = regs->data[i];
    fprintf(fp, "%s%d", i == 0 ? "" : ",", vreg->virt);
  }
  if (newline)
    fprintf(fp, "]\n");
  else
    fprintf(fp, "]");
}

static int tmpValNo;

static int load_value(FILE *fp, VReg *vreg, RegAlloc *ra) {
  if ((vreg->flag & (VRF_CONST | _VRF_ALLOCATED)) != _VRF_ALLOCATED)
    return -1;

  int valNo = ++tmpValNo;
  fprintf(fp, "\t%%%d = load %s, ptr ", valNo, get_llvm_type_from_vreg(vreg));
  dump_vreg(fp, vreg, ra);
  fprintf(fp, "\n");
  return valNo;
}

static void dump_value(FILE *fp, VReg *vreg, int valNo) {
  if (vreg->flag & VRF_CONST) {
    fprintf(fp, "%" PRId64, vreg->fixnum);
  } else if (vreg->flag & _VRF_ALLOCATED) {
    fprintf(fp, "%%%d", valNo);
  } else {
    assert(vreg->phys > 0);
    fprintf(fp, "%%%d", vreg->phys);
  }
}

static void dir_bofs(FILE *fp, IR *ir, RegAlloc *ra) {
  int64_t offset = ir->bofs.frameinfo->offset + ir->bofs.offset;
  fprintf(fp, "\t");
  dump_vreg(fp, ir->dst, ra);
  fprintf(fp, " = %%fp %c %" PRId64 "\n", offset >= 0 ? '+' : '-', offset > 0 ? offset : -offset);
}

static void dir_iofs(FILE *fp, IR *ir, RegAlloc *ra) {
  fprintf(fp, "\t");
  dump_vreg(fp, ir->dst, ra);
  fprintf(fp, " = &%.*s", NAMES(ir->iofs.label));
  if (ir->iofs.offset != 0) {
    int64_t offset = ir->iofs.offset; fprintf(fp, " %c %" PRId64, offset >= 0 ? '+' : '-', offset > 0 ? offset : -offset);
  }
  fprintf(fp, "\n");
}

static void dir_sofs(FILE *fp, IR *ir, RegAlloc *ra) {
  fprintf(fp, "\t");
  dump_vreg(fp, ir->dst, ra);
  fprintf(fp, " = %%sp %c %" PRId64 "\n", ir->opr1->fixnum >= 0 ? '+' : '-', ir->opr1->fixnum > 0 ? ir->opr1->fixnum : -ir->opr1->fixnum);
}

static void dir_load(FILE *fp, IR *ir, RegAlloc *ra) {
  fprintf(fp, "\t");
  dump_vreg(fp, ir->dst, ra);
  fprintf(fp, " = load [");
  dump_vreg(fp, ir->opr1, ra);
  if (ir->load.offset > 0)
    fprintf(fp, " + %" PRId64, ir->load.offset);
  else if (ir->load.offset < 0)
    fprintf(fp, " - %" PRId64, -ir->load.offset);
  fprintf(fp, "]\n");
}

static void dir_store(FILE *fp, IR *ir, RegAlloc *ra) {
  fprintf(fp, "\tstore [");
  dump_vreg(fp, ir->opr2, ra);
  fprintf(fp, "] = ");
  dump_vreg(fp, ir->opr1, ra);
  fprintf(fp, "\n");
}

static void dir_unary(FILE *fp, IR *ir, RegAlloc *ra) {
  static const char *kOps[] = {
    [IR_NEG] = "sub", [IR_BITNOT] = "xor",
  };
  static const int kConsts[] = {
    [IR_NEG] = 0, [IR_BITNOT] = -1,
  };

  int valNo = load_value(fp, ir->opr1, ra);

  VReg *dst = ir->dst;
  int valNoDst = ++tmpValNo;
  if (!(dst->flag & _VRF_ALLOCATED))
    assert(dst->phys < 0), dst->phys = tmpValNo;
  fprintf(fp, "\t%%%d = %s %s %d, ", valNoDst, kOps[ir->kind], get_llvm_type_from_vreg(ir->dst), kConsts[ir->kind]);
  dump_value(fp, ir->opr1, valNo);
  fprintf(fp, "\n");
  if (dst->flag & _VRF_ALLOCATED) {
    fprintf(fp, "\tstore %s %%%d, ptr ", get_llvm_type_from_vreg(dst), valNoDst);
    dump_vreg(fp, dst, ra);
    fprintf(fp, "\n");
  }
}

static void dir_bop(FILE *fp, IR *ir, RegAlloc *ra) {
  static char *kOps[] = {
    [IR_ADD] = "add", [IR_SUB] = "sub", [IR_MUL] = "mul", [IR_DIV] = "sdiv", [IR_MOD] = "srem",
    [IR_BITAND] = "and", [IR_BITOR] = "or", [IR_BITXOR] = "xor",
    [IR_LSHIFT] = "shl", [IR_RSHIFT] = "ashr",
  };

  int valNo1 = load_value(fp, ir->opr1, ra);
  int valNo2 = load_value(fp, ir->opr2, ra);

  VReg *dst = ir->dst;
  int valNoDst = ++tmpValNo;
  if (!(dst->flag & _VRF_ALLOCATED))
    assert(dst->phys < 0), dst->phys = tmpValNo;
  fprintf(fp, "\t%%%d = %s %s ", valNoDst, kOps[ir->kind], get_llvm_type_from_vreg(ir->dst));
  dump_value(fp, ir->opr1, valNo1);
  fprintf(fp, ", ");
  dump_value(fp, ir->opr2, valNo2);
  fprintf(fp, "\n");
  if (dst->flag & _VRF_ALLOCATED) {
    fprintf(fp, "\tstore %s %%%d, ptr ", get_llvm_type_from_vreg(dst), valNoDst);
    dump_vreg(fp, dst, ra);
    fprintf(fp, "\n");
  }
}

static void dir_cast(FILE *fp, IR *ir, RegAlloc *ra) {
  fprintf(fp, "\t");
  dump_vreg(fp, ir->dst, ra);
  fprintf(fp, " = (CAST)");
  dump_vreg(fp, ir->opr1, ra);
  fprintf(fp, "\n");
}

static void dir_mov(FILE *fp, IR *ir, RegAlloc *ra) {
  int valNo = load_value(fp, ir->opr1, ra);
  fprintf(fp, "\tstore %s ", get_llvm_type_from_vreg(ir->opr1));
  dump_value(fp, ir->opr1, valNo);
  fprintf(fp, ", ptr ");
  dump_vreg(fp, ir->dst, ra);
  fprintf(fp, "\n");
}

static void dir_result(FILE *fp, IR *ir, RegAlloc *ra) {
  if (ir->opr1 == NULL)  // No effect: suppress output.
    return;

  int valNo = load_value(fp, ir->opr1, ra);

  if (ir->dst != NULL) {
    dump_vreg(fp, ir->dst, ra);
    fprintf(fp, " = ");
    dump_vreg(fp, ir->opr1, ra);
    fprintf(fp, "\n");
    fprintf(fp, "\tret");
  } else {
    fprintf(fp, "\tret %s ", get_llvm_type_from_vreg(ir->opr1));
    dump_value(fp, ir->opr1, valNo);
    fprintf(fp, "\n");
  }
}

static void dir_jmp(FILE *fp, IR *ir, RegAlloc *ra) {
  UNUSED(ra);
  if (ir->jmp.cond == COND_NONE)
    return;
  assert(ir->jmp.cond == COND_ANY);
  fprintf(fp, "\tbr label %%%.*s\n", NAMES(ir->jmp.bb->label));
}

static void dir_jmp_cond(FILE *fp, IR *ir, RegAlloc *ra, BB *nextbb) {
  assert(ir->jmp.cond != COND_ANY);
  static char *kCond[] = {NULL, NULL, "eq", "ne", "slt", "sle", "sge", "sgt", NULL, NULL, "eq", "ne", "ult", "ule", "uge", "ugt"};
  int valNo1 = load_value(fp, ir->opr1, ra);
  int valNo2 = load_value(fp, ir->opr2, ra);
  int valNoCond = ++tmpValNo;
  fprintf(fp, "\t%%%d = icmp %s %s ", valNoCond, kCond[ir->jmp.cond], get_llvm_type_from_vreg(ir->opr1));
  dump_value(fp, ir->opr1, valNo1);
  fprintf(fp, ", ");
  dump_value(fp, ir->opr2, valNo2);
  fprintf(fp, "\n");

  fprintf(fp, "\tbr i1 %%%d, label %%%.*s, label %%%.*s\n", valNoCond, NAMES(ir->jmp.bb->label), NAMES(nextbb->label));
}

static void dir_pusharg(FILE *fp, IR *ir, RegAlloc *ra) {
  fprintf(fp, "\t; pusharg ");
  dump_vreg(fp, ir->opr1, ra);
  fprintf(fp, " (index=#%d)", ir->pusharg.index);
  fprintf(fp, "\n");
}

static void dir_call(FILE *fp, IR *ir, RegAlloc *ra) {
  int valNoFunc = -1;
  if (ir->call->label == NULL) {
    valNoFunc = load_value(fp, ir->opr1, ra);
  }

  VReg **args = ir->call->args;
  Vector *argValNos = new_vector();
  for (int i = 0, n = ir->call->total_arg_count; i < n; ++i) {
    VReg *arg = args[i];
    int valNo = load_value(fp, arg, ra);
    vec_push(argValNos, INT2VOIDP(valNo));
  }

  int valNoDst = -1;
  VReg *dst = ir->dst;
  if (dst != NULL) {
    valNoDst = ++tmpValNo;
    if (!(dst->flag & _VRF_ALLOCATED))
      assert(dst->phys < 0), dst->phys = tmpValNo;
    fprintf(fp, "\t%%%d = call %s ", valNoDst, get_llvm_type_from_vreg(dst));
  } else {
    fprintf(fp, "\tcall void ");
  }
  if (ir->call->vaarg_start >= 0) {
    fprintf(fp, "(");
    int n = ir->call->vaarg_start;
    for (int i = 0; i < n; ++i) {
      if (i != 0)
        fprintf(fp, ", ");
      VReg *arg = args[i];
      fprintf(fp, "%s", get_llvm_type_from_vreg(arg));
    }
    fprintf(fp, "%s...) ", n > 0 ? ", " : "");
  }
  if (ir->call->label != NULL) {
    fprintf(fp, "@%.*s(", NAMES(ir->call->label));
  } else {
    fprintf(fp, "%%%d(", valNoFunc);
  }

  for (int i = 0, n = ir->call->total_arg_count; i < n; ++i) {
    if (i != 0)
      fprintf(fp, ", ");
    VReg *arg = args[i];
    int valNo = VOIDP2INT(argValNos->data[i]);
    fprintf(fp, "%s ", get_llvm_type_from_vreg(arg));
    dump_value(fp, arg, valNo);
  }
  fprintf(fp, ")\n");

  free_vector(argValNos);
}

static void dir_keep(FILE *fp, IR *ir, RegAlloc *ra) {
  UNUSED(ra);
  assert(ir->opr1 != NULL);
  fprintf(fp, "\t; keep ");
  dump_vreg(fp, ir->opr1, ra);
  if (ir->opr2 != NULL) {
    fprintf(fp, ", ");
    dump_vreg(fp, ir->opr2, ra);
  }
  fprintf(fp, "\n");
}

static void dir_dummy(FILE *fp, IR *ir, RegAlloc *ra) {
  UNUSED(ra);
  fprintf(fp, "\tIR%d   // TODO\n", ir->kind);
}

#define dir_load_s   dir_load
#define dir_store_s   dir_store
#define dir_add   dir_bop
#define dir_sub   dir_bop
#define dir_mul   dir_bop
#define dir_div   dir_bop
#define dir_mod   dir_bop
#define dir_bitand   dir_bop
#define dir_bitor   dir_bop
#define dir_bitxor   dir_bop
#define dir_lshift   dir_bop
#define dir_rshift   dir_bop
#define dir_cond   dir_dummy
#define dir_neg   dir_unary
#define dir_bitnot   dir_unary
#define dir_tjmp   dir_dummy
#define dir_subsp   dir_dummy

static void (*kIrTable[])(FILE *fp, IR *ir, RegAlloc *ra) = {
  [IR_BOFS] = dir_bofs, [IR_IOFS] = dir_iofs, [IR_SOFS] = dir_sofs,
  [IR_LOAD] = dir_load, [IR_LOAD_S] = dir_load_s, [IR_STORE] = dir_store, [IR_STORE_S] = dir_store_s,

  [IR_ADD] = dir_add, [IR_SUB] = dir_sub, [IR_MUL] = dir_mul, [IR_DIV] = dir_div,
  [IR_MOD] = dir_mod, [IR_BITAND] = dir_bitand, [IR_BITOR] = dir_bitor,
  [IR_BITXOR] = dir_bitxor, [IR_LSHIFT] = dir_lshift, [IR_RSHIFT] = dir_rshift,
  [IR_COND] = dir_cond,

  [IR_NEG] = dir_neg, [IR_BITNOT] = dir_bitnot, [IR_CAST] = dir_cast,
  [IR_MOV] = dir_mov, [IR_RESULT] = dir_result,

  [IR_JMP] = dir_jmp, [IR_TJMP] = dir_tjmp,
  [IR_PUSHARG] = dir_pusharg, [IR_CALL] = dir_call,
  [IR_SUBSP] = dir_subsp, [IR_KEEP] = dir_keep,
};

static void dump_func_param(FILE *fp, const Type *functype) {
  if (functype->func.param_vars != NULL) {
    for (int i = 0; i < functype->func.param_vars->len; ++i) {
      if (i != 0)
        fprintf(fp, ", ");
      VarInfo *varinfo = functype->func.param_vars->data[i];
      VReg *vreg = varinfo->local.vreg;
      if (vreg == NULL)
        fprintf(fp, "%s", get_llvm_type(varinfo->type));
      else
        fprintf(fp, "%s %%.param.%.*s", get_llvm_type_from_vreg(vreg), NAMES(varinfo->ident->ident));
    }

    if (functype->func.vaargs)
      fprintf(fp, "%s...", functype->func.param_vars->len > 0 ? ", " : "");
  }
}

static void dump_func_ir(FILE *fp, Function *func) {
  FuncBackend *fnbe = func->extra;
  if (fnbe == NULL)
    return;
  BBContainer *bbcon = fnbe->bbcon;
  assert(bbcon != NULL);

  const Type *functype = func->type;
  fprintf(fp, "define %s @%.*s(", get_llvm_type(functype->func.ret), NAMES(func->ident->ident));
  dump_func_param(fp, functype);
  fprintf(fp, ") {\n");

  Vector *stack_vars = new_vector();
  for (int i = 0; i < func->scopes->len; ++i) {
    Scope *scope = func->scopes->data[i];
    for (int j = 0; j < scope->vars->len; ++j) {
      VarInfo *varinfo = scope->vars->data[j];
      if (!is_local_storage(varinfo))
        continue;
      VReg *vreg = varinfo->local.vreg;
      if (vreg == NULL) {
        vec_push(stack_vars, varinfo);
        continue;
      }

      vreg->flag |= _VRF_ALLOCATED;  // Mark.
      fprintf(fp, "\t%%V%d = alloca %s", vreg->virt + 1, get_llvm_type_from_vreg(vreg));
      fprintf(fp, "  ; (flag=%x): %.*s  : ", vreg->flag, NAMES(varinfo->ident->ident));
      print_type(fp, varinfo->type);
      if (vreg->flag & VRF_PARAM) {
        fprintf(fp, ", param(%d)", vreg->reg_param_index);
      }
      fprintf(fp, "\n");

      if (vreg->flag & VRF_PARAM) {
        fprintf(fp, "\tstore %s %%.param.%.*s, ptr %%V%d\n", get_llvm_type_from_vreg(vreg), NAMES(varinfo->ident->ident), vreg->virt + 1);
      }
    }
  }
  for (int i = 0; i < stack_vars->len; ++i) {
    VarInfo *varinfo = stack_vars->data[i];
    const Type *type = varinfo->type;
    const Type *elemtype = &tyChar;
    if (type->kind == TY_ARRAY && is_prim_type(type->pa.ptrof))
      elemtype = type->pa.ptrof;
    fprintf(fp, "\t%%V%.*s = alloca [%zu x %s], align %zu  ; ",
            NAMES(varinfo->ident->ident), type_size(varinfo->type) / type_size(elemtype),
            get_llvm_type(elemtype), align_size(varinfo->type));
    print_type(fp, varinfo->type);
    fprintf(fp, ", stack-offset=%4d, storage=0x%x\n", varinfo->local.frameinfo->offset, varinfo->storage);
  }

  RegAlloc *ra = fnbe->ra;
  // fprintf(fp, "VREG: #%d\n", ra->vregs->len);
  // LiveInterval **sorted_intervals = ra->sorted_intervals;
  // if (sorted_intervals != NULL) {
  //   for (int i = 0; i < ra->vregs->len; ++i) {
  //     LiveInterval *li = sorted_intervals[i];
  //     VReg *vreg = ra->vregs->data[li->virt];
  //     if (vreg == NULL)
  //       continue;

  //     switch (li->state) {
  //     case LI_NORMAL:
  //       {
  //         fprintf(fp, "  V%3d (flag=%x): live %3d - %3d", li->virt, vreg->flag, li->start, li->end);
  //         if (keep_virtual_register) {
  //           if (vreg->original != vreg) {
  //             fprintf(fp, ", ");
  //             dump_vvreg(fp, vreg, ra);
  //           }
  //         } else {
  //           char regtype = vreg->flag & VRF_FLONUM ? 'F' : 'R';
  //           fprintf(fp, ", => %c%3d", regtype, li->phys);
  //         }
  //         if (li->occupied_reg_bit != 0)
  //           fprintf(fp, ", occupied=%lx", li->occupied_reg_bit);
  //         fprintf(fp, "\n");
  //       }
  //       break;
  //     case LI_SPILL:
  //       fprintf(fp, "  V%3d (flag=%x): live %3d - %3d (spilled, offset=%d)\n", li->virt, vreg->flag,
  //               li->start, li->end, vreg->frame.offset);
  //       break;
  //     }
  //   }
  // } else if (ra->vreg_table != NULL) {
  //   for (int i = 0; i < ra->original_vreg_count; ++i) {
  //     Vector *vregs = ra->vreg_table[i];
  //     assert(vregs != NULL);
  //     if (vregs->len <= 1)
  //       continue;
  //     fprintf(fp, "  V%3d: #%d [", i, vregs->len);
  //     for (int j = 0; j < vregs->len; ++j) {
  //       VReg *vreg = vregs->data[j];
  //       if (j > 0)
  //         fprintf(fp, ", ");
  //       dump_vreg(fp, vreg, ra);
  //       fprintf(fp, "(%d)", vreg->virt);
  //     }
  //     fprintf(fp, "]\n");
  //   }
  // }

  fprintf(fp, "\n");

  // fprintf(fp, "BB: #%d\n", bbcon->len);
  int nip = 0;
  for (int i = 0; i < bbcon->len; ++i) {
    BB *bb = bbcon->data[i];
    fprintf(fp, "; BB %d\n", i);
    if (i == bbcon->len - 1) {
      assert(bb->irs->len == 0);
      break;
    }

    bool fallthrough = false;
    if (i > 0) {
      BB *prevbb = bbcon->data[i - 1];
      if (prevbb->irs->len > 0) {
        IR *lastir = prevbb->irs->data[prevbb->irs->len - 1];
        if (lastir->kind != IR_JMP && lastir->kind != IR_TJMP && lastir->kind != IR_RESULT) {
          fallthrough = true;
        }
      }
    }
    if (i == 0 || fallthrough)
      fprintf(fp, "  br label %%%.*s\n", NAMES(bb->label));

    fprintf(fp, "%.*s: ; ", NAMES(bb->label));
    if (bb->from_bbs->len > 0) {
      fprintf(fp, " from=[");
      for (int j = 0; j < bb->from_bbs->len; ++j) {
        BB *fbb = bb->from_bbs->data[j];
        fprintf(fp, "%s%.*s", (j > 0 ? ", " : ""), NAMES(fbb->label));
      }
      fprintf(fp, "]");
    }
    if (bb->in_regs->len > 0)
      dump_vregs(fp, " in", bb->in_regs, false);
    if (bb->out_regs->len > 0)
      dump_vregs(fp, " out", bb->out_regs, false);
    fprintf(fp, "\n");

    // if (bb->phis != NULL) {
    //   for (int j = 0; j < bb->phis->len; ++j) {
    //     Phi *phi = bb->phis->data[j];
    //     fprintf(fp, "       \tPHI ");
    //     dump_vreg2(fp, phi->dst, ra);
    //     fprintf(fp, " = {");
    //     for (int i = 0; i < phi->params->len; ++i) {
    //       VReg *vreg = phi->params->data[i];
    //       if (i > 0)
    //         fprintf(fp, ", ");
    //       dump_vreg2(fp, vreg, ra);
    //     }
    //     fprintf(fp, "}\n");
    //   }
    // }

    for (int j = 0; j < bb->irs->len; ++j, ++nip) {
      // fprintf(fp, "\t");
      IR *ir = bb->irs->data[j];
      if (ir->kind == IR_JMP && ir->jmp.cond != COND_ANY) {
        BB *nextbb = bbcon->data[i + 1];
        dir_jmp_cond(fp, ir, ra, nextbb);
        continue;
      }

      (*kIrTable[ir->kind])(fp, ir, ra);
      // dump_ir(fp, ir, ra);
    }
  }
  fprintf(fp, "}\n\n");
}

void do_dump_ir(FILE *fp, Vector *decls) {
  if (decls == NULL)
    return;

  for (int i = 0, len = decls->len; i < len; ++i) {
    Declaration *decl = decls->data[i];
    if (decl == NULL || decl->kind != DCL_DEFUN)
      continue;
    Function *func = decl->defun.func;
    if (!gen_defun(func))
      continue;

    FuncBackend *fnbe = func->extra;
    curfunc = func;
    curra = fnbe->ra;

    optimize(fnbe->ra, fnbe->bbcon);

    curfunc = NULL;
    curra = NULL;

    dump_func_ir(fp, func);
  }
}

void gen_llvm(Vector *decls) {
  do_dump_ir(stdout, decls);
}

////

static void init_compiler(Vector *decls) {
  init_lexer();
  init_global();

  // set_fixnum_size(FX_CHAR,  1, 1);
  // set_fixnum_size(FX_SHORT, 2, 2);
  // set_fixnum_size(FX_INT,   4, 4);
  // set_fixnum_size(FX_LONG,  8, 8);
  // set_fixnum_size(FX_LLONG, 8, 8);
  // set_fixnum_size(FX_ENUM,  4, 4);

  install_builtins(decls);
}

static void compile1(FILE *ifp, const char *filename, Vector *decls) {
  set_source_file(ifp, filename);
  parse(decls);
}

int main(int argc, char *argv[]) {
  // enum {
  //   OPT_SSA,
  // };

  static const struct option options[] = {
    // {"-apply-ssa", no_argument, OPT_SSA},

    {NULL},
  };

  int opt;
  while ((opt = optparse(argc, argv, options)) != -1) {
    switch (opt) {
    default: assert(false); break;
    // case OPT_SSA:
    //   {
    //     extern bool apply_ssa;
    //     apply_ssa = true;
    //   }
    //   break;

    case '?':
      fprintf(stderr, "Warning: unknown option: %s\n", argv[optind - 1]);
      break;
    }
  }

  // LLVM requires SSA.
  // extern bool apply_ssa;
  // apply_ssa = true;

  // Compile.
  Vector *toplevel = new_vector();
  init_compiler(toplevel);

  int iarg = optind;
  if (iarg < argc) {
    for (int i = iarg; i < argc; ++i) {
      const char *filename = argv[i];
      FILE *ifp = fopen(filename, "r");
      if (ifp == NULL)
        error("Cannot open file: %s\n", filename);
      compile1(ifp, filename, toplevel);
      fclose(ifp);
    }
  } else {
    compile1(stdin, "*stdin*", toplevel);
  }
  if (compile_error_count != 0)
    exit(1);

  gen_llvm(toplevel);

  return 0;
}
