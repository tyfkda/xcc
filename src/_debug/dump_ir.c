#include <assert.h>
#include <inttypes.h>
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
#include "util.h"
#include "var.h"

extern bool keep_phi;
static bool keep_virtual_register;

////////////////////////////////////////////////

extern void install_builtins(void);

static const char *kSize[] = {".b", ".w", ".d", ""};

static void dump_vvreg(FILE *fp, VReg *vreg, RegAlloc *ra) {
  if (vreg->original == vreg) {
    fprintf(fp, "V%d%s", vreg->virt, kSize[vreg->vsize]);
  } else {
    Vector *versions = ra->vreg_table[vreg->original->virt];
    int version;
    for (version = 0; version < versions->len; ++version) {
      if (versions->data[version] == vreg)
        break;
    }
    assert(version < versions->len);

    char buf[16], *p = buf + sizeof(buf);
    *(--p) = '\0';
    do {
      --version;
      *(--p) = 'a' + (version % 26);
      version /= 26;
    } while (version > 0);
    fprintf(fp, "v%d%s%s", vreg->original->virt, p, kSize[vreg->vsize]);
  }
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
    fprintf(fp, "(%" PRId64 ")", vreg->fixnum);
  } else if (vreg->phys >= 0) {
    assert(!(vreg->flag & VRF_SPILLED));
    char regtype = 'R';
    if (vreg->flag & VRF_FLONUM)
      regtype = 'F';
    fprintf(fp, "%c%d%s<v%d>", regtype, vreg->phys, kSize[vreg->vsize], vreg->virt);
  } else {
    dump_vvreg(fp, vreg, ra);
  }
}

static void dump_vreg2(FILE *fp, VReg *vreg, RegAlloc *ra) {
  if (vreg->flag & VRF_SPILLED) {
    fprintf(fp, "spilled(v%d)", vreg->virt);
  } else {
    dump_vreg(fp, vreg, ra);
  }
}

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

static void dump_ir(FILE *fp, IR *ir, RegAlloc *ra) {
  static char *kOps[] = {
    "BOFS", "IOFS", "SOFS", "LOAD", "LOAD_S", "STORE", "STORE_S",
    "ADD", "SUB", "MUL", "DIV", "MOD", "BITAND", "BITOR", "BITXOR", "LSHIFT", "RSHIFT", "COND",
    "NEG", "BITNOT", "CAST", "MOV", "RESULT",
    "JMP", "TJMP", "PUSHARG", "CALL", "SUBSP", "KEEP", "ASM",
  };
  static char *kCond[] = {NULL, "MP", "EQ", "NE", "LT", "LE", "GE", "GT", NULL, "MP", "EQ", "NE", "ULT", "ULE", "UGE", "UGT"};
  static char *kCond2[] = {NULL, "MP", "==", "!=", "<", "<=", ">=", ">", NULL, "MP", "==", "!=", "<", "<=", ">=", ">"};

  const char *us = ir->flag & IRF_UNSIGNED ? "U" : "";
  switch (ir->kind) {
  case IR_DIV:
  case IR_MOD:
    fprintf(fp, "%s%s\t", kOps[ir->kind], us);
    break;
  case IR_JMP:
    fprintf(fp, "J%s%s\t", kCond[ir->jmp.cond & (COND_MASK | COND_UNSIGNED)], us);
    break;
  default:
    assert(0 <= ir->kind && ir->kind <= IR_ASM);
    fprintf(fp, "%s%s\t", kOps[ir->kind], us);
    break;
  }

  switch (ir->kind) {
  case IR_BOFS:   { int64_t offset = ir->bofs.frameinfo->offset + ir->bofs.offset; dump_vreg(fp, ir->dst, ra); fprintf(fp, " = &[rbp %c %" PRId64 "]\n", offset >= 0 ? '+' : '-', offset > 0 ? offset : -offset); } break;
  case IR_IOFS:   dump_vreg(fp, ir->dst, ra); fprintf(fp, " = &%.*s", NAMES(ir->iofs.label)); if (ir->iofs.offset != 0) { int64_t offset = ir->iofs.offset; fprintf(fp, " %c %" PRId64, offset >= 0 ? '+' : '-', offset > 0 ? offset : -offset); } fprintf(fp, "\n"); break;
  case IR_SOFS:   dump_vreg(fp, ir->dst, ra); fprintf(fp, " = &[rsp %c %" PRId64 "]\n", ir->opr1->fixnum >= 0 ? '+' : '-', ir->opr1->fixnum > 0 ? ir->opr1->fixnum : -ir->opr1->fixnum); break;
  case IR_LOAD:   dump_vreg(fp, ir->dst, ra); fprintf(fp, " = ["); dump_vreg(fp, ir->opr1, ra); fprintf(fp, "]\n"); break;
  case IR_LOAD_S: dump_vreg(fp, ir->dst, ra); fprintf(fp, " = [v%d]\n", ir->opr1->virt); break;
  case IR_STORE:  fprintf(fp, "["); dump_vreg(fp, ir->opr2, ra); fprintf(fp, "] = "); dump_vreg(fp, ir->opr1, ra); fprintf(fp, "\n"); break;
  case IR_STORE_S:fprintf(fp, "[v%d] = ", ir->opr2->virt); dump_vreg(fp, ir->opr1, ra); fprintf(fp, "\n"); break;
  case IR_ADD:    dump_vreg(fp, ir->dst, ra); fprintf(fp, " = "); dump_vreg(fp, ir->opr1, ra); fprintf(fp, " + "); dump_vreg(fp, ir->opr2, ra); fprintf(fp, "\n"); break;
  case IR_SUB:    dump_vreg(fp, ir->dst, ra); fprintf(fp, " = "); dump_vreg(fp, ir->opr1, ra); fprintf(fp, " - "); dump_vreg(fp, ir->opr2, ra); fprintf(fp, "\n"); break;
  case IR_MUL:    dump_vreg(fp, ir->dst, ra); fprintf(fp, " = "); dump_vreg(fp, ir->opr1, ra); fprintf(fp, " * "); dump_vreg(fp, ir->opr2, ra); fprintf(fp, "\n"); break;
  case IR_DIV:    dump_vreg(fp, ir->dst, ra); fprintf(fp, " = "); dump_vreg(fp, ir->opr1, ra); fprintf(fp, " / "); dump_vreg(fp, ir->opr2, ra); fprintf(fp, "\n"); break;
  case IR_MOD:    dump_vreg(fp, ir->dst, ra); fprintf(fp, " = "); dump_vreg(fp, ir->opr1, ra); fprintf(fp, " %% "); dump_vreg(fp, ir->opr2, ra); fprintf(fp, "\n"); break;
  case IR_BITAND: dump_vreg(fp, ir->dst, ra); fprintf(fp, " = "); dump_vreg(fp, ir->opr1, ra); fprintf(fp, " & "); dump_vreg(fp, ir->opr2, ra); fprintf(fp, "\n"); break;
  case IR_BITOR:  dump_vreg(fp, ir->dst, ra); fprintf(fp, " = "); dump_vreg(fp, ir->opr1, ra); fprintf(fp, " | "); dump_vreg(fp, ir->opr2, ra); fprintf(fp, "\n"); break;
  case IR_BITXOR: dump_vreg(fp, ir->dst, ra); fprintf(fp, " = "); dump_vreg(fp, ir->opr1, ra); fprintf(fp, " ^ "); dump_vreg(fp, ir->opr2, ra); fprintf(fp, "\n"); break;
  case IR_LSHIFT: dump_vreg(fp, ir->dst, ra); fprintf(fp, " = "); dump_vreg(fp, ir->opr1, ra); fprintf(fp, " << "); dump_vreg(fp, ir->opr2, ra); fprintf(fp, "\n"); break;
  case IR_RSHIFT: dump_vreg(fp, ir->dst, ra); fprintf(fp, " = "); dump_vreg(fp, ir->opr1, ra); fprintf(fp, " >> "); dump_vreg(fp, ir->opr2, ra); fprintf(fp, "\n"); break;
  case IR_COND:   dump_vreg(fp, ir->dst, ra); fprintf(fp, " = "); if (ir->cond.kind != COND_ANY && ir->cond.kind != COND_NONE) {dump_vreg(fp, ir->opr1, ra); fprintf(fp, " %s ", kCond2[ir->cond.kind & (COND_MASK | COND_UNSIGNED)]); dump_vreg(fp, ir->opr2, ra);} fprintf(fp, "\n"); break;
  case IR_NEG:    dump_vreg(fp, ir->dst, ra); fprintf(fp, " = -"); dump_vreg(fp, ir->opr1, ra); fprintf(fp, "\n"); break;
  case IR_BITNOT: dump_vreg(fp, ir->dst, ra); fprintf(fp, " = ~"); dump_vreg(fp, ir->opr1, ra); fprintf(fp, "\n"); break;
  case IR_CAST:   dump_vreg(fp, ir->dst, ra); fprintf(fp, " = "); dump_vreg(fp, ir->opr1, ra); fprintf(fp, "\n"); break;
  case IR_MOV:    dump_vreg(fp, ir->dst, ra); fprintf(fp, " = "); dump_vreg(fp, ir->opr1, ra); fprintf(fp, "\n"); break;
  case IR_RESULT: if (ir->dst != NULL) { dump_vreg(fp, ir->dst, ra); fprintf(fp, " = "); } dump_vreg(fp, ir->opr1, ra); fprintf(fp, "\n"); break;
  case IR_JMP:    if (ir->jmp.cond != COND_ANY && ir->jmp.cond != COND_NONE) {dump_vreg(fp, ir->opr1, ra); fprintf(fp, ", "); dump_vreg(fp, ir->opr2, ra); fprintf(fp, ", ");} fprintf(fp, "%.*s\n", NAMES(ir->jmp.bb->label)); break;
  case IR_TJMP:
    dump_vreg(fp, ir->opr1, ra);
    for (size_t i = 0; i < ir->tjmp.len; ++i)
      fprintf(fp, "%s%.*s", i == 0 ? ", [" : ", ", NAMES(((BB*)ir->tjmp.bbs[i])->label));
    fprintf(fp, "]");
    if (ir->opr2 != NULL) {fprintf(fp, " (tmp="); dump_vreg(fp, ir->opr2, ra); fprintf(fp, ")");}
    fprintf(fp, "\n");
    break;
  case IR_PUSHARG: fprintf(fp, "%d, ", ir->pusharg.index); dump_vreg(fp, ir->opr1, ra); fprintf(fp, "\n"); break;
  case IR_CALL:
    if (ir->dst != NULL) { dump_vreg(fp, ir->dst, ra); fprintf(fp, " = "); }
    if (ir->call->label != NULL) {
      fprintf(fp, "%.*s(args=#%d)\n", NAMES(ir->call->label), ir->call->reg_arg_count);
    } else {
      fprintf(fp, "*"); dump_vreg(fp, ir->opr1, ra); fprintf(fp, "(args=#%d)\n", ir->call->reg_arg_count);
    }
    break;
  case IR_SUBSP:  dump_vreg(fp, ir->opr1, ra); fprintf(fp, "\n"); break;
  case IR_KEEP:
    if (ir->dst != NULL) { fprintf(fp, "dst:"); dump_vreg(fp, ir->dst, ra); fprintf(fp, ", "); }
    if (ir->opr1 != NULL) {
      dump_vreg(fp, ir->opr1, ra);
      if (ir->opr2 != NULL) {
        fprintf(fp, ", ");
        dump_vreg(fp, ir->opr2, ra);
      }
    }
    fprintf(fp, "\n");
    break;
  case IR_ASM:
    {
      fprintf(fp, "[\n");
      Vector *templates = ir->asm_.templates;
      for (int i = 0; i < templates->len; i += 2) {
        const char *str = templates->data[i];
        fputs(str, fp);
        if (i + 1 < templates->len) {
          uintptr_t index = (uintptr_t)templates->data[i + 1];
          fprintf(fp, "%%%" PRIdPTR, index);
        }
      }
      fprintf(fp, "]\n");
    }
    break;
  }
}

static void dump_func_ir(Function *func) {
  FILE *fp = stdout;

  if (func->scopes == NULL)  // Prototype definition
    return;

  FuncBackend *fnbe = func->extra;
  if (fnbe == NULL)
    return;
  BBContainer *bbcon = fnbe->bbcon;
  assert(bbcon != NULL);

  fprintf(fp, "### %.*s\n\n", NAMES(func->ident->ident));

  fprintf(fp, "params and locals:\n");
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
      fprintf(fp, "  V%3d (flag=%x): %.*s  : ", vreg->virt, vreg->flag, NAMES(varinfo->ident->ident));
      print_type(fp, varinfo->type);
      fprintf(fp, "\n");
    }
  }
  for (int i = 0; i < stack_vars->len; ++i) {
    VarInfo *varinfo = stack_vars->data[i];
    fprintf(fp, "  stack (offset=%4d, size=%zu): %.*s  : ", varinfo->local.frameinfo->offset,
            type_size(varinfo->type), NAMES(varinfo->ident->ident));
    print_type(fp, varinfo->type);
    fprintf(fp, "\n");
  }

  RegAlloc *ra = fnbe->ra;
  fprintf(fp, "VREG: #%d\n", ra->vregs->len);
  LiveInterval **sorted_intervals = ra->sorted_intervals;
  if (sorted_intervals != NULL) {
    for (int i = 0; i < ra->vregs->len; ++i) {
      LiveInterval *li = sorted_intervals[i];
      VReg *vreg = ra->vregs->data[li->virt];
      if (vreg == NULL)
        continue;

      switch (li->state) {
      case LI_NORMAL:
        {
          fprintf(fp, "  V%3d (flag=%x): live %3d - %3d", li->virt, vreg->flag, li->start, li->end);
          if (keep_virtual_register) {
            if (vreg->original != vreg) {
              fprintf(fp, ", ");
              dump_vvreg(fp, vreg, ra);
            }
          } else {
            char regtype = vreg->flag & VRF_FLONUM ? 'F' : 'R';
            fprintf(fp, ", => %c%3d", regtype, li->phys);
          }
          if (li->occupied_reg_bit != 0)
            fprintf(fp, ", occupied=%lx", li->occupied_reg_bit);
          fprintf(fp, "\n");
        }
        break;
      case LI_SPILL:
        fprintf(fp, "  V%3d (flag=%x): live %3d - %3d (spilled, offset=%d)\n", li->virt, vreg->flag,
                li->start, li->end, vreg->frame.offset);
        break;
      }
    }
  } else if (ra->vreg_table != NULL) {
    for (int i = 0; i < ra->original_vreg_count; ++i) {
      Vector *vregs = ra->vreg_table[i];
      assert(vregs != NULL);
      if (vregs->len <= 1)
        continue;
      fprintf(fp, "  V%3d: #%d [", i, vregs->len);
      for (int j = 0; j < vregs->len; ++j) {
        VReg *vreg = vregs->data[j];
        if (j > 0)
          fprintf(fp, ", ");
        dump_vreg(fp, vreg, ra);
        fprintf(fp, "(%d)", vreg->virt);
      }
      fprintf(fp, "]\n");
    }
  }

  fprintf(fp, "BB: #%d\n", bbcon->len);
  int nip = 0;
  for (int i = 0; i < bbcon->len; ++i) {
    BB *bb = bbcon->data[i];
    fprintf(fp, "// BB %d\n", i);
    fprintf(fp, "%.*s:", NAMES(bb->label));
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

    if (bb->phis != NULL) {
      for (int j = 0; j < bb->phis->len; ++j) {
        Phi *phi = bb->phis->data[j];
        fprintf(fp, "       \tPHI ");
        dump_vreg2(fp, phi->dst, ra);
        fprintf(fp, " = {");
        for (int i = 0; i < phi->params->len; ++i) {
          VReg *vreg = phi->params->data[i];
          if (i > 0)
            fprintf(fp, ", ");
          dump_vreg2(fp, vreg, ra);
        }
        fprintf(fp, "}\n");
      }
    }

    for (int j = 0; j < bb->irs->len; ++j, ++nip) {
      fprintf(fp, "%6d|\t", nip);
      IR *ir = bb->irs->data[j];
      dump_ir(fp, ir, ra);
    }
  }
  fprintf(fp, "\n");
}

void do_dump_ir(Vector *decls) {
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

    if (!keep_phi) {
      prepare_register_allocation(func);
      // tweak_irs(fnbe);
      analyze_reg_flow(fnbe->bbcon);

      alloc_physical_registers(fnbe->ra, fnbe->bbcon);
      if (!keep_virtual_register)
        map_virtual_to_physical_registers(fnbe->ra);
      detect_living_registers(fnbe->ra, fnbe->bbcon);

      alloc_stack_variables_onto_stack_frame(func);
    }

    curfunc = NULL;
    curra = NULL;

    dump_func_ir(func);
  }
}

static void init_compiler(void) {
  init_lexer();
  init_global();

  //set_fixnum_size(FX_CHAR,  1, 1);
  //set_fixnum_size(FX_SHORT, 2, 2);
  //set_fixnum_size(FX_INT,   4, 4);
  //set_fixnum_size(FX_LONG,  8, 8);
  //set_fixnum_size(FX_LLONG, 8, 8);
  //set_fixnum_size(FX_ENUM,  4, 4);

  install_builtins();
}

static void compile1(FILE *ifp, const char *filename, Vector *decls) {
  set_source_file(ifp, filename);
  parse(decls);
}

int main(int argc, char *argv[]) {
  enum {
    OPT_KEEP_VIRTUAL_REGISTER = 128,
    OPT_KEEP_PHI,
    OPT_SSA,
  };

  static const struct option options[] = {
    {"-keep-virtual", no_argument, OPT_KEEP_VIRTUAL_REGISTER},
    {"-keep-phi", no_argument, OPT_KEEP_PHI},
    {"-apply-ssa", no_argument, OPT_SSA},

    {NULL},
  };

  int opt;
  while ((opt = optparse(argc, argv, options)) != -1) {
    switch (opt) {
    default: assert(false); break;
    case OPT_KEEP_VIRTUAL_REGISTER:
      keep_virtual_register = true;
      break;

    case OPT_KEEP_PHI:
      keep_phi = true;
      break;

    case OPT_SSA:
      {
        extern bool apply_ssa;
        apply_ssa = true;
      }
      break;

    case '?':
      fprintf(stderr, "Warning: unknown option: %s\n", argv[optind - 1]);
      break;
    }
  }

  // Compile.
  init_compiler();

  Vector *toplevel = new_vector();
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

  do_dump_ir(toplevel);

  return 0;
}
