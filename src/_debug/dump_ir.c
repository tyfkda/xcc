#include <assert.h>
#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "codegen.h"
#include "fe_misc.h"
#include "ir.h"
#include "lexer.h"
#include "parser.h"
#include "regalloc.h"
#include "table.h"
#include "util.h"
#include "var.h"

////////////////////////////////////////////////

extern void install_builtins(void);

static void dump_vreg(FILE *fp, VReg *vreg) {
  assert(vreg != NULL);
  assert(!(vreg->flag & VRF_SPILLED));
  static char *kSize[] = {"0", "b", "w", "3", "d", "5", "6", "7", ""};
  if (vreg->flag & VRF_CONST) {
    fprintf(fp, "(%" PRId64 ")", vreg->fixnum);
  } else {
    char regtype = 'R';
    if (vreg->vtype->flag & VRTF_FLONUM)
      regtype = 'F';
    fprintf(fp, "%c%d%s<v%d>", regtype, vreg->phys, kSize[vreg->vtype->size], vreg->virt);
  }
}

static void dump_ir(FILE *fp, IR *ir) {
  static char *kOps[] = {
    "BOFS", "IOFS", "SOFS", "LOAD", "LOAD_S", "STORE", "STORE_S",
    "ADD", "SUB", "MUL", "DIV", "MOD", "BITAND", "BITOR", "BITXOR", "LSHIFT", "RSHIFT",
    "CMP", "NEG", "BITNOT", "COND", "JMP", "TJMP",
    "PRECALL", "PUSHARG", "CALL", "RESULT", "SUBSP",
    "CAST", "MOV", "ASM",
  };
  static char *kCond[] = {NULL, "MP", "EQ", "NE", "LT", "LE", "GE", "GT", NULL, "MP", "EQ", "NE", "ULT", "ULE", "UGE", "UGT"};

  switch (ir->kind) {
  case IR_DIV:
  case IR_MOD:
    fprintf(fp, "%s%s\t", kOps[ir->kind], ir->dst->vtype->flag & VRTF_UNSIGNED ? "U" : "");
    break;
  case IR_JMP:
    fprintf(fp, "J%s\t", kCond[ir->jmp.cond & (COND_MASK | COND_UNSIGNED)]);
    break;
  default:
    assert(0 <= ir->kind && ir->kind <= IR_ASM);
    fprintf(fp, "%s\t", kOps[ir->kind]);
    break;
  }

  switch (ir->kind) {
  case IR_BOFS:   dump_vreg(fp, ir->dst); fprintf(fp, " = &[rbp %c %d]\n", ir->bofs.frameinfo->offset >= 0 ? '+' : '-', ir->bofs.frameinfo->offset > 0 ? ir->bofs.frameinfo->offset : -ir->bofs.frameinfo->offset); break;
  case IR_IOFS:   dump_vreg(fp, ir->dst); fprintf(fp, " = &%.*s\n", NAMES(ir->iofs.label)); break;
  case IR_SOFS:   dump_vreg(fp, ir->dst); fprintf(fp, " = &[rsp %c %" PRId64 "]\n", ir->opr1->fixnum >= 0 ? '+' : '-', ir->opr1->fixnum > 0 ? ir->opr1->fixnum : -ir->opr1->fixnum); break;
  case IR_LOAD:   dump_vreg(fp, ir->dst); fprintf(fp, " = ["); dump_vreg(fp, ir->opr1); fprintf(fp, "]\n"); break;
  case IR_LOAD_S: dump_vreg(fp, ir->dst); fprintf(fp, " = [v%d]\n", ir->opr1->virt); break;
  case IR_STORE:  fprintf(fp, "["); dump_vreg(fp, ir->opr2); fprintf(fp, "] = "); dump_vreg(fp, ir->opr1); fprintf(fp, "\n"); break;
  case IR_STORE_S:fprintf(fp, "[v%d] = ", ir->opr2->virt); dump_vreg(fp, ir->opr1); fprintf(fp, "\n"); break;
  case IR_ADD:    dump_vreg(fp, ir->dst); fprintf(fp, " = "); dump_vreg(fp, ir->opr1); fprintf(fp, " + "); dump_vreg(fp, ir->opr2); fprintf(fp, "\n"); break;
  case IR_SUB:    dump_vreg(fp, ir->dst); fprintf(fp, " = "); dump_vreg(fp, ir->opr1); fprintf(fp, " - "); dump_vreg(fp, ir->opr2); fprintf(fp, "\n"); break;
  case IR_MUL:    dump_vreg(fp, ir->dst); fprintf(fp, " = "); dump_vreg(fp, ir->opr1); fprintf(fp, " * "); dump_vreg(fp, ir->opr2); fprintf(fp, "\n"); break;
  case IR_DIV:    dump_vreg(fp, ir->dst); fprintf(fp, " = "); dump_vreg(fp, ir->opr1); fprintf(fp, " / "); dump_vreg(fp, ir->opr2); fprintf(fp, "\n"); break;
  case IR_MOD:    dump_vreg(fp, ir->dst); fprintf(fp, " = "); dump_vreg(fp, ir->opr1); fprintf(fp, " %% "); dump_vreg(fp, ir->opr2); fprintf(fp, "\n"); break;
  case IR_BITAND: dump_vreg(fp, ir->dst); fprintf(fp, " = "); dump_vreg(fp, ir->opr1); fprintf(fp, " & "); dump_vreg(fp, ir->opr2); fprintf(fp, "\n"); break;
  case IR_BITOR:  dump_vreg(fp, ir->dst); fprintf(fp, " = "); dump_vreg(fp, ir->opr1); fprintf(fp, " | "); dump_vreg(fp, ir->opr2); fprintf(fp, "\n"); break;
  case IR_BITXOR: dump_vreg(fp, ir->dst); fprintf(fp, " = "); dump_vreg(fp, ir->opr1); fprintf(fp, " ^ "); dump_vreg(fp, ir->opr2); fprintf(fp, "\n"); break;
  case IR_LSHIFT: dump_vreg(fp, ir->dst); fprintf(fp, " = "); dump_vreg(fp, ir->opr1); fprintf(fp, " << "); dump_vreg(fp, ir->opr2); fprintf(fp, "\n"); break;
  case IR_RSHIFT: dump_vreg(fp, ir->dst); fprintf(fp, " = "); dump_vreg(fp, ir->opr1); fprintf(fp, " >> "); dump_vreg(fp, ir->opr2); fprintf(fp, "\n"); break;
  case IR_CMP:    dump_vreg(fp, ir->opr1); fprintf(fp, " - "); dump_vreg(fp, ir->opr2); fprintf(fp, "\n"); break;
  case IR_NEG:    dump_vreg(fp, ir->dst); fprintf(fp, " = -"); dump_vreg(fp, ir->opr1); fprintf(fp, "\n"); break;
  case IR_BITNOT: dump_vreg(fp, ir->dst); fprintf(fp, " = ~"); dump_vreg(fp, ir->opr1); fprintf(fp, "\n"); break;
  case IR_COND:   dump_vreg(fp, ir->dst); fprintf(fp, " = %s\n", kCond[ir->cond.kind & (COND_MASK | COND_UNSIGNED)]); break;
  case IR_JMP:    fprintf(fp, "%.*s\n", NAMES(ir->jmp.bb->label)); break;
  case IR_TJMP:
    dump_vreg(fp, ir->opr1);
    for (size_t i = 0; i < ir->tjmp.len; ++i)
      fprintf(fp, "%s%.*s", i == 0 ? ", [" : ", ", NAMES(((BB*)ir->tjmp.bbs[i])->label));
    fprintf(fp, "]\n");
    break;
  case IR_PRECALL: fprintf(fp, "\n"); break;
  case IR_PUSHARG: fprintf(fp, "%d, ", ir->pusharg.index); dump_vreg(fp, ir->opr1); fprintf(fp, "\n"); break;
  case IR_CALL:
    if (ir->dst != NULL) { dump_vreg(fp, ir->dst); fprintf(fp, " = "); }
    if (ir->call.label != NULL) {
      fprintf(fp, "%.*s(args=#%d)\n", NAMES(ir->call.label), ir->call.reg_arg_count);
    } else {
      fprintf(fp, "*"); dump_vreg(fp, ir->opr1); fprintf(fp, "(args=#%d)\n", ir->call.reg_arg_count);
    }
    break;
  case IR_RESULT: dump_vreg(fp, ir->opr1); fprintf(fp, "\n"); break;
  case IR_SUBSP:  dump_vreg(fp, ir->opr1); fprintf(fp, "\n"); break;
  case IR_CAST:   dump_vreg(fp, ir->dst); fprintf(fp, " = "); dump_vreg(fp, ir->opr1); fprintf(fp, "\n"); break;
  case IR_MOV:    dump_vreg(fp, ir->dst); fprintf(fp, " = "); dump_vreg(fp, ir->opr1); fprintf(fp, "\n"); break;
  case IR_ASM:    fprintf(fp, "\"%s\"\n", ir->asm_.str); break;
  }
}

static void dump_func_ir(Function *func) {
  FILE *fp = stdout;

  if (func->scopes == NULL)  // Prototype definition
    return;

  FuncBackend *fnbe = func->extra;
  BBContainer *bbcon = fnbe->bbcon;
  assert(bbcon != NULL);

  fprintf(fp, "### %.*s\n\n", NAMES(func->name));

  fprintf(fp, "params and locals:\n");
  for (int i = 0; i < func->scopes->len; ++i) {
    Scope *scope = func->scopes->data[i];
    if (scope->vars == NULL)
      continue;
    for (int j = 0; j < scope->vars->len; ++j) {
      VarInfo *varinfo = scope->vars->data[j];
      if (!is_local_storage(varinfo))
        continue;
      VReg *vreg = varinfo->local.vreg;
      if (vreg == NULL)
        continue;
      fprintf(fp, "  V%3d (flag=%x): %.*s\n", vreg->virt, vreg->flag, NAMES(varinfo->name));
    }
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
          char regtype = 'R';
          if (vreg->vtype->flag & VRTF_FLONUM)
            regtype = 'F';
          fprintf(fp, "  V%3d (flag=%x): live %3d - %3d, => %c%3d", li->virt, vreg->flag, li->start, li->end, regtype, li->phys);
          if (li->occupied_reg_bit != 0)
            fprintf(fp, ", occupied=%lx", li->occupied_reg_bit);
          fprintf(fp, "\n");
        }
        break;
      case LI_SPILL:
        fprintf(fp, "  V%3d (flag=%x): live %3d - %3d (spilled, offset=%d)\n", li->virt, vreg->flag, li->start, li->end, vreg->frame.offset);
        break;
      case LI_CONST:
        continue;
      }
    }
  }

  fprintf(fp, "BB: #%d\n", bbcon->bbs->len);
  int nip = 0;
  for (int i = 0; i < bbcon->bbs->len; ++i) {
    BB *bb = bbcon->bbs->data[i];
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
    if (bb->in_regs != NULL && bb->in_regs->len > 0) {
      fprintf(fp, " in=[");
      for (int j = 0; j < bb->in_regs->len; ++j) {
        VReg *vreg = bb->in_regs->data[j];
        fprintf(fp, "%s%d", (j > 0 ? ", " : ""), vreg->virt);
      }
      fprintf(fp, "]");
    }
    if (bb->out_regs != NULL && bb->out_regs->len > 0) {
      fprintf(fp, " out=[");
      for (int j = 0; j < bb->out_regs->len; ++j) {
        VReg *vreg = bb->out_regs->data[j];
        fprintf(fp, "%s%d", (j > 0 ? ", " : ""), vreg->virt);
      }
      fprintf(fp, "]");
    }
    fprintf(fp, "\n");

    for (int j = 0; j < bb->irs->len; ++j, ++nip) {
      fprintf(fp, "%6d|\t", nip);
      IR *ir = bb->irs->data[j];
      dump_ir(fp, ir);
    }
  }
  fprintf(fp, "\n");
}

void do_dump_ir(Vector *decls) {
  for (int i = 0, len = decls->len; i < len; ++i) {
    Declaration *decl = decls->data[i];
    if (decl == NULL)
      continue;

    switch (decl->kind) {
    case DCL_DEFUN:
      dump_func_ir(decl->defun.func);
      break;
    case DCL_VARDECL:
      break;
    }
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
  int iarg = 1;

  // Compile.
  init_compiler();

  Vector *toplevel = new_vector();
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

  gen(toplevel);

  do_dump_ir(toplevel);

  return 0;
}
