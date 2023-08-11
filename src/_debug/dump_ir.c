#include <assert.h>
#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "codegen.h"
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
  static char *kSize[] = {"0", "b", "w", "3", "d", "5", "6", "7", ""};
  if (vreg->flag & VRF_CONST) {
    fprintf(fp, "(%" PRId64 ")", vreg->fixnum);
  } else {
    char regtype = 'R';
#ifndef __NO_FLONUM
    if (vreg->vtype->flag & VRTF_FLONUM)
      regtype = 'F';
#endif
    fprintf(fp, "%c%d%s<v%d>", regtype, vreg->phys, kSize[vreg->vtype->size], vreg->virt);
  }
}

static void dump_ir(FILE *fp, IR *ir) {
  static char *kCond[] = {"__", "MP", "EQ", "NE", "LT", "LE", "GE", "GT", "ULT", "ULE", "UGE", "UGT"};

  switch (ir->kind) {
  case IR_BOFS:   fprintf(fp, "\tBOFS\t"); dump_vreg(fp, ir->dst); fprintf(fp, " = &[rbp %c %d]\n", ir->opr1->offset >= 0 ? '+' : '-', ir->opr1->offset > 0 ? ir->opr1->offset : -ir->opr1->offset); break;
  case IR_IOFS:   fprintf(fp, "\tIOFS\t"); dump_vreg(fp, ir->dst); fprintf(fp, " = &%.*s\n", ir->iofs.label->bytes, ir->iofs.label->chars); break;
  case IR_SOFS:   fprintf(fp, "\tSOFS\t"); dump_vreg(fp, ir->dst); fprintf(fp, " = &[rsp %c %" PRId64 "]\n", ir->opr1->fixnum >= 0 ? '+' : '-', ir->opr1->fixnum > 0 ? ir->opr1->fixnum : -ir->opr1->fixnum); break;
  case IR_LOAD:   fprintf(fp, "\tLOAD\t"); dump_vreg(fp, ir->dst); fprintf(fp, " = ["); dump_vreg(fp, ir->opr1); fprintf(fp, "]\n"); break;
  case IR_STORE:  fprintf(fp, "\tSTORE\t["); dump_vreg(fp, ir->opr2); fprintf(fp, "] = "); dump_vreg(fp, ir->opr1); fprintf(fp, "\n"); break;
  case IR_ADD:    fprintf(fp, "\tADD\t"); dump_vreg(fp, ir->dst); fprintf(fp, " = "); dump_vreg(fp, ir->opr1); fprintf(fp, " + "); dump_vreg(fp, ir->opr2); fprintf(fp, "\n"); break;
  case IR_SUB:    fprintf(fp, "\tSUB\t"); dump_vreg(fp, ir->dst); fprintf(fp, " = "); dump_vreg(fp, ir->opr1); fprintf(fp, " - "); dump_vreg(fp, ir->opr2); fprintf(fp, "\n"); break;
  case IR_MUL:    fprintf(fp, "\tMUL\t"); dump_vreg(fp, ir->dst); fprintf(fp, " = "); dump_vreg(fp, ir->opr1); fprintf(fp, " * "); dump_vreg(fp, ir->opr2); fprintf(fp, "\n"); break;
  case IR_DIV:    fprintf(fp, "\tDIV%s", ir->dst->vtype->flag & VRTF_UNSIGNED ? "U\t" : "\t"); dump_vreg(fp, ir->dst); fprintf(fp, " = "); dump_vreg(fp, ir->opr1); fprintf(fp, " / "); dump_vreg(fp, ir->opr2); fprintf(fp, "\n"); break;
  case IR_MOD:    fprintf(fp, "\tMOD%s", ir->dst->vtype->flag & VRTF_UNSIGNED ? "U\t" : "\t"); dump_vreg(fp, ir->dst); fprintf(fp, " = "); dump_vreg(fp, ir->opr1); fprintf(fp, " %% "); dump_vreg(fp, ir->opr2); fprintf(fp, "\n"); break;
  case IR_BITAND: fprintf(fp, "\tBITAND\t"); dump_vreg(fp, ir->dst); fprintf(fp, " = "); dump_vreg(fp, ir->opr1); fprintf(fp, " & "); dump_vreg(fp, ir->opr2); fprintf(fp, "\n"); break;
  case IR_BITOR:  fprintf(fp, "\tBITOR\t"); dump_vreg(fp, ir->dst); fprintf(fp, " = "); dump_vreg(fp, ir->opr1); fprintf(fp, " | "); dump_vreg(fp, ir->opr2); fprintf(fp, "\n"); break;
  case IR_BITXOR: fprintf(fp, "\tBITXOR\t"); dump_vreg(fp, ir->dst); fprintf(fp, " = "); dump_vreg(fp, ir->opr1); fprintf(fp, " ^ "); dump_vreg(fp, ir->opr2); fprintf(fp, "\n"); break;
  case IR_LSHIFT: fprintf(fp, "\tLSHIFT\t"); dump_vreg(fp, ir->dst); fprintf(fp, " = "); dump_vreg(fp, ir->opr1); fprintf(fp, " << "); dump_vreg(fp, ir->opr2); fprintf(fp, "\n"); break;
  case IR_RSHIFT: fprintf(fp, "\tRSHIFT\t"); dump_vreg(fp, ir->dst); fprintf(fp, " = "); dump_vreg(fp, ir->opr1); fprintf(fp, " >> "); dump_vreg(fp, ir->opr2); fprintf(fp, "\n"); break;
  case IR_CMP:    fprintf(fp, "\tCMP\t"); dump_vreg(fp, ir->opr1); fprintf(fp, " - "); dump_vreg(fp, ir->opr2); fprintf(fp, "\n"); break;
  case IR_NEG:    fprintf(fp, "\tNEG\t"); dump_vreg(fp, ir->dst); fprintf(fp, " = -"); dump_vreg(fp, ir->opr1); fprintf(fp, "\n"); break;
  case IR_BITNOT: fprintf(fp, "\tBITNOT\t"); dump_vreg(fp, ir->dst); fprintf(fp, " = ~"); dump_vreg(fp, ir->opr1); fprintf(fp, "\n"); break;
  case IR_COND:   fprintf(fp, "\tCOND\t"); dump_vreg(fp, ir->dst); fprintf(fp, " = %s\n", kCond[ir->cond.kind]); break;
  case IR_JMP:    fprintf(fp, "\tJ%s\t%.*s\n", kCond[ir->jmp.cond], ir->jmp.bb->label->bytes, ir->jmp.bb->label->chars); break;
  case IR_TJMP:
    fprintf(fp, "\tTJMP\t");
    dump_vreg(fp, ir->opr1);
    for (size_t i = 0; i < ir->tjmp.len; ++i) fprintf(fp, "%s%.*s", i == 0 ? ", [" : ", ", ((BB*)ir->tjmp.bbs[i])->label->bytes, ((BB*)ir->tjmp.bbs[i])->label->chars);
    fprintf(fp, "]\n");
    break;
  case IR_PRECALL: fprintf(fp, "\tPRECALL\n"); break;
  case IR_PUSHARG: fprintf(fp, "\tPUSHARG\t%d, ", ir->pusharg.index); dump_vreg(fp, ir->opr1); fprintf(fp, "\n"); break;
  case IR_CALL:
    if (ir->call.label != NULL) {
      fprintf(fp, "\tCALL\t"); if (ir->dst != NULL) { dump_vreg(fp, ir->dst); fprintf(fp, " = "); } fprintf(fp, "%.*s(args=#%d)\n", ir->call.label->bytes, ir->call.label->chars, ir->call.reg_arg_count);
    } else {
      fprintf(fp, "\tCALL\t"); if (ir->dst != NULL) { dump_vreg(fp, ir->dst); fprintf(fp, " = "); } fprintf(fp, "*"); dump_vreg(fp, ir->opr1); fprintf(fp, "(args=#%d)\n", ir->call.reg_arg_count);
    }
    break;
  case IR_RESULT: fprintf(fp, "\tRESULT\t"); dump_vreg(fp, ir->opr1); fprintf(fp, "\n"); break;
  case IR_SUBSP:  fprintf(fp, "\tSUBSP\t"); dump_vreg(fp, ir->opr1); fprintf(fp, "\n"); break;
  case IR_CAST:   fprintf(fp, "\tCAST\t"); dump_vreg(fp, ir->dst); fprintf(fp, " = "); dump_vreg(fp, ir->opr1); fprintf(fp, "\n"); break;
  case IR_MOV:    fprintf(fp, "\tMOV\t"); dump_vreg(fp, ir->dst); fprintf(fp, " = "); dump_vreg(fp, ir->opr1); fprintf(fp, "\n"); break;
  case IR_MEMCPY: fprintf(fp, "\tMEMCPY(dst="); dump_vreg(fp, ir->opr2); fprintf(fp, ", src="); dump_vreg(fp, ir->opr1); fprintf(fp, ", size=%zu)\n", ir->memcpy.size); break;
  case IR_CLEAR:  fprintf(fp, "\tCLEAR\t"); dump_vreg(fp, ir->opr1); fprintf(fp, ", %zu\n", ir->clear.size); break;
  case IR_ASM:    fprintf(fp, "\tASM \"%s\"\n", ir->asm_.str); break;
  case IR_LOAD_SPILLED:   fprintf(fp, "\tLOAD_SPILLED "); dump_vreg(fp, ir->dst); fprintf(fp, " = [v%d]\n", ir->opr1->virt); break;
  case IR_STORE_SPILLED:  fprintf(fp, "\tSTORE_SPILLED [v%d] = ", ir->opr2->virt); dump_vreg(fp, ir->opr1); fprintf(fp, "\n"); break;

  default: assert(false); break;
  }
}

static void dump_func_ir(Function *func) {
  FILE *fp = stdout;

  if (func->scopes == NULL)  // Prototype definition
    return;

  FuncBackend *fnbe = func->extra;
  BBContainer *bbcon = fnbe->bbcon;
  assert(bbcon != NULL);

  fprintf(fp, "### %.*s\n\n", func->name->bytes, func->name->chars);

  fprintf(fp, "params and locals:\n");
  for (int i = 0; i < func->scopes->len; ++i) {
    Scope *scope = func->scopes->data[i];
    if (scope->vars == NULL)
      continue;
    for (int j = 0; j < scope->vars->len; ++j) {
      VarInfo *varinfo = scope->vars->data[j];
      if (varinfo->storage & (VS_STATIC | VS_EXTERN | VS_ENUM_MEMBER) ||
          varinfo->local.vreg == NULL)
        continue;
      fprintf(fp, "  V%3d (flag=%x): %.*s\n", varinfo->local.vreg->virt, varinfo->local.vreg->flag,
              varinfo->name->bytes, varinfo->name->chars);
    }
  }

  RegAlloc *ra = fnbe->ra;
  fprintf(fp, "VREG: #%d\n", ra->vregs->len);
  LiveInterval **sorted_intervals = ra->sorted_intervals;
  if (sorted_intervals != NULL) {
    for (int i = 0; i < ra->vregs->len; ++i) {
      LiveInterval *li = sorted_intervals[i];
      VReg *vreg = ra->vregs->data[li->virt];
      if (vreg == NULL) {
        fprintf(fp, "  V%3d: unused\n", li->virt);
        continue;
      }

      switch (li->state) {
      case LI_NORMAL:
        {
          char regtype = 'R';
#ifndef __NO_FLONUM
          if (vreg->vtype->flag & VRTF_FLONUM)
            regtype = 'F';
#endif
          fprintf(fp, "  V%3d (flag=%x): live %3d - %3d, => %c%3d\n", li->virt, vreg->flag, li->start, li->end, regtype, li->phys);
        }
        break;
      case LI_SPILL:
        fprintf(fp, "  V%3d (flag=%x): live %3d - %3d (spilled, offset=%d)\n", li->virt, vreg->flag, li->start, li->end, vreg->offset);
        break;
      case LI_CONST:
        fprintf(fp, "  V%3d (flag=%x): (const, value=%" PRId64 ")\n", li->virt, vreg->flag, vreg->fixnum);
        break;
      default:  assert(false); break;
      }
    }
  }

  fprintf(fp, "BB: #%d\n", bbcon->bbs->len);
  for (int i = 0; i < bbcon->bbs->len; ++i) {
    BB *bb = bbcon->bbs->data[i];
    fprintf(fp, "// BB %d\n", i);
    fprintf(fp, "%.*s:", bb->label->bytes, bb->label->chars);
    if (bb->from_bbs->len > 0) {
      fprintf(fp, " from=[");
      for (int j = 0; j < bb->from_bbs->len; ++j) {
        BB *fbb = bb->from_bbs->data[j];
        fprintf(fp, "%s%.*s", (j > 0 ? ", " : ""), fbb->label->bytes, fbb->label->chars);
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

    for (int j = 0; j < bb->irs->len; ++j) {
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

    default:
      assert(false);
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

  toplevel = new_vector();
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
