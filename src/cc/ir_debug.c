#include "ir_debug.h"

#if !defined(SELF_HOSTING) && !defined(__XV6)
#include <assert.h>
#include <inttypes.h>
#include <stdio.h>

#include "ast.h"
#include "ir.h"
#include "regalloc.h"
#include "table.h"
#include "util.h"
#include "var.h"

static void dump_vreg(FILE *fp, VReg *vreg, int size) {
  assert(vreg != NULL);
  static char *kSize[] = {"0", "b", "w", "3", "d", "5", "6", "7", ""};
  if (vreg->flag & VRF_CONST) {
    fprintf(fp, "(%" PRIdPTR ")", vreg->fixnum);
  } else {
    char regtype = 'R';
#ifndef __NO_FLONUM
    if (vreg->vtype->flag & VRTF_FLONUM)
      regtype = 'F';
#endif
    fprintf(fp, "%c%d%s<v%d>", regtype, vreg->phys, kSize[size], vreg->virt);
  }
}

static void dump_ir(FILE *fp, IR *ir) {
  static char *kSize[] = {"0", "b", "w", "3", "d", "5", "6", "7", ""};
  static char *kCond[] = {"__", "MP", "EQ", "NE", "LT", "LE", "GE", "GT", "ULT", "ULE", "UGE", "UGT"};

  switch (ir->kind) {
  case IR_BOFS:   fprintf(fp, "\tBOFS\t"); dump_vreg(fp, ir->dst, ir->size); fprintf(fp, " = &[rbp %c %d]\n", ir->opr1->offset >= 0 ? '+' : '-', ir->opr1->offset > 0 ? ir->opr1->offset : -ir->opr1->offset); break;
  case IR_IOFS:   fprintf(fp, "\tIOFS\t"); dump_vreg(fp, ir->dst, ir->size); fprintf(fp, " = &%.*s\n", ir->iofs.label->bytes, ir->iofs.label->chars); break;
  case IR_SOFS:   fprintf(fp, "\tSOFS\t"); dump_vreg(fp, ir->dst, ir->size); fprintf(fp, " = &[rsp %c %ld]\n", ir->opr1->fixnum >= 0 ? '+' : '-', ir->opr1->fixnum > 0 ? ir->opr1->fixnum : -ir->opr1->fixnum); break;
  case IR_LOAD:   fprintf(fp, "\tLOAD\t"); dump_vreg(fp, ir->dst, ir->size); fprintf(fp, " = ["); dump_vreg(fp, ir->opr1, WORD_SIZE); fprintf(fp, "]\n"); break;
  case IR_STORE:  fprintf(fp, "\tSTORE\t["); dump_vreg(fp, ir->opr2, WORD_SIZE); fprintf(fp, "] = "); dump_vreg(fp, ir->opr1, ir->size); fprintf(fp, "\n"); break;
  case IR_ADD:    fprintf(fp, "\tADD\t"); dump_vreg(fp, ir->dst, ir->size); fprintf(fp, " = "); dump_vreg(fp, ir->opr1, ir->size); fprintf(fp, " + "); dump_vreg(fp, ir->opr2, ir->size); fprintf(fp, "\n"); break;
  case IR_SUB:    fprintf(fp, "\tSUB\t"); dump_vreg(fp, ir->dst, ir->size); fprintf(fp, " = "); dump_vreg(fp, ir->opr1, ir->size); fprintf(fp, " - "); dump_vreg(fp, ir->opr2, ir->size); fprintf(fp, "\n"); break;
  case IR_MUL:    fprintf(fp, "\tMUL\t"); dump_vreg(fp, ir->dst, ir->size); fprintf(fp, " = "); dump_vreg(fp, ir->opr1, ir->size); fprintf(fp, " * "); dump_vreg(fp, ir->opr2, ir->size); fprintf(fp, "\n"); break;
  case IR_DIV:    fprintf(fp, "\tDIV\t"); dump_vreg(fp, ir->dst, ir->size); fprintf(fp, " = "); dump_vreg(fp, ir->opr1, ir->size); fprintf(fp, " / "); dump_vreg(fp, ir->opr2, ir->size); fprintf(fp, "\n"); break;
  case IR_DIVU:   fprintf(fp, "\tDIVU\t"); dump_vreg(fp, ir->dst, ir->size); fprintf(fp, " = "); dump_vreg(fp, ir->opr1, ir->size); fprintf(fp, " / "); dump_vreg(fp, ir->opr2, ir->size); fprintf(fp, "\n"); break;
  case IR_MOD:    fprintf(fp, "\tMOD\t"); dump_vreg(fp, ir->dst, ir->size); fprintf(fp, " = "); dump_vreg(fp, ir->opr1, ir->size); fprintf(fp, " %% "); dump_vreg(fp, ir->opr2, ir->size); fprintf(fp, "\n"); break;
  case IR_MODU:   fprintf(fp, "\tMODU\t"); dump_vreg(fp, ir->dst, ir->size); fprintf(fp, " = "); dump_vreg(fp, ir->opr1, ir->size); fprintf(fp, " %% "); dump_vreg(fp, ir->opr2, ir->size); fprintf(fp, "\n"); break;
  case IR_BITAND: fprintf(fp, "\tBITAND\t"); dump_vreg(fp, ir->dst, ir->size); fprintf(fp, " = "); dump_vreg(fp, ir->opr1, ir->size); fprintf(fp, " & "); dump_vreg(fp, ir->opr2, ir->size); fprintf(fp, "\n"); break;
  case IR_BITOR:  fprintf(fp, "\tBITOR\t"); dump_vreg(fp, ir->dst, ir->size); fprintf(fp, " = "); dump_vreg(fp, ir->opr1, ir->size); fprintf(fp, " | "); dump_vreg(fp, ir->opr2, ir->size); fprintf(fp, "\n"); break;
  case IR_BITXOR: fprintf(fp, "\tBITXOR\t"); dump_vreg(fp, ir->dst, ir->size); fprintf(fp, " = "); dump_vreg(fp, ir->opr1, ir->size); fprintf(fp, " ^ "); dump_vreg(fp, ir->opr2, ir->size); fprintf(fp, "\n"); break;
  case IR_LSHIFT: fprintf(fp, "\tLSHIFT\t"); dump_vreg(fp, ir->dst, ir->size); fprintf(fp, " = "); dump_vreg(fp, ir->opr1, ir->size); fprintf(fp, " << "); dump_vreg(fp, ir->opr2, ir->size); fprintf(fp, "\n"); break;
  case IR_RSHIFT: fprintf(fp, "\tRSHIFT\t"); dump_vreg(fp, ir->dst, ir->size); fprintf(fp, " = "); dump_vreg(fp, ir->opr1, ir->size); fprintf(fp, " >> "); dump_vreg(fp, ir->opr2, ir->size); fprintf(fp, "\n"); break;
  case IR_CMP:    fprintf(fp, "\tCMP\t"); dump_vreg(fp, ir->opr1, ir->size); fprintf(fp, " - "); dump_vreg(fp, ir->opr2, ir->size); fprintf(fp, "\n"); break;
  case IR_INC:    fprintf(fp, "\tINC\t"); dump_vreg(fp, ir->opr1, WORD_SIZE); fprintf(fp, "%s += %" PRIdPTR "\n", kSize[ir->size], ir->value); break;
  case IR_DEC:    fprintf(fp, "\tDEC\t"); dump_vreg(fp, ir->opr1, WORD_SIZE); fprintf(fp, "%s -= %" PRIdPTR "\n", kSize[ir->size], ir->value); break;
  case IR_NEG:    fprintf(fp, "\tNEG\t"); dump_vreg(fp, ir->dst, ir->size); fprintf(fp, " = -"); dump_vreg(fp, ir->opr1, ir->size); fprintf(fp, "\n"); break;
  case IR_BITNOT: fprintf(fp, "\tBITNOT\t"); dump_vreg(fp, ir->dst, ir->size); fprintf(fp, " = ~"); dump_vreg(fp, ir->opr1, ir->size); fprintf(fp, "\n"); break;
  case IR_COND:    fprintf(fp, "\tCOND\t"); dump_vreg(fp, ir->dst, 4); fprintf(fp, " = %s\n", kCond[ir->cond.kind]); break;
  case IR_TEST:   fprintf(fp, "\tTEST\t"); dump_vreg(fp, ir->opr1, ir->size); fprintf(fp, "\n"); break;
  case IR_JMP:    fprintf(fp, "\tJ%s\t%.*s\n", kCond[ir->jmp.cond], ir->jmp.bb->label->bytes, ir->jmp.bb->label->chars); break;
  case IR_PRECALL: fprintf(fp, "\tPRECALL\n"); break;
  case IR_PUSHARG: fprintf(fp, "\tPUSHARG\t"); dump_vreg(fp, ir->opr1, ir->size); fprintf(fp, "\n"); break;
  case IR_CALL:
    if (ir->call.label != NULL) {
      fprintf(fp, "\tCALL\t"); dump_vreg(fp, ir->dst, ir->size); fprintf(fp, " = call %.*s(args=#%d)\n", ir->call.label->bytes, ir->call.label->chars, ir->call.reg_arg_count);
    } else {
      fprintf(fp, "\tCALL\t"); dump_vreg(fp, ir->dst, ir->size); fprintf(fp, " = *"); dump_vreg(fp, ir->opr1, WORD_SIZE); fprintf(fp, "(args=#%d)\n", ir->call.reg_arg_count);
    }
    break;
  case IR_RESULT: fprintf(fp, "\tRESULT\t"); dump_vreg(fp, ir->opr1, ir->size); fprintf(fp, "\n"); break;
  case IR_ADDSP:  fprintf(fp, "\tADDSP\t%" PRIdPTR "\n", ir->value); break;
  case IR_CAST:   fprintf(fp, "\tCAST\t"); dump_vreg(fp, ir->dst, ir->size); fprintf(fp, " = "); dump_vreg(fp, ir->opr1, ir->opr1->vtype->size); fprintf(fp, "\n"); break;
  case IR_MOV:    fprintf(fp, "\tMOV\t"); dump_vreg(fp, ir->dst, ir->size); fprintf(fp, " = "); dump_vreg(fp, ir->opr1, ir->size); fprintf(fp, "\n"); break;
  case IR_MEMCPY: fprintf(fp, "\tMEMCPY(dst="); dump_vreg(fp, ir->opr2, WORD_SIZE); fprintf(fp, ", src="); dump_vreg(fp, ir->opr1, WORD_SIZE); fprintf(fp, ", size=%d)\n", ir->size); break;
  case IR_CLEAR:  fprintf(fp, "\tCLEAR\t"); dump_vreg(fp, ir->opr1, WORD_SIZE); fprintf(fp, ", %d\n", ir->size); break;
  case IR_ASM:    fprintf(fp, "\tASM \"%s\"\n", ir->asm_.str); break;
  case IR_LOAD_SPILLED:   fprintf(fp, "\tLOAD_SPILLED "); dump_vreg(fp, ir->dst, ir->size); fprintf(fp, " = [rbp %+d]\n", (int)ir->value); break;
  case IR_STORE_SPILLED:  fprintf(fp, "\tSTORE_SPILLED [rbp %+d] = ", (int)ir->value); dump_vreg(fp, ir->opr1, ir->size); fprintf(fp, "\n"); break;

  default: assert(false); break;
  }
}

static void dump_func_ir(Function *func) {
  FILE *fp = stdout;

  if (func->scopes == NULL)  // Prototype definition
    return;

  BBContainer *bbcon = func->bbcon;
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
          varinfo->local.reg == NULL)
        continue;
      fprintf(fp, "  V%3d (flag=%x): %.*s\n", varinfo->local.reg->virt, varinfo->local.reg->flag,
              varinfo->name->bytes, varinfo->name->chars);
    }
  }

  RegAlloc *ra = func->ra;
  fprintf(fp, "VREG: #%d\n", ra->vregs->len);
  LiveInterval **sorted_intervals = func->ra->sorted_intervals;
  for (int i = 0; i < ra->vregs->len; ++i) {
    LiveInterval *li = sorted_intervals[i];
    VReg *vreg = ra->vregs->data[li->virt];
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
      fprintf(fp, "  V%3d (flag=%x): (const, value=%" PRIdPTR ")\n", li->virt, vreg->flag, vreg->fixnum);
      break;
    default:  assert(false); break;
    }
  }

  fprintf(fp, "BB: #%d\n", bbcon->bbs->len);
  for (int i = 0; i < bbcon->bbs->len; ++i) {
    BB *bb = bbcon->bbs->data[i];
    fprintf(fp, "// BB %d\n", i);
    fprintf(fp, "%.*s:\n", bb->label->bytes, bb->label->chars);
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
#endif
