#include <assert.h>
#include <inttypes.h>
#include <printf.h>
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

////////////////////////////////////////////////

extern void install_builtins(void);

static int dump_vreg(FILE *fp, VReg *vreg) {
  assert(vreg != NULL);
  assert(!(vreg->flag & VRF_SPILLED));
  static const char *kSize[] = {"b", "w", "d", ""};
  if (vreg->flag & VRF_CONST) {
    return fprintf(fp, "(%" PRId64 ")", vreg->fixnum);
  } else if (vreg->phys >= 0) {
    char regtype = (vreg->flag & VRF_FLONUM) ? 'F' : 'R';
    return fprintf(fp, "%c%d%s<v%d>", regtype, vreg->phys, kSize[vreg->vsize], vreg->virt);
  } else {
    return fprintf(fp, "v%d", vreg->virt);
  }
}

////////////////////////////////////////////////

#ifndef __APPLE__
#pragma GCC diagnostic ignored "-Wformat"
#pragma GCC diagnostic ignored "-Wformat-extra-args"

#define FPRINTF  fprintf
#define REGISTER_PRINTF_SPECIFIER  register_printf_specifier
#define GLUE_PRINTF_ARGINFO_FUNC(specfn)
#else
#include <alloca.h>
printf_domain_t g_domain;
#define FPRINTF(fp, ...)  fxprintf(fp, g_domain, NULL, __VA_ARGS__)
#define REGISTER_PRINTF_SPECIFIER(spec, render, arginfo)  \
    register_printf_domain_function(g_domain, spec, render, arginfo ## _glue, NULL)
#define GLUE_PRINTF_ARGINFO_FUNC(specfn)  \
    static int specfn ## _glue(const struct printf_info *info, size_t n, int *argtypes) { \
        int *psize = alloca(n * sizeof(*psize)); return specfn(info, n, argtypes, psize); }
#endif

static int print_vreg_fn(FILE *stream, const struct printf_info *info, const void *const *args) {
  UNUSED(info);
  VReg *vreg = *((VReg**)args[0]);
  return dump_vreg(stream, vreg);
}

static int print_single_pointer_arginfo(const struct printf_info *info, size_t n, int *argtypes, int *psize) {
  UNUSED(info);
  UNUSED(psize);
  if (n > 0)
    argtypes[0] = PA_POINTER;
  return 1;
}
GLUE_PRINTF_ARGINFO_FUNC(print_single_pointer_arginfo)

////////////////////////////////////////////////

static void dump_ir(FILE *fp, IR *ir) {
  static char *kOps[] = {
    "BOFS", "IOFS", "SOFS", "LOAD", "LOAD_S", "STORE", "STORE_S",
    "ADD", "SUB", "MUL", "DIV", "MOD", "BITAND", "BITOR", "BITXOR", "LSHIFT", "RSHIFT",
    "NEG", "BITNOT", "COND", "JMP", "TJMP",
    "PRECALL", "PUSHARG", "CALL", "RESULT", "SUBSP",
    "CAST", "MOV", "KEEP", "ASM",
  };
  static char *kCond[] = {NULL, "MP", "EQ", "NE", "LT", "LE", "GE", "GT", NULL, "MP", "EQ", "NE", "ULT", "ULE", "UGE", "UGT"};
  static char *kCond2[] = {NULL, "MP", "==", "!=", "<", "<=", ">=", ">", NULL, "MP", "==", "!=", "<", "<=", ">=", ">"};

  switch (ir->kind) {
  case IR_DIV:
  case IR_MOD:
    fprintf(fp, "%s%s\t", kOps[ir->kind], ir->flag & IRF_UNSIGNED ? "U" : "");
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
  case IR_BOFS:   { int64_t offset = ir->bofs.frameinfo->offset + ir->bofs.offset; FPRINTF(fp, "%R = &[rbp %c %" PRId64 "]\n", ir->dst, offset >= 0 ? '+' : '-', offset > 0 ? offset : -offset); } break;
  case IR_IOFS:   FPRINTF(fp, "%R = &%.*s", ir->dst, NAMES(ir->iofs.label)); if (ir->iofs.offset != 0) { int64_t offset = ir->iofs.offset; fprintf(fp, " %c %" PRId64, offset >= 0 ? '+' : '-', offset > 0 ? offset : -offset); } fprintf(fp, "\n"); break;
  case IR_SOFS:   FPRINTF(fp, "%R = &[rsp %c %" PRId64 "]\n", ir->dst, ir->opr1->fixnum >= 0 ? '+' : '-', ir->opr1->fixnum > 0 ? ir->opr1->fixnum : -ir->opr1->fixnum); break;
  case IR_LOAD:   FPRINTF(fp, "%R = [%R]\n", ir->dst, ir->opr1); break;
  case IR_LOAD_S: FPRINTF(fp, "%R = [v%d]\n", ir->dst, ir->opr1->virt); break;
  case IR_STORE:  FPRINTF(fp, "[%R] = %R\n", ir->opr2, ir->opr1); break;
  case IR_STORE_S:FPRINTF(fp, "[v%d] = %R\n", ir->opr2->virt, ir->opr1); break;
  case IR_ADD:    FPRINTF(fp, "%R = %R + %R\n", ir->dst, ir->opr1, ir->opr2); break;
  case IR_SUB:    FPRINTF(fp, "%R = %R - %R\n", ir->dst, ir->opr1, ir->opr2); break;
  case IR_MUL:    FPRINTF(fp, "%R = %R * %R\n", ir->dst, ir->opr1, ir->opr2); break;
  case IR_DIV:    FPRINTF(fp, "%R = %R / %R\n", ir->dst, ir->opr1, ir->opr2); break;
  case IR_MOD:    FPRINTF(fp, "%R = %R %% %R\n", ir->dst, ir->opr1, ir->opr2); break;
  case IR_BITAND: FPRINTF(fp, "%R = %R & %R\n", ir->dst, ir->opr1, ir->opr2); break;
  case IR_BITOR:  FPRINTF(fp, "%R = %R | %R\n", ir->dst, ir->opr1, ir->opr2); break;
  case IR_BITXOR: FPRINTF(fp, "%R = %R ^ %R\n", ir->dst, ir->opr1, ir->opr2); break;
  case IR_LSHIFT: FPRINTF(fp, "%R = %R << %R\n", ir->dst, ir->opr1, ir->opr2); break;
  case IR_RSHIFT: FPRINTF(fp, "%R = %R >> %R\n", ir->dst, ir->opr1, ir->opr2); break;
  case IR_NEG:    FPRINTF(fp, "%R = -%R\n", ir->dst, ir->opr1); break;
  case IR_BITNOT: FPRINTF(fp, "%R = ~%R\n", ir->dst, ir->opr1); break;
  case IR_COND:   FPRINTF(fp, "%R = ", ir->dst); if (ir->cond.kind != COND_ANY && ir->cond.kind != COND_NONE) {FPRINTF(fp, "%R %s %R", ir->opr1, kCond2[ir->cond.kind & (COND_MASK | COND_UNSIGNED)], ir->opr2);} fprintf(fp, "\n"); break;
  case IR_JMP:    if (ir->jmp.cond != COND_ANY && ir->jmp.cond != COND_NONE) {FPRINTF(fp, "%R, %R, ", ir->opr1, ir->opr2);} fprintf(fp, "%.*s\n", NAMES(ir->jmp.bb->label)); break;
  case IR_TJMP:
    FPRINTF(fp, "%R", ir->opr1);
    for (size_t i = 0; i < ir->tjmp.len; ++i)
      fprintf(fp, "%s%.*s", i == 0 ? ", [" : ", ", NAMES(((BB*)ir->tjmp.bbs[i])->label));
    fprintf(fp, "]");
    if (ir->opr2 != NULL) {fprintf(fp, " (tmp="); dump_vreg(fp, ir->opr2); fprintf(fp, ")");}
    fprintf(fp, "\n");
    break;
  case IR_PRECALL: fprintf(fp, "\n"); break;
  case IR_PUSHARG: FPRINTF(fp, "%d, %R\n", ir->pusharg.index, ir->opr1); break;
  case IR_CALL:
    if (ir->dst != NULL)
      FPRINTF(fp, "%R = ", ir->dst);
    if (ir->call.label != NULL)
      fprintf(fp, "%.*s", NAMES(ir->call.label));
    else
      FPRINTF(fp, "*%R", ir->opr1);
    fprintf(fp, "(args=#%d)\n", ir->call.reg_arg_count);
    break;
  case IR_RESULT: if (ir->dst != NULL) { FPRINTF(fp, "%R\n", ir->dst); } FPRINTF(fp, "%R\n", ir->opr1); break;
  case IR_SUBSP:  FPRINTF(fp, "%R\n", ir->opr1); break;
  case IR_CAST:   FPRINTF(fp, "%R = %R\n", ir->dst, ir->opr1); break;
  case IR_MOV:    FPRINTF(fp, "%R = %R\n", ir->dst, ir->opr1); break;
  case IR_KEEP:
    if (ir->dst != NULL) FPRINTF(fp, "dst:%R, ", ir->dst);
    if (ir->opr1 != NULL) {
      FPRINTF(fp, "%R", ir->opr1);
      if (ir->opr2 != NULL)
        FPRINTF(fp, ", %R", ir->opr2);
    }
    fprintf(fp, "\n");
    break;
  case IR_ASM:    fprintf(fp, "\"%s\"\n", ir->asm_.str); break;
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

  fprintf(fp, "### %.*s\n\n", NAMES(func->name));

  fprintf(fp, "params and locals:\n");
  Vector *stack_vars = new_vector();
  for (int i = 0; i < func->scopes->len; ++i) {
    Scope *scope = func->scopes->data[i];
    if (scope->vars == NULL)
      continue;
    for (int j = 0; j < scope->vars->len; ++j) {
      VarInfo *varinfo = scope->vars->data[j];
      if (!is_local_storage(varinfo))
        continue;
      VReg *vreg = varinfo->local.vreg;
      if (vreg == NULL) {
        vec_push(stack_vars, varinfo);
        continue;
      }
      fprintf(fp, "  V%3d (flag=%x): %.*s  : ", vreg->virt, vreg->flag, NAMES(varinfo->name));
      print_type(fp, varinfo->type);
      fprintf(fp, "\n");
    }
  }
  for (int i = 0; i < stack_vars->len; ++i) {
    VarInfo *varinfo = stack_vars->data[i];
    fprintf(fp, "  stack (offset=%4d, size=%zu): %.*s  : ", varinfo->local.frameinfo->offset,
            type_size(varinfo->type), NAMES(varinfo->name));
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
          char regtype = 'R';
          if (vreg->flag & VRF_FLONUM)
            regtype = 'F';
          fprintf(fp, "  V%3d (flag=%x): live %3d - %3d, => %c%3d", li->virt, vreg->flag, li->start,
                  li->end, regtype, li->phys);
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
    if (bb->in_regs->len > 0) {
      fprintf(fp, " in=[");
      for (int j = 0; j < bb->in_regs->len; ++j) {
        VReg *vreg = bb->in_regs->data[j];
        fprintf(fp, "%s%d", (j > 0 ? ", " : ""), vreg->virt);
      }
      fprintf(fp, "]");
    }
    if (bb->out_regs->len > 0) {
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
  if (decls == NULL)
    return;

  for (int i = 0, len = decls->len; i < len; ++i) {
    Declaration *decl = decls->data[i];
    if (decl == NULL || decl->kind != DCL_DEFUN)
      continue;
    Function *func = decl->defun.func;
    if (!gen_defun(func))
      continue;

    curfunc = func;
    FuncBackend *fnbe = func->extra;

    optimize(fnbe->ra, fnbe->bbcon);

    prepare_register_allocation(func);
    tweak_irs(fnbe);
    analyze_reg_flow(fnbe->bbcon);

    alloc_physical_registers(fnbe->ra, fnbe->bbcon);
    map_virtual_to_physical_registers(fnbe->ra);
    detect_living_registers(fnbe->ra, fnbe->bbcon);

    alloc_stack_variables_onto_stack_frame(func);

    curfunc = NULL;

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
  int iarg = 1;

#ifdef __APPLE__
  g_domain = new_printf_domain();
  if (g_domain == NULL) {
    fprintf(stderr, "new_printf_domain failed\n");
    return 1;
  }
#endif
  REGISTER_PRINTF_SPECIFIER('R', print_vreg_fn, print_single_pointer_arginfo);

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

  do_dump_ir(toplevel);

#ifdef __APPLE__
  free_printf_domain(g_domain);
#endif

  return 0;
}
