#include <assert.h>
#include <inttypes.h>
#include <printf.h>
#include <stdio.h>
#include <string.h>

#include "ast.h"
#include "fe_misc.h"
#include "lexer.h"
#include "parser.h"
#include "table.h"
#include "type.h"
#include "util.h"
#include "var.h"

#ifdef __GNUC__
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-function"
#endif

void dump_init(FILE *fp, const Initializer *init);
void dump_expr(FILE *fp, Expr *expr);

////////////////////////////////////////////////

#ifndef __APPLE__
#pragma GCC diagnostic ignored "-Wformat"
#pragma GCC diagnostic ignored "-Wformat-extra-args"

// typedef int printf_arginfo_size_function (const struct printf_info *__info,
// 					  size_t __n, int *__argtypes,
// 					  int *__size);

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

static int print_init_fn(FILE *stream, const struct printf_info *info, const void *const *args) {
  UNUSED(info);
  Initializer *init = *((Initializer**)args[0]);
  dump_init(stream, init);
  return 0;  // TODO!
}

static int print_expr_fn(FILE *stream, const struct printf_info *info, const void *const *args) {
  UNUSED(info);
  Expr *expr = *((Expr**)args[0]);
  dump_expr(stream, expr);
  return 0;  // TODO!
}

static int print_type_fn(FILE *stream, const struct printf_info *info, const void *const *args) {
  UNUSED(info);
  Type *type = *((Type**)args[0]);
  print_type(stream, type);
  return 0;  // TODO!
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

static const char *table[] = {
  [EX_ADD] = "+",
  [EX_SUB] = "-",
  [EX_MUL] = "*",
  [EX_DIV] = "/",
  [EX_MOD] = "%",
  [EX_BITAND] = "&",
  [EX_BITOR] = "|",
  [EX_BITXOR] = "^",
  [EX_LSHIFT] = "<<",
  [EX_RSHIFT] = ">>",
  [EX_EQ] = "==",
  [EX_NE] = "!=",
  [EX_LT] = "<",
  [EX_LE] = "<=",
  [EX_GE] = ">=",
  [EX_GT] = ">",
  [EX_LOGAND] = "&&",
  [EX_LOGIOR] = "||",
  [EX_ASSIGN] = "=",
  [EX_COMMA] = ",",

  [EX_POS] = "+",
  [EX_NEG] = "-",

  [EX_BITNOT] = "~",  // ~x
  [EX_REF] = "&",     // &
  [EX_DEREF] = "*",   // *
  // [EX_CAST] = "",
};

static const char incdec[][3] = {"++", "--"};

void dump_init(FILE *fp, const Initializer *init) {
  if (init == NULL) {
    fprintf(fp, "NULL");
    return;
  }

  switch (init->kind) {
  case IK_SINGLE:
    dump_expr(fp, init->single);
    break;
  case IK_MULTI:
    {
      fprintf(fp, "{");
      Vector *multi = init->multi;
      const char *sep = "";
      for (int i = 0; i < multi->len; ++i) {
        FPRINTF(fp, "%s%I", sep, multi->data[i]);
        sep = ", ";
      }
      fprintf(fp, "}");
    }
    break;
  case IK_DOT:
    FPRINTF(fp, ".%.*s=%I", NAMES(init->dot.name), init->dot.value);
    break;
  case IK_BRKT:
    FPRINTF(fp, "[%zu]=%I", init->bracket.index, init->bracket.value);
    break;
  }
}

static void dump_args(FILE *fp, Vector *args) {
  fprintf(fp, "(");
  for (int i = 0; i < args->len; ++i)
    FPRINTF(fp, "%s%V", i == 0 ? "" : ", ", args->data[i]);
  fprintf(fp, ")");
}

void dump_expr(FILE *fp, Expr *expr) {
  assert(expr != NULL);
  switch (expr->kind) {
  case EX_FIXNUM:
    if (expr->type->kind != TY_FIXNUM || expr->type->fixnum.is_unsigned)
      fprintf(fp, "%" PRIu64 "U", expr->fixnum);
    else
      fprintf(fp, "%" PRId64, expr->fixnum);
    if (expr->type->kind == TY_FIXNUM) {
      switch (expr->type->fixnum.kind) {
      case FX_LONG:
        fputc('L', fp);
        break;
      case FX_LLONG:
        fputs("LL", fp);
        break;
      default: break;
      }
    }
    break;
  case EX_FLONUM:
#ifndef __NO_FLONUM
    {
      char buf[64];
      snprintf(buf, sizeof(buf) - 4, "%Lg", expr->flonum);
      if (strchr(buf, '.') == NULL)
        strcat(buf, ".0");
      switch (expr->type->flonum.kind) {
      case FL_FLOAT:  strcat(buf, "f"); break;
      case FL_DOUBLE:  break;
      case FL_LDOUBLE:  strcat(buf, "L"); break;
      }
      fputs(buf, fp);
    }
#else
    assert(false);
#endif
    break;
  case EX_STR:
    {
      StringBuffer sb;
      sb_init(&sb);
      sb_append(&sb, "\"", NULL);
      escape_string(expr->str.buf, expr->str.len, &sb);
      sb_append(&sb, "\"", NULL);
      fputs(sb_to_string(&sb), fp);
    }
    break;
  case EX_VAR:
    fprintf(fp, "%.*s", NAMES(expr->var.name));
    break;
  case EX_ADD:
  case EX_SUB:
  case EX_MUL:
  case EX_DIV:
  case EX_MOD:
  case EX_BITAND:
  case EX_BITOR:
  case EX_BITXOR:
  case EX_LSHIFT:
  case EX_RSHIFT:
  case EX_EQ:
  case EX_NE:
  case EX_LT:
  case EX_LE:
  case EX_GE:
  case EX_GT:
  case EX_LOGAND:
  case EX_LOGIOR:
  case EX_ASSIGN:
    FPRINTF(fp, "(%V %s %V)", expr->bop.lhs, table[expr->kind], expr->bop.rhs);
    break;
  case EX_COMMA:
    FPRINTF(fp, "(%V, %V)", expr->bop.lhs, expr->bop.rhs);
    break;

  case EX_POS:
  case EX_NEG:
  case EX_BITNOT:
  case EX_REF:
  case EX_DEREF:
    FPRINTF(fp, "%s(%V)", table[expr->kind], expr->unary.sub);
    break;
  case EX_PREINC:
  case EX_PREDEC:
  case EX_POSTINC:
  case EX_POSTDEC:
    {
#define IS_POST(expr)  ((expr)->kind >= EX_POSTINC)
#define IS_DEC(expr)   (((expr)->kind - EX_PREINC) & 1)
      if (!IS_POST(expr))
        fputs(incdec[IS_DEC(expr)], fp);
      Expr *target = expr->unary.sub;
      FPRINTF(fp, target->kind == EX_VAR ? "%V" : "(%V)", target);
      if (IS_POST(expr))
        fputs(incdec[IS_DEC(expr)], fp);
#undef IS_POST
#undef IS_DEC
    }
    break;
  case EX_CAST:
    FPRINTF(fp, "(%T)%V", expr->type, expr->unary.sub);
    break;

  case EX_TERNARY:
    FPRINTF(fp, "(%V ? %V : %V)", expr->ternary.cond, expr->ternary.tval, expr->ternary.fval);
    break;

  case EX_MEMBER:
    {
      FPRINTF(fp, "%V%s", expr->member.target, expr->token->kind == TK_DOT ? "." : "->");
      const Name *ident = expr->member.ident;
      if (ident != NULL)
        fprintf(fp, "%.*s", NAMES(ident));
      else
        fprintf(fp, "*anonymous*");
    }
    break;
  case EX_FUNCALL:
    {
      Expr *func = expr->funcall.func;
      if (func->kind == EX_VAR)
        fprintf(fp, "%.*s", NAMES(func->var.name));
      else
        FPRINTF(fp, "(%V)", func);
      dump_args(fp, expr->funcall.args);
    }
    break;
  case EX_INLINED:
    fprintf(fp, "%.*s", NAMES(expr->inlined.funcname));
    dump_args(fp, expr->inlined.args);
    break;
  case EX_COMPLIT:
    FPRINTF(fp, "((%T)%I)", expr->type, expr->complit.original_init);
    break;
  case EX_BLOCK:
    // TODO: Dump statement.
    assert(!"not implemented");
    break;
  }
}

#ifndef NO_MAIN_DUMP_EXPR
int main(int argc, char *argv[]) {
  if (argc < 2) {
    fprintf(stderr, "dump_expr: [declarations] expr...\n");
    return 1;
  }

#ifdef __APPLE__
  g_domain = new_printf_domain();
  if (g_domain == NULL) {
    fprintf(stderr, "new_printf_domain failed\n");
    return 1;
  }
#endif
  REGISTER_PRINTF_SPECIFIER('I', print_init_fn, print_single_pointer_arginfo);
  REGISTER_PRINTF_SPECIFIER('V', print_expr_fn, print_single_pointer_arginfo);
  REGISTER_PRINTF_SPECIFIER('T', print_type_fn, print_single_pointer_arginfo);

  init_lexer();
  init_global();

  Vector *toplevel = new_vector();
  int i = 1;
  if (i < argc - 1) {
    set_source_string(argv[i++], "*decl*", 1);
    parse(toplevel);
  }

  Scope *scope = new_scope(global_scope);
  curscope = scope;

  for (; i < argc; ++i) {
    char *source = argv[i];
    set_source_string(source, "*exp*", 1);

    Expr *expr = parse_expr();
    if (compile_error_count != 0)
      return 1;

    FILE *fp = stdout;
    fprintf(fp, "%s : ", source);
    print_type(fp, expr->type);
    FPRINTF(fp, " => %V\n", expr);
  }

#ifdef __APPLE__
  free_printf_domain(g_domain);
#endif

  return 0;
}
#endif

#ifdef __GNUC__
#pragma GCC diagnostic pop
#endif
