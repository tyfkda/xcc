#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/wait.h>
#include <unistd.h>

#include "lexer.h"
#include "parser.h"
#include "table.h"
#include "type.h"
#include "util.h"

void expect_parse_type(const char *title, const Type *expected, const char *ident_expected, const char *source) {
  pid_t pid = fork();
  if (pid < 0) {
    fprintf(stderr, "fork failed\n");
    exit(1);
  }
  if (pid == 0) {
    set_source_file(NULL, title);
    set_source_string(source, "*test*", 1);
    int storage;
    Token *ident;
    const Type *actual = parse_var_def(NULL, &storage, &ident);
    if (actual == NULL && expected != NULL)
      error("%s: parsing type failed\n", title);

    const Token *end = fetch_token();
    if (end->kind != TK_EOF)
      parse_error(PE_FATAL, end, "EOF expected\n");

    if (!same_type(expected, actual))
      error("%s: type different\n", title);
    if (ident_expected == NULL && ident != NULL)
      error("%s: ident is not NULL (%.*s)\n", title, ident->ident->bytes, ident->ident->chars);
    if (ident_expected != NULL && ident == NULL)
      error("%s: ident(%s) expected, but NULL\n", title, ident_expected);
    if (ident_expected != NULL && ident != NULL &&
        (ident->ident->bytes != (int)strlen(ident_expected) || strncmp(ident->ident->chars, ident_expected, strlen(ident_expected))))
      error("%s: ident(%s) expected, but (%.*s)\n", title, ident_expected, ident->ident->bytes, ident->ident->chars);

    printf("%s => OK\n", title);
    exit(0);
  }

  int ec = -1;
  if (waitpid(pid, &ec, 0) < 0)
    error("wait failed");
  if (ec != 0)
    exit(1);
}

void test_parse_full_type(void) {
  expect_parse_type("int", get_fixnum_type(FX_INT, false, 0), NULL, "int");
  expect_parse_type("short", get_fixnum_type(FX_SHORT, false, 0), NULL, "short");
  expect_parse_type("short int", get_fixnum_type(FX_SHORT, false, 0), NULL, "short int");
  expect_parse_type("int short", get_fixnum_type(FX_SHORT, false, 0), NULL, "int short");
  expect_parse_type("long", get_fixnum_type(FX_LONG, false, 0), NULL, "long");
  expect_parse_type("long int", get_fixnum_type(FX_LONG, false, 0), NULL, "long int");
  expect_parse_type("long long", get_fixnum_type(FX_LLONG, false, 0), NULL, "long long");
  expect_parse_type("int long", get_fixnum_type(FX_LONG, false, 0), NULL, "int long");
  expect_parse_type("void ptr", ptrof(&tyVoid), NULL, "void*");
  expect_parse_type("int array", arrayof(&tyInt, 3), "a", "int a[3]");
  expect_parse_type("array w/o size", arrayof(&tyChar, -1), NULL, "char[]");
  expect_parse_type("2d array", arrayof(arrayof(&tyInt, 3), 2), NULL, "int[2][3]");

  {
    Vector *param_types = new_vector();
    vec_push(param_types, get_fixnum_type(FX_LONG, false, 0));
    const Type *func = new_func_type(&tyInt, NULL, param_types, false);
    expect_parse_type("func", func, "func", "int func(long)");
  }
  {
    Vector *param_types = new_vector();
    vec_push(param_types, get_fixnum_type(FX_LONG, false, 0));
    const Type *funcptr = ptrof(new_func_type(&tyInt, NULL, param_types, false));
    expect_parse_type("func ptr", funcptr, "func", "int(*func)(long)");
  }
  {
    const Type *funcptr = ptrof(new_func_type(&tyVoid, NULL, NULL, true));
    expect_parse_type("func w/o params", funcptr, "func", "void(*func)()");
  }

  expect_parse_type("array of ptr", arrayof(ptrof(&tyInt), 3), NULL, "int *[3]");
  expect_parse_type("ptr of array", ptrof(arrayof(&tyInt, 3)), NULL, "int (*)[3]");

  {
    Vector *param_types = new_vector();
    Type *funcptr = ptrof(new_func_type(&tyInt, NULL, param_types, false));
    const Type *aofp = arrayof(funcptr, 4);
    expect_parse_type("array of func ptr", aofp, NULL, "int(*[4])(void)");
  }

  {
    Vector *param_types2 = new_vector();
    vec_push(param_types2, &tyInt);
    Type *param_funcptr = ptrof(new_func_type(&tyVoid, NULL, param_types2, false));

    Vector *param_types = new_vector();
    vec_push(param_types, &tyInt);
    vec_push(param_types, param_funcptr);
    const Type *functype = new_func_type(param_funcptr, NULL, param_types, false);
    expect_parse_type("signal", functype, "signal", "void(*signal(int, void(*)(int)))(int)");
  }

#ifndef __NO_FLONUM
  expect_parse_type("double", &tyDouble, NULL, "double");
  expect_parse_type("float", &tyFloat, NULL, "float");
  expect_parse_type("long double", &tyLDouble, NULL, "long double");
#endif
}

void runtest(void) {
  test_parse_full_type();
}

int main(void) {
  init_lexer();

  runtest();
  return 0;
}
