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

#include "./xtest.h"

bool expect_parse_type(const char *title, const Type *expected, const char *ident_expected,
                       const char *source) {
  begin_test(title);

  set_source_file(NULL, title);
  set_source_string(source, "*test*", 1);
  int storage;
  Token *ident;
  const Type *actual = parse_var_def(NULL, &storage, &ident);
  if (actual == NULL && expected != NULL) {
    return fail("parsing type failed");
  } else if (!same_type(expected, actual)) {
    return fail("type different");
  } else if (ident_expected == NULL && ident != NULL) {
    return fail("ident is not NULL (%.*s)", NAMES(ident->ident));
  } else if (ident_expected != NULL && ident == NULL) {
    return fail("ident(%s) expected, but NULL", ident_expected);
  } else if (ident_expected != NULL && ident != NULL &&
             (ident->ident->bytes != (int)strlen(ident_expected) ||
              strncmp(ident->ident->chars, ident_expected, strlen(ident_expected)))) {
    return fail("ident(%s) expected, but (%.*s)", ident_expected, NAMES(ident->ident));
  } else if (fetch_token()->kind != TK_EOF) {
    return fail("EOF expected");
  }
  // OK.
  return true;
}

TEST(parse_full_type) {
  expect_parse_type("int", get_fixnum_type(FX_INT, false, 0), NULL, "int");
  expect_parse_type("short", get_fixnum_type(FX_SHORT, false, 0), NULL, "short");
  expect_parse_type("short int", get_fixnum_type(FX_SHORT, false, 0), NULL, "short int");
  expect_parse_type("int short", get_fixnum_type(FX_SHORT, false, 0), NULL, "int short");
  expect_parse_type("long", get_fixnum_type(FX_LONG, false, 0), NULL, "long");
  expect_parse_type("long int", get_fixnum_type(FX_LONG, false, 0), NULL, "long int");
  expect_parse_type("long long", get_fixnum_type(FX_LLONG, false, 0), NULL, "long long");
  expect_parse_type("int long", get_fixnum_type(FX_LONG, false, 0), NULL, "int long");
  expect_parse_type("void ptr", ptrof(&tyVoid), NULL, "void*");

#ifndef __NO_FLONUM
  expect_parse_type("double", &tyDouble, NULL, "double");
  expect_parse_type("float", &tyFloat, NULL, "float");
  expect_parse_type("long double", &tyLDouble, NULL, "long double");
#endif

  expect_parse_type("int array", arrayof(&tyInt, 3), "a", "int a[3]");
  expect_parse_type("array w/o size", arrayof(&tyChar, -1), NULL, "char[]");
  expect_parse_type("2d array", arrayof(arrayof(&tyInt, 3), 2), NULL, "int[2][3]");

  {
    Vector *params = new_vector();
    vec_push(params, get_fixnum_type(FX_LONG, false, 0));
    const Type *func = new_func_type(&tyInt, params, false);
    expect_parse_type("func", func, "func", "int func(long)");
  }
  {
    Vector *params = new_vector();
    vec_push(params, get_fixnum_type(FX_LONG, false, 0));
    const Type *funcptr = ptrof(new_func_type(&tyInt, params, false));
    expect_parse_type("func ptr", funcptr, "func", "int(*func)(long)");
  }
  {
    const Type *funcptr = ptrof(new_func_type(&tyVoid, NULL, true));
    expect_parse_type("func w/o params", funcptr, "func", "void(*func)()");
  }

  expect_parse_type("array of ptr", arrayof(ptrof(&tyInt), 3), NULL, "int *[3]");
  expect_parse_type("ptr of array", ptrof(arrayof(&tyInt, 3)), NULL, "int (*)[3]");

  {
    Vector *params = new_vector();
    Type *funcptr = ptrof(new_func_type(&tyInt, params, false));
    const Type *aofp = arrayof(funcptr, 4);
    expect_parse_type("array of func ptr", aofp, NULL, "int(*[4])(void)");
  }

  {
    Vector *params2 = new_vector();
    vec_push(params2, &tyInt);
    Type *param_funcptr = ptrof(new_func_type(&tyVoid, params2, false));

    Vector *params = new_vector();
    vec_push(params, &tyInt);
    vec_push(params, param_funcptr);
    const Type *functype = new_func_type(param_funcptr, params, false);
    expect_parse_type("signal", functype, "signal", "void(*signal(int, void(*)(int)))(int)");
  }
}

int main(void) {
  init_lexer();

  return xtest_main();
}
