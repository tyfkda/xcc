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
    set_source_string(source, "*test*", 1);
    int flag;
    Token *ident;
    const Type *actual = parse_full_type(&flag, &ident);
    if (!same_type(expected, actual, NULL))
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
  expect_parse_type("int", &tyInt, NULL, "int");
  expect_parse_type("long int", &tyLong, NULL, "long int");
  expect_parse_type("long long", &tyLLong, NULL, "long long");
  expect_parse_type("void ptr", ptrof(&tyVoid), NULL, "void*");
  expect_parse_type("int array", arrayof(&tyInt, 3), "a", "int a[3]");
  expect_parse_type("array w/o size", arrayof(&tyChar, -1), NULL, "char[]");
  expect_parse_type("2d array", arrayof(arrayof(&tyInt, 3), 2), NULL, "int[2][3]");

  {
    Vector *param_types = new_vector();
    vec_push(param_types, &tyLong);
    const Type *funcptr = ptrof(new_func_type(&tyInt, NULL, param_types, false));
    expect_parse_type("func ptr", funcptr, "func", "int(*func)(long)");
  }
  {
    const Type *funcptr = ptrof(new_func_type(&tyVoid, NULL, NULL, false));
    expect_parse_type("func w/o params", funcptr, "func", "void(*func)()");
  }

  expect_parse_type("array of ptr", arrayof(ptrof(&tyInt), 3), NULL, "int *[3]");
  expect_parse_type("ptr of array", ptrof(arrayof(&tyInt, 3)), NULL, "int (*)[3]");

  {
    Vector *param_types = new_vector();
    const Type *funcptr = ptrof(new_func_type(&tyInt, NULL, param_types, false));
    const Type *aofp = arrayof(funcptr, 4);
    expect_parse_type("array of func ptr", aofp, NULL, "int(*[4])(void)");
  }
}

void runtest(void) {
  test_parse_full_type();
}

int main(void) {
  init_lexer();

  runtest();
  return 0;
}
