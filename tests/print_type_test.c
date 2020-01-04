#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "type.h"
#include "util.h"

void check_print_type(const char *expected, const Type *type) {
  printf("%s - ", expected);
  fflush(stdout);

  FILE *fp = fopen("/dev/null", "w");
  if (fp == NULL) {
    fprintf(stderr, "Failed to create tmpfile\n");
    exit(1);
  }
  char buf[256];
  memset(buf, '\0', sizeof(buf));
  if (setvbuf(fp, buf, _IOFBF, sizeof(buf)) != 0) {
    fprintf(stderr, "Failed to set buffering\n");
    exit(1);
  }

  print_type(fp, type);

  if (strcmp(buf, expected) != 0) {
    fprintf(stderr, "ERROR: `%s' expected, but `%s'\n", expected, buf);
    exit(1);
  }

  fclose(fp);

  printf("OK\n");
}

void print_type_test(void) {
  check_print_type("char", &tyChar);
#ifndef __NO_FLONUM
  check_print_type("float", &tyFloat);
  check_print_type("double", &tyDouble);
#endif
  check_print_type("void*", ptrof(&tyVoid));
  check_print_type("int[5]", arrayof(&tyInt, 5));
  check_print_type("long[]", arrayof(get_fixnum_type(FX_LONG, false, 0), -1));

  check_print_type("int*[3]", arrayof(ptrof(&tyInt), 3));
  check_print_type("int(*)[3]", ptrof(arrayof(&tyInt, 3)));
  check_print_type("int(***)[3]", ptrof(ptrof(ptrof(arrayof(&tyInt, 3)))));

  check_print_type("int[2][3]", arrayof(arrayof(&tyInt, 3), 2));
  check_print_type("int*[][3]", arrayof(arrayof(ptrof(&tyInt), 3), -1));

  {
    Vector *param_types = new_vector();
    vec_push(param_types, &tyInt);
    Type* func = new_func_type(&tyVoid, NULL, param_types, false);
    check_print_type("void(int)", func);  // "void()(int)"?
  }
  {
    Vector *param_types = new_vector();
    vec_push(param_types, &tyInt);
    Type* func = new_func_type(&tyVoid, NULL, param_types, false);
    Type* funcptr = ptrof(func);
    check_print_type("void(*)(int)", funcptr);
  }

  {
    Vector *param_types = new_vector();
    const Type *funcptr = ptrof(new_func_type(&tyInt, NULL, param_types, false));
    const Type *aofp = arrayof(funcptr, 4);
    check_print_type("int(*[4])(void)", aofp);
  }

  {
    // signal
    Vector *param_types2 = new_vector();
    vec_push(param_types2, &tyInt);
    const Type *funcptr_type = ptrof(new_func_type(&tyVoid, NULL, param_types2, false));

    Vector *param_types = new_vector();
    vec_push(param_types, &tyInt);
    vec_push(param_types, funcptr_type);
    const Type *functype = new_func_type(funcptr_type, NULL, param_types, false);

    check_print_type("void(*(int, void(*)(int)))(int)", functype);
  }
}

void runtest(void) {
  print_type_test();
}

int main(void) {
  runtest();
  return 0;
}
