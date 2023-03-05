#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "table.h"
#include "type.h"
#include "util.h"

#include "./xtest.h"

void check_print_type(const char *expected, const Type *type) {
  begin_test(expected);

  char *buf;
  size_t size;
  FILE *fp = open_memstream(&buf, &size);
  if (fp == NULL) {
    fail("Failed to create tmpfile");
  } else {
    print_type(fp, type);
    fclose(fp);

    if (strcmp(buf, expected) != 0) {
      fail("`%s' expected, but `%s'", expected, buf);
    }
    free(buf);
  }
}

TEST(print_type) {
  check_print_type("char", &tyChar);
  check_print_type("unsigned int", &tyUnsignedInt);
  check_print_type("long long", get_fixnum_type(FX_LLONG, false, 0));
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
    Type *type = create_enum_type(alloc_name("Enum", NULL, false));
    check_print_type("enum Enum", type);
  }

  check_print_type("const int", get_fixnum_type(FX_INT, false, TQ_CONST));
  check_print_type("const int*", ptrof(get_fixnum_type(FX_INT, false, TQ_CONST)));
  { Type *t = ptrof(&tyInt); t->qualifier = TQ_CONST; check_print_type("int* const", t); }
  {
    Type *t = ptrof(get_fixnum_type(FX_INT, false, TQ_CONST)); t->qualifier = TQ_CONST;
    Type *u = ptrof(t); u->qualifier = TQ_CONST;
    check_print_type("const int* const* const", u);
  }

  {
    StructInfo *sinfo = create_struct_info(NULL, 0, false);
    const Name *name = alloc_name("Foo", NULL, false);
    Type *type = create_struct_type(sinfo, name, 0);
    check_print_type("struct Foo", type);
    check_print_type("struct Foo*", ptrof(type));
    check_print_type("struct Foo[3]", arrayof(type, 3));
  }

  {
    Vector *param_types = new_vector();
    vec_push(param_types, &tyInt);
    Type *func = new_func_type(&tyVoid, NULL, param_types, false);
    check_print_type("void(int)", func);  // "void()(int)"?
  }
  {
    Vector *param_types = new_vector();
    vec_push(param_types, &tyInt);
    Type *func = new_func_type(&tyVoid, NULL, param_types, false);
    Type *funcptr = ptrof(func);
    check_print_type("void(*)(int)", funcptr);
  }

  {
    Vector *param_types = new_vector();
    Type *funcptr = ptrof(new_func_type(&tyInt, NULL, param_types, false));
    const Type *aofp = arrayof(funcptr, 4);
    check_print_type("int(*[4])(void)", aofp);
  }

  {
    // signal
    Vector *param_types2 = new_vector();
    vec_push(param_types2, &tyInt);
    Type *funcptr_type = ptrof(new_func_type(&tyVoid, NULL, param_types2, false));

    Vector *param_types = new_vector();
    vec_push(param_types, &tyInt);
    vec_push(param_types, funcptr_type);
    const Type *functype = new_func_type(funcptr_type, NULL, param_types, false);

    check_print_type("void(*(int, void(*)(int)))(int)", functype);
  }
} END_TEST()

int main(void) {
  return RUN_ALL_TESTS(
    test_print_type,
  );
}
