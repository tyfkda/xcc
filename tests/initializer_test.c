#include <assert.h>
#include <inttypes.h>  // PRId64
#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "lexer.h"
#include "parser.h"
#include "table.h"
#include "type.h"
#include "util.h"

#include "./xtest.h"

extern void dump_expr(FILE *fp, Expr *expr);
extern void dump_init(FILE *fp, const Initializer *init);

static bool same_expr(const Expr *expr1, const Expr *expr2) {
  if (expr1->kind != expr2->kind)
    return false;

  switch (expr1->kind) {
  case EX_FIXNUM:
    return expr1->fixnum == expr2->fixnum;
  case EX_STR:
    return expr1->str.size == expr2->str.size && memcmp(expr1->str.buf, expr2->str.buf, expr1->str.size) == 0;
  default: assert(false); break;
  }
}

static bool same_init(const Initializer *init1, const Initializer *init2) {
  if (init1 == NULL)
    return init2 == NULL;
  if (init2 == NULL)
    return false;

  if (init1->kind != init2->kind)
    return false;

  switch (init1->kind) {
  case IK_SINGLE:
    return same_expr(init1->single, init2->single);
  case IK_MULTI:
    {
      Vector *m1 = init1->multi, *m2 = init2->multi;
      if (m1->len != m2->len)
        return false;
      for (int i = 0; i < m1->len; ++i) {
        if (!same_init(m1->data[i], m2->data[i]))
          return false;
      }
    }
    return true;
  case IK_ARR:
    return init1->arr.index == init2->arr.index && same_init(init1->arr.value, init2->arr.value);
  default:
    return false;
  }
}

static Initializer *parse_init(const char *source) {
  const char fn[] = "*test*";
  set_source_file(NULL, fn);
  set_source_string(source, fn, 1);
  return parse_initializer();
}

void expect(Initializer *expected, const char *input_str, Type *type) {
  begin_test(input_str);

  Initializer *init = parse_init(input_str);
  Initializer *actual = flatten_initializer(type, init);

  // Compare initializer
  if (!same_init(expected, actual))
    fail("different");
}

void expect2(const char *expected_str, const char *input_str, Type *type) {
  Initializer *expected = parse_init(expected_str);
  expect(expected, input_str, type);
}

Initializer *new_init_single(Expr *expr) {
  Initializer *init = new_initializer(IK_SINGLE, NULL);
  init->single = expr;
  return init;
}

Initializer *new_init_multi(int count, ...) {
  va_list ap;
  va_start(ap, count);
  Vector *elems = new_vector();
  for (int i = 0; i < count; ++i) {
    Initializer *elem = va_arg(ap, Initializer*);
    vec_push(elems, elem);
  }
  va_end(ap);

  Initializer *init = new_initializer(IK_MULTI, NULL);
  init->multi = elems;
  return init;
}

Initializer *new_init_arr(size_t index, Initializer *value) {
  Initializer *init = new_initializer(IK_ARR, NULL);
  init->arr.index = index;
  init->arr.value = value;
  return init;
}

TEST(flatten) {
  expect2("1234", "1234", &tyInt);
  expect2("\"str\"", "\"str\"", ptrof(&tyChar));
  expect2("\"array\"", "\"array\"", arrayof(&tyChar, 4));
  expect2("{1, 2, 3}", "{1, 2, 3}", arrayof(&tyInt, -1));
  expect2("{\"str\"}", "{\"str\"}", arrayof(ptrof(&tyChar), -1));

  {  // Struct initializer shortage.
    MemberInfo *members = malloc(sizeof(*members) * 3);
    members[0] = (MemberInfo){ .name = alloc_name("x", NULL, false), .type = &tyChar };
    members[1] = (MemberInfo){ .name = alloc_name("y", NULL, false), .type = get_fixnum_type(FX_SHORT, false, 0) };
    members[2] = (MemberInfo){ .name = alloc_name("z", NULL, false), .type = get_fixnum_type(FX_LONG, true, 0) };
    StructInfo *sinfo = create_struct_info(members, 3, false);
    Type *type = create_struct_type(sinfo, NULL, 0);

    Initializer *expected = new_init_multi(3,
        new_init_single(new_expr_fixlit(&tyInt, NULL, 11)),
        new_init_single(new_expr_fixlit(&tyInt, NULL, 22)),
        NULL);
    expect(expected, "{11, 22}", type);
  }

  // Braced string for char pointer.
  expect2("\"hello\"", "{\"hello\"}", ptrof(&tyChar));
  expect2("\"array\"", "{\"array\"}", arrayof(&tyChar, 4));


  {  // String for char array in struct.
    MemberInfo *members = malloc(sizeof(*members) * 1);
    members[0] = (MemberInfo){ .name = alloc_name("str", NULL, false), .type = arrayof(&tyChar, 4) };
    StructInfo *sinfo = create_struct_info(members, 1, false);
    Type *type = create_struct_type(sinfo, NULL, 0);
    expect2("{\"abcd\"}", "{\"abcd\"}", type);
  }

  {  // Array index initializer.
    Initializer *expected = new_init_multi(2,
        new_init_arr(1, new_init_single(new_expr_fixlit(&tyInt, NULL, 11))),
        new_init_arr(3, new_init_single(new_expr_fixlit(&tyInt, NULL, 33))),
        NULL);
    expect(expected, "{[3] = 33, [1] = 11}", arrayof(&tyInt, -1));
  }

  {  // Dotted initializer.
    MemberInfo *members = malloc(sizeof(*members) * 3);
    members[0] = (MemberInfo){ .name = alloc_name("x", NULL, false), .type = &tyInt };
    members[1] = (MemberInfo){ .name = alloc_name("y", NULL, false), .type = &tyInt };
    members[2] = (MemberInfo){ .name = alloc_name("z", NULL, false), .type = &tyInt };
    StructInfo *sinfo = create_struct_info(members, 3, false);
    Type *type = create_struct_type(sinfo, NULL, 0);
    expect2("{7, 8, 9}", "{.z = 9, .y = 8, .x = 7}", type);
  }

  {  // 2D array.
    Initializer *expected = new_init_multi(2,
        new_init_multi(3,
            new_init_single(new_expr_fixlit(&tyInt, NULL, 2)),
            new_init_single(new_expr_fixlit(&tyInt, NULL, 4)),
            new_init_single(new_expr_fixlit(&tyInt, NULL, 6))),
        new_init_multi(2,
            new_init_single(new_expr_fixlit(&tyInt, NULL, 9)),
            new_init_single(new_expr_fixlit(&tyInt, NULL, 11))));
    expect(expected, "{{2, 4, 6}, {9, 11}}", arrayof(arrayof(&tyInt, 3), 2));
  }

  // 2D array without brace.
  expect2("{{2, 4, 6}, {9, 11}}", "{2, 4, 6, 9, 11}", arrayof(arrayof(&tyInt, 3), 2));
  expect2("{{3, 1}, {4, 1}, {5, 9}}", "{{3, 1}, 4, 1, {5, 9}}", arrayof(arrayof(&tyInt, 2), -1));

  // Array of struct without brace.
  {
    MemberInfo *members = malloc(sizeof(*members) * 2);
    members[0] = (MemberInfo){ .name = alloc_name("x", NULL, false), .type = &tyChar };
    members[1] = (MemberInfo){ .name = alloc_name("y", NULL, false), .type = get_fixnum_type(FX_SHORT, false, 0) };
    StructInfo *sinfo = create_struct_info(members, 2, false);
    Type *type = create_struct_type(sinfo, NULL, 0);

    expect2("{{11, 12}, {21, 22}}", "{11, 12, 21, 22}", arrayof(type, -1));
    expect2("{{11, 12}, {21, 22}}", "{{11, 12}, 21, 22}", arrayof(type, 2));
  }
} END_TEST()

int main(void) {
  return RUN_ALL_TESTS(
    test_flatten,
  );
}
