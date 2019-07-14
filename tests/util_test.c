#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#include "util.h"

#define EXPECT(expected, actual)  expect(__LINE__, expected, actual)

void expect(int line, int expected, int actual) {
  if (expected == actual)
    return;
  fprintf(stderr, "%d: %d expected, but got %d\n",
          line, expected, actual);
  exit(1);
}

void test_vector(void) {
  Vector *vec = new_vector();
  EXPECT(0, vec->len);

  for (int i = 0; i < 100; i++)
    vec_push(vec, (void *)(intptr_t)i);

  EXPECT(100, vec->len);
  EXPECT(0, (intptr_t)vec->data[0]);
  EXPECT(50, (intptr_t)vec->data[50]);
  EXPECT(99, (intptr_t)vec->data[99]);
}

void test_map(void) {
  Map *map = new_map();
  EXPECT(0, map_count(map));
  EXPECT(0, (intptr_t)map_get(map, "foo"));

  map_put(map, "foo", (void *)2);
  EXPECT(2, (intptr_t)map_get(map, "foo"));

  map_put(map, "bar", (void *)4);
  EXPECT(4, (intptr_t)map_get(map, "bar"));

  map_put(map, "foo", (void *)6);
  EXPECT(6, (intptr_t)map_get(map, "foo"));

  EXPECT(2, map_count(map));
}

void runtest(void) {
  test_vector();
  test_map();

  printf("OK\n");
}

int main(void) {
  runtest();
  return 0;
}
