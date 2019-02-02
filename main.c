#include "assert.h"
#include "stdarg.h"
#include "stdint.h"
#include "stdio.h"
#include "stdlib.h"
#include "string.h"

#include "9cc.h"

#define PROG_START   (0x80)

#if defined(__XV6)
// XV6

#define START_ADDRESS    0x1000

#elif defined(__linux__)
// Linux

#define START_ADDRESS    (0x1000000 + PROG_START)

#else

#error Target not supported

#endif

#define LOAD_ADDRESS    START_ADDRESS

////////////////////////////////////////////////

int expect(int line, int expected, int actual) {
  if (expected == actual)
    return TRUE;
  fprintf(stderr, "%d: %d expected, but got %d\n",
          line, expected, actual);
  exit(1);
}

void test_vector() {
  Vector *vec = new_vector();
  expect(__LINE__, 0, vec->len);

  for (int i = 0; i < 100; i++)
    vec_push(vec, (void *)(intptr_t)i);

  expect(__LINE__, 100, vec->len);
  expect(__LINE__, 0, (intptr_t)vec->data[0]);
  expect(__LINE__, 50, (intptr_t)vec->data[50]);
  expect(__LINE__, 99, (intptr_t)vec->data[99]);
}

void test_map() {
  Map *map = new_map();
  expect(__LINE__, 0, (intptr_t)map_get(map, "foo"));

  map_put(map, "foo", (void *)2);
  expect(__LINE__, 2, (intptr_t)map_get(map, "foo"));

  map_put(map, "bar", (void *)4);
  expect(__LINE__, 4, (intptr_t)map_get(map, "bar"));

  map_put(map, "foo", (void *)6);
  expect(__LINE__, 6, (intptr_t)map_get(map, "foo"));
}

void runtest() {
  test_vector();
  test_map();

  printf("OK\n");
}

////////////////////////////////////////////////

void error(const char* fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  vfprintf(stderr, fmt, ap);
  va_end(ap);
  fprintf(stderr, "\n");
  exit(1);
}

void init_compiler(uintptr_t adr) {
  start_address = adr;
  token_vector = new_vector();
  node_vector = new_vector();
  label_map = new_map();
  loc_vector = new_vector();
}

extern void add_foo();

int main(int argc, char* argv[]) {
  if (argc < 2) {
    fprintf(stderr, "argc < 2\n");
    return 1;
  }

  if (strcmp(argv[1], "-test") == 0) {
    runtest();
    return 0;
  }

  init_compiler(LOAD_ADDRESS);
  compile(argv[1]);
  size_t binsize = fixup_locations();

  FILE* fp = stdout;

  out_elf_header(fp, LOAD_ADDRESS);
  out_program_header(fp, PROG_START, LOAD_ADDRESS, binsize, binsize);
  put_padding(fp, PROG_START);
  output_code(fp);

  return 0;
}
