#include "stdarg.h"
#include "stdint.h"
#include "stdio.h"
#include "stdlib.h"
#include "string.h"

#include "xcc.h"
#include "x86_64.h"

#define PROG_START   (0x80)

#if defined(__XV6)
// XV6

#define START_ADDRESS    0x1000

#define SYSTEMCALL(no)  do { MOV_IM32_EAX(no); INT(T_SYSCALL); } while(0)

#define SYSCALL_EXIT   (SYS_exit)
#define SYSCALL_WRITE  (SYS_write)

#elif defined(__linux__)
// Linux

#define START_ADDRESS    (0x1000000 + PROG_START)

#define SYSTEMCALL(no)  do { MOV_IM32_EAX(no); SYSCALL(); } while(0)

#define SYSCALL_EXIT   (60 /*__NR_exit*/)
#define SYSCALL_WRITE  (1 /*__NR_write*/)

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
  loc_vector = new_vector();
  struct_map = new_map();
  global = new_map();

  init_gen(adr);
}

void compile(FILE *fp) {
  init_lexer(fp);
  Vector *node_vector = parse_program();

  for (int i = 0, len = node_vector->len; i < len; ++i)
    gen(node_vector->data[i]);
}

int main(int argc, char* argv[]) {
  if (argc > 1 && strcmp(argv[1], "-test") == 0) {
    runtest();
    return 0;
  }

  init_compiler(LOAD_ADDRESS);

  // Test.
  {
    Vector *params = new_vector();
    static Type tyInt = {.type=TY_INT, .ptrof=NULL};
    static Type tyFunc = {.type=TY_FUNC, .func={.ret=&tyInt}};
    tyFunc.func.params = params;
    define_global(&tyFunc, "_exit");
    define_global(&tyFunc, "_write");
  }

  if (argc > 1) {
    for (int i = 1; i < argc; ++i) {
      FILE *fp = fopen(argv[i], "rb");
      if (fp == NULL)
        error("Cannot open file: %s\n", argv[i]);
      compile(fp);
      fclose(fp);
    }
  } else {
    compile(stdin);
  }

  // Test.
  {
    add_label("_exit");
    SYSTEMCALL(SYSCALL_EXIT);
    RET();

    add_label("_write");
    SYSTEMCALL(SYSCALL_WRITE);
    RET();
  }

  size_t binsize = fixup_locations();

  uintptr_t entry = label_adr("main");
  if (entry == (uintptr_t)-1)
    error("Cannot find label: `%s'", "main");

  FILE* fp = stdout;

  out_elf_header(fp, entry);
  out_program_header(fp, PROG_START, LOAD_ADDRESS, binsize, binsize);
  put_padding(fp, PROG_START);
  output_code(fp);

  return 0;
}
