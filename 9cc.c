#include "ctype.h"
#include "stdarg.h"
#include "stdint.h"
#include "stdio.h"
#include "stdlib.h"
#include "string.h"

#ifndef FALSE
#define FALSE  (0)
#endif
#ifndef TRUE
#define TRUE   (1)
#endif

#define PROG_START   (0x80)

#if defined(__XV6)
// XV6
#include "../kernel/types.h"
#include "../kernel/elf.h"
#include "../kernel/syscall.h"
#include "../kernel/traps.h"

#define SYSCALL(no)  \
  ADD_CODE(0xb8, (no), (no) >> 8, (no) >> 16, (no) >> 24,  /* mov $EXIT, %eax */ \
           0xcd, T_SYSCALL)                                /* int $64 */

#define SYSCALL_EXIT   (SYS_exit)

#define START_ADDRESS    0x1000

#elif defined(__linux__)
// Linux
#include <elf.h>

#define SYSCALL(no)  \
  ADD_CODE(0xb8, (no), (no) >> 8, (no) >> 16, (no) >> 24,  /* mov $EXIT, %eax */ \
           0x0f, 0x05)                                     /* syscall */

#define SYSCALL_EXIT   (60 /*__NR_exit*/)

#define START_ADDRESS    (0x1000000 + PROG_START)

#else

#error Target not supported

#endif

#define LOAD_ADDRESS    START_ADDRESS

////////////////////////////////////////////////

typedef struct {
  void **data;
  int capacity;
  int len;
} Vector;

Vector *new_vector() {
  Vector *vec = malloc(sizeof(Vector));
  vec->data = malloc(sizeof(void *) * 16);
  vec->capacity = 16;
  vec->len = 0;
  return vec;
}

void vec_push(Vector *vec, void *elem) {
  if (vec->capacity == vec->len) {
    vec->capacity *= 2;
    vec->data = realloc(vec->data, sizeof(void *) * vec->capacity);
  }
  vec->data[vec->len++] = elem;
}

int expect(int line, int expected, int actual) {
  if (expected == actual)
    return TRUE;
  fprintf(stderr, "%d: %d expected, but got %d\n",
          line, expected, actual);
  exit(1);
}

void runtest() {
  Vector *vec = new_vector();
  expect(__LINE__, 0, vec->len);

  for (int i = 0; i < 100; i++)
    vec_push(vec, (void *)(intptr_t)i);

  expect(__LINE__, 100, vec->len);
  expect(__LINE__, 0, (intptr_t)vec->data[0]);
  expect(__LINE__, 50, (intptr_t)vec->data[50]);
  expect(__LINE__, 99, (intptr_t)vec->data[99]);

  printf("OK\n");
}

////////////////////////////////////////////////

void error(const char* fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  vfprintf(stderr, fmt, ap);
  va_end(ap);
  exit(1);
}
// Token type value
enum TokenType {
  TK_NUM = 256,  // Integer token
  TK_EOF,        // Represent input end
};

// Token type
typedef struct {
  int ty;
  long val;
  const char *input;
} Token;

Token tokens[100];

void tokenize(const char *p) {
  int i = 0;
  while (*p != '\0') {
    if (isspace(*p)) {
      ++p;
      continue;
    }

    if (*p == '+' || *p == '-' || *p == '*' || *p == '/' || *p == '(' || *p == ')') {
      tokens[i].ty = *p;
      tokens[i].input = p;
      ++i;
      ++p;
      continue;
    }

    if (isdigit(*p)) {
      tokens[i].ty = TK_NUM;
      tokens[i].input = p;
      tokens[i].val = strtol(p, (char**)&p, 10);
      ++i;
      continue;
    }

    fprintf(stderr, "Cannot tokenize: %s\n", p);
    exit(1);
  }

  tokens[i].ty = TK_EOF;
  tokens[i].input = p;
}

enum {
  ND_NUM = 256,     // Number nodes
};

typedef struct Node {
  int ty;
  union {
    struct {
      struct Node *lhs;
      struct Node *rhs;
    } bop;
    long val;
  };
} Node;

int pos;

Node *new_node(int ty, Node *lhs, Node *rhs) {
  Node *node = malloc(sizeof(Node));
  node->ty = ty;
  node->bop.lhs = lhs;
  node->bop.rhs = rhs;
  return node;
}

Node *new_node_num(int val) {
  Node *node = malloc(sizeof(Node));
  node->ty = ND_NUM;
  node->val = val;
  return node;
}

int consume(int ty) {
  if (tokens[pos].ty != ty)
    return FALSE;
  ++pos;
  return TRUE;
}

Node *add();

Node *term() {
  if (consume('(')) {
    Node *node = add();
    if (!consume(')'))
      error("No close paren: %s", tokens[pos].input);
    return node;
  }

  if (tokens[pos].ty == TK_NUM)
    return new_node_num(tokens[pos++].val);

  error("Number or open paren expected: %s", tokens[pos].input);
  return NULL;
}

Node *mul() {
  Node *node = term();

  for (;;) {
    if (consume('*'))
      node = new_node('*', node, term());
    else if (consume('/'))
      node = new_node('/', node, term());
    else
      return node;
  }
}

Node *add() {
  Node *node = mul();

  for (;;) {
    if (consume('+'))
      node = new_node('+', node, mul());
    else if (consume('-'))
      node = new_node('-', node, mul());
    else
      return node;
  }
}

unsigned char* code;
size_t codesize;

void add_code(const unsigned char* buf, size_t size) {
  size_t newsize = codesize + size;
  code = realloc(code, newsize);
  if (code == NULL)
    error("not enough memory");
  memcpy(code + codesize, buf, size);
  codesize = newsize;
}

#define ADD_CODE(...)  do { unsigned char buf[] = {__VA_ARGS__}; add_code(buf, sizeof(buf)); } while (0)
#define IM32(x)  (x), ((x) >> 8), ((x) >> 16), ((x) >> 24)
#define IM64(x)  (x), ((x) >> 8), ((x) >> 16), ((x) >> 24), ((x) >> 32), ((x) >> 40), ((x) >> 48), ((x) >> 56)

#define MOV_I32_EAX(x)   ADD_CODE(0xb8, IM32(x))  // mov $0xNN,%eax
#define MOV_I64_RAX(x)   ADD_CODE(0x48, 0xb8, IM64(x))  // mov $0x123456789abcdef0,%rax
#define MOV_I64_RDI(x)   ADD_CODE(0x48, 0xbf, IM64(x))  // mov $0x123456789abcdef0,%rdi
#define MOV_I32_RDX(x)   ADD_CODE(0x48, 0xc7, 0xc2, IM32(x)) // mov $0x0,%rdx
#define MOVSX_EAX_RDI()  ADD_CODE(0x48, 0x63, 0xf8)  // movsx %eax, %rdi
#define MOV_RAX_RDI()    ADD_CODE(0x48, 0x89, 0xc7)  // mov %rax,%rdi
#define ADD_RDI_RAX()    ADD_CODE(0x48, 0x01, 0xf8)  // add %rdi,%rax
#define ADD_IM32_RAX(x)  ADD_CODE(0x48, 0x05, IM32(x))  // add $12345678,%rax
#define SUB_RDI_RAX()    ADD_CODE(0x48, 0x29, 0xf8)  // sub %rdi,%rax
#define SUB_IM32_RAX(x)  ADD_CODE(0x48, 0x2d, IM32(x))  // sub $12345678,%rax
#define MUL_RDI()        ADD_CODE(0x48, 0xf7, 0xe7)  // mul %rdi
#define DIV_RDI()        ADD_CODE(0x48, 0xf7, 0xf7)  // div %rdi
#define PUSH_RAX()       ADD_CODE(0x50)  // push %rax
#define POP_RAX()        ADD_CODE(0x58)  // pop %rax
#define POP_RDI()        ADD_CODE(0x5f)  // pop %rdi

void gen(Node *node) {
  if (node->ty == ND_NUM) {
    MOV_I64_RAX(node->val);
    PUSH_RAX();
    return;
  }

  gen(node->bop.lhs);
  gen(node->bop.rhs);

  POP_RDI();
  POP_RAX();

  switch (node->ty) {
  case '+':
    ADD_RDI_RAX();
    break;
  case '-':
    SUB_RDI_RAX();
    break;
  case '*':
    MUL_RDI();
    break;
  case '/':
    MOV_I32_RDX(0);
    DIV_RDI();
    break;
  }

  PUSH_RAX();
}

void compile(const char* source) {
  tokenize(source);
  Node *node = add();

  gen(node);

  POP_RAX();

  // Ending.
  MOV_RAX_RDI();

  SYSCALL(SYSCALL_EXIT);
}

void output_code(FILE* fp) {
  fwrite(code, codesize, 1, fp);
}

////////////////////////////////////////////////

void out_elf_header(FILE* fp, uintptr_t entry) {
  Elf64_Ehdr ehdr = {
    .e_ident     = { ELFMAG0, ELFMAG1, ELFMAG2 ,ELFMAG3,
                     ELFCLASS64, ELFDATA2LSB, EV_CURRENT, ELFOSABI_SYSV },
    .e_type      = ET_EXEC,
    .e_machine   = EM_X86_64,
    .e_version   = EV_CURRENT,
    .e_entry     = entry,
    .e_phoff     = sizeof(Elf64_Ehdr),
    .e_shoff     = 0, // dummy
    .e_flags     = 0x0,
    .e_ehsize    = sizeof(Elf64_Ehdr),
    .e_phentsize = sizeof(Elf64_Phdr),
    .e_phnum     = 1,
    .e_shentsize = 0, // dummy
    .e_shnum     = 0,
    .e_shstrndx  = 0, // dummy
  };

  fwrite(&ehdr, sizeof(Elf64_Ehdr), 1, fp);
}

void out_program_header(FILE* fp, uintptr_t offset, uintptr_t vaddr, uintptr_t filesz, uintptr_t memsz) {
  Elf64_Phdr phdr = {
    .p_type   = PT_LOAD,
    .p_offset = offset,
    .p_vaddr  = vaddr,
    .p_paddr  = 0, // dummy
    .p_filesz = filesz,
    .p_memsz  = memsz,
    .p_flags  = PF_R | PF_X,
    .p_align  = 0x10,
  };

  fwrite(&phdr, sizeof(Elf64_Phdr), 1, fp);
}


int main(int argc, char* argv[]) {
  if (argc < 2) {
    fprintf(stderr, "argc < 2\n");
    return 1;
  }

  if (strcmp(argv[1], "-test") == 0) {
    runtest();
    return 0;
  }

  compile(argv[1]);

  FILE* fp = stdout;

  out_elf_header(fp, LOAD_ADDRESS);
  out_program_header(fp, PROG_START, LOAD_ADDRESS, codesize, codesize);

  {
    char buf[PROG_START];
    memset(buf, 0, PROG_START);
    fwrite(buf, PROG_START - (sizeof(Elf64_Ehdr) + sizeof(Elf64_Phdr)), 1, fp);
  }

  output_code(fp);

  return 0;
}
