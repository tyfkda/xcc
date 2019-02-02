#include <assert.h>
#include <stdlib.h>
#include <string.h>

#include "9cc.h"

#if defined(__XV6)
// XV6
#include "../kernel/types.h"
#include "../kernel/syscall.h"
#include "../kernel/traps.h"

#define SYSTEMCALL(no)  \
  do { MOV_I32_EAX(no); INT(T_SYSCALL); } while(0)

#define SYSCALL_EXIT   (SYS_exit)

#define START_ADDRESS    0x1000

#elif defined(__linux__)
// Linux

#define SYSTEMCALL(no)  \
  do { MOV_I32_EAX(no); SYSCALL(); } while(0)

#define SYSCALL_EXIT   (60 /*__NR_exit*/)

#define START_ADDRESS    (0x1000000 + PROG_START)

#else

#error Target not supported

#endif

const int WORD_SIZE = 8;

Map *label_map;

enum LocType {
  LOC_REL32,
};

typedef struct {
  enum LocType type;
  uintptr_t ip;
  const char *label;
  union {
    struct {
      uintptr_t base;
    } rel;
  };
} LocInfo;

uintptr_t start_address;
unsigned char* code;
size_t codesize;

#define CURIP(ofs)  (start_address + codesize + ofs)

void add_code(const unsigned char* buf, size_t size) {
  size_t newsize = codesize + size;
  code = realloc(code, newsize);
  if (code == NULL)
    error("not enough memory");
  memcpy(code + codesize, buf, size);
  codesize = newsize;
}

// Put label at the current.
void add_label(const char *label) {
  map_put(label_map, (char*)label, (void*)CURIP(0));
}

Vector *loc_vector;

LocInfo *new_loc(enum LocType type, uintptr_t ip, const char *label) {
  LocInfo *loc = malloc(sizeof(*loc));
  loc->type = type;
  loc->ip = ip;
  loc->label = label;
  vec_push(loc_vector, loc);
  return loc;
}

void add_loc_rel32(uintptr_t ip, const char *label, uintptr_t base) {
  LocInfo *loc = new_loc(LOC_REL32, ip, label);
  loc->rel.base = base;
}

size_t fixup_locations(void) {
  for (int i = 0; i < loc_vector->len; ++i) {
    LocInfo *loc = loc_vector->data[i];
    void *val = map_get(label_map, (char*)loc->label);
    if (val == NULL) {
      error("Cannot find label: `%s'", loc->label);
    }

    intptr_t v = (intptr_t)val;
    switch (loc->type) {
    case LOC_REL32:
      {
        intptr_t d = v - loc->rel.base;
        // TODO: Check out of range
        code[loc->ip    ] = d;
        code[loc->ip + 1] = d >> 8;
        code[loc->ip + 2] = d >> 16;
        code[loc->ip + 3] = d >> 24;
      }
      break;
    default:
      assert(FALSE);
      break;
    }
  }

  return codesize;
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
#define MOV_RSP_RBP()    ADD_CODE(0x48, 0x89, 0xe5)  // mov %rsp,%rbp
#define MOV_RBP_RSP()    ADD_CODE(0x48, 0x89, 0xec)  // mov %rbp,%rsp
#define MOV_RBP_RAX()    ADD_CODE(0x48, 0x89, 0xe8)  // mov %rbp,%rax
#define MOV_IND_RAX_RAX()  ADD_CODE(0x48, 0x8b, 0x00)  // mov (%rax),%rax
#define MOV_RAX_IND_RAX()  ADD_CODE(0x48, 0x89, 0x00)  // mov %rax,(%rax)
#define MOV_RDI_IND_RAX()  ADD_CODE(0x48, 0x89, 0x38)  // mov %rdi,(%rax)
#define MOVZB_AL_RAX()   ADD_CODE(0x48, 0x0f, 0xb6, 0xc0)  // movzbq %al,%rax
#define ADD_RDI_RAX()    ADD_CODE(0x48, 0x01, 0xf8)  // add %rdi,%rax
#define ADD_IM32_RAX(x)  ADD_CODE(0x48, 0x05, IM32(x))  // add $12345678,%rax
#define SUB_RDI_RAX()    ADD_CODE(0x48, 0x29, 0xf8)  // sub %rdi,%rax
#define SUB_IM32_RAX(x)  ADD_CODE(0x48, 0x2d, IM32(x))  // sub $12345678,%rax
#define SUB_IM32_RSP(x)  ADD_CODE(0x48, 0x81, 0xec, IM32(x))  // sub $IM32,%rsp
#define MUL_RDI()        ADD_CODE(0x48, 0xf7, 0xe7)  // mul %rdi
#define DIV_RDI()        ADD_CODE(0x48, 0xf7, 0xf7)  // div %rdi
#define CMP_RAX_RDI()    ADD_CODE(0x48, 0x39, 0xc7)  // cmp %rax,%rdi
#define SETE_AL()        ADD_CODE(0x0f, 0x94, 0xc0)  // sete %al
#define SETNE_AL()       ADD_CODE(0x0f, 0x95, 0xc0)  // setne %al
#define PUSH_RAX()       ADD_CODE(0x50)  // push %rax
#define PUSH_RBP()       ADD_CODE(0x55)  // push %rbp
#define PUSH_RDI()       ADD_CODE(0x57)  // push %rdi
#define POP_RAX()        ADD_CODE(0x58)  // pop %rax
#define POP_RBP()        ADD_CODE(0x5d)  // pop %rbp
#define POP_RDI()        ADD_CODE(0x5f)  // pop %rdi
#define CALL(label)      do { add_loc_rel32(codesize + 1, label, CURIP(5)); ADD_CODE(0xe8, IM32(0)); } while(0)  // call
#define RET()            ADD_CODE(0xc3)  // retq
#define INT(x)           ADD_CODE(0xcd, x)  // int $x
#define SYSCALL()        ADD_CODE(0x0f, 0x05)  // syscall

void gen_lval(Node *node) {
  if (node->type != ND_IDENT)
    error("No lvalue: %d", node->type);

  int offset = (node->varidx + 1) * WORD_SIZE;
  MOV_RBP_RAX();
  SUB_IM32_RAX(offset);
  PUSH_RAX();
}

void gen(Node *node) {
  switch (node->type) {
  case ND_NUM:
    MOV_I64_RAX(node->val);
    PUSH_RAX();
    return;

  case ND_IDENT:
    gen_lval(node);
    POP_RAX();
    MOV_IND_RAX_RAX();
    PUSH_RAX();
    return;

  case ND_ASSIGN:
    gen_lval(node->bop.lhs);
    gen(node->bop.rhs);

    POP_RDI();
    POP_RAX();
    MOV_RDI_IND_RAX();
    PUSH_RDI();
    return;

  case ND_FUNCALL:
    CALL(node->funcall.name);
    PUSH_RAX();
    return;

  case ND_EQ:
  case ND_NE:
    gen(node->bop.lhs);
    gen(node->bop.rhs);

    POP_RAX();
    POP_RDI();
    CMP_RAX_RDI();
    if (node->type == ND_EQ)
      SETE_AL();
    else
      SETNE_AL();
    MOVZB_AL_RAX();
    PUSH_RAX();
    return;

  case ND_ADD:
  case ND_SUB:
  case ND_MUL:
  case ND_DIV:
    gen(node->bop.lhs);
    gen(node->bop.rhs);

    POP_RDI();
    POP_RAX();

    switch (node->type) {
    case ND_ADD:
      ADD_RDI_RAX();
      break;
    case ND_SUB:
      SUB_RDI_RAX();
      break;
    case ND_MUL:
      MUL_RDI();
      break;
    case ND_DIV:
      MOV_I32_RDX(0);
      DIV_RDI();
      break;
    default:
      assert(FALSE);
      break;
    }

    PUSH_RAX();
    return;

  default:
    error("Unhandled node: %d", node->type);
    break;
  }
}

void compile(const char* source) {
  tokenize(source);
  program();

  // Prologue
  // Allocate variable bufer.
  PUSH_RBP();
  MOV_RSP_RBP();
  SUB_IM32_RSP(var_vector->len * WORD_SIZE);

  int len = node_vector->len;
  for (int i = 0; i < len; ++i) {
    gen(node_vector->data[i]);

    POP_RAX();
  }

  // Epilogue
  // Get last value.
  MOV_RBP_RSP();
  POP_RBP();

  // Ending.
  MOV_RAX_RDI();

  SYSTEMCALL(SYSCALL_EXIT);
}

void output_code(FILE* fp) {
  fwrite(code, codesize, 1, fp);
}

void add_foo() {
  add_label("foo");
  const long val = 123;
  MOV_I64_RAX(val);
  RET();
}
