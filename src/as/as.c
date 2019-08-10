#include <ctype.h>
#include <stdio.h>
#include <stdint.h>  // uintptr_t
#include <stdlib.h>  // calloc
#include <string.h>
#include <strings.h>  // strncasecmp

#include "elfutil.h"
#include "gen.h"
#include "util.h"

#define PROG_START   (0x100)

#if defined(__XV6)
// XV6
#include "../kernel/syscall.h"
#include "../kernel/traps.h"

#define START_ADDRESS    0x1000

#elif defined(__linux__)
// Linux

#include <sys/stat.h>

#define START_ADDRESS    (0x01000000 + PROG_START)

#else

#error Target not supported

#endif

#define LOAD_ADDRESS    START_ADDRESS

static void init(uintptr_t adr) {
  init_gen(adr);
}

enum Opcode {
  NOOP,
  MOV,
  MOVSX,
  LEA,
  CALL,
  JMP,
  JE,
  JNE,
  JGE,
  JL,
  RET,
  PUSH,
  POP,
  ADD,
  ADDQ,
  SUB,
  MUL,
  DIV,
  XOR,
  NEG,
  NOT,
  INC,
  INCL,
  DEC,
  DECL,
  SHL,
  SHR,
  AND,
  OR,
  CMP,
  TEST,
  SETE,
  SETNE,
  SYSCALL,
};

static const char *kOpTable[] = {
  "*noop*",
  "mov",
  "movsx",
  "lea",
  "call",
  "jmp",
  "je",
  "jne",
  "jge",
  "jl",
  "ret",
  "push",
  "pop",
  "add",
  "addq",
  "sub",
  "mul",
  "div",
  "xor",
  "neg",
  "not",
  "inc",
  "incl",
  "dec",
  "decl",
  "shl",
  "shr",
  "and",
  "or",
  "cmp",
  "test",
  "sete",
  "setne",
  "syscall",
};

enum OperandType {
  REG,
};

enum RegType {
  EAX,
  RAX,
};

typedef struct {
  enum OperandType type;
  union {
    enum RegType reg;
  } u;
} Operand;

typedef struct {
  const char *label;
  enum Opcode op;
  Operand src;
  Operand dst;
} Line;

static bool is_label_first_chr(char c) {
  return isalpha(c) || c == '_' || c == '.';
}

static bool is_label_chr(char c) {
  return is_label_first_chr(c) || isdigit(c);
}

static const char *skip_whitespace(const char *p) {
  while (isspace(*p))
    ++p;
  return p;
}

static enum Opcode parse_opcode(const char **pp) {
  const char *p = *pp;
  const char *start = p;

  while (isalpha(*p))
    ++p;

  size_t n = p - start;
  for (int i = 0, len = sizeof(kOpTable) / sizeof(*kOpTable); i < len; ++i) {
    const char *op = kOpTable[i];
    size_t len = strlen(op);
    if (n == len && strncasecmp(start, op, n) == 0) {
      *pp = p;
      return (enum Opcode)i;
    }
  }
  return NOOP;
}


static void parse_line(const char *str, Line *line) {
  // Clear
  line->label = NULL;
  line->op = NOOP;

  const char *p = str;
  if (is_label_first_chr(*p)) {
    do {
      ++p;
    } while (is_label_chr(*p));
    line->label = strndup_(str, p - str);

    if (*p != ':')
      error("`:' expected");
    ++p;
  }

  p = skip_whitespace(p);
  if (*p != '\0') {
    line->op = parse_opcode(&p);
  }

  //if ((c = *p) != '\0' && !is_space(c)) {
  //}
}

static void assemble(FILE *fp) {
  for (;;) {
    char *rawline = NULL;
    size_t capa = 0;
    ssize_t len = getline_(&rawline, &capa, fp, 0);
    if (len == EOF)
      break;

    Line line;
    parse_line(rawline, &line);
    if (line.label != NULL) {
      printf("label: [%s]\n", line.label);
    }
    printf("op=%d:  (%s)\n", line.op, rawline);
  }
}

static void put_padding(FILE* fp, uintptr_t start) {
  long cur = ftell(fp);
   if (start > (size_t)cur) {
    size_t size = start - (uintptr_t)cur;
    char* buf = calloc(1, size);
    fwrite(buf, size, 1, fp);
    free(buf);
  }
}

int main(int argc, char* argv[]) {
  const char *ofn = "a.out";
  int iarg;

  for (iarg = 1; iarg < argc; ++iarg) {
    if (*argv[iarg] != '-')
      break;
    if (strncmp(argv[iarg], "-o", 2) == 0)
      ofn = strdup_(argv[iarg] + 2);
  }

  init(LOAD_ADDRESS);

  FILE* fp = fopen(ofn, "wb");
  if (fp == NULL) {
    fprintf(stderr, "Failed to open output file: %s\n", ofn);
    return 1;
  }

  if (iarg < argc) {
    for (int i = iarg; i < argc; ++i) {
      FILE *fp = fopen(argv[i], "r");
      if (fp == NULL)
        error("Cannot open %s\n", argv[i]);
      assemble(fp);
      fclose(fp);
    }
  } else {
    assemble(stdin);
  }

  uintptr_t entry = label_adr("_start");
  if (entry == (uintptr_t)-1)
    error("Cannot find label: `%s'", "_start");

  size_t codefilesz, codememsz;
  size_t datafilesz, datamemsz;
  uintptr_t codeloadadr, dataloadadr;
  get_section_size(0, &codefilesz, &codememsz, &codeloadadr);
  get_section_size(1, &datafilesz, &datamemsz, &dataloadadr);

  int phnum = datamemsz > 0 ? 2 : 1;

  out_elf_header(fp, entry, phnum);
  out_program_header(fp, 0, PROG_START, codeloadadr, codefilesz, codememsz);
  if (phnum > 1)
    out_program_header(fp, 1, ALIGN(PROG_START + codefilesz, 0x1000), dataloadadr, datafilesz, datamemsz);

  put_padding(fp, PROG_START);
  output_section(fp, 0);
  if (phnum > 1) {
    put_padding(fp, ALIGN(PROG_START + codefilesz, 0x1000));
    output_section(fp, 1);
  }
  fclose(fp);

#if !defined(__XV6) && defined(__linux__)
  if (chmod(ofn, 0755) == -1) {
    perror("chmod failed\n");
    return 1;
  }
#endif
  return 0;
}
