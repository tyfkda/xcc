#include "inst.h"

#include <ctype.h>
#include <stddef.h>
#include <stdlib.h>  // strtol
#include <string.h>
#include <strings.h>

#include "util.h"

static const char *kOpTable[] = {
  "mov",
  "movsx",
  "lea",

  "add",
  "addq",
  "sub",
  "subq",
  "mul",
  "idiv",
  "neg",
  "not",
  "inc",
  "incl",
  "incq",
  "dec",
  "decl",
  "decq",
  "and",
  "or",
  "xor",
  "shl",
  "shr",
  "cmp",
  "test",
  "cltd",
  "cqto",

  "seto",
  "setno",
  "setb",
  "setae",
  "sete",
  "setne",
  "setbe",
  "seta",
  "sets",
  "setns",
  "setp",
  "setnp",
  "setl",
  "setge",
  "setle",
  "setg",

  "jmp",
  "jo",
  "jno",
  "jb",
  "jae",
  "je",
  "jne",
  "jbe",
  "ja",
  "js",
  "jns",
  "jp",
  "jnp",
  "jl",
  "jge",
  "jle",
  "jg",
  "call",
  "ret",
  "push",
  "pop",

  "int",
  "syscall",
};

static const struct {
  const char *name;
  enum RegType reg;
} kRegisters[] = {
  {"al", AL},
  {"cl", CL},
  {"dl", DL},
  {"bl", BL},
  {"spl", SPL},
  {"bpl", BPL},
  {"sil", SIL},
  {"dil", DIL},

  {"r8b", R8B},
  {"r9b", R9B},
  {"r10b", R10B},
  {"r11b", R11B},
  {"r12b", R12B},
  {"r13b", R13B},
  {"r14b", R14B},
  {"r15b", R15B},

  {"ax", AX},
  {"cx", CX},
  {"dx", DX},
  {"bx", BX},

  {"eax", EAX},
  {"ecx", ECX},
  {"edx", EDX},
  {"ebx", EBX},
  {"esp", ESP},
  {"ebp", EBP},
  {"esi", ESI},
  {"edi", EDI},

  {"r8d", R8D},
  {"r9d", R9D},
  {"r10d", R10D},
  {"r11d", R11D},
  {"r12d", R12D},
  {"r13d", R13D},
  {"r14d", R14D},
  {"r15d", R15D},

  {"rax", RAX},
  {"rcx", RCX},
  {"rdx", RDX},
  {"rbx", RBX},
  {"rsp", RSP},
  {"rbp", RBP},
  {"rsi", RSI},
  {"rdi", RDI},

  {"r8", R8},
  {"r9", R9},
  {"r10", R10},
  {"r11", R11},
  {"r12", R12},
  {"r13", R13},
  {"r14", R14},
  {"r15", R15},

  {"rip", RIP},
};

static const char *kDirectiveTable[] = {
  "ascii",
  "section",
  "text",
  "data",
  "align",
  "byte",
  "word",
  "long",
  "quad",
  "comm",
  "globl",
  "extern",
};

bool is_reg8(enum RegType reg) {
  return reg >= AL && reg <= BL;
}

bool is_reg8s(enum RegType reg) {
  return reg >= AL && reg <= DIL;
}

bool is_reg8x(enum RegType reg) {
  return reg >= R8B && reg <= R15B;
}

bool is_reg16(enum RegType reg) {
  return reg >= AX && reg <= BX;
}

bool is_reg32(enum RegType reg) {
  return reg >= EAX && reg <= EDI;
}

bool is_reg32x(enum RegType reg) {
  return reg >= R8D && reg <= R15D;
}

bool is_reg64(enum RegType reg) {
  return reg >= RAX && reg <= RDI;
}

bool is_reg64x(enum RegType reg) {
  return reg >= R8 && reg <= R15;
}

const char *skip_whitespace(const char *p) {
  while (isspace(*p))
    ++p;
  return p;
}

static int find_match_index(const char **pp, const char **table, size_t count) {
  const char *p = *pp;
  const char *start = p;

  while (isalpha(*p))
    ++p;
  if (*p == '\0' || isspace(*p)) {
    size_t n = p - start;
    for (size_t i = 0; i < count; ++i) {
      const char *name = table[i];
      size_t len = strlen(name);
      if (n == len && strncasecmp(start, name, n) == 0) {
        *pp = skip_whitespace(p);
        return i;
      }
    }
  }
  return -1;
}

enum Opcode parse_opcode(const char **pp) {
  return find_match_index(pp, kOpTable, sizeof(kOpTable) / sizeof(*kOpTable)) + 1;
}

enum DirectiveType parse_directive(const char **pp) {
  return find_match_index(pp, kDirectiveTable, sizeof(kDirectiveTable) / sizeof(*kDirectiveTable)) + 1;
}

enum RegType parse_register(const char **pp) {
  const char *p = *pp;
  for (int i = 0, len = sizeof(kRegisters) / sizeof(*kRegisters); i < len; ++i) {
    const char *name = kRegisters[i].name;
    size_t n = strlen(name);
    if (strncmp(p, name, n) == 0) {
      *pp = p + n;
      return kRegisters[i].reg;
    }
  }
  return NOREG;
}

bool parse_immediate(const char **pp, long *value) {
  const char *p = *pp;
  bool negative = false;
  if (*p == '-') {
    negative = true;
    ++p;
  }
  if (!isdigit(*p))
    return false;
  long v = strtol(p, (char**)pp, 10);
  *value = negative ? -v : v;
  return true;
}

static bool is_label_first_chr(char c) {
  return isalpha(c) || c == '_' || c == '.';
}

static bool is_label_chr(char c) {
  return is_label_first_chr(c) || isdigit(c);
}

const char *parse_label(const char **pp) {
  const char *p = *pp;
  const char *start = p;
  if (!is_label_first_chr(*p))
    return NULL;

  do {
    ++p;
  } while (is_label_chr(*p));
  *pp = p;
  return strndup_(start, p - start);
}

bool parse_operand(const char **pp, Operand *operand) {
  const char *p = *pp;
  if (*p == '%') {
    *pp = p + 1;
    enum RegType reg = parse_register(pp);
    if (reg == NOREG) {
      error("Illegal register");
    }
    operand->type = REG;
    operand->u.reg = reg;
    return true;
  }

  if (*p == '*' && p[1] == '%') {
    *pp = p + 2;
    enum RegType reg = parse_register(pp);
    if (!is_reg64(reg))
      error("Illegal register");
    operand->type = DEREF_REG;
    operand->u.deref_reg = reg;
    return true;
  }

  if (*p == '$') {
    *pp = p + 1;
    if (!parse_immediate(pp, &operand->u.immediate))
      error("Syntax error");
    operand->type = IMMEDIATE;
    return true;
  }

  bool has_offset = false;
  long offset = 0;
  const char *label = parse_label(pp);
  if (label == NULL) {
    bool neg = false;
    if (*p == '-') {
      neg = true;
      ++p;
    }
    if (isdigit(*p)) {
      offset = strtol(p, (char**)pp, 10);
      if (*pp > p)
        has_offset = true;
      if (neg)
        offset = -offset;
    } else if (neg) {
      error("Illegal `-'");
    }
  }
  p = skip_whitespace(*pp);
  if (*p != '(') {
    if (label != NULL) {
      operand->type = LABEL;
      operand->u.label = label;
      *pp = p;
      return true;
    }
    if (has_offset)
      error("direct number not implemented");
  } else {
    if (p[1] == '%') {
      *pp = p + 2;
      enum RegType reg = parse_register(pp);
      if (reg == NOREG)
        error("Register expected");
      p = skip_whitespace(*pp);
      if (*p != ')')
        error("`)' expected");
      *pp = ++p;
      operand->type = INDIRECT;
      operand->u.indirect.reg = reg;
      operand->u.indirect.label = label;
      operand->u.indirect.offset = offset;
      return true;
    }
    error("Illegal `('");
  }

  return false;
}
