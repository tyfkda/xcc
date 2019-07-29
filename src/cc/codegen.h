#pragma once

#include <stdbool.h>
#include <stdint.h>  // uintptr_t
#include <stdio.h>  // FILE

typedef struct Expr Expr;
typedef struct Node Node;
typedef struct StructInfo StructInfo;
typedef struct Type Type;

#define MAX_REG_ARGS  (6)
#define WORD_SIZE  (8)  /*sizeof(void*)*/

// Public

void init_gen(uintptr_t start_address);
void gen(Node *node);

// Private

#define PUSH_STACK_POS()  do { stackpos += 8; } while (0)
#define POP_STACK_POS()   do { stackpos -= 8; } while (0)

extern int stackpos;

enum SectionType {
  SEC_CODE,
  SEC_DATA,
};

void set_asm_fp(FILE *fp);
void gen_expr(Expr *expr);
size_t type_size(const Type *type);
void calc_struct_size(StructInfo *sinfo);
void gen_rodata(void);
void output_section(FILE* fp, int section);
void add_label(const char *label);
void add_asm_label(const char *label);
void add_code(const unsigned char* buf, size_t size);
void add_asm(const char *fmt, ...);
void add_loc_rel8(const char *label, int ofs, int baseofs);
void add_loc_rel32(const char *label, int ofs, int baseofs);
void fixup_locations(void);
void get_section_size(int section, size_t *pfilesz, size_t *pmemsz, uintptr_t *ploadadr);
uintptr_t label_adr(const char *label);

void gen_cond_jmp(Expr *cond, bool tf, const char *label);
