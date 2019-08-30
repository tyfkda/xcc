// Intermediate Representation

#pragma once

#include <stdint.h>  // intptr_t

enum IrType {
  IR_IMM,   // Immediate value
  IR_ADD,
  IR_SUB,
  IR_MUL,
  IR_DIV,
  IR_PUSH,
  IR_JMP,
};

typedef struct {
  enum IrType type;
  int size;
  intptr_t value;

  union {
    struct {
      const char *label;
    } jmp;
  } u;
} IR;

IR *new_ir_imm(intptr_t value, int size);
IR *new_ir_op(enum IrType type, int size);
IR *new_ir_st(enum IrType type);
IR *new_ir_jmp(const char *label);

void ir_out(const IR *ir);
