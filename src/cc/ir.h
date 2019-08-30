// Intermediate Representation

#pragma once

#include <stdint.h>  // intptr_t

enum IrType {
  IR_IMM,   // Immediate value
};

typedef struct {
  enum IrType type;
  int size;
  intptr_t value;
} IR;

IR *new_ir_imm(intptr_t value, int size);

void ir_out(const IR *ir);
