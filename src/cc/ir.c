#include "ir.h"

#include <assert.h>
#include <inttypes.h>
#include <stdlib.h>

#include "codegen.h"
#include "parser.h"
#include "sema.h"  // curfunc
#include "type.h"
#include "util.h"
#include "var.h"
#include "x86_64.h"

static IR *new_ir(enum IrType type) {
  IR *ir = malloc(sizeof(*ir));
  ir->type = type;
  vec_push(curfunc->irs, ir);
  return ir;
}

IR *new_ir_imm(intptr_t value, int size) {
  IR *ir = new_ir(IR_IMM);
  ir->value = value;
  ir->size = size;
  return ir;
}

void ir_out(const IR *ir) {
  switch (ir->type) {
  case IR_IMM:
    {
      intptr_t value = ir->value;
      switch (ir->size) {
      case 1:
        if (value == 0)
          XOR(AL, AL);
        else
          MOV(IM(value), AL);
        return;

      case 2:
        if (value == 0)
          XOR(AX, AX);
        else
          MOV(IM(value), AX);
        return;

      case 4:
        if (value == 0)
          XOR(EAX, EAX);
        else
          MOV(IM(value), EAX);
        return;

      case 8:
        if (value == 0)
          XOR(EAX, EAX);  // upper 32bit is also cleared.
        else
          MOV(IM(value), RAX);
        return;

      default: assert(false); break;
      }
      break;
    }
    break;

  default:
    assert(false);
    break;
  }
}
