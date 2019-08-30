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

IR *new_ir_op(enum IrType type, int size) {
  IR *ir = new_ir(type);
  ir->size = size;
  return ir;
}

IR *new_ir_st(enum IrType type) {
  return new_ir(type);
}

IR *new_ir_jmp(const char *label) {
  IR *ir = new_ir(IR_JMP);
  ir->u.jmp.label = label;
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

  case IR_ADD:
    POP(RDI); POP_STACK_POS();
    switch (ir->size) {
    case 1:  ADD(DIL, AL); break;
    case 2:  ADD(DI, AX); break;
    case 4:  ADD(EDI, EAX); break;
    case 8:  ADD(RDI, RAX); break;
    default: assert(false); break;
    }
    break;

  case IR_SUB:
    POP(RDI); POP_STACK_POS();
    switch (ir->size) {
    case 1:  SUB(DIL, AL); break;
    case 2:  SUB(DI, AX); break;
    case 4:  SUB(EDI, EAX); break;
    case 8:  SUB(RDI, RAX); break;
    default: assert(false); break;
    }
    break;

  case IR_MUL:
    POP(RDI); POP_STACK_POS();
    switch (ir->size) {
    case 1:  MUL(DIL); break;
    case 2:  MUL(DI); break;
    case 4:  MUL(EDI); break;
    case 8:  MUL(RDI); break;
    default: assert(false); break;
    }
    break;

  case IR_DIV:
    POP(RDI); POP_STACK_POS();
    XOR(EDX, EDX);  // RDX = 0
    switch (ir->size) {
    case 1:
      MOVSX(DIL, RDI);
      MOVSX(AL, EAX);
      CLTD();
      IDIV(EDI);
      break;
    case 2:
      MOVSX(DI, EDI);
      MOVSX(AX, EAX);
      // Fallthrough
    case 4:
      CLTD();
      IDIV(EDI);
      break;
    case 8:
      CQTO();
      IDIV(RDI);
      break;
    default: assert(false); break;
    }
    break;

  case IR_PUSH:
    PUSH(RAX); PUSH_STACK_POS();
    break;

  case IR_JMP:
    JMP(ir->u.jmp.label);
    break;

  default:
    assert(false);
    break;
  }
}
