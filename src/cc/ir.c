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

IR *new_ir_bofs(int offset) {
  IR *ir = new_ir(IR_BOFS);
  ir->value = offset;
  return ir;
}

IR *new_ir_load(int size) {
  IR *ir = new_ir(IR_LOAD);
  ir->size = size;
  return ir;
}

IR *new_ir_store(int size) {
  IR *ir = new_ir(IR_STORE);
  ir->size = size;
  return ir;
}

IR *new_ir_memcpy(size_t size) {
  IR *ir = new_ir(IR_MEMCPY);
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

IR *new_ir_set(enum ConditionType cond) {
  IR *ir = new_ir(IR_SET);
  ir->u.set.cond = cond;
  return ir;
}

IR *new_ir_jmp(enum ConditionType cond, const char *label) {
  IR *ir = new_ir(IR_JMP);
  ir->u.jmp.label = label;
  ir->u.jmp.cond = cond;
  return ir;
}

IR *new_ir_call(const char *label, int arg_count) {
  IR *ir = new_ir(IR_CALL);
  ir->u.call.label = label;
  ir->u.call.arg_count = arg_count;
  return ir;
}

IR *new_ir_addsp(int value) {
  IR *ir = new_ir(IR_ADDSP);
  ir->value = value;
  return ir;
}

IR *new_ir_label(const char *label, bool global) {
  IR *ir = new_ir(IR_LABEL);
  ir->u.label.name = label;
  ir->u.label.global = global;
  return ir;
}

static void ir_memcpy(ssize_t size) {
  const char *dst = RDI;
  const char *src = RAX;

  // Break %rcx, %dl
  switch (size) {
  case 1:
    MOV(INDIRECT(src), DL);
    MOV(DL, INDIRECT(dst));
    break;
  case 2:
    MOV(INDIRECT(src), DX);
    MOV(DX, INDIRECT(dst));
    break;
  case 4:
    MOV(INDIRECT(src), EDX);
    MOV(EDX, INDIRECT(dst));
    break;
  case 8:
    MOV(INDIRECT(src), RDX);
    MOV(RDX, INDIRECT(dst));
    break;
  default:
    {
      const char * label = alloc_label();
      PUSH(RAX);
      MOV(IM(size), RCX);
      EMIT_LABEL(label);
      MOV(INDIRECT(src), DL);
      MOV(DL, INDIRECT(dst));
      INC(src);
      INC(dst);
      DEC(RCX);
      JNE(label);
      POP(RAX);
    }
    break;
  }
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

  case IR_BOFS:
    LEA(OFFSET_INDIRECT(ir->value, RBP), RAX);
    break;

  case IR_LOAD:
    switch (ir->size) {
    case 1:  MOV(INDIRECT(RAX), AL); break;
    case 2:  MOV(INDIRECT(RAX), AX); break;
    case 4:  MOV(INDIRECT(RAX), EAX); break;
    case 8:  MOV(INDIRECT(RAX), RAX); break;
    default:  assert(false); break;
    }
    break;

  case IR_STORE:
    POP(RDI); POP_STACK_POS();
    switch (ir->size) {
    case 1:  MOV(AL, INDIRECT(RDI)); break;
    case 2:  MOV(AX, INDIRECT(RDI)); break;
    case 4:  MOV(EAX, INDIRECT(RDI)); break;
    case 8:  MOV(RAX, INDIRECT(RDI)); break;
    default:  assert(false); break;
    }
    break;

  case IR_MEMCPY:
    POP(RDI); POP_STACK_POS();
    ir_memcpy(ir->size);
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

  case IR_CMP:
    {
      POP(RDI); POP_STACK_POS();

      switch (ir->size) {
      case 1:  CMP(AL, DIL); break;
      case 2:  CMP(AX, DI); break;
      case 4:  CMP(EAX, EDI); break;
      case 8:  CMP(RAX, RDI); break;
      default: assert(false); break;
      }
    }
    break;

  case IR_SET:
    {
      switch (ir->u.set.cond) {
      case COND_EQ:  SETE(AL); break;
      case COND_NE:  SETNE(AL); break;
      case COND_LT:  SETL(AL); break;
      case COND_GT:  SETG(AL); break;
      case COND_LE:  SETLE(AL); break;
      case COND_GE:  SETGE(AL); break;
      default: assert(false); break;
      }
      MOVSX(AL, EAX);
    }
    break;

  case IR_PUSH:
    PUSH(RAX); PUSH_STACK_POS();
    break;

  case IR_JMP:
    switch (ir->u.jmp.cond) {
    case COND_ANY:  JMP(ir->u.jmp.label); break;
    case COND_EQ:   JE(ir->u.jmp.label); break;
    case COND_NE:   JNE(ir->u.jmp.label); break;
    case COND_LT:   JL(ir->u.jmp.label); break;
    case COND_GT:   JG(ir->u.jmp.label); break;
    case COND_LE:   JLE(ir->u.jmp.label); break;
    case COND_GE:   JGE(ir->u.jmp.label); break;
    default:  assert(false); break;
    }
    break;

  case IR_CALL:
    {
      static const char *kReg64s[] = {RDI, RSI, RDX, RCX, R8, R9};
      int reg_args = MIN((int)ir->u.call.arg_count, MAX_REG_ARGS);
      for (int i = 0; i < reg_args; ++i) {
        POP(kReg64s[i]); POP_STACK_POS();
      }
      CALL(ir->u.call.label);
    }
    break;

  case IR_ADDSP:
    if (ir->value > 0)
      ADD(IM(ir->value), RSP);
    else
      SUB(IM(-ir->value), RSP);
    stackpos -= ir->value;
    break;

  case IR_LABEL:
    if (ir->u.label.global)
      _GLOBL(ir->u.label.name);
    EMIT_LABEL(ir->u.label.name);
    break;

  default:
    assert(false);
    break;
  }
}
