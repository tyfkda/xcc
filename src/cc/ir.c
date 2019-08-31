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
  vec_push(curbb->irs, ir);
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

IR *new_ir_iofs(const char *label) {
  IR *ir = new_ir(IR_IOFS);
  ir->u.iofs.label = label;
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

IR *new_ir_cmpi(intptr_t value, int size) {
  IR *ir = new_ir(IR_CMPI);
  ir->value = value;
  ir->size = size;
  return ir;
}

IR *new_ir_incdec(bool inc, bool pre, int size, intptr_t value) {
  IR *ir = new_ir(IR_INCDEC);
  ir->u.incdec.inc = inc;
  ir->u.incdec.pre = pre;
  ir->size = size;
  ir->value = value;
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

IR *new_ir_cast(int dstsize, int srcsize) {
  IR *ir = new_ir(IR_CAST);
  ir->size = dstsize;
  ir->u.cast.srcsize = srcsize;
  return ir;
}

IR *new_ir_label(const char *label, bool global) {
  IR *ir = new_ir(IR_LABEL);
  ir->u.label.name = label;
  ir->u.label.global = global;
  return ir;
}

IR *new_ir_assign_lval(int size) {
  IR *ir = new_ir(IR_ASSIGN_LVAL);
  ir->size = size;
  return ir;
}

IR *new_ir_clear(size_t size) {
  IR *ir = new_ir(IR_CLEAR);
  ir->size = size;
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

static void ir_out_store(int size) {
  // Store %rax to %rdi
  switch (size) {
  case 1:  MOV(AL, INDIRECT(RDI)); break;
  case 2:  MOV(AX, INDIRECT(RDI)); break;
  case 4:  MOV(EAX, INDIRECT(RDI)); break;
  case 8:  MOV(RAX, INDIRECT(RDI)); break;
  default:  assert(false); break;
  }
}

static void ir_out_incdec(const IR *ir) {
  static const char *kRegATable[] = {AL, AX, EAX, RAX};
  static const char *kRegDiTable[] = {DIL, DI, EDI, RDI};

  int size;
  switch (ir->size) {
  default: assert(false); // Fallthrough to suppress compile error
  case 1:  size = 0; break;
  case 2:  size = 1; break;
  case 4:  size = 2; break;
  case 8:  size = 3; break;
  }

  if (ir->value == 1) {
    if (!ir->u.incdec.pre)
      MOV(INDIRECT(RAX), kRegDiTable[size]);

    switch (size) {
    case 0:  if (ir->u.incdec.inc) INCB(INDIRECT(RAX)); else DECB(INDIRECT(RAX)); break;
    case 1:  if (ir->u.incdec.inc) INCW(INDIRECT(RAX)); else DECW(INDIRECT(RAX)); break;
    case 2:  if (ir->u.incdec.inc) INCL(INDIRECT(RAX)); else DECL(INDIRECT(RAX)); break;
    case 3:  if (ir->u.incdec.inc) INCQ(INDIRECT(RAX)); else DECQ(INDIRECT(RAX)); break;
    default: assert(false); break;
    }

    if (ir->u.incdec.pre)
      MOV(INDIRECT(RAX), kRegATable[size]);
    else
      MOV(kRegDiTable[size], kRegATable[size]);
  } else {
    intptr_t value = ir->value;
    if (ir->u.incdec.pre) {
      if (value <= ((1L << 31) - 1)) {
        if (ir->u.incdec.inc)  ADDQ(IM(value), INDIRECT(RAX));
        else                   SUBQ(IM(value), INDIRECT(RAX));
      } else {
        MOV(IM(value), RDI);
        if (ir->u.incdec.inc)  ADD(RDI, INDIRECT(RAX));
        else                   SUB(RDI, INDIRECT(RAX));
      }
      MOV(INDIRECT(RAX), RAX);
    } else {
      MOV(INDIRECT(RAX), RDI);
      if (value <= ((1L << 31) - 1)) {
        if (ir->u.incdec.inc)  ADDQ(IM(value), INDIRECT(RAX));
        else                   SUBQ(IM(value), INDIRECT(RAX));
      } else {
        MOV(IM(value), RCX);
        if (ir->u.incdec.inc)  ADD(RCX, INDIRECT(RAX));
        else                   SUB(RCX, INDIRECT(RAX));
      }
      MOV(RDI, RAX);
    }
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

  case IR_IOFS:
    LEA(LABEL_INDIRECT(ir->u.iofs.label, RIP), RAX);
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
    ir_out_store(ir->size);
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

  case IR_MOD:
    POP(RDI); POP_STACK_POS();
    XOR(EDX, EDX);  // RDX = 0
    switch (ir->size) {
    case 1:  IDIV(DIL); MOV(DL, AL); break;
    case 2:  IDIV(DI);  MOV(DX, AX); break;
    case 4:  IDIV(EDI); MOV(EDX, EAX); break;
    case 8:  IDIV(RDI); MOV(RDX, RAX); break;
    default: assert(false); break;
    }
    break;

  case IR_BITAND:
    POP(RDI); POP_STACK_POS();
    switch (ir->size) {
    case 1:  AND(DIL, AL); break;
    case 2:  AND(DI, AX); break;
    case 4:  AND(EDI, EAX); break;
    case 8:  AND(RDI, RAX); break;
    default: assert(false); break;
    }
    break;

  case IR_BITOR:
    POP(RDI); POP_STACK_POS();
    switch (ir->size) {
    case 1:  OR(DIL, AL); break;
    case 2:  OR(DI, AX); break;
    case 4:  OR(EDI, EAX); break;
    case 8:  OR(RDI, RAX); break;
    default: assert(false); break;
    }
    break;

  case IR_BITXOR:
    POP(RDI); POP_STACK_POS();
    switch (ir->size) {
    case 1:  XOR(DIL, AL); break;
    case 2:  XOR(DI, AX); break;
    case 4:  XOR(EDI, EAX); break;
    case 8:  XOR(RDI, RAX); break;
    default: assert(false); break;
    }
    break;

  case IR_LSHIFT:
  case IR_RSHIFT:
    POP(RCX); POP_STACK_POS();
    if (ir->type == IR_LSHIFT) {
      switch (ir->size) {
      case 1:  SHL(CL, AL); break;
      case 2:  SHL(CL, AX); break;
      case 4:  SHL(CL, EAX); break;
      case 8:  SHL(CL, RAX); break;
      default: assert(false); break;
      }
    } else {
      switch (ir->size) {
      case 1:  SHR(CL, AL); break;
      case 2:  SHR(CL, AX); break;
      case 4:  SHR(CL, EAX); break;
      case 8:  SHR(CL, RAX); break;
      default: assert(false); break;
      }
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

  case IR_INCDEC:
    ir_out_incdec(ir);
    break;

  case IR_NEG:
    switch (ir->size) {
    case 1:  NEG(AL); break;
    case 2:  NEG(AX); break;
    case 4:  NEG(EAX); break;
    case 8:  NEG(RAX); break;
    default:  assert(false); break;
    }
    break;

  case IR_NOT:
    switch (ir->size) {
    case 1:  TEST(AL, AL); break;
    case 2:  TEST(AX, AX); break;
    case 4:  TEST(EAX, EAX); break;
    case 8:  TEST(RAX, RAX); break;
    default:  assert(false); break;
    }
    SETE(AL);
    MOVSX(AL, EAX);
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

  case IR_CMPI:
    {
      intptr_t x = ir->value;
      switch (ir->size) {
      case 1:  CMP(IM(x), AL); break;
      case 2:  CMP(IM(x), AX); break;
      case 4:  CMP(IM(x), EAX); break;
      case 8:
        if (is_im32(x)) {
          CMP(IM(x), RAX);
        } else {
          MOV(IM(x), RDI);
          CMP(RDI, RAX);
        }
        break;
      default: assert(false); break;
      }
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
      if (ir->u.call.label != NULL)
        CALL(ir->u.call.label);
      else
        CALL(fmt("*%s", RAX));
    }
    break;

  case IR_ADDSP:
    if (ir->value > 0)
      ADD(IM(ir->value), RSP);
    else
      SUB(IM(-ir->value), RSP);
    stackpos -= ir->value;
    break;

  case IR_CAST:
    if (ir->size > ir->u.cast.srcsize) {
      switch (ir->size) {
      case 2:
        switch (ir->u.cast.srcsize) {
        case 1:  MOVSX(AL, AX); break;
        default:  assert(false); break;
        }
        break;
      case 4:
        switch (ir->u.cast.srcsize) {
        case 1:  MOVSX(AL, EAX); break;
        case 2:  MOVSX(AX, EAX); break;
        default:  assert(false); break;
        }
        break;
      case 8:
        switch (ir->u.cast.srcsize) {
        case 1:  MOVSX(AL, RAX); break;
        case 2:  MOVSX(AX, RAX); break;
        case 4:  MOVSX(EAX, RAX); break;
        default:
          assert(false); break;
        }
        break;
      default:  assert(false); break;
      }
    }
    break;

  case IR_LABEL:
    if (ir->u.label.global)
      _GLOBL(ir->u.label.name);
    EMIT_LABEL(ir->u.label.name);
    break;

  case IR_SAVE_LVAL:
    MOV(RAX, RSI);  // Save lhs address to %rsi.
    break;

  case IR_ASSIGN_LVAL:
    MOV(RSI, RDI);
    ir_out_store(ir->size);
    break;

  case IR_CLEAR:
    {
      const char *loop = alloc_label();
      MOV(RAX, RSI);
      MOV(IM(ir->size), EDI);
      XOR(AL, AL);
      EMIT_LABEL(loop);
      MOV(AL, INDIRECT(RSI));
      INC(RSI);
      DEC(EDI);
      JNE(loop);
    }
    break;

  default:
    assert(false);
    break;
  }
}

// Basic Block

BB *curbb;

BB *new_bb(void) {
  BB *bb = malloc(sizeof(*bb));
  bb->next = NULL;
  bb->label = alloc_label();
  bb->irs = new_vector();
  return bb;
}

BB *bb_split(BB *bb) {
  BB *cc = new_bb();
  cc->next = bb->next;
  bb->next = cc;
  return cc;
}
