#include "ir.h"

#include <assert.h>
#include <inttypes.h>
#include <stdlib.h>
#include <string.h>

#include "ast.h"
#include "codegen.h"
#include "regalloc.h"
#include "table.h"
#include "type.h"
#include "util.h"
#include "var.h"
#include "x86_64.h"

static enum ConditionKind invert_cond(enum ConditionKind cond) {
  assert(COND_EQ <= cond && cond <= COND_GT);
  if (cond <= COND_NE)
    return COND_NE + COND_EQ - cond;
  return COND_LT + ((cond - COND_LT) ^ 2);
}

// Virtual register

VReg *new_vreg(int vreg_no, const Type *type) {
  VReg *vreg = malloc(sizeof(*vreg));
  vreg->v = vreg_no;
  vreg->r = -1;
  vreg->type = type;
  vreg->param_index = -1;
  vreg->offset = 0;
  return vreg;
}

void vreg_spill(VReg *vreg) {
  vreg->r = SPILLED_REG_NO;
}

// Register allocator

const char *kRegSizeTable[][7] = {
  { BL, R10B, R11B, R12B, R13B, R14B, R15B},
  { BX, R10W, R11W, R12W, R13W, R14W, R15W},
  {EBX, R10D, R11D, R12D, R13D, R14D, R15D},
  {RBX, R10,  R11,  R12,  R13,  R14,  R15},
};

#define kReg8s   (kRegSizeTable[0])
#define kReg32s  (kRegSizeTable[2])
#define kReg64s  (kRegSizeTable[3])

const char *kRegATable[] = {AL, AX, EAX, RAX};
const char *kRegDTable[] = {DL, DX, EDX, RDX};

#define CALLEE_SAVE_REG_COUNT  (5)
static struct {
  const char * reg;
  short bit;
} const kCalleeSaveRegs[] = {
  {RBX, 1 << 0},
  {R12, 1 << 3},
  {R13, 1 << 4},
  {R14, 1 << 5},
  {R15, 1 << 6},
};

//
RegAlloc *curra;

// Intermediate Representation

static IR *new_ir(enum IrKind kind) {
  IR *ir = malloc(sizeof(*ir));
  ir->kind = kind;
  ir->dst = ir->opr1 = ir->opr2 = NULL;
  ir->size = -1;
  if (curbb != NULL)
    vec_push(curbb->irs, ir);
  return ir;
}

static const int kPow2Table[] = {-1, 0, 1, -1, 2, -1, -1, -1, 3};
#define kPow2TableSize  ((int)(sizeof(kPow2Table) / sizeof(*kPow2Table)))

VReg *new_ir_imm(intptr_t value, const Type *type) {
  IR *ir = new_ir(IR_IMM);
  ir->value = value;
  ir->size = type_size(type);
  return ir->dst = reg_alloc_spawn(curra, type);
}

VReg *new_ir_bop(enum IrKind kind, VReg *opr1, VReg *opr2, const Type *type) {
  IR *ir = new_ir(kind);
  ir->opr1 = opr1;
  ir->opr2 = opr2;
  ir->size = type_size(type);
  return ir->dst = reg_alloc_spawn(curra, type);
}

VReg *new_ir_unary(enum IrKind kind, VReg *opr, const Type *type) {
  IR *ir = new_ir(kind);
  ir->opr1 = opr;
  ir->size = type_size(type);
  return ir->dst = reg_alloc_spawn(curra, type);
}

VReg *new_ir_bofs(VReg *src) {
  IR *ir = new_ir(IR_BOFS);
  ir->opr1 = src;
  ir->size = WORD_SIZE;
  return ir->dst = reg_alloc_spawn(curra, &tyVoidPtr);
}

VReg *new_ir_iofs(const Name *label, bool global) {
  IR *ir = new_ir(IR_IOFS);
  ir->iofs.label = label;
  ir->iofs.global = global;
  ir->size = WORD_SIZE;
  return ir->dst = reg_alloc_spawn(curra, &tyVoidPtr);
}

void new_ir_store(VReg *dst, VReg *src, int size) {
  IR *ir = new_ir(IR_STORE);
  ir->opr1 = src;
  ir->size = size;
  ir->opr2 = dst;  // `dst` is used by indirect, so it is not actually `dst`.
}

void new_ir_memcpy(VReg *dst, VReg *src, int size) {
  IR *ir = new_ir(IR_MEMCPY);
  ir->opr1 = src;
  ir->opr2 = dst;
  ir->size = size;
}


void new_ir_cmp(VReg *opr1, VReg *opr2, int size) {
  IR *ir = new_ir(IR_CMP);
  ir->opr1 = opr1;
  ir->opr2 = opr2;
  ir->size = size;
}

void new_ir_test(VReg *reg, int size) {
  IR *ir = new_ir(IR_TEST);
  ir->opr1 = reg;
  ir->size = size;
}

void new_ir_incdec(enum IrKind kind, VReg *reg, int size, intptr_t value) {
  IR *ir = new_ir(kind);
  ir->opr1 = reg;
  ir->size = size;
  ir->value = value;
}

VReg *new_ir_set(enum ConditionKind cond) {
  IR *ir = new_ir(IR_SET);
  ir->set.cond = cond;
  return ir->dst = reg_alloc_spawn(curra, &tyBool);
}

void new_ir_jmp(enum ConditionKind cond, BB *bb) {
  IR *ir = new_ir(IR_JMP);
  ir->jmp.bb = bb;
  ir->jmp.cond = cond;
}

void new_ir_pusharg(VReg *vreg) {
  IR *ir = new_ir(IR_PUSHARG);
  ir->opr1 = vreg;
  ir->size = WORD_SIZE;
}

void new_ir_precall(int arg_count, bool *stack_aligned) {
  IR *ir = new_ir(IR_PRECALL);
  ir->call.stack_aligned = stack_aligned;
  ir->call.arg_count = arg_count;
}

VReg *new_ir_call(const Name *label, bool global, VReg *freg, int arg_count, const Type *result_type, bool *stack_aligned) {
  IR *ir = new_ir(IR_CALL);
  ir->call.label = label;
  ir->call.global = global;
  ir->opr1 = freg;
  ir->call.stack_aligned = stack_aligned;
  ir->call.arg_count = arg_count;
  ir->size = type_size(result_type);
  return ir->dst = reg_alloc_spawn(curra, result_type);
}

void new_ir_addsp(int value) {
  IR *ir = new_ir(IR_ADDSP);
  ir->value = value;
}

VReg *new_ir_cast(VReg *vreg, const Type *dsttype, int srcsize, bool is_unsigned) {
  IR *ir = new_ir(IR_CAST);
  ir->opr1 = vreg;
  ir->size = type_size(dsttype);
  ir->cast.srcsize = srcsize;
  ir->cast.is_unsigned = is_unsigned;
  return ir->dst = reg_alloc_spawn(curra, dsttype);
}

void new_ir_mov(VReg *dst, VReg *src, int size) {
  IR *ir = new_ir(IR_MOV);
  ir->dst = dst;
  ir->opr1 = src;
  ir->size = size;
}

void new_ir_clear(VReg *reg, size_t size) {
  IR *ir = new_ir(IR_CLEAR);
  ir->size = size;
  ir->opr1 = reg;
}

void new_ir_result(VReg *reg, int size) {
  IR *ir = new_ir(IR_RESULT);
  ir->opr1 = reg;
  ir->size = size;
}

void new_ir_asm(const char *asm_) {
  IR *ir = new_ir(IR_ASM);
  ir->asm_.str = asm_;
}

IR *new_ir_load_spilled(int offset, int size) {
  IR *ir = new_ir(IR_LOAD_SPILLED);
  ir->value = offset;
  ir->size = size;
  return ir;
}

IR *new_ir_store_spilled(int offset, int size) {
  IR *ir = new_ir(IR_STORE_SPILLED);
  ir->value = offset;
  ir->size = size;
  return ir;
}

static void ir_memcpy(int dst_reg, int src_reg, ssize_t size) {
  const char *dst = kReg64s[dst_reg];
  const char *src = kReg64s[src_reg];

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
      const Name *name = alloc_label();
      const char *label = fmt_name(name);
      PUSH(src);
      MOV(IM(size), RCX);
      EMIT_LABEL(label);
      MOV(INDIRECT(src), DL);
      MOV(DL, INDIRECT(dst));
      INC(src);
      INC(dst);
      DEC(RCX);
      JNE(label);
      POP(src);
    }
    break;
  }
}

static void ir_out(const IR *ir) {
  switch (ir->kind) {
  case IR_IMM:
    {
      intptr_t value = ir->value;
      assert(0 <= ir->size && ir->size < kPow2TableSize);
      int pow = kPow2Table[ir->size];
      assert(0 <= pow && pow < 4);
      const char **regs = kRegSizeTable[pow];
      const char *dst = regs[ir->dst->r];
      if (value == 0)
        XOR(dst, dst);
      else
        MOV(IM(value), dst);
      break;
    }
    break;

  case IR_BOFS:
    LEA(OFFSET_INDIRECT(ir->opr1->offset, RBP), kReg64s[ir->dst->r]);
    break;

  case IR_IOFS:
    {
      const char *label = fmt_name(ir->iofs.label);
      if (ir->iofs.global)
        label = MANGLE(label);
      LEA(LABEL_INDIRECT(label, RIP), kReg64s[ir->dst->r]);
    }
    break;

  case IR_LOAD:
    {
      assert(0 <= ir->size && ir->size < kPow2TableSize);
      int pow = kPow2Table[ir->size];
      assert(0 <= pow && pow < 4);
      const char **regs = kRegSizeTable[pow];
      MOV(INDIRECT(kReg64s[ir->opr1->r]), regs[ir->dst->r]);
    }
    break;

  case IR_STORE:
    {
      assert(0 <= ir->size && ir->size < kPow2TableSize);
      int pow = kPow2Table[ir->size];
      assert(0 <= pow && pow < 4);
      const char **regs = kRegSizeTable[pow];
      MOV(regs[ir->opr1->r], INDIRECT(kReg64s[ir->opr2->r]));
    }
    break;

  case IR_MEMCPY:
    ir_memcpy(ir->opr2->r, ir->opr1->r, ir->size);
    break;

  case IR_ADD:
    {
      assert(ir->dst->r == ir->opr1->r);
      assert(0 <= ir->size && ir->size < kPow2TableSize);
      int pow = kPow2Table[ir->size];
      assert(0 <= pow && pow < 4);
      const char **regs = kRegSizeTable[pow];
      ADD(regs[ir->opr2->r], regs[ir->dst->r]);
    }
    break;

  case IR_SUB:
    {
      assert(ir->dst->r == ir->opr1->r);
      assert(0 <= ir->size && ir->size < kPow2TableSize);
      int pow = kPow2Table[ir->size];
      assert(0 <= pow && pow < 4);
      const char **regs = kRegSizeTable[pow];
      SUB(regs[ir->opr2->r], regs[ir->dst->r]);
    }
    break;

  case IR_MUL:
    {
      assert(0 <= ir->size && ir->size < kPow2TableSize);
      int pow = kPow2Table[ir->size];
      assert(0 <= pow && pow < 4);
      const char **regs = kRegSizeTable[pow];
      const char *a = kRegATable[pow];
      MOV(regs[ir->opr1->r], a);
      MUL(regs[ir->opr2->r]);
      MOV(a, regs[ir->dst->r]);
    }
    break;

  case IR_DIV:
  case IR_DIVU:
    if (ir->size == 1) {
      if (ir->kind == IR_DIV) {
        MOVSX(kReg8s[ir->opr1->r], AX);
        IDIV(kReg8s[ir->opr2->r]);
      } else {
        MOVZX(kReg8s[ir->opr1->r], AX);
        DIV(kReg8s[ir->opr2->r]);
      }
      MOV(AL, kReg8s[ir->dst->r]);
    } else {
      assert(0 <= ir->size && ir->size < kPow2TableSize);
      int pow = kPow2Table[ir->size];
      assert(0 <= pow && pow < 4);
      const char **regs = kRegSizeTable[pow];
      const char *a = kRegATable[pow];
      MOV(regs[ir->opr1->r], a);
      if (ir->kind == IR_DIV) {
        switch (pow) {
        case 1:  CWTL(); break;
        case 2:  CLTD(); break;
        case 3:  CQTO(); break;
        default: assert(false); break;
        }
        IDIV(regs[ir->opr2->r]);
      } else {
        switch (pow) {
        case 1:  XOR(DX, DX); break;
        case 2:  XOR(EDX, EDX); break;
        case 3:  XOR(EDX, EDX); break;  // Clear 64bit register.
        default: assert(false); break;
        }
        DIV(regs[ir->opr2->r]);
      }
      MOV(a, regs[ir->dst->r]);
    }
    break;

  case IR_MOD:
  case IR_MODU:
    if (ir->size == 1) {
      if (ir->kind == IR_MOD) {
        MOVSX(kReg8s[ir->opr1->r], AX);
        IDIV(kReg8s[ir->opr2->r]);
      } else {
        MOVZX(kReg8s[ir->opr1->r], AX);
        DIV(kReg8s[ir->opr2->r]);
      }
      MOV(AH, kReg8s[ir->dst->r]);
    } else {
      assert(0 <= ir->size && ir->size < kPow2TableSize);
      int pow = kPow2Table[ir->size];
      assert(0 <= pow && pow < 4);
      const char **regs = kRegSizeTable[pow];
      const char *a = kRegATable[pow];
      const char *d = kRegDTable[pow];
      MOV(regs[ir->opr1->r], a);
      if (ir->kind == IR_MOD) {
        switch (pow) {
        case 1:  CWTL(); break;
        case 2:  CLTD(); break;
        case 3:  CQTO(); break;
        default: assert(false); break;
        }
        IDIV(regs[ir->opr2->r]);
      } else {
        switch (pow) {
        case 1:  XOR(DX, DX); break;
        case 2:  XOR(EDX, EDX); break;
        case 3:  XOR(EDX, EDX); break;  // Clear 64bit register.
        default: assert(false); break;
        }
        DIV(regs[ir->opr2->r]);
      }
      MOV(d, regs[ir->dst->r]);
    }
    break;

  case IR_BITAND:
    {
      assert(ir->dst->r == ir->opr1->r);
      assert(0 <= ir->size && ir->size < kPow2TableSize);
      int pow = kPow2Table[ir->size];
      assert(0 <= pow && pow < 4);
      const char **regs = kRegSizeTable[pow];
      AND(regs[ir->opr2->r], regs[ir->dst->r]);
    }
    break;

  case IR_BITOR:
    {
      assert(ir->dst->r == ir->opr1->r);
      assert(0 <= ir->size && ir->size < kPow2TableSize);
      int pow = kPow2Table[ir->size];
      assert(0 <= pow && pow < 4);
      const char **regs = kRegSizeTable[pow];
      OR(regs[ir->opr2->r], regs[ir->dst->r]);
    }
    break;

  case IR_BITXOR:
    {
      assert(ir->dst->r == ir->opr1->r);
      assert(0 <= ir->size && ir->size < kPow2TableSize);
      int pow = kPow2Table[ir->size];
      assert(0 <= pow && pow < 4);
      const char **regs = kRegSizeTable[pow];
      XOR(regs[ir->opr2->r], regs[ir->dst->r]);
    }
    break;

  case IR_LSHIFT:
    {
      assert(ir->dst->r == ir->opr1->r);
      MOV(kReg8s[ir->opr2->r], CL);
      assert(0 <= ir->size && ir->size < kPow2TableSize);
      int pow = kPow2Table[ir->size];
      assert(0 <= pow && pow < 4);
      const char **regs = kRegSizeTable[pow];
      SHL(CL, regs[ir->dst->r]);
    }
    break;
  case IR_RSHIFT:
    {
      assert(ir->dst->r == ir->opr1->r);
      MOV(kReg8s[ir->opr2->r], CL);
      assert(0 <= ir->size && ir->size < kPow2TableSize);
      int pow = kPow2Table[ir->size];
      assert(0 <= pow && pow < 4);
      const char **regs = kRegSizeTable[pow];
      SHR(CL, regs[ir->dst->r]);
    }
    break;

  case IR_CMP:
    {
      assert(0 <= ir->size && ir->size < kPow2TableSize);
      int pow = kPow2Table[ir->size];
      assert(0 <= pow && pow < 4);
      const char **regs = kRegSizeTable[pow];
      CMP(regs[ir->opr2->r], regs[ir->opr1->r]);
    }
    break;

  case IR_INC:
    {
      const char *reg = INDIRECT(kReg64s[ir->opr1->r]);
      if (ir->value == 1) {
        switch (ir->size) {
        case 1:  INCB(reg); break;
        case 2:  INCW(reg); break;
        case 4:  INCL(reg); break;
        case 8:  INCQ(reg); break;
        default:  assert(false); break;
        }
      } else {
        assert(ir->size == 8);
        intptr_t value = ir->value;
        if (value <= ((1L << 31) - 1)) {
          ADDQ(IM(value), reg);
        } else {
          MOV(IM(value), RAX);
          ADD(RAX, reg);
        }
      }
    }
    break;

  case IR_DEC:
    {
      const char *reg = INDIRECT(kReg64s[ir->opr1->r]);
      if (ir->value == 1) {
        switch (ir->size) {
        case 1:  DECB(reg); break;
        case 2:  DECW(reg); break;
        case 4:  DECL(reg); break;
        case 8:  DECQ(reg); break;
        default:  assert(false); break;
        }
      } else {
        assert(ir->size == 8);
        intptr_t value = ir->value;
        if (value <= ((1L << 31) - 1)) {
          SUBQ(IM(value), reg);
        } else {
          MOV(IM(value), RAX);
          SUB(RAX, reg);
        }
      }
    }
    break;

  case IR_NEG:
    {
      assert(ir->dst->r == ir->opr1->r);
      assert(0 <= ir->size && ir->size < kPow2TableSize);
      int pow = kPow2Table[ir->size];
      assert(0 <= pow && pow < 4);
      const char **regs = kRegSizeTable[pow];
      NEG(regs[ir->dst->r]);
    }
    break;

  case IR_NOT:
    {
      assert(0 <= ir->size && ir->size < kPow2TableSize);
      int pow = kPow2Table[ir->size];
      assert(0 <= pow && pow < 4);
      const char **regs = kRegSizeTable[pow];
      const char *opr1 = regs[ir->opr1->r];
      TEST(opr1, opr1);
      const char *dst8 = kReg8s[ir->dst->r];
      SETE(dst8);
      MOVSX(dst8, kReg32s[ir->dst->r]);
    }
    break;

  case IR_BITNOT:
    {
      assert(ir->dst->r == ir->opr1->r);
      assert(0 <= ir->size && ir->size < kPow2TableSize);
      int pow = kPow2Table[ir->size];
      assert(0 <= pow && pow < 4);
      const char **regs = kRegSizeTable[pow];
      NOT(regs[ir->dst->r]);
    }
    break;

  case IR_SET:
    {
      const char *dst = kReg8s[ir->dst->r];
      switch (ir->set.cond) {
      case COND_EQ:  SETE(dst); break;
      case COND_NE:  SETNE(dst); break;
      case COND_LT:  SETL(dst); break;
      case COND_GT:  SETG(dst); break;
      case COND_LE:  SETLE(dst); break;
      case COND_GE:  SETGE(dst); break;
      default: assert(false); break;
      }
      MOVSX(dst, kReg32s[ir->dst->r]);  // Assume bool is 4 byte.
    }
    break;

  case IR_TEST:
    {
      assert(0 <= ir->size && ir->size < kPow2TableSize);
      int pow = kPow2Table[ir->size];
      assert(0 <= pow && pow < 4);
      const char **regs = kRegSizeTable[pow];
      const char *opr1 = regs[ir->opr1->r];
      TEST(opr1, opr1);
    }
    break;

  case IR_JMP:
    switch (ir->jmp.cond) {
    case COND_ANY:  JMP(fmt_name(ir->jmp.bb->label)); break;
    case COND_EQ:   JE(fmt_name(ir->jmp.bb->label)); break;
    case COND_NE:   JNE(fmt_name(ir->jmp.bb->label)); break;
    case COND_LT:   JL(fmt_name(ir->jmp.bb->label)); break;
    case COND_GT:   JG(fmt_name(ir->jmp.bb->label)); break;
    case COND_LE:   JLE(fmt_name(ir->jmp.bb->label)); break;
    case COND_GE:   JGE(fmt_name(ir->jmp.bb->label)); break;
    default:  assert(false); break;
    }
    break;

  case IR_PRECALL:
    {
      // Caller save.
      PUSH(R10); PUSH_STACK_POS();
      PUSH(R11); PUSH_STACK_POS();

      int stack_args = MAX(ir->call.arg_count - MAX_REG_ARGS, 0);
      bool align_stack = ((stackpos + stack_args * WORD_SIZE) & 15) != 0;
      if (align_stack) {
        SUB(IM(8), RSP);
        stackpos += 8;
      }
      *ir->call.stack_aligned = align_stack;
    }
    break;

  case IR_PUSHARG:
    PUSH(kReg64s[ir->opr1->r]); PUSH_STACK_POS();
    break;

  case IR_CALL:
    {
      static const char *kArgReg64s[] = {RDI, RSI, RDX, RCX, R8, R9};

      // Pop register arguments.
      int reg_args = MIN((int)ir->call.arg_count, MAX_REG_ARGS);
      for (int i = 0; i < reg_args; ++i) {
        POP(kArgReg64s[i]); POP_STACK_POS();
      }

      if (ir->call.label != NULL) {
        const char *label = fmt_name(ir->call.label);
        if (ir->call.global)
          CALL(MANGLE(label));
        else
          CALL(label);
      } else {
        CALL(fmt("*%s", kReg64s[ir->opr1->r]));
      }

      int stack_args = MAX(ir->call.arg_count - MAX_REG_ARGS, 0);
      bool align_stack = *ir->call.stack_aligned;
      if (stack_args > 0 || align_stack) {
        int add = stack_args * WORD_SIZE + (align_stack ? 8 : 0);
        ADD(IM(add), RSP);
        stackpos -= add;
      }

      // Resore caller save registers.
      POP(R11); POP_STACK_POS();
      POP(R10); POP_STACK_POS();

      assert(0 <= ir->size && ir->size < kPow2TableSize);
      int pow = kPow2Table[ir->size];
      assert(0 <= pow && pow < 4);
      const char **regs = kRegSizeTable[pow];
      MOV(kRegATable[pow], regs[ir->dst->r]);
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
    if (ir->size <= ir->cast.srcsize) {
      assert(0 <= ir->size && ir->size < kPow2TableSize);
      int pow = kPow2Table[ir->size];
      assert(0 <= pow && pow < 4);
      const char **regs = kRegSizeTable[pow];
      MOV(regs[ir->opr1->r], regs[ir->dst->r]);
    } else {
      assert(0 <= ir->cast.srcsize && ir->cast.srcsize < kPow2TableSize);
      int pows = kPow2Table[ir->cast.srcsize];
      assert(0 <= ir->size && ir->size < kPow2TableSize);
      int powd = kPow2Table[ir->size];
      assert(0 <= pows && pows < 4);
      assert(0 <= powd && powd < 4);
      if (ir->cast.is_unsigned) {
        if (pows == 2 && powd == 3) {
          // MOVZX %64bit, %32bit doesn't exist!
          MOV(kRegSizeTable[pows][ir->opr1->r], kRegSizeTable[pows][ir->dst->r]);
        } else {
          MOVZX(kRegSizeTable[pows][ir->opr1->r], kRegSizeTable[powd][ir->dst->r]);
        }
      } else {
        MOVSX(kRegSizeTable[pows][ir->opr1->r], kRegSizeTable[powd][ir->dst->r]);
      }
    }
    break;

  case IR_MOV:
    if (ir->opr1->r != ir->dst->r) {
      assert(0 <= ir->size && ir->size < kPow2TableSize);
      int pow = kPow2Table[ir->size];
      assert(0 <= pow && pow < 4);
      const char **regs = kRegSizeTable[pow];
      MOV(regs[ir->opr1->r], regs[ir->dst->r]);
    }
    break;

  case IR_CLEAR:
    {
      const char *loop = fmt_name(alloc_label());
      MOV(kReg64s[ir->opr1->r], RSI);
      MOV(IM(ir->size), EDI);
      XOR(AL, AL);
      EMIT_LABEL(loop);
      MOV(AL, INDIRECT(RSI));
      INC(RSI);
      DEC(EDI);
      JNE(loop);
    }
    break;

  case IR_RESULT:
    {
      assert(0 <= ir->size && ir->size < kPow2TableSize);
      int pow = kPow2Table[ir->size];
      assert(0 <= pow && pow < 4);
      const char **regs = kRegSizeTable[pow];
      MOV(regs[ir->opr1->r], kRegATable[pow]);
    }
    break;

  case IR_ASM:
    EMIT_ASM0(ir->asm_.str);
    break;

  case IR_LOAD_SPILLED:
    {
      assert(0 <= ir->size && ir->size < kPow2TableSize);
      int pow = kPow2Table[ir->size];
      assert(0 <= pow && pow < 4);
      const char **regs = kRegSizeTable[pow];
      MOV(OFFSET_INDIRECT(ir->value, RBP), regs[SPILLED_REG_NO]);
    }
    break;

  case IR_STORE_SPILLED:
    {
      assert(0 <= ir->size && ir->size < kPow2TableSize);
      int pow = kPow2Table[ir->size];
      assert(0 <= pow && pow < 4);
      const char **regs = kRegSizeTable[pow];
      MOV(regs[SPILLED_REG_NO], OFFSET_INDIRECT(ir->value, RBP));
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
  bb->in_regs = NULL;
  bb->out_regs = NULL;
  bb->assigned_regs = NULL;
  return bb;
}

BB *bb_split(BB *bb) {
  BB *cc = new_bb();
  cc->next = bb->next;
  bb->next = cc;
  return cc;
}

void bb_insert(BB *bb, BB *cc) {
  cc->next = bb->next;
  bb->next = cc;
}

//

BBContainer *new_func_blocks(void) {
  BBContainer *bbcon = malloc(sizeof(*bbcon));
  bbcon->bbs = new_vector();
  return bbcon;
}

static IR *is_last_jmp(BB *bb) {
  int len;
  IR *ir;
  if ((len = bb->irs->len) > 0 &&
      (ir = bb->irs->data[len - 1])->kind == IR_JMP)
    return ir;
  return NULL;
}

static IR *is_last_any_jmp(BB *bb) {
  IR *ir = is_last_jmp(bb);
  return ir != NULL && ir->jmp.cond == COND_ANY ? ir : NULL;
}

static void replace_jmp_destination(BBContainer *bbcon, BB *src, BB *dst) {
  Vector *bbs = bbcon->bbs;
  for (int j = 0; j < bbs->len; ++j) {
    BB *bb = bbs->data[j];
    if (bb == src)
      continue;

    IR *ir = is_last_jmp(bb);
    if (ir != NULL && ir->jmp.bb == src)
      ir->jmp.bb = dst;
  }
}

void remove_unnecessary_bb(BBContainer *bbcon) {
  Vector *bbs = bbcon->bbs;
  for (;;) {
    bool again = false;
    for (int i = 0; i < bbs->len - 1; ++i) {  // Make last one keeps alive.
      BB *bb = bbs->data[i];
      IR *ir;
      if (bb->irs->len == 0) {  // Empty BB.
        replace_jmp_destination(bbcon, bb, bb->next);
      } else if (bb->irs->len == 1 && (ir = is_last_any_jmp(bb)) != NULL) {  // jmp only.
        replace_jmp_destination(bbcon, bb, ir->jmp.bb);
        if (i == 0)
          continue;
        BB *pbb = bbs->data[i - 1];
        if (!is_last_jmp(pbb))
          continue;
        if (!is_last_any_jmp(pbb)) {  // Fallthrough pass exists.
          IR *ir0 = pbb->irs->data[pbb->irs->len - 1];
          if (ir0->jmp.bb != bb->next)  // Non skip jmp: Keep bb connection.
            continue;
          // Invert prev jmp condition and change jmp destination.
          ir0->jmp.cond = invert_cond(ir0->jmp.cond);
          ir0->jmp.bb = ir->jmp.bb;
        }
      } else {
        continue;
      }

      if (i > 0) {
        BB *pbb = bbs->data[i - 1];
        pbb->next = bb->next;
      }

      vec_remove_at(bbs, i);
      --i;
      again = true;
    }
    if (!again)
      break;
  }

  // Remove jmp to next instruction.
  for (int i = 0; i < bbs->len - 1; ++i) {  // Make last one keeps alive.
    BB *bb = bbs->data[i];
    IR *ir = is_last_any_jmp(bb);
    if (ir != NULL && ir->jmp.bb == bb->next)
      vec_pop(bb->irs);
  }
}

void push_callee_save_regs(Function *func) {
  short used = func->ra->used_reg_bits;
  for (int i = 0; i < CALLEE_SAVE_REG_COUNT; ++i) {
    if (used & kCalleeSaveRegs[i].bit) {
      PUSH(kCalleeSaveRegs[i].reg); PUSH_STACK_POS();
    }
  }
}

void pop_callee_save_regs(Function *func) {
  short used = func->ra->used_reg_bits;
  for (int i = CALLEE_SAVE_REG_COUNT; --i >= 0; ) {
    if (used & kCalleeSaveRegs[i].bit) {
      POP(kCalleeSaveRegs[i].reg); POP_STACK_POS();
    }
  }
}

void emit_bb_irs(BBContainer *bbcon) {
  for (int i = 0; i < bbcon->bbs->len; ++i) {
    BB *bb = bbcon->bbs->data[i];
#ifndef NDEBUG
    // Check BB connection.
    if (i < bbcon->bbs->len - 1) {
      BB *nbb = bbcon->bbs->data[i + 1];
      UNUSED(nbb);
      assert(bb->next == nbb);
    } else {
      assert(bb->next == NULL);
    }
#endif

    EMIT_LABEL(fmt_name(bb->label));
    for (int j = 0; j < bb->irs->len; ++j) {
      IR *ir = bb->irs->data[j];
      ir_out(ir);
    }
  }
}

#if !defined(SELF_HOSTING)
static void dump_ir(FILE *fp, IR *ir) {
  static char *kSize[] = {"0", "b", "w", "3", "d", "5", "6", "7", ""};
  static char *kCond[] = {"MP", "EQ", "NE", "LT", "LE", "GE", "GT"};

  int dst = ir->dst != NULL ? ir->dst->r : -1;
  int opr1 = ir->opr1 != NULL ? ir->opr1->r : -1;
  int opr2 = ir->opr2 != NULL ? ir->opr2->r : -1;

  switch (ir->kind) {
  case IR_IMM:    fprintf(fp, "\tIMM\tR%d%s = %"PRIdPTR"\n", dst, kSize[ir->size], ir->value); break;
  case IR_BOFS:   fprintf(fp, "\tBOFS\tR%d = &[rbp %c %d]\n", dst, ir->opr1->offset > 0 ? '+' : '-', ir->opr1->offset > 0 ? ir->opr1->offset : -ir->opr1->offset); break;
  case IR_IOFS:   fprintf(fp, "\tIOFS\tR%d = &%.*s\n", dst, ir->iofs.label->bytes, ir->iofs.label->chars); break;
  case IR_LOAD:   fprintf(fp, "\tLOAD\tR%d%s = (R%d)\n", dst, kSize[ir->size], opr1); break;
  case IR_STORE:  fprintf(fp, "\tSTORE\t(R%d) = R%d%s\n", opr2, opr1, kSize[ir->size]); break;
  case IR_MEMCPY: fprintf(fp, "\tMEMCPY(dst=R%d, src=R%d, size=%d)\n", opr2, opr1, ir->size); break;
  case IR_ADD:    fprintf(fp, "\tADD\tR%d%s = R%d%s + R%d%s\n", dst, kSize[ir->size], opr1, kSize[ir->size], opr2, kSize[ir->size]); break;
  case IR_SUB:    fprintf(fp, "\tSUB\tR%d%s = R%d%s - R%d%s\n", dst, kSize[ir->size], opr1, kSize[ir->size], opr2, kSize[ir->size]); break;
  case IR_MUL:    fprintf(fp, "\tMUL\tR%d%s = R%d%s * R%d%s\n", dst, kSize[ir->size], opr1, kSize[ir->size], opr2, kSize[ir->size]); break;
  case IR_DIV:    fprintf(fp, "\tDIV\tR%d%s = R%d%s / R%d%s\n", dst, kSize[ir->size], opr1, kSize[ir->size], opr2, kSize[ir->size]); break;
  case IR_DIVU:   fprintf(fp, "\tDIVU\tR%d%s = R%d%s / R%d%s\n", dst, kSize[ir->size], opr1, kSize[ir->size], opr2, kSize[ir->size]); break;
  case IR_MOD:    fprintf(fp, "\tMOD\tR%d%s = R%d%s %% R%d%s\n", dst, kSize[ir->size], opr1, kSize[ir->size], opr2, kSize[ir->size]); break;
  case IR_MODU:   fprintf(fp, "\tMODU\tR%d%s = R%d%s %% R%d%s\n", dst, kSize[ir->size], opr1, kSize[ir->size], opr2, kSize[ir->size]); break;
  case IR_BITAND: fprintf(fp, "\tBITAND\tR%d%s = R%d%s & R%d%s\n", dst, kSize[ir->size], opr1, kSize[ir->size], opr2, kSize[ir->size]); break;
  case IR_BITOR:  fprintf(fp, "\tBITOR\tR%d%s = R%d%s | R%d%s\n", dst, kSize[ir->size], opr1, kSize[ir->size], opr2, kSize[ir->size]); break;
  case IR_BITXOR: fprintf(fp, "\tBITXOR\tR%d%s = R%d%s ^ R%d%s\n", dst, kSize[ir->size], opr1, kSize[ir->size], opr2, kSize[ir->size]); break;
  case IR_LSHIFT: fprintf(fp, "\tLSHIFT\tR%d%s = R%d%s << R%d%s\n", dst, kSize[ir->size], opr1, kSize[ir->size], opr2, kSize[ir->size]); break;
  case IR_RSHIFT: fprintf(fp, "\tRSHIFT\tR%d%s = R%d%s >> R%d%s\n", dst, kSize[ir->size], opr1, kSize[ir->size], opr2, kSize[ir->size]); break;
  case IR_CMP:    fprintf(fp, "\tCMP\tR%d%s - R%d%s\n", opr1, kSize[ir->size], opr2, kSize[ir->size]); break;
  case IR_INC:    fprintf(fp, "\tINC\t(R%d)%s += %"PRIdPTR"\n", opr1, kSize[ir->size], ir->value); break;
  case IR_DEC:    fprintf(fp, "\tDEC\t(R%d)%s -= %"PRIdPTR"\n", opr1, kSize[ir->size], ir->value); break;
  case IR_NEG:    fprintf(fp, "\tNEG\tR%d%s = -R%d%s\n", dst, kSize[ir->size], opr1, kSize[ir->size]); break;
  case IR_NOT:    fprintf(fp, "\tNOT\tR%d%s = !R%d%s\n", dst, kSize[ir->size], opr1, kSize[ir->size]); break;
  case IR_BITNOT: fprintf(fp, "\tBIT\tR%d%s = -R%d%s\n", dst, kSize[ir->size], opr1, kSize[ir->size]); break;
  case IR_SET:    fprintf(fp, "\tSET\tR%d%s = %s\n", dst, kSize[4], kCond[ir->set.cond]); break;
  case IR_TEST:   fprintf(fp, "\tTEST\tR%d%s\n", opr1, kSize[ir->size]); break;
  case IR_JMP:    fprintf(fp, "\tJ%s\t%.*s\n", kCond[ir->jmp.cond], ir->jmp.bb->label->bytes, ir->jmp.bb->label->chars); break;
  case IR_PRECALL: fprintf(fp, "\tPRECALL\n"); break;
  case IR_PUSHARG: fprintf(fp, "\tPUSHARG\tR%d\n", opr1); break;
  case IR_CALL:
    if (ir->call.label != NULL)
      fprintf(fp, "\tCALL\tR%d%s = call %.*s\n", dst, kSize[ir->size], ir->call.label->bytes, ir->call.label->chars);
    else
      fprintf(fp, "\tCALL\tR%d%s = *R%d\n", dst, kSize[ir->size], opr1);
    break;
  case IR_ADDSP:  fprintf(fp, "\tADDSP\t%"PRIdPTR"\n", ir->value); break;
  case IR_CAST:   fprintf(fp, "\tCAST\tR%d%s = R%d%s\n", dst, kSize[ir->size], opr1, kSize[ir->cast.srcsize]); break;
  case IR_MOV:    fprintf(fp, "\tMOV\tR%d%s = R%d%s\n", dst, kSize[ir->size], opr1, kSize[ir->size]); break;
  case IR_CLEAR:  fprintf(fp, "\tCLEAR\tR%d, %d\n", opr1, ir->size); break;
  case IR_RESULT: fprintf(fp, "\tRESULT\tR%d%s\n", opr1, kSize[ir->size]); break;
  case IR_ASM:    fprintf(fp, "\tASM \"%s\"\n", ir->asm_.str); break;
  case IR_LOAD_SPILLED:   fprintf(fp, "\tLOAD_SPILLED %d(%s)\n", (int)ir->value, kSize[ir->size]); break;
  case IR_STORE_SPILLED:  fprintf(fp, "\tSTORE_SPILLED %d(%s)\n", (int)ir->value, kSize[ir->size]); break;

  default: assert(false); break;
  }
}

void dump_func_ir(Function *func) {
  FILE *fp = stdout;

  if (func->scopes == NULL)  // Prototype definition
    return;

  BBContainer *bbcon = func->bbcon;
  assert(bbcon != NULL);

  fprintf(fp, "### %.*s\n\n", func->name->bytes, func->name->chars);

  fprintf(fp, "params and locals:\n");
  for (int i = 0; i < func->scopes->len; ++i) {
    Scope *scope = func->scopes->data[i];
    if (scope->vars == NULL)
      continue;
    for (int j = 0; j < scope->vars->len; ++j) {
      VarInfo *varinfo = scope->vars->data[j];
      if (varinfo->reg == NULL)
        continue;
      fprintf(fp, "  V%3d: %.*s\n", varinfo->reg->v, varinfo->name->bytes, varinfo->name->chars);
    }
  }

  RegAlloc *ra = func->ra;
  fprintf(fp, "VREG: #%d\n", ra->vregs->len);
  LiveInterval **sorted_intervals = func->ra->sorted_intervals;
  for (int i = 0; i < ra->vregs->len; ++i) {
    LiveInterval *li = sorted_intervals[i];
    VReg *vreg = ra->vregs->data[li->vreg];
    if (!li->spill) {
      fprintf(fp, "  V%3d: live %3d - %3d, => R%3d\n", li->vreg, li->start, li->end, li->rreg);
    } else {
      fprintf(fp, "  V%3d: live %3d - %3d (spilled, offset=%d)\n", li->vreg, li->start, li->end, vreg->offset);
    }
  }

  fprintf(fp, "BB: #%d\n", bbcon->bbs->len);
  for (int i = 0; i < bbcon->bbs->len; ++i) {
    BB *bb = bbcon->bbs->data[i];
    fprintf(fp, "// BB %d\n", i);
    fprintf(fp, "%.*s:\n", bb->label->bytes, bb->label->chars);
    for (int j = 0; j < bb->irs->len; ++j) {
      IR *ir = bb->irs->data[j];
      dump_ir(fp, ir);
    }
  }
  fprintf(fp, "\n");
}
#endif
