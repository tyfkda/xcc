#include "ir.h"

#include <assert.h>
#include <inttypes.h>
#include <stdlib.h>
#include <string.h>

#include "codegen.h"
#include "type.h"
#include "util.h"
#include "var.h"
#include "x86_64.h"

#define REG_COUNT  (7 - 1)
#define SPILLED_REG_NO  (REG_COUNT)

// Virtual register

VReg *new_vreg(int vreg_no) {
  VReg *vreg = malloc(sizeof(*vreg));
  vreg->v = vreg_no;
  vreg->r = -1;
  vreg->type = NULL;
  vreg->offset = 0;
  return vreg;
}

void vreg_spill(VReg *vreg) {
  vreg->r = SPILLED_REG_NO;
}

// Register allocator

typedef struct RegAlloc {
  int regno;
  Vector *vregs;
} RegAlloc;

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

static void reg_alloc_clear(RegAlloc *ra) {
  ra->regno = 0;
  vec_clear(ra->vregs);
}

RegAlloc *new_reg_alloc(void) {
  RegAlloc *ra = malloc(sizeof(*ra));
  ra->vregs = new_vector();
  reg_alloc_clear(ra);
  return ra;
}

VReg *reg_alloc_spawn(RegAlloc *ra) {
  VReg *vreg = new_vreg(ra->regno++);
  vec_push(ra->vregs, vreg);
  return vreg;
}

//
static RegAlloc *ra;

void init_reg_alloc(void) {
  if (ra == NULL)
    ra = new_reg_alloc();
  else
    reg_alloc_clear(ra);
}

VReg *add_new_reg(void) {
  return reg_alloc_spawn(ra);
}

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

VReg *new_ir_imm(intptr_t value, int size) {
  IR *ir = new_ir(IR_IMM);
  ir->value = value;
  ir->size = size;
  return ir->dst = reg_alloc_spawn(ra);
}

VReg *new_ir_bop(enum IrKind kind, VReg *opr1, VReg *opr2, int size) {
  IR *ir = new_ir(kind);
  ir->opr1 = opr1;
  ir->opr2 = opr2;
  ir->size = size;
  return ir->dst = reg_alloc_spawn(ra);
}

VReg *new_ir_unary(enum IrKind kind, VReg *opr, int size) {
  IR *ir = new_ir(kind);
  ir->opr1 = opr;
  ir->size = size;
  return ir->dst = reg_alloc_spawn(ra);
}

VReg *new_ir_bofs(VReg *src) {
  IR *ir = new_ir(IR_BOFS);
  ir->opr1 = src;
  ir->size = WORD_SIZE;
  return ir->dst = reg_alloc_spawn(ra);
}

VReg *new_ir_iofs(const char *label) {
  IR *ir = new_ir(IR_IOFS);
  ir->iofs.label = label;
  ir->size = WORD_SIZE;
  return ir->dst = reg_alloc_spawn(ra);
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
  return ir->dst = reg_alloc_spawn(ra);
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

VReg *new_ir_call(const char *label, VReg *freg, int arg_count, int result_size, bool *stack_aligned) {
  IR *ir = new_ir(IR_CALL);
  ir->call.label = label;
  ir->opr1 = freg;
  ir->call.stack_aligned = stack_aligned;
  ir->call.arg_count = arg_count;
  ir->size = result_size;
  return ir->dst = reg_alloc_spawn(ra);
}

void new_ir_addsp(int value) {
  IR *ir = new_ir(IR_ADDSP);
  ir->value = value;
}

VReg *new_ir_cast(VReg *vreg, int dstsize, int srcsize) {
  IR *ir = new_ir(IR_CAST);
  ir->opr1 = vreg;
  ir->size = dstsize;
  ir->cast.srcsize = srcsize;
  return ir->dst = reg_alloc_spawn(ra);
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

static IR *new_ir_load_spilled(int offset, int size) {
  IR *ir = new_ir(IR_LOAD_SPILLED);
  ir->value = offset;
  ir->size = size;
  return ir;
}

static IR *new_ir_store_spilled(int offset, int size) {
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
      const char * label = alloc_label();
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
    LEA(LABEL_INDIRECT(ir->iofs.label, RIP), kReg64s[ir->dst->r]);
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
    if (ir->size == 1) {
      MOVSX(kReg8s[ir->opr1->r], AX);
      IDIV(kReg8s[ir->opr2->r]);
      MOV(AL, kReg8s[ir->dst->r]);
    } else {
      assert(0 <= ir->size && ir->size < kPow2TableSize);
      int pow = kPow2Table[ir->size];
      assert(0 <= pow && pow < 4);
      const char **regs = kRegSizeTable[pow];
      const char *a = kRegATable[pow];
      MOV(regs[ir->opr1->r], a);
      switch (pow) {
      case 1:  CWTL(); break;
      case 2:  CLTD(); break;
      case 3:  CQTO(); break;
      default: assert(false); break;
      }
      IDIV(regs[ir->opr2->r]);
      MOV(a, regs[ir->dst->r]);
    }
    break;

  case IR_MOD:
    if (ir->size == 1) {
      MOVSX(kReg8s[ir->opr1->r], AX);
      IDIV(kReg8s[ir->opr2->r]);
      MOV(AH, kReg8s[ir->dst->r]);
    } else {
      assert(0 <= ir->size && ir->size < kPow2TableSize);
      int pow = kPow2Table[ir->size];
      assert(0 <= pow && pow < 4);
      const char **regs = kRegSizeTable[pow];
      const char *a = kRegATable[pow];
      const char *d = kRegDTable[pow];
      MOV(regs[ir->opr1->r], a);
      switch (pow) {
      case 1:  CWTL(); break;
      case 2:  CLTD(); break;
      case 3:  CQTO(); break;
      default: assert(false); break;
      }
      IDIV(regs[ir->opr2->r]);
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
    case COND_ANY:  JMP(ir->jmp.bb->label); break;
    case COND_EQ:   JE(ir->jmp.bb->label); break;
    case COND_NE:   JNE(ir->jmp.bb->label); break;
    case COND_LT:   JL(ir->jmp.bb->label); break;
    case COND_GT:   JG(ir->jmp.bb->label); break;
    case COND_LE:   JLE(ir->jmp.bb->label); break;
    case COND_GE:   JGE(ir->jmp.bb->label); break;
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

      if (ir->call.label != NULL)
        CALL(ir->call.label);
      else
        CALL(fmt("*%s", kReg64s[ir->opr1->r]));

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
      MOVSX(kRegSizeTable[pows][ir->opr1->r], kRegSizeTable[powd][ir->dst->r]); break;
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
      const char *loop = alloc_label();
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

static bool is_last_any_jmp(BB *bb) {
  int len;
  IR *ir;
  return (len = bb->irs->len) > 0 &&
      (ir = bb->irs->data[len - 1])->kind == IR_JMP &&
      ir->jmp.cond == COND_ANY;
}

static void replace_jmp_target(BBContainer *bbcon, BB *src, BB *dst) {
  Vector *bbs = bbcon->bbs;
  for (int j = 0; j < bbs->len; ++j) {
    BB *bb = bbs->data[j];
    if (bb == src)
      continue;

    IR *ir;
    if (bb->next == src) {
      if (dst == src->next || is_last_any_jmp(bb))
        bb->next = src->next;
    }
    if (bb->irs->len > 0 &&
        (ir = bb->irs->data[bb->irs->len - 1])->kind == IR_JMP &&
        ir->jmp.bb == src)
      ir->jmp.bb = dst;
  }
}

void remove_unnecessary_bb(BBContainer *bbcon) {
  Vector *bbs = bbcon->bbs;
  for (int i = 0; i < bbs->len - 1; ++i) {  // Make last one keeps alive.
    BB *bb = bbs->data[i];
    if (bb->irs->len == 0) {  // Empty BB.
      replace_jmp_target(bbcon, bb, bb->next);
    } else if (is_last_any_jmp(bb) && bb->irs->len == 1) {  // jmp only.
      IR *ir = bb->irs->data[bb->irs->len - 1];
      replace_jmp_target(bbcon, bb, ir->jmp.bb);
      if (i == 0)
        continue;
      BB *pbb = bbs->data[i - 1];
      if (!is_last_any_jmp(pbb))  // Fallthrough pass exists: keep the bb.
        continue;
    } else {
      continue;
    }

    vec_remove_at(bbs, i);
    --i;
  }

  // Remove jmp to next instruction.
  for (int i = 0; i < bbs->len - 1; ++i) {  // Make last one keeps alive.
    BB *bb = bbs->data[i];
    if (!is_last_any_jmp(bb))
      continue;
    IR *ir = bb->irs->data[bb->irs->len - 1];
    if (ir->jmp.bb == bb->next)
      vec_pop(bb->irs);
  }
}

// Rewrite `A = B op C` to `A = B; A = A op C`.
static void three_to_two(BB *bb) {
  Vector *irs = bb->irs;
  for (int i = 0; i < irs->len; ++i) {
    IR *ir = bb->irs->data[i];

    switch (ir->kind) {
    case IR_ADD:  // binops
    case IR_SUB:
    case IR_MUL:
    case IR_DIV:
    case IR_MOD:
    case IR_BITAND:
    case IR_BITOR:
    case IR_BITXOR:
    case IR_LSHIFT:
    case IR_RSHIFT:
    case IR_NEG:  // unary ops
    case IR_BITNOT:
      {
        IR *ir2 = malloc(sizeof(*ir));
        ir2->kind = IR_MOV;
        ir2->dst = ir->dst;
        ir2->opr1 = ir->opr1;
        ir2->opr2 = NULL;
        ir2->size = ir->size;
        vec_insert(irs, i, ir2);

        ir->opr1 = ir->dst;
        ++i;
      }
      break;

    default:
      break;
    }
  }
  bb->irs = irs;
}

typedef struct {
  int vreg;
  int rreg;
  int start;
  int end;
  bool spill;
} LiveInterval;

static int insert_active(LiveInterval **active, int active_count, LiveInterval *li) {
  int j;
  for (j = 0; j < active_count; ++j) {
    LiveInterval *p = active[j];
    if (li->end < p->end)
      break;
  }
  if (j < active_count)
    memmove(&active[j + 1], &active[j], sizeof(LiveInterval*) * (active_count - j));
  active[j] = li;
  return j;
}

static void remove_active(LiveInterval **active, int active_count, int start, int n) {
  if (n <= 0)
    return;
  int tail = active_count - (start + n);
  assert(tail >= 0);

  if (tail > 0)
    memmove(&active[start], &active[start + n], sizeof(LiveInterval*) * tail);
}

static int sort_live_interval(LiveInterval **pa, LiveInterval **pb) {
  LiveInterval *a = *pa, *b = *pb;
  int d = a->start - b->start;
  if (d == 0)
    d = b->end - a->start;
  return d;
}

static void split_at_interval(LiveInterval **active, int active_count, LiveInterval *li) {
  assert(active_count > 0);
  LiveInterval *spill = active[active_count - 1];
  if (spill->end > li->end) {
    li->rreg = spill->rreg;
    spill->rreg = SPILLED_REG_NO;
    spill->spill = true;
    insert_active(active, active_count - 1, li);
  } else {
    li->rreg = SPILLED_REG_NO;
    li->spill = true;
  }
}

static void expire_old_intervals(LiveInterval **active, int *pactive_count, short *pusing_bits, int start) {
  int active_count = *pactive_count;
  int j;
  short using_bits = *pusing_bits;
  for (j = 0; j < active_count; ++j) {
    LiveInterval *li = active[j];
    if (li->end > start)
      break;
    using_bits &= ~((short)1 << li->rreg);
  }
  remove_active(active, active_count, 0, j);
  *pactive_count = active_count - j;
  *pusing_bits = using_bits;
}

static LiveInterval **check_live_interval(BBContainer *bbcon, int vreg_count, LiveInterval **pintervals) {
  LiveInterval *intervals = malloc(sizeof(LiveInterval) * vreg_count);
  for (int i = 0; i < vreg_count; ++i) {
    LiveInterval *li = &intervals[i];
    li->vreg = i;
    li->rreg = -1;
    li->start = li->end = -1;
    li->spill = false;
  }

  int nip = 0;
  for (int i = 0; i < bbcon->bbs->len; ++i) {
    BB *bb = bbcon->bbs->data[i];
    for (int j = 0; j < bb->irs->len; ++j, ++nip) {
      IR *ir = bb->irs->data[j];
      VReg *regs[] = {ir->dst, ir->opr1, ir->opr2};
      for (int k = 0; k < 3; ++k) {
        VReg *reg = regs[k];
        if (reg == NULL)
          continue;
        LiveInterval *li = &intervals[reg->v];
        if (li->start < 0)
          li->start = nip;
        if (li->end < nip)
          li->end = nip;
      }
    }

    for (int j = 0; j < bb->out_regs->len; ++j) {
      VReg *reg = bb->out_regs->data[j];
      LiveInterval *li = &intervals[reg->v];
      if (li->start < 0)
        li->start = nip;
      if (li->end < nip)
        li->end = nip;
    }
  }

  // Sort by start, end
  LiveInterval **sorted_intervals = malloc(sizeof(LiveInterval*) * vreg_count);
  for (int i = 0; i < vreg_count; ++i)
    sorted_intervals[i] = &intervals[i];
  myqsort(sorted_intervals, vreg_count, sizeof(LiveInterval*), (int (*)(const void*, const void*))sort_live_interval);

  *pintervals = intervals;
  return sorted_intervals;
}

static short linear_scan_register_allocation(LiveInterval **sorted_intervals, int vreg_count) {
  LiveInterval *active[REG_COUNT];
  int active_count = 0;
  short using_bits = 0;
  short used_bits = 0;

  for (int i = 0; i < vreg_count; ++i) {
    LiveInterval *li = sorted_intervals[i];
    if (li->spill)
      continue;
    expire_old_intervals(active, &active_count, &using_bits, li->start);
    if (active_count >= REG_COUNT) {
      split_at_interval(active, active_count, li);
    } else {
      int regno = -1;
      for (int j = 0; j < REG_COUNT; ++j) {
        if (!(using_bits & (1 << j))) {
          regno = j;
          break;
        }
      }
      assert(regno >= 0);
      li->rreg = regno;
      using_bits |= 1 << regno;

      insert_active(active, active_count, li);
      ++active_count;
    }
    used_bits |= using_bits;
  }
  return used_bits;
}

static int insert_load_store_spilled(BBContainer *bbcon, Vector *vregs) {
  int inserted = 0;
  for (int i = 0; i < bbcon->bbs->len; ++i) {
    BB *bb = bbcon->bbs->data[i];
    Vector *irs = bb->irs;
    for (int j = 0; j < irs->len; ++j) {
      IR *ir = irs->data[j];

      switch (ir->kind) {
      case IR_IMM:
      case IR_BOFS:
      case IR_IOFS:
      case IR_MOV:
      case IR_ADD:  // binops
      case IR_SUB:
      case IR_MUL:
      case IR_DIV:
      case IR_MOD:
      case IR_BITAND:
      case IR_BITOR:
      case IR_BITXOR:
      case IR_LSHIFT:
      case IR_RSHIFT:
      case IR_CMP:
      case IR_NEG:  // unary ops
      case IR_NOT:
      case IR_BITNOT:
      case IR_SET:
      case IR_TEST:
      case IR_PUSHARG:
      case IR_CALL:
      case IR_CAST:
      case IR_RESULT:
        if (ir->opr1 != NULL && ir->opr1->r == SPILLED_REG_NO) {
          vec_insert(irs, j,
                     new_ir_load_spilled(((VReg*)vregs->data[ir->opr1->v])->offset, ir->size));
          ++j;
          inserted |= 1;
        }

        if (ir->opr2 != NULL && ir->opr2->r == SPILLED_REG_NO) {
          vec_insert(irs, j,
                     new_ir_load_spilled(((VReg*)vregs->data[ir->opr2->v])->offset, ir->size));
          ++j;
          inserted |= 2;
        }

        if (ir->dst != NULL && ir->dst->r == SPILLED_REG_NO) {
          ++j;
          vec_insert(irs, j,
                     new_ir_store_spilled(((VReg*)vregs->data[ir->dst->v])->offset, ir->size));
          inserted |= 4;
        }
        break;

      case IR_LOAD:
      case IR_STORE:
      case IR_MEMCPY:
        if (ir->opr1 != NULL && ir->opr1->r == SPILLED_REG_NO) {
          vec_insert(irs, j,
                     new_ir_load_spilled(((VReg*)vregs->data[ir->opr1->v])->offset, WORD_SIZE));
          ++j;
          inserted |= 1;
        }

        if (ir->opr2 != NULL && ir->opr2->r == SPILLED_REG_NO) {
          vec_insert(irs, j,
                     new_ir_load_spilled(((VReg*)vregs->data[ir->opr2->v])->offset, WORD_SIZE));
          ++j;
          inserted |= 2;
        }

        if (ir->dst != NULL && ir->dst->r == SPILLED_REG_NO) {
          ++j;
          vec_insert(irs, j,
                     new_ir_store_spilled(((VReg*)vregs->data[ir->dst->v])->offset, ir->size));
          inserted |= 4;
        }
        break;

      default:
        break;
      }
    }
  }
  return inserted;
}

static void analyze_reg_flow(BBContainer *bbcon) {
  // Enumerate in and defined regsiters for each BB.
  for (int i = 0; i < bbcon->bbs->len; ++i) {
    BB *bb = bbcon->bbs->data[i];
    Vector *in_regs = new_vector();
    Vector *assigned_regs = new_vector();
    Vector *irs = bb->irs;
    for (int j = 0; j < irs->len; ++j) {
      IR *ir = irs->data[j];
      VReg *regs[] = {ir->opr1, ir->opr2};
      for (int k = 0; k < 2; ++k) {
        VReg *reg = regs[k];
        if (reg == NULL)
          continue;
        if (!vec_contains(in_regs, reg) &&
            !vec_contains(assigned_regs, reg))
          vec_push(in_regs, reg);
      }
      if (ir->dst != NULL && !vec_contains(assigned_regs, ir->dst))
        vec_push(assigned_regs, ir->dst);
    }

    bb->in_regs = in_regs;
    bb->out_regs = new_vector();
    bb->assigned_regs = assigned_regs;
  }

  // Propagate in regs to previous BB.
  bool cont;
  do {
    cont = false;
    for (int i = 0; i < bbcon->bbs->len; ++i) {
      BB *bb = bbcon->bbs->data[i];
      Vector *irs = bb->irs;

      BB *next_bbs[2];
      next_bbs[0] = bb->next;
      next_bbs[1] = NULL;

      if (irs->len > 0) {
        IR *ir = irs->data[irs->len - 1];
        if (ir->kind == IR_JMP) {
          next_bbs[1] = ir->jmp.bb;
          if (ir->jmp.cond == COND_ANY)
            next_bbs[0] = NULL;
        }
      }
      for (int j = 0; j < 2; ++j) {
        BB *next = next_bbs[j];
        if (next == NULL)
          continue;
        Vector *in_regs = next->in_regs;
        for (int k = 0; k < in_regs->len; ++k) {
          VReg *reg = in_regs->data[k];
          if (!vec_contains(bb->out_regs, reg))
            vec_push(bb->out_regs, reg);
          if (vec_contains(bb->assigned_regs, reg) ||
              vec_contains(bb->in_regs, reg))
            continue;
          vec_push(bb->in_regs, reg);
          cont = true;
        }
      }
    }
  } while (cont);
}

size_t alloc_real_registers(Function *func) {
  BBContainer *bbcon = func->bbcon;
  for (int i = 0; i < bbcon->bbs->len; ++i) {
    BB *bb = bbcon->bbs->data[i];
    three_to_two(bb);
  }

  analyze_reg_flow(bbcon);

  int vreg_count = ra->regno;
  LiveInterval *intervals;
  LiveInterval **sorted_intervals = check_live_interval(bbcon, ra->regno, &intervals);

  for (int i = 0; i < vreg_count; ++i) {
    LiveInterval *li = &intervals[i];
    VReg *vreg = ra->vregs->data[i];
    if (vreg->r == SPILLED_REG_NO) {
      li->spill = true;
      li->rreg = vreg->r;
    }
  }

  if (func->params != NULL) {
    // Make function parameters all spilled.
    for (int i = 0; i < func->params->len; ++i) {
      VarInfo *varinfo = func->params->data[i];

      LiveInterval *li = &intervals[varinfo->reg->v];
      li->start = 0;
      li->spill = true;
      li->rreg = SPILLED_REG_NO;
    }
  }

  func->used_reg_bits = linear_scan_register_allocation(sorted_intervals, vreg_count);

  // Map vreg to rreg.
  for (int i = 0; i < vreg_count; ++i) {
    VReg *vreg = ra->vregs->data[i];
    vreg->r = intervals[vreg->v].rreg;
  }

  // Allocated spilled virtual registers onto stack.
  size_t frame_size = 0;
  for (int i = 0; i < vreg_count; ++i) {
    LiveInterval *li = sorted_intervals[i];
    if (!li->spill)
      continue;
    VReg *vreg = ra->vregs->data[li->vreg];
    int size = WORD_SIZE, align = WORD_SIZE;
    if (vreg->type != NULL) {
      if (vreg->offset != 0) {  // Variadic function parameter or stack parameter.
        if (-vreg->offset > (int)frame_size)
          frame_size = -vreg->offset;
        continue;
      }

      const Type *type = vreg->type;
      size = type_size(type);
      align = align_size(type);
      if (size < 1)
        size = 1;
    }
    frame_size = ALIGN(frame_size + size, align);
    vreg->offset = -frame_size;
  }

  int inserted = insert_load_store_spilled(bbcon, ra->vregs);
  if (inserted != 0)
    func->used_reg_bits |= 1 << SPILLED_REG_NO;

  return frame_size;
}

void push_callee_save_regs(Function *func) {
  short used = func->used_reg_bits;
  for (int i = 0; i < CALLEE_SAVE_REG_COUNT; ++i) {
    if (used & kCalleeSaveRegs[i].bit) {
      PUSH(kCalleeSaveRegs[i].reg); PUSH_STACK_POS();
    }
  }
}

void pop_callee_save_regs(Function *func) {
  short used = func->used_reg_bits;
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
      assert(bb->next == nbb);
    } else {
      assert(bb->next == NULL);
    }
#endif

    EMIT_LABEL(bb->label);
    for (int j = 0; j < bb->irs->len; ++j) {
      IR *ir = bb->irs->data[j];
      ir_out(ir);
    }
  }
}

#if !defined(SELF_HOSTING)
void dump_ir(IR *ir) {
  static char *kSize[] = {"0", "b", "w", "3", "d", "5", "6", "7", ""};
  static char *kCond[] = {"MP", "EQ", "NE", "LT", "LE", "GE", "GT"};

  int dst = ir->dst != NULL ? ir->dst->r : -1;
  int opr1 = ir->opr1 != NULL ? ir->opr1->r : -1;
  int opr2 = ir->opr2 != NULL ? ir->opr2->r : -1;

  FILE *fp = stderr;
  switch (ir->kind) {
  case IR_IMM:    fprintf(fp, "\tIMM\tR%d%s = %"PRIdPTR"\n", dst, kSize[ir->size], ir->value); break;
  case IR_BOFS:   fprintf(fp, "\tBOFS\tR%d = &[rbp %c %d]\n", dst, ir->opr1->offset > 0 ? '+' : '-', ir->opr1->offset > 0 ? ir->opr1->offset : -ir->opr1->offset); break;
  case IR_IOFS:   fprintf(fp, "\tIOFS\tR%d = &%s\n", dst, ir->iofs.label); break;
  case IR_LOAD:   fprintf(fp, "\tLOAD\tR%d%s = (R%d)\n", dst, kSize[ir->size], opr1); break;
  case IR_STORE:  fprintf(fp, "\tSTORE\t(R%d) = R%d%s\n", opr2, opr1, kSize[ir->size]); break;
  case IR_MEMCPY: fprintf(fp, "\tMEMCPY(dst=R%d, src=R%d, size=%d)\n", opr2, opr1, ir->size); break;
  case IR_ADD:    fprintf(fp, "\tADD\tR%d%s = R%d%s + R%d%s\n", dst, kSize[ir->size], opr1, kSize[ir->size], opr2, kSize[ir->size]); break;
  case IR_SUB:    fprintf(fp, "\tSUB\tR%d%s = R%d%s - R%d%s\n", dst, kSize[ir->size], opr1, kSize[ir->size], opr2, kSize[ir->size]); break;
  case IR_MUL:    fprintf(fp, "\tMUL\tR%d%s = R%d%s * R%d%s\n", dst, kSize[ir->size], opr1, kSize[ir->size], opr2, kSize[ir->size]); break;
  case IR_DIV:    fprintf(fp, "\tDIV\tR%d%s = R%d%s / R%d%s\n", dst, kSize[ir->size], opr1, kSize[ir->size], opr2, kSize[ir->size]); break;
  case IR_MOD:    fprintf(fp, "\tMOD\tR%d%s = R%d%s %% R%d%s\n", dst, kSize[ir->size], opr1, kSize[ir->size], opr2, kSize[ir->size]); break;
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
  case IR_JMP:    fprintf(fp, "\tJ%s\t%s\n", kCond[ir->jmp.cond], ir->jmp.bb->label); break;
  case IR_PRECALL: fprintf(fp, "\tPRECALL\n"); break;
  case IR_PUSHARG: fprintf(fp, "\tPUSHARG\tR%d\n", opr1); break;
  case IR_CALL:
    if (ir->call.label != NULL)
      fprintf(fp, "\tCALL\tR%d%s = call %s\n", dst, kSize[ir->size], ir->call.label);
    else
      fprintf(fp, "\tCALL\tR%d%s = *R%d\n", dst, kSize[ir->size], opr1);
    break;
  case IR_ADDSP:  fprintf(fp, "\tADDSP\t%"PRIdPTR"\n", ir->value); break;
  case IR_CAST:   fprintf(fp, "\tCAST\tR%d%s = R%d%s\n", dst, kSize[ir->size], opr1, kSize[ir->cast.srcsize]); break;
  case IR_MOV:    fprintf(fp, "\tMOV\tR%d%s = R%d%s\n", dst, kSize[ir->size], opr1, kSize[ir->size]); break;
  case IR_CLEAR:  fprintf(fp, "\tCLEAR\tR%d, %d\n", opr1, ir->size); break;
  case IR_RESULT: fprintf(fp, "\tRESULT\tR%d%s\n", opr1, kSize[ir->size]); break;
  case IR_ASM:    fprintf(fp, "\tASM\n"); break;
  case IR_LOAD_SPILLED:   fprintf(fp, "\tLOAD_SPILLED %d(%s)\n", (int)ir->value, kSize[ir->size]); break;
  case IR_STORE_SPILLED:  fprintf(fp, "\tSTORE_SPILLED %d(%s)\n", (int)ir->value, kSize[ir->size]); break;

  default: assert(false); break;
  }
}
#endif
