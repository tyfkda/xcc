#include "ir.h"

#include <assert.h>
#include <inttypes.h>
#include <stdlib.h>
#include <string.h>

#include "codegen.h"
#include "parser.h"
#include "sema.h"  // curfunc
#include "type.h"
#include "util.h"
#include "var.h"
#include "x86_64.h"

#define REG_COUNT  (7)

// Virtual register

VReg *new_vreg(int vreg_no) {
  VReg *vreg = malloc(sizeof(*vreg));
  vreg->v = vreg_no;
  vreg->r = -1;
  return vreg;
}

// Register allocator

typedef struct RegAlloc {
  int regno;
  Vector *vregs;
  bool used[REG_COUNT];
} RegAlloc;

static char *kReg8s[] = {BL, R10B, R11B, R12B, R13B, R14B, R15B};
static char *kReg16s[] = {BX, R10W, R11W, R12W, R13W, R14W, R15W};
static char *kReg32s[] = {EBX, R10D, R11D, R12D, R13D, R14D, R15D};
static char *kReg64s[] = {RBX, R10, R11, R12, R13, R14, R15};

static void reg_alloc_clear(RegAlloc *ra) {
  ra->regno = 0;
  vec_clear(ra->vregs);
  memset(ra->used, 0, sizeof(ra->used));
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

void reg_alloc_map(RegAlloc *ra, VReg *vreg) {
  assert(vreg != NULL && vreg->v >= 0 && vreg->v < ra->regno);
  if (vreg->r != -1) {
    assert(ra->used[vreg->r]);
    return;
  }

  for (int i = 0; i < REG_COUNT; ++i) {
    if (ra->used[i])
      continue;
    ra->used[i] = true;
    vreg->r = i;
    return;
  }
  error("register exhausted");
}

static void reg_alloc_unreg(RegAlloc *ra, VReg *vreg) {
  assert(vreg->v >= 0 && vreg->v < ra->regno);
  int r = vreg->r;
  assert(ra->used[r]);
  ra->used[r] = false;
}

//
static RegAlloc *ra;

void init_reg_alloc(void) {
  if (ra == NULL)
    ra = new_reg_alloc();
  else
    reg_alloc_clear(ra);
}

void check_all_reg_unused(void) {
  for (int i = 0; i < REG_COUNT; ++i) {
    if (ra->used[i])
      error("reg %d is used", i);
  }
}

// Intermediate Representation

static IR *new_ir(enum IrType type) {
  IR *ir = malloc(sizeof(*ir));
  ir->type = type;
  ir->dst = ir->opr1 = ir->opr2 = NULL;
  vec_push(curbb->irs, ir);
  return ir;
}

VReg *new_ir_imm(intptr_t value, int size) {
  IR *ir = new_ir(IR_IMM);
  ir->value = value;
  ir->size = size;
  return ir->dst = reg_alloc_spawn(ra);
}

VReg *new_ir_bop(enum IrType type, VReg *opr1, VReg *opr2, int size) {
  IR *ir = new_ir(type);
  ir->opr1 = opr1;
  ir->opr2 = opr2;
  ir->size = size;
  return ir->dst = reg_alloc_spawn(ra);
}

VReg *new_ir_unary(enum IrType type, VReg *opr, int size) {
  IR *ir = new_ir(type);
  ir->opr1 = opr;
  ir->size = size;
  return ir->dst = reg_alloc_spawn(ra);
}

VReg *new_ir_bofs(int offset) {
  IR *ir = new_ir(IR_BOFS);
  ir->value = offset;
  return ir->dst = reg_alloc_spawn(ra);
}

VReg *new_ir_iofs(const char *label) {
  IR *ir = new_ir(IR_IOFS);
  ir->u.iofs.label = label;
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
  ir->dst = dst;
  ir->opr1 = src;
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

void new_ir_incdec(enum IrType type, VReg *reg, int size, intptr_t value) {
  IR *ir = new_ir(type);
  ir->opr1 = reg;
  ir->size = size;
  ir->value = value;
}

VReg *new_ir_set(enum ConditionType cond) {
  IR *ir = new_ir(IR_SET);
  ir->u.set.cond = cond;
  return ir->dst = reg_alloc_spawn(ra);
}

void new_ir_jmp(enum ConditionType cond, BB *bb) {
  IR *ir = new_ir(IR_JMP);
  ir->u.jmp.bb = bb;
  ir->u.jmp.cond = cond;
}

void new_ir_pusharg(VReg *vreg) {
  IR *ir = new_ir(IR_PUSHARG);
  ir->opr1 = vreg;
}

void new_ir_precall(int arg_count) {
  IR *ir = new_ir(IR_PRECALL);
  ir->u.call.arg_count = arg_count;
}

VReg *new_ir_call(const char *label, VReg *freg, int arg_count, int result_size) {
  IR *ir = new_ir(IR_CALL);
  ir->u.call.label = label;
  ir->opr1 = freg;
  ir->u.call.arg_count = arg_count;
  ir->size = result_size;
  return ir->dst = reg_alloc_spawn(ra);
}

void new_ir_addsp(int value) {
  IR *ir = new_ir(IR_ADDSP);
  ir->value = value;
}

void new_ir_cast(VReg *vreg, int dstsize, int srcsize) {
  IR *ir = new_ir(IR_CAST);
  ir->opr1 = vreg;
  ir->size = dstsize;
  ir->u.cast.srcsize = srcsize;
}

void new_ir_clear(VReg *reg, size_t size) {
  IR *ir = new_ir(IR_CLEAR);
  ir->size = size;
  ir->opr1 = reg;
}

void new_ir_copy(VReg *dst, VReg *src, int size) {
  IR *ir = new_ir(IR_COPY);
  ir->dst = dst;
  ir->opr1 = src;
  ir->size = size;
}

void new_ir_result(VReg *reg, int size) {
  IR *ir = new_ir(IR_RESULT);
  ir->opr1 = reg;
  ir->size = size;
}

void new_ir_asm(const char *asm_) {
  IR *ir = new_ir(IR_ASM);
  ir->u.asm_.str = asm_;
}

void new_ir_unreg(VReg *reg) {
  IR *ir = new_ir(IR_UNREG);
  ir->opr1 = reg;
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

void ir_alloc_reg(IR *ir) {
  if (ir->dst != NULL)
    reg_alloc_map(ra, ir->dst);
  if (ir->opr1 != NULL)
    reg_alloc_map(ra, ir->opr1);
  if (ir->opr2 != NULL)
    reg_alloc_map(ra, ir->opr2);

  switch (ir->type) {
  case IR_UNREG:
    reg_alloc_unreg(ra, ir->opr1);
    break;

  case IR_IMM:
  case IR_BOFS:
  case IR_IOFS:
  case IR_LOAD:
  case IR_STORE:
  case IR_MEMCPY:
  case IR_ADD:
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
  case IR_TEST:
  case IR_INC:
  case IR_DEC:
  case IR_NEG:
  case IR_NOT:
  case IR_COPY:
  case IR_SET:
  case IR_PRECALL:
  case IR_PUSHARG:
  case IR_CALL:
  case IR_CAST:
  case IR_CLEAR:
  case IR_RESULT:
  case IR_JMP:
  case IR_ADDSP:
  case IR_ASM:
    break;

  default:  assert(false); break;
  }
}

void ir_out(const IR *ir) {
  switch (ir->type) {
  case IR_IMM:
    {
      intptr_t value = ir->value;
      VReg *vreg = ir->dst;
      switch (ir->size) {
      case 1:
        if (value == 0)
          XOR(kReg8s[vreg->r], kReg8s[vreg->r]);
        else
          MOV(IM(value), kReg8s[vreg->r]);
        return;

      case 2:
        if (value == 0)
          XOR(kReg16s[vreg->r], kReg16s[vreg->r]);
        else
          MOV(IM(value), kReg16s[vreg->r]);
        return;

      case 4:
        if (value == 0)
          XOR(kReg32s[vreg->r], kReg32s[vreg->r]);
        else
          MOV(IM(value), kReg32s[vreg->r]);
        return;

      case 8:
        if (value == 0)
          XOR(kReg32s[vreg->r], kReg32s[vreg->r]);  // upper 32bit is also cleared.
        else
          MOV(IM(value), kReg64s[vreg->r]);
        return;

      default: assert(false); break;
      }
      break;
    }
    break;

  case IR_BOFS:
    LEA(OFFSET_INDIRECT(ir->value, RBP), kReg64s[ir->dst->r]);
    break;

  case IR_IOFS:
    LEA(LABEL_INDIRECT(ir->u.iofs.label, RIP), kReg64s[ir->dst->r]);
    break;

  case IR_LOAD:
    switch (ir->size) {
    case 1:  MOV(INDIRECT(kReg64s[ir->opr1->r]), kReg8s[ir->dst->r]); break;
    case 2:  MOV(INDIRECT(kReg64s[ir->opr1->r]), kReg16s[ir->dst->r]); break;
    case 4:  MOV(INDIRECT(kReg64s[ir->opr1->r]), kReg32s[ir->dst->r]); break;
    case 8:  MOV(INDIRECT(kReg64s[ir->opr1->r]), kReg64s[ir->dst->r]); break;
    default:  assert(false); break;
    }
    break;

  case IR_STORE:
    switch (ir->size) {
    case 1:  MOV(kReg8s[ir->opr1->r], INDIRECT(kReg64s[ir->opr2->r])); break;
    case 2:  MOV(kReg16s[ir->opr1->r], INDIRECT(kReg64s[ir->opr2->r])); break;
    case 4:  MOV(kReg32s[ir->opr1->r], INDIRECT(kReg64s[ir->opr2->r])); break;
    case 8:  MOV(kReg64s[ir->opr1->r], INDIRECT(kReg64s[ir->opr2->r])); break;
    default:  assert(false); break;
    }
    break;

  case IR_MEMCPY:
    ir_memcpy(ir->dst->r, ir->opr1->r, ir->size);
    break;

  case IR_ADD:
    switch (ir->size) {
    case 1:  MOV(kReg8s[ir->opr1->r], kReg8s[ir->dst->r]); ADD(kReg8s[ir->opr2->r], kReg8s[ir->dst->r]); break;
    case 2:  MOV(kReg16s[ir->opr1->r], kReg16s[ir->dst->r]); ADD(kReg16s[ir->opr2->r], kReg16s[ir->dst->r]); break;
    case 4:  MOV(kReg32s[ir->opr1->r], kReg32s[ir->dst->r]); ADD(kReg32s[ir->opr2->r], kReg32s[ir->dst->r]); break;
    case 8:  MOV(kReg64s[ir->opr1->r], kReg64s[ir->dst->r]); ADD(kReg64s[ir->opr2->r], kReg64s[ir->dst->r]); break;
    default: assert(false); break;
    }
    break;

  case IR_SUB:
    switch (ir->size) {
    case 1:  MOV(kReg8s[ir->opr1->r], kReg8s[ir->dst->r]); SUB(kReg8s[ir->opr2->r], kReg8s[ir->dst->r]); break;
    case 2:  MOV(kReg16s[ir->opr1->r], kReg16s[ir->dst->r]); SUB(kReg16s[ir->opr2->r], kReg16s[ir->dst->r]); break;
    case 4:  MOV(kReg32s[ir->opr1->r], kReg32s[ir->dst->r]); SUB(kReg32s[ir->opr2->r], kReg32s[ir->dst->r]); break;
    case 8:  MOV(kReg64s[ir->opr1->r], kReg64s[ir->dst->r]); SUB(kReg64s[ir->opr2->r], kReg64s[ir->dst->r]); break;
    default: assert(false); break;
    }
    break;

  case IR_MUL:
    switch (ir->size) {
    case 1:  MOV(kReg8s[ir->opr1->r], AL); MUL(kReg8s[ir->opr2->r]); MOV(AL, kReg8s[ir->dst->r]); break;
    case 2:  MOV(kReg16s[ir->opr1->r], AX); MUL(kReg16s[ir->opr2->r]); MOV(AX, kReg16s[ir->dst->r]); break;
    case 4:  MOV(kReg32s[ir->opr1->r], EAX); MUL(kReg32s[ir->opr2->r]); MOV(EAX, kReg32s[ir->dst->r]); break;
    case 8:  MOV(kReg64s[ir->opr1->r], RAX); MUL(kReg64s[ir->opr2->r]); MOV(RAX, kReg64s[ir->dst->r]); break;
    default: assert(false); break;
    }
    break;

  case IR_DIV:
    switch (ir->size) {
    case 1:
      MOVSX(kReg8s[ir->opr1->r], AX);
      IDIV(kReg8s[ir->opr2->r]);
      MOV(AL, kReg8s[ir->dst->r]);
      break;
    case 2:
      MOV(kReg16s[ir->opr1->r], AX);
      CWTL();
      IDIV(kReg16s[ir->opr2->r]);
      MOV(AX, kReg16s[ir->dst->r]);
      break;
    case 4:
      MOV(kReg32s[ir->opr1->r], EAX);
      CLTD();
      IDIV(kReg32s[ir->opr2->r]);
      MOV(EAX, kReg32s[ir->dst->r]);
      break;
    case 8:
      MOV(kReg64s[ir->opr1->r], RAX);
      CQTO();
      IDIV(kReg64s[ir->opr2->r]);
      MOV(RAX, kReg64s[ir->dst->r]);
      break;
    default: assert(false); break;
    }
    break;

  case IR_MOD:
    switch (ir->size) {
    case 1:
      MOVSX(kReg8s[ir->opr1->r], AX);
      IDIV(kReg8s[ir->opr2->r]);
      MOV(AH, kReg8s[ir->dst->r]);
      break;
    case 2:
      MOV(kReg16s[ir->opr1->r], AX);
      CWTL();
      IDIV(kReg16s[ir->opr2->r]);
      MOV(DX, kReg16s[ir->dst->r]);
      break;
    case 4:
      MOV(kReg32s[ir->opr1->r], EAX);
      CLTD();
      IDIV(kReg32s[ir->opr2->r]);
      MOV(EDX, kReg32s[ir->dst->r]);
      break;
    case 8:
      MOV(kReg64s[ir->opr1->r], RAX);
      CQTO();
      IDIV(kReg64s[ir->opr2->r]);
      MOV(RDX, kReg64s[ir->dst->r]);
      break;
    default: assert(false); break;
    }
    break;

  case IR_BITAND:
    switch (ir->size) {
    case 1:  MOV(kReg8s[ir->opr1->r], kReg8s[ir->dst->r]); AND(kReg8s[ir->opr2->r], kReg8s[ir->dst->r]); break;
    case 2:  MOV(kReg16s[ir->opr1->r], kReg16s[ir->dst->r]); AND(kReg16s[ir->opr2->r], kReg16s[ir->dst->r]); break;
    case 4:  MOV(kReg32s[ir->opr1->r], kReg32s[ir->dst->r]); AND(kReg32s[ir->opr2->r], kReg32s[ir->dst->r]); break;
    case 8:  MOV(kReg64s[ir->opr1->r], kReg64s[ir->dst->r]); AND(kReg64s[ir->opr2->r], kReg64s[ir->dst->r]); break;
    default: assert(false); break;
    }
    break;

  case IR_BITOR:
    switch (ir->size) {
    case 1:  MOV(kReg8s[ir->opr1->r], kReg8s[ir->dst->r]); OR(kReg8s[ir->opr2->r], kReg8s[ir->dst->r]); break;
    case 2:  MOV(kReg16s[ir->opr1->r], kReg16s[ir->dst->r]); OR(kReg16s[ir->opr2->r], kReg16s[ir->dst->r]); break;
    case 4:  MOV(kReg32s[ir->opr1->r], kReg32s[ir->dst->r]); OR(kReg32s[ir->opr2->r], kReg32s[ir->dst->r]); break;
    case 8:  MOV(kReg64s[ir->opr1->r], kReg64s[ir->dst->r]); OR(kReg64s[ir->opr2->r], kReg64s[ir->dst->r]); break;
    default: assert(false); break;
    }
    break;

  case IR_BITXOR:
    switch (ir->size) {
    case 1:  MOV(kReg8s[ir->opr1->r], kReg8s[ir->dst->r]); XOR(kReg8s[ir->opr2->r], kReg8s[ir->dst->r]); break;
    case 2:  MOV(kReg16s[ir->opr1->r], kReg16s[ir->dst->r]); XOR(kReg16s[ir->opr2->r], kReg16s[ir->dst->r]); break;
    case 4:  MOV(kReg32s[ir->opr1->r], kReg32s[ir->dst->r]); XOR(kReg32s[ir->opr2->r], kReg32s[ir->dst->r]); break;
    case 8:  MOV(kReg64s[ir->opr1->r], kReg64s[ir->dst->r]); XOR(kReg64s[ir->opr2->r], kReg64s[ir->dst->r]); break;
    default: assert(false); break;
    }
    break;

  case IR_LSHIFT:
    MOV(kReg8s[ir->opr2->r], CL);
    switch (ir->size) {
    case 1:  MOV(kReg8s[ir->opr1->r], kReg8s[ir->dst->r]); SHL(CL, kReg8s[ir->dst->r]); break;
    case 2:  MOV(kReg16s[ir->opr1->r], kReg16s[ir->dst->r]); SHL(CL, kReg16s[ir->dst->r]); break;
    case 4:  MOV(kReg32s[ir->opr1->r], kReg32s[ir->dst->r]); SHL(CL, kReg32s[ir->dst->r]); break;
    case 8:  MOV(kReg64s[ir->opr1->r], kReg64s[ir->dst->r]); SHL(CL, kReg64s[ir->dst->r]); break;
    default: assert(false); break;
    }
    break;
  case IR_RSHIFT:
    MOV(kReg8s[ir->opr2->r], CL);
    switch (ir->size) {
    case 1:  MOV(kReg8s[ir->opr1->r], kReg8s[ir->dst->r]); SHR(CL, kReg8s[ir->dst->r]); break;
    case 2:  MOV(kReg16s[ir->opr1->r], kReg16s[ir->dst->r]); SHR(CL, kReg16s[ir->dst->r]); break;
    case 4:  MOV(kReg32s[ir->opr1->r], kReg32s[ir->dst->r]); SHR(CL, kReg32s[ir->dst->r]); break;
    case 8:  MOV(kReg64s[ir->opr1->r], kReg64s[ir->dst->r]); SHR(CL, kReg64s[ir->dst->r]); break;
    default: assert(false); break;
    }
    break;

  case IR_CMP:
    {
      switch (ir->size) {
      case 1:  CMP(kReg8s[ir->opr2->r], kReg8s[ir->opr1->r]); break;
      case 2:  CMP(kReg16s[ir->opr2->r], kReg16s[ir->opr1->r]); break;
      case 4:  CMP(kReg32s[ir->opr2->r], kReg32s[ir->opr1->r]); break;
      case 8:  CMP(kReg64s[ir->opr2->r], kReg64s[ir->opr1->r]); break;
      default: assert(false); break;
      }
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
    switch (ir->size) {
    case 1:  MOV(kReg8s[ir->opr1->r], kReg8s[ir->dst->r]); NEG(kReg8s[ir->dst->r]); break;
    case 2:  MOV(kReg16s[ir->opr1->r], kReg16s[ir->dst->r]); NEG(kReg16s[ir->dst->r]); break;
    case 4:  MOV(kReg32s[ir->opr1->r], kReg32s[ir->dst->r]); NEG(kReg32s[ir->dst->r]); break;
    case 8:  MOV(kReg64s[ir->opr1->r], kReg64s[ir->dst->r]); NEG(kReg64s[ir->dst->r]); break;
    default:  assert(false); break;
    }
    break;

  case IR_NOT:
    switch (ir->size) {
    case 1:  TEST(kReg8s[ir->opr1->r], kReg8s[ir->opr1->r]); break;
    case 2:  TEST(kReg16s[ir->opr1->r], kReg16s[ir->opr1->r]); break;
    case 4:  TEST(kReg32s[ir->opr1->r], kReg32s[ir->opr1->r]); break;
    case 8:  TEST(kReg64s[ir->opr1->r], kReg64s[ir->opr1->r]); break;
    default:  assert(false); break;
    }
    SETE(kReg8s[ir->dst->r]);
    MOVSX(kReg8s[ir->dst->r], kReg32s[ir->dst->r]);
    break;

  case IR_SET:
    {
      const char *dst = kReg8s[ir->dst->r];
      switch (ir->u.set.cond) {
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
      switch (ir->size) {
      case 1:  TEST(kReg8s[ir->opr1->r], kReg8s[ir->opr1->r]); break;
      case 2:  TEST(kReg16s[ir->opr1->r], kReg16s[ir->opr1->r]); break;
      case 4:  TEST(kReg32s[ir->opr1->r], kReg32s[ir->opr1->r]); break;
      case 8:  TEST(kReg64s[ir->opr1->r], kReg64s[ir->opr1->r]); break;
      default: assert(false); break;
      }
    }
    break;

  case IR_JMP:
    switch (ir->u.jmp.cond) {
    case COND_ANY:  JMP(ir->u.jmp.bb->label); break;
    case COND_EQ:   JE(ir->u.jmp.bb->label); break;
    case COND_NE:   JNE(ir->u.jmp.bb->label); break;
    case COND_LT:   JL(ir->u.jmp.bb->label); break;
    case COND_GT:   JG(ir->u.jmp.bb->label); break;
    case COND_LE:   JLE(ir->u.jmp.bb->label); break;
    case COND_GE:   JGE(ir->u.jmp.bb->label); break;
    default:  assert(false); break;
    }
    break;

  case IR_PRECALL:
    // Caller save.
    PUSH(R10); PUSH_STACK_POS();
    PUSH(R11); PUSH_STACK_POS();
    break;

  case IR_PUSHARG:
    PUSH(kReg64s[ir->opr1->r]); PUSH_STACK_POS();
    break;

  case IR_CALL:
    {
      static const char *kArgReg64s[] = {RDI, RSI, RDX, RCX, R8, R9};

      // Pop register arguments.
      int reg_args = MIN((int)ir->u.call.arg_count, MAX_REG_ARGS);
      for (int i = 0; i < reg_args; ++i) {
        POP(kArgReg64s[i]); POP_STACK_POS();
      }

      if (ir->u.call.label != NULL)
        CALL(ir->u.call.label);
      else
        CALL(fmt("*%s", kReg64s[ir->opr1->r]));

      // Resore caller save registers.
      POP(R11); POP_STACK_POS();
      POP(R10); POP_STACK_POS();

      switch (ir->size) {
      case 1:  MOV(AL, kReg8s[ir->dst->r]); break;
      case 2:  MOV(AX, kReg16s[ir->dst->r]); break;
      case 4:  MOV(EAX, kReg32s[ir->dst->r]); break;
      case 8:  MOV(RAX, kReg64s[ir->dst->r]); break;
      default: assert(false); break;
      }
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
        case 1:  MOVSX(kReg8s[ir->opr1->r], kReg16s[ir->opr1->r]); break;
        default:  assert(false); break;
        }
        break;
      case 4:
        switch (ir->u.cast.srcsize) {
        case 1:  MOVSX(kReg8s[ir->opr1->r], kReg32s[ir->opr1->r]); break;
        case 2:  MOVSX(kReg16s[ir->opr1->r], kReg32s[ir->opr1->r]); break;
        default:  assert(false); break;
        }
        break;
      case 8:
        switch (ir->u.cast.srcsize) {
        case 1:  MOVSX(kReg8s[ir->opr1->r], kReg64s[ir->opr1->r]); break;
        case 2:  MOVSX(kReg16s[ir->opr1->r], kReg64s[ir->opr1->r]); break;
        case 4:  MOVSX(kReg32s[ir->opr1->r], kReg64s[ir->opr1->r]); break;
        default:
          assert(false); break;
        }
        break;
      default:  assert(false); break;
      }
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

  case IR_COPY:
    switch (ir->size) {
    case 1:  MOV(kReg8s[ir->opr1->r], kReg8s[ir->dst->r]); break;
    case 2:  MOV(kReg16s[ir->opr1->r], kReg16s[ir->dst->r]); break;
    case 4:  MOV(kReg32s[ir->opr1->r], kReg32s[ir->dst->r]); break;
    case 8:  MOV(kReg64s[ir->opr1->r], kReg64s[ir->dst->r]); break;
    }
    break;

  case IR_RESULT:
    switch (ir->size) {
    case 1:  MOV(kReg8s[ir->opr1->r], AL); break;
    case 2:  MOV(kReg16s[ir->opr1->r], AX); break;
    case 4:  MOV(kReg32s[ir->opr1->r], EAX); break;
    case 8:  MOV(kReg64s[ir->opr1->r], RAX); break;
    default: assert(false); break;
    }
    break;

  case IR_UNREG:
    break;

  case IR_ASM:
    EMIT_ASM0(ir->u.asm_.str);
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
      (ir = bb->irs->data[len - 1])->type == IR_JMP &&
      ir->u.jmp.cond == COND_ANY;
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
        (ir = bb->irs->data[bb->irs->len - 1])->type == IR_JMP &&
        ir->u.jmp.bb == src)
      ir->u.jmp.bb = dst;
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
      replace_jmp_target(bbcon, bb, ir->u.jmp.bb);
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
    if (ir->u.jmp.bb == bb->next)
      vec_pop(bb->irs);
  }
}

static void alloc_regs(Vector *irs) {
  for (int i = 0, len = irs->len; i < len; ++i) {
    IR *ir = irs->data[i];
    ir_alloc_reg(ir);
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
    alloc_regs(bb->irs);

    emit_comment("  BB %d/%d", i, bbcon->bbs->len);
    EMIT_LABEL(bb->label);
    for (int j = 0; j < bb->irs->len; ++j) {
      IR *ir = bb->irs->data[j];
      ir_out(ir);
    }
  }
}
