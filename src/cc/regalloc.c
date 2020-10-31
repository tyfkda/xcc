#include "regalloc.h"

#include <assert.h>
#include <stdlib.h>  // malloc
#include <string.h>

#include "ast.h"
#include "codegen.h"  // WORD_SIZE
#include "ir.h"
#include "type.h"
#include "util.h"
#include "var.h"

#if (defined(__linux__) || defined(__APPLE__)) && !defined(__XCC) && !defined(__XV6)
#define USE_ALLOCA
#endif

#if defined(USE_ALLOCA)
#include <alloca.h>
#define ALLOCA(size)  alloca(size)
#else
#define ALLOCA(size)  malloc(size)
#endif

#define SPILLED_REG_NO(ra)  (ra->phys_max)

static void spill_vreg(RegAlloc *ra, VReg *vreg) {
  vreg->phys = SPILLED_REG_NO(ra);
}

// Register allocator

RegAlloc *new_reg_alloc(int phys_max) {
  RegAlloc *ra = malloc(sizeof(*ra));
  ra->vregs = new_vector();
  //ra->regno = 0;
  vec_clear(ra->vregs);
  ra->frame_size = 0;
  ra->phys_max = phys_max;
  ra->used_reg_bits = 0;
  return ra;
}

VReg *reg_alloc_spawn(RegAlloc *ra, const VRegType *vtype, int flag) {
  VReg *vreg = new_vreg(ra->vregs->len, vtype, flag);
  vec_push(ra->vregs, vreg);
  return vreg;
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
    case IR_DIVU:
    case IR_MOD:
    case IR_MODU:
    case IR_BITAND:
    case IR_BITOR:
    case IR_BITXOR:
    case IR_LSHIFT:
    case IR_RSHIFT:
    case IR_NEG:  // unary ops
    case IR_BITNOT:
      {
        assert(!(ir->dst->flag & VRF_CONST));
        IR *ir2 = malloc(sizeof(*ir2));
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

static int sort_live_interval(const void *pa, const void *pb) {
  LiveInterval *a = *(LiveInterval**)pa, *b = *(LiveInterval**)pb;
  int d = a->start - b->start;
  if (d == 0)
    d = b->end - a->start;
  return d;
}

static void split_at_interval(RegAlloc *ra, LiveInterval **active, int active_count,
                              LiveInterval *li) {
  assert(active_count > 0);
  LiveInterval *spill = active[active_count - 1];
  if (spill->end > li->end) {
    li->phys = spill->phys;
    spill->phys = ra->phys_max;
    spill->state = LI_SPILL;
    insert_active(active, active_count - 1, li);
  } else {
    li->phys = ra->phys_max;
    li->state = LI_SPILL;
  }
}

static void expire_old_intervals(LiveInterval **active, int *pactive_count, short *pusing_bits,
                                 int start) {
  int active_count = *pactive_count;
  int j;
  short using_bits = *pusing_bits;
  for (j = 0; j < active_count; ++j) {
    LiveInterval *li = active[j];
    if (li->end > start)
      break;
    using_bits &= ~((short)1 << li->phys);
  }
  remove_active(active, active_count, 0, j);
  *pactive_count = active_count - j;
  *pusing_bits = using_bits;
}

static LiveInterval **check_live_interval(BBContainer *bbcon, int vreg_count,
                                          LiveInterval **pintervals) {
  LiveInterval *intervals = malloc(sizeof(LiveInterval) * vreg_count);
  for (int i = 0; i < vreg_count; ++i) {
    LiveInterval *li = &intervals[i];
    li->virt = i;
    li->phys = -1;
    li->start = li->end = -1;
    li->state = LI_NORMAL;
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
        LiveInterval *li = &intervals[reg->virt];
        if (li->start < 0)
          li->start = nip;
        if (li->end < nip)
          li->end = nip;
      }
    }

    for (int j = 0; j < bb->out_regs->len; ++j) {
      VReg *reg = bb->out_regs->data[j];
      LiveInterval *li = &intervals[reg->virt];
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
  myqsort(sorted_intervals, vreg_count, sizeof(LiveInterval*), sort_live_interval);

  *pintervals = intervals;
  return sorted_intervals;
}

static short linear_scan_register_allocation(RegAlloc *ra, LiveInterval **sorted_intervals,
                                             int vreg_count) {
  const int PHYS_MAX = ra->phys_max;
  LiveInterval **active = ALLOCA(sizeof(LiveInterval*) * ra->phys_max);
  int active_count = 0;
  short using_bits = 0;
  short used_bits = 0;

  for (int i = 0; i < vreg_count; ++i) {
    LiveInterval *li = sorted_intervals[i];
    if (li->state != LI_NORMAL)
      continue;
    expire_old_intervals(active, &active_count, &using_bits, li->start);
    if (active_count >= PHYS_MAX) {
      split_at_interval(ra, active, active_count, li);
    } else {
      int regno = -1;
      for (int j = 0; j < PHYS_MAX; ++j) {
        if (!(using_bits & (1 << j))) {
          regno = j;
          break;
        }
      }
      assert(regno >= 0);
      li->phys = regno;
      using_bits |= 1 << regno;

      insert_active(active, active_count, li);
      ++active_count;
    }
    used_bits |= using_bits;
  }
  return used_bits;
}

static int insert_load_store_spilled(BBContainer *bbcon, Vector *vregs, const int spilled) {
  int inserted = 0;
  for (int i = 0; i < bbcon->bbs->len; ++i) {
    BB *bb = bbcon->bbs->data[i];
    Vector *irs = bb->irs;
    for (int j = 0; j < irs->len; ++j) {
      IR *ir = irs->data[j];

      int flag = 0;
      int load_size = 0;
      switch (ir->kind) {
      case IR_MOV:
      case IR_ADD:  // binops
      case IR_SUB:
      case IR_MUL:
      case IR_DIV:
      case IR_DIVU:
      case IR_MOD:
      case IR_MODU:
      case IR_PTRADD:
      case IR_BITAND:
      case IR_BITOR:
      case IR_BITXOR:
      case IR_LSHIFT:
      case IR_RSHIFT:
      case IR_CMP:
      case IR_NEG:  // unary ops
      case IR_NOT:
      case IR_BITNOT:
      case IR_COND:
      case IR_TEST:
      case IR_PUSHARG:
      case IR_RESULT:
        flag = 7;
        load_size = ir->size;
        break;

      case IR_CAST:
        flag = 5;
        load_size = ir->opr1->vtype->size;
        break;

      case IR_CALL:
        flag = 7;
        load_size = WORD_SIZE;
        break;

      case IR_LOAD:
      case IR_STORE:
      case IR_MEMCPY:
        flag = 7;
        load_size = WORD_SIZE;
        break;

      case IR_BOFS:
      case IR_IOFS:
      case IR_SOFS:
        flag = 4;
        break;

      default:
        continue;
      }

      assert(!((ir->opr1 != NULL && (flag & 1) != 0 && !(ir->opr1->flag & VRF_CONST) && ir->opr1->phys == spilled) &&
               (ir->opr2 != NULL && (flag & 2) != 0 && !(ir->opr2->flag & VRF_CONST) && ir->opr2->phys == spilled)));

      if (ir->opr1 != NULL && (flag & 1) != 0 &&
          !(ir->opr1->flag & VRF_CONST) && ir->opr1->phys == spilled) {
        vec_insert(irs, j,
                   new_ir_load_spilled(ir->opr1, ((VReg*)vregs->data[ir->opr1->virt])->offset, load_size));
        ++j;
        inserted |= 1;
      }

      if (ir->opr2 != NULL && (flag & 2) != 0 &&
          !(ir->opr2->flag & VRF_CONST) && ir->opr2->phys == spilled) {
        vec_insert(irs, j,
                   new_ir_load_spilled(ir->opr2, ((VReg*)vregs->data[ir->opr2->virt])->offset, load_size));
        ++j;
        inserted |= 2;
      }

      if (ir->dst != NULL && (flag & 4) != 0 &&
          !(ir->dst->flag & VRF_CONST) && ir->dst->phys == spilled) {
        assert(!(ir->dst->flag & VRF_CONST));
        ++j;
        vec_insert(irs, j,
                   new_ir_store_spilled(ir->dst, ((VReg*)vregs->data[ir->dst->virt])->offset, ir->size));
        inserted |= 4;
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
        if (reg == NULL || reg->flag & VRF_CONST)
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

void prepare_register_allocation(Function *func) {
  const int DEFAULT_OFFSET = WORD_SIZE * 2;  // Return address, saved base pointer.
  int reg_param_index = 0;
  int stack_argument_offset = 0;
  for (int i = 0; i < func->scopes->len; ++i) {
    Scope *scope = (Scope*)func->scopes->data[i];
    if (scope->vars == NULL)
      continue;

    for (int j = 0; j < scope->vars->len; ++j) {
      VarInfo *varinfo = scope->vars->data[j];
      VReg *vreg = varinfo->reg;
      if (vreg == NULL)
        continue;

      bool spill = false;
      if (vreg->flag & VRF_REF)
        spill = true;
      if (vreg->flag & VRF_PARAM) {
        spill = true;
        // stack parameters
        if (is_stack_param(varinfo->type)) {
          int offset = ALIGN(stack_argument_offset, align_size(varinfo->type));
          vreg->offset = offset + DEFAULT_OFFSET;
          stack_argument_offset = offset + type_size(varinfo->type);
        } else {
          if (func->type->func.vaargs) {  // Variadic function parameters.
            vreg->offset = (reg_param_index - MAX_REG_ARGS) * WORD_SIZE;
          }
          ++reg_param_index;
        }
      }

      switch (varinfo->type->kind) {
      case TY_ARRAY:
      case TY_STRUCT:
        // Make non-primitive variable spilled.
        spill = true;
        break;
      default:
        break;
      }
      if (i == 0 && vreg->param_index >= MAX_REG_ARGS) {
        // Function argument passed through the stack.
        spill = true;
        vreg->offset = (vreg->param_index - MAX_REG_ARGS + 2) * WORD_SIZE;
      }

      if (spill)
        spill_vreg(func->ra, vreg);
    }
  }
}

void alloc_physical_registers(RegAlloc *ra, BBContainer *bbcon) {
  for (int i = 0; i < bbcon->bbs->len; ++i) {
    BB *bb = bbcon->bbs->data[i];
    three_to_two(bb);
  }

  analyze_reg_flow(bbcon);

  int vreg_count = ra->vregs->len;
  LiveInterval *intervals;
  LiveInterval **sorted_intervals = check_live_interval(bbcon, vreg_count, &intervals);

  for (int i = 0; i < vreg_count; ++i) {
    LiveInterval *li = &intervals[i];
    VReg *vreg = ra->vregs->data[i];

    if (vreg->flag & VRF_CONST) {
      li->state = LI_CONST;
      continue;
    }

    // Force function parameter spilled.
    if (vreg->param_index >= 0) {
      spill_vreg(ra, vreg);
      li->start = 0;
      li->state = LI_SPILL;
    }
    if (vreg->phys >= ra->phys_max) {
      li->state = LI_SPILL;
      li->phys = vreg->phys;
    }
  }

  ra->used_reg_bits = linear_scan_register_allocation(ra, sorted_intervals, vreg_count);

  // Map vreg to preg.
  for (int i = 0; i < vreg_count; ++i) {
    VReg *vreg = ra->vregs->data[i];
    LiveInterval *li = &intervals[i];
    if (li->state != LI_CONST)
      vreg->phys = intervals[vreg->virt].phys;
  }

  // Allocated spilled virtual registers onto stack.
  int frame_size = 0;
  for (int i = 0; i < vreg_count; ++i) {
    LiveInterval *li = sorted_intervals[i];
    if (li->state != LI_SPILL)
      continue;
    VReg *vreg = ra->vregs->data[li->virt];
    if (vreg->offset != 0) {  // Variadic function parameter or stack parameter.
      if (-vreg->offset > frame_size)
        frame_size = -vreg->offset;
      continue;
    }

    int size, align;
    const VRegType *vtype = vreg->vtype;
    assert(vtype != NULL);
    size = vtype->size;
    align = vtype->align;
    if (size < 1)
      size = 1;

    frame_size = ALIGN(frame_size + size, align);
    vreg->offset = -frame_size;
  }

  int spilled = SPILLED_REG_NO(ra);
  int inserted = insert_load_store_spilled(bbcon, ra->vregs, spilled);
  if (inserted != 0)
    ra->used_reg_bits |= 1 << spilled;

  ra->sorted_intervals = sorted_intervals;

  ra->frame_size = ALIGN(frame_size, 8);
}
