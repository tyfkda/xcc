#include "regalloc.h"

#include <assert.h>
#include <limits.h>
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
#ifndef __NO_FLONUM
#define SPILLED_FREG_NO(ra)  (ra->fphys_max)
#endif

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
#ifndef __NO_FLONUM
  ra->fphys_max = 0;
#else
  assert(phys_max < (int)(sizeof(ra->used_reg_bits) * CHAR_BIT));
#endif
  ra->used_reg_bits = 0;
  return ra;
}

VReg *reg_alloc_spawn(RegAlloc *ra, const VRegType *vtype, int flag) {
  VReg *vreg = new_vreg(ra->vregs->len, vtype, flag);
  vec_push(ra->vregs, vreg);
  return vreg;
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

static void expire_old_intervals(
  LiveInterval **active, int *pactive_count, unsigned short *pusing_bits, int start
) {
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

static void set_inout_interval(Vector *regs, LiveInterval *intervals, int nip) {
  for (int j = 0; j < regs->len; ++j) {
    VReg *reg = regs->data[j];
    LiveInterval *li = &intervals[reg->virt];
    if (li->start < 0 || li->start > nip)
      li->start = nip;
    if (li->end < nip)
      li->end = nip;
  }
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

    set_inout_interval(bb->in_regs, intervals, nip);

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

    set_inout_interval(bb->out_regs, intervals, nip);
  }

  // Sort by start, end
  LiveInterval **sorted_intervals = malloc(sizeof(LiveInterval*) * vreg_count);
  for (int i = 0; i < vreg_count; ++i)
    sorted_intervals[i] = &intervals[i];
  QSORT(sorted_intervals, vreg_count, sizeof(LiveInterval*), sort_live_interval);

  *pintervals = intervals;
  return sorted_intervals;
}

static void linear_scan_register_allocation(RegAlloc *ra, LiveInterval **sorted_intervals,
                                            int vreg_count) {
  typedef struct {
    LiveInterval **active;
    int phys_max;
    int active_count;
    unsigned short using_bits;
    unsigned short used_bits;
  } Info;

  Info ireg_info = {
    .active = ALLOCA(sizeof(LiveInterval*) * ra->phys_max),
    .phys_max = ra->phys_max,
    .active_count = 0,
    .using_bits = 0,
    .used_bits = 0,
  };
#ifndef __NO_FLONUM
  Info freg_info = {
    .active = ALLOCA(sizeof(LiveInterval*) * ra->fphys_max),
    .phys_max = ra->fphys_max,
    .active_count = 0,
    .using_bits = 0,
    .used_bits = 0,
  };
#endif
  Info *info = &ireg_info;

  for (int i = 0; i < vreg_count; ++i) {
    LiveInterval *li = sorted_intervals[i];
    if (li->state != LI_NORMAL)
      continue;
    expire_old_intervals(ireg_info.active, &ireg_info.active_count, &ireg_info.using_bits,
                         li->start);
#ifndef __NO_FLONUM
    expire_old_intervals(freg_info.active, &freg_info.active_count, &freg_info.using_bits,
                         li->start);
    if (((VReg*)ra->vregs->data[li->virt])->vtype->flag & VRTF_FLONUM)
      info = &freg_info;
    else
      info = &ireg_info;
#endif
    if (info->active_count >= info->phys_max) {
      split_at_interval(ra, info->active, info->active_count, li);
    } else {
      int regno = -1;
      for (int j = 0; j < info->phys_max; ++j) {
        if (!(info->using_bits & (1 << j))) {
          regno = j;
          break;
        }
      }
      assert(regno >= 0);
      li->phys = regno;
      info->using_bits |= 1 << regno;

      insert_active(info->active, info->active_count, li);
      ++info->active_count;
    }
    info->used_bits |= info->using_bits;
  }
  ra->used_reg_bits = ireg_info.used_bits;
#ifndef __NO_FLONUM
  ra->used_freg_bits = freg_info.used_bits;
#endif
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
      case IR_BITAND:
      case IR_BITOR:
      case IR_BITXOR:
      case IR_LSHIFT:
      case IR_RSHIFT:
      case IR_CMP:
      case IR_NEG:  // unary ops
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
        int flag = 0;
#ifndef __NO_FLONUM
        if (ir->opr1->vtype->flag & VRTF_FLONUM)
          flag |= VRTF_FLONUM;
#endif
        vec_insert(irs, j++,
                   new_ir_load_spilled(ir->opr1, ((VReg*)vregs->data[ir->opr1->virt])->offset, load_size, flag));
        inserted |= 1;
      }

      if (ir->opr2 != NULL && (flag & 2) != 0 &&
          !(ir->opr2->flag & VRF_CONST) && ir->opr2->phys == spilled) {
        int flag = 0;
#ifndef __NO_FLONUM
        if (ir->opr2->vtype->flag & VRTF_FLONUM)
          flag |= VRTF_FLONUM;
#endif
        vec_insert(irs, j++,
                   new_ir_load_spilled(ir->opr2, ((VReg*)vregs->data[ir->opr2->virt])->offset, load_size, flag));
        inserted |= 2;
      }

      if (ir->dst != NULL && (flag & 4) != 0 &&
          !(ir->dst->flag & VRF_CONST) && ir->dst->phys == spilled) {
        assert(!(ir->dst->flag & VRF_CONST));
        int flag = 0;
#ifndef __NO_FLONUM
        if (ir->dst->vtype->flag & VRTF_FLONUM)
          flag |= VRTF_FLONUM;
#endif
        vec_insert(irs, ++j,
                   new_ir_store_spilled(ir->dst, ((VReg*)vregs->data[ir->dst->virt])->offset, ir->size, flag));
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

// Detect living registers for each instruction.
static void detect_living_registers(
  RegAlloc *ra, BBContainer *bbcon, LiveInterval **sorted_intervals, int vreg_count
) {
  UNUSED(ra);
  unsigned int living_pregs = 0;
  int nip = 0;
  for (int i = 0; i < bbcon->bbs->len; ++i) {
    BB *bb = bbcon->bbs->data[i];
    for (int j = 0; j < bb->irs->len; ++j, ++nip) {
      for (int k = 0; k < vreg_count; ++k) {
        LiveInterval *li = sorted_intervals[k];
        if (li->state != LI_NORMAL)
          continue;
        if (nip < li->start)
          break;
        int phys = li->phys;
#ifndef __NO_FLONUM
        if (((VReg*)ra->vregs->data[li->virt])->vtype->flag & VRTF_FLONUM)
          phys += ra->phys_max;
#endif
        if (nip == li->start)
          living_pregs |= 1U << phys;
        if (nip == li->end)
          living_pregs &= ~(1U << phys);
      }

      // Store living regs to IR.
      IR *ir = bb->irs->data[j];
      if (ir->kind == IR_CALL) {
        ir->call.precall->precall.living_pregs = living_pregs;
        // Store it into corresponding precall, too.
        IR *ir_precall = ir->call.precall;
        ir_precall->precall.living_pregs = living_pregs;
      }
    }
  }
}

void prepare_register_allocation(Function *func) {
  // Handle function parameters first.
  if (func->type->func.params != NULL) {
    const int DEFAULT_OFFSET = WORD_SIZE * 2;  // Return address, saved base pointer.
    assert((Scope*)func->scopes->data[0] != NULL);
    int ireg_index = is_stack_param(func->type->func.ret) ? 1 : 0;
#ifndef __NO_FLONUM
    int freg_index = 0;
#endif
    int reg_param_index = ireg_index;
    int offset = DEFAULT_OFFSET;
    for (int j = 0; j < func->type->func.params->len; ++j) {
      VarInfo *varinfo = func->type->func.params->data[j];
      VReg *vreg = varinfo->local.reg;
      // Currently, all parameters are force spilled.
      spill_vreg(func->ra, vreg);
      // stack parameters
      if (is_stack_param(varinfo->type)) {
        vreg->offset = offset = ALIGN(offset, align_size(varinfo->type));
        offset += type_size(varinfo->type);
        continue;
      }

      if (func->type->func.vaargs) {  // Variadic function parameters.
        vreg->offset = (reg_param_index - MAX_REG_ARGS) * WORD_SIZE;
      }
      ++reg_param_index;
      bool through_stack;
#ifndef __NO_FLONUM
      if (is_flonum(varinfo->type)) {
        through_stack = freg_index >= MAX_FREG_ARGS;
        ++freg_index;
      } else
#endif
      {
        through_stack = ireg_index >= MAX_REG_ARGS;
        ++ireg_index;
      }

      if (through_stack) {
        // Function argument passed through the stack.
        vreg->offset = offset;
        offset += WORD_SIZE;
      }
    }
  }

  for (int i = 0; i < func->scopes->len; ++i) {
    Scope *scope = (Scope*)func->scopes->data[i];
    if (scope->vars == NULL)
      continue;

    for (int j = 0; j < scope->vars->len; ++j) {
      VarInfo *varinfo = scope->vars->data[j];
      if (varinfo->storage & (VS_STATIC | VS_EXTERN | VS_ENUM_MEMBER))
        continue;
      VReg *vreg = varinfo->local.reg;
      if (vreg == NULL || vreg->flag & VRF_PARAM)
        continue;

      bool spill = false;
      if (vreg->flag & VRF_REF)
        spill = true;

      switch (varinfo->type->kind) {
      case TY_ARRAY:
      case TY_STRUCT:
        // Make non-primitive variable spilled.
        spill = true;
        break;
      default:
        break;
      }

      if (spill)
        spill_vreg(func->ra, vreg);
    }
  }
}

void alloc_physical_registers(RegAlloc *ra, BBContainer *bbcon) {
#ifndef __NO_FLONUM
  assert(ra->phys_max + ra->fphys_max < (int)(sizeof(ra->used_reg_bits) * CHAR_BIT));
#endif
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

  linear_scan_register_allocation(ra, sorted_intervals, vreg_count);

  // Map vreg to preg.
  for (int i = 0; i < vreg_count; ++i) {
    VReg *vreg = ra->vregs->data[i];
    LiveInterval *li = &intervals[i];
    if (li->state != LI_CONST)
      vreg->phys = intervals[vreg->virt].phys;
  }

  detect_living_registers(ra, bbcon, sorted_intervals, vreg_count);

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
