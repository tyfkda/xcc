#include "../../config.h"
#include "regalloc.h"

#include <assert.h>
#include <limits.h>  // CHAR_BIT
#include <stdlib.h>  // malloc
#include <string.h>

#include "ast.h"
#include "codegen.h"  // WORD_SIZE
#include "ir.h"
#include "type.h"
#include "util.h"
#include "var.h"

// Register allocator

RegAlloc *new_reg_alloc(int phys_max) {
  RegAlloc *ra = malloc_or_die(sizeof(*ra));
  assert(phys_max < (int)(sizeof(ra->used_reg_bits) * CHAR_BIT));
  ra->vregs = new_vector();
  ra->intervals = NULL;
  ra->sorted_intervals = NULL;
  ra->phys_max = phys_max;
  ra->fphys_max = 0;
  ra->used_reg_bits = 0;
  ra->used_freg_bits = 0;
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
    d = b->end - a->end;
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
  LiveInterval **active, int *pactive_count, unsigned long *pusing_bits, int start
) {
  int active_count = *pactive_count;
  int j;
  unsigned long using_bits = *pusing_bits;
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

static void check_live_interval(BBContainer *bbcon, int vreg_count, LiveInterval *intervals) {
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
}

static void linear_scan_register_allocation(RegAlloc *ra, LiveInterval **sorted_intervals,
                                            int vreg_count) {
  typedef struct {
    LiveInterval **active;
    int phys_max;
    int active_count;
    unsigned long using_bits;
    unsigned long used_bits;
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

  for (int i = 0; i < vreg_count; ++i) {
    LiveInterval *li = sorted_intervals[i];
    if (li->state != LI_NORMAL)
      continue;
    expire_old_intervals(ireg_info.active, &ireg_info.active_count, &ireg_info.using_bits,
                         li->start);
    Info *info = &ireg_info;
#ifndef __NO_FLONUM
    expire_old_intervals(freg_info.active, &freg_info.active_count, &freg_info.using_bits,
                         li->start);
    if (((VReg*)ra->vregs->data[li->virt])->vtype->flag & VRTF_FLONUM)
      info = &freg_info;
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

static int insert_tmp_reg(RegAlloc *ra, Vector *irs, int j, VReg *spilled) {
  VReg *tmp = reg_alloc_spawn(ra, spilled->vtype, VRF_NO_SPILL);
  IR *ir = irs->data[j];
  VReg *opr = ir->opr1 == spilled ? ir->opr1 : ir->opr2 == spilled ? ir->opr2 : NULL;
  if (opr != NULL) {
    vec_insert(irs, j++, new_ir_load_spilled(tmp, opr));
    if (ir->opr1 == spilled)
      ir->opr1 = tmp;
    if (ir->opr2 == spilled)
      ir->opr2 = tmp;
  }
  if (ir->dst == spilled) {
    vec_insert(irs, ++j, new_ir_store_spilled(ir->dst, tmp));
    ir->dst = tmp;
  }
  return j;
}

static int insert_load_store_spilled_irs(RegAlloc *ra, BBContainer *bbcon) {
  int inserted = 0;
  for (int i = 0; i < bbcon->bbs->len; ++i) {
    BB *bb = bbcon->bbs->data[i];
    Vector *irs = bb->irs;
    for (int j = 0; j < irs->len; ++j) {
      IR *ir = irs->data[j];

      int flag = 7;
      switch (ir->kind) {
      default:
        assert(false);
        // Fallthrough.
      case IR_LOAD:
      case IR_STORE:
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
      case IR_BITNOT:
      case IR_COND:
      case IR_JMP:
      case IR_TJMP:
      case IR_PUSHARG:
      case IR_CALL:
      case IR_RESULT:
      case IR_PRECALL:
      case IR_MEMCPY:
      case IR_CLEAR:
      case IR_ASM:
        break;

      case IR_SUBSP:
      case IR_CAST:
        flag = 5;
        break;

      case IR_BOFS:
      case IR_IOFS:
      case IR_SOFS:
        flag = 4;
        break;

      case IR_LOAD_SPILLED:
      case IR_STORE_SPILLED:
        continue;
      }

      if (ir->opr1 != NULL && (flag & 1) != 0 &&
          !(ir->opr1->flag & VRF_CONST) && (ir->opr1->flag & VRF_SPILLED)) {
        j = insert_tmp_reg(ra, irs, j, ir->opr1);
        ++inserted;
      }

      if (ir->opr2 != NULL && (flag & 2) != 0 &&
          !(ir->opr2->flag & VRF_CONST) && (ir->opr2->flag & VRF_SPILLED)) {
        j = insert_tmp_reg(ra, irs, j, ir->opr2);
        ++inserted;
      }

      if (ir->dst != NULL && (flag & 4) != 0 &&
          !(ir->dst->flag & VRF_CONST) && (ir->dst->flag & VRF_SPILLED)) {
        j = insert_tmp_reg(ra, irs, j, ir->dst);
        ++inserted;
      }
    }
  }
  return inserted;
}

void alloc_physical_registers(RegAlloc *ra, BBContainer *bbcon) {
  assert(ra->phys_max < (int)(sizeof(ra->used_reg_bits) * CHAR_BIT));
#ifndef __NO_FLONUM
  assert(ra->fphys_max < (int)(sizeof(ra->used_freg_bits) * CHAR_BIT));
#endif

  LiveInterval *intervals = NULL;
  LiveInterval **sorted_intervals = NULL;

  for (;;) {
    int vreg_count = ra->vregs->len;
    intervals = realloc_or_die(intervals, sizeof(LiveInterval) * vreg_count);
    check_live_interval(bbcon, vreg_count, intervals);

    for (int i = 0; i < vreg_count; ++i) {
      LiveInterval *li = &intervals[i];
      VReg *vreg = ra->vregs->data[i];

      if (vreg->flag & VRF_CONST) {
        li->state = LI_CONST;
        continue;
      }

      // Force function parameter spilled.
      if (vreg->param_index >= 0) {
        spill_vreg(vreg);
        li->start = 0;
        li->state = LI_SPILL;
      }
      if (vreg->flag & VRF_SPILLED) {
        li->state = LI_SPILL;
        li->phys = vreg->phys;
      }
    }

    // Sort by start, end
    sorted_intervals = realloc_or_die(sorted_intervals, sizeof(LiveInterval*) * vreg_count);
    for (int i = 0; i < vreg_count; ++i)
      sorted_intervals[i] = &intervals[i];
    qsort(sorted_intervals, vreg_count, sizeof(LiveInterval*), sort_live_interval);
    ra->sorted_intervals = sorted_intervals;

    linear_scan_register_allocation(ra, sorted_intervals, vreg_count);

    // Spill vregs.
    for (int i = 0; i < vreg_count; ++i) {
      LiveInterval *li = &intervals[i];
      if (li->state == LI_SPILL) {
        VReg *vreg = ra->vregs->data[i];
        if (vreg->flag & VRF_SPILLED)
          continue;
        spill_vreg(vreg);
      }
    }

    if (insert_load_store_spilled_irs(ra, bbcon) <= 0)
      break;
  }

  ra->intervals = intervals;
  ra->sorted_intervals = sorted_intervals;
}
