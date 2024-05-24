#include "../../config.h"
#include "regalloc.h"

#include <assert.h>
#include <limits.h>  // CHAR_BIT
#include <stdlib.h>  // free, qsort
#include <string.h>

#include "ir.h"
#include "util.h"

// Register allocator

RegAlloc *new_reg_alloc(const RegAllocSettings *settings) {
  RegAlloc *ra = malloc_or_die(sizeof(*ra));
  assert(settings->phys_max < (int)(sizeof(ra->used_reg_bits) * CHAR_BIT));
  ra->settings = settings;
  ra->vregs = new_vector();
  ra->consts = new_vector();
  ra->intervals = NULL;
  ra->sorted_intervals = NULL;
  ra->used_reg_bits = 0;
  ra->used_freg_bits = 0;
  ra->flag = 0;
  return ra;
}

inline VReg *alloc_vreg(enum VRegSize vsize, int vflag) {
  VReg *vreg = malloc_or_die(sizeof(*vreg));
  vreg->virt = -1;
  vreg->phys = -1;
  vreg->vsize = vsize;
  vreg->flag = vflag;
  vreg->reg_param_index = -1;
  vreg->frame.offset = 0;
  return vreg;
}

VReg *reg_alloc_spawn(RegAlloc *ra, enum VRegSize vsize, int vflag) {
  VReg *vreg = alloc_vreg(vsize, vflag);
  if (!(vflag & VRF_CONST)) {
    vreg->virt = ra->vregs->len;
    vec_push(ra->vregs, vreg);
  } else {
    vec_push(ra->consts, vreg);
  }
  return vreg;
}

VReg *reg_alloc_spawn_const(RegAlloc *ra, int64_t value, enum VRegSize vsize) {
  for (int i = 0; i < ra->consts->len; ++i) {
    VReg *v = ra->consts->data[i];
    if (v->fixnum == value && v->vsize == vsize)
      return v;
  }

  VReg *vreg = reg_alloc_spawn(ra, vsize, VRF_CONST);
  vreg->fixnum = value;
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
  if (d == 0) {
    d = b->end - a->end;
    if (d == 0)
      d = a->virt - b->virt;
  }
  return d;
}

static void split_at_interval(RegAlloc *ra, LiveInterval **active, int active_count,
                              LiveInterval *li) {
  assert(active_count > 0);
  LiveInterval *spill = active[active_count - 1];
  if (spill->end > li->end) {
    li->phys = spill->phys;
    spill->phys = ra->settings->phys_max;
    spill->state = LI_SPILL;
    insert_active(active, active_count - 1, li);
  } else {
    li->phys = ra->settings->phys_max;
    li->state = LI_SPILL;
  }
}

typedef struct {
  LiveInterval **active;
  int phys_max;
  int phys_temporary;
  int active_count;
  uint64_t using_bits;
  uint64_t used_bits;
} PhysicalRegisterSet;

static void expire_old_intervals(PhysicalRegisterSet *p, int start) {
  int active_count = p->active_count;
  uint64_t using_bits = p->using_bits;
  int j;
  for (j = 0; j < active_count; ++j) {
    LiveInterval *li = p->active[j];
    if (li->end > start)
      break;
    int phys = li->phys;
    using_bits &= ~(1ULL << phys);
  }
  remove_active(p->active, active_count, 0, j);
  p->active_count = active_count - j;
  p->using_bits = using_bits;
}

static void set_inout_interval(Vector *vregs, LiveInterval *intervals, int nip) {
  for (int j = 0; j < vregs->len; ++j) {
    VReg *vreg = vregs->data[j];
    LiveInterval *li = &intervals[vreg->virt];
    if (vreg->flag & VRF_PARAM) {
      // If the vreg is register parameter for function,
      // it is given a priori and keep live interval start as is.
    } else {
      if (li->start < 0 || li->start > nip)
        li->start = nip;
    }
    if (li->end < nip)
      li->end = nip;
  }
}

static void check_live_interval(BBContainer *bbcon, int vreg_count, LiveInterval *intervals) {
  for (int i = 0; i < vreg_count; ++i) {
    LiveInterval *li = &intervals[i];
    li->occupied_reg_bit = 0;
    li->state = LI_NORMAL;
    li->start = li->end = -1;
    li->virt = i;
    li->phys = -1;
  }

  int nip = 0;
  for (int i = 0; i < bbcon->bbs->len; ++i) {
    BB *bb = bbcon->bbs->data[i];

    set_inout_interval(bb->in_regs, intervals, nip);

    for (int j = 0; j < bb->irs->len; ++j, ++nip) {
      IR *ir = bb->irs->data[j];
      VReg *vregs[] = {ir->dst, ir->opr1, ir->opr2};
      for (int k = 0; k < 3; ++k) {
        VReg *vreg = vregs[k];
        if (vreg == NULL || (vreg->flag & VRF_CONST))
          continue;
        LiveInterval *li = &intervals[vreg->virt];
        if (li->start < 0 && !(vreg->flag & VRF_PARAM))
          li->start = nip;
        if (li->end < nip)
          li->end = nip;
      }
    }

    set_inout_interval(bb->out_regs, intervals, nip);
  }
}

static void occupy_regs(RegAlloc *ra, Vector *actives, uint64_t ioccupy,
                        uint64_t foccupy) {
  for (int k = 0; k < actives->len; ++k) {
    LiveInterval *li = actives->data[k];
    VReg *vreg = ra->vregs->data[li->virt];
    assert(vreg != NULL);
    li->occupied_reg_bit |= (vreg->flag & VRF_FLONUM) ? foccupy : ioccupy;
  }
}

static void detect_live_interval_flags(RegAlloc *ra, BBContainer *bbcon, int vreg_count,
                                       LiveInterval **sorted_intervals) {
  Vector *inactives = new_vector();
  Vector *actives = new_vector();
  for (int i = 0; i < vreg_count; ++i) {
    LiveInterval *li = sorted_intervals[i];
    if (li->end < 0)
      continue;
    vec_push(li->start < 0 ? actives : inactives, li);
  }

  const RegAllocSettings *settings = ra->settings;
  int nip = 0;
  uint64_t iargset = 0, fargset = 0;
  for (int i = 0; i < bbcon->bbs->len; ++i) {
    BB *bb = bbcon->bbs->data[i];
    for (int j = 0; j < bb->irs->len; ++j, ++nip) {
      IR *ir = bb->irs->data[j];
      if (settings->detect_extra_occupied != NULL) {
        uint64_t ioccupy = (*settings->detect_extra_occupied)(ra, ir);
        if (ioccupy != 0)
          occupy_regs(ra, actives, ioccupy, 0);
      }

      if (iargset != 0 || fargset != 0)
        occupy_regs(ra, actives, iargset, fargset);

      // Deactivate registers which end at this ip.
      for (int k = 0; k < actives->len; ++k) {
        LiveInterval *li = actives->data[k];
        if (li->end <= nip)
          vec_remove_at(actives, k--);
      }

      // Update function parameter register occupation after setting it.
      if (ir->kind == IR_PUSHARG) {
        VReg *opr1 = ir->opr1;
        if (opr1->flag & VRF_FLONUM
#if VAARG_FP_AS_GP
            && !ir->pusharg.fp_as_gp
#endif
        ) {
          int n = ir->pusharg.index;
          // Assume same order on FP-register.
          fargset |= 1ULL << n;
        } else {
          int n = settings->reg_param_mapping[ir->pusharg.index];
          if (n >= 0)
            iargset |= 1ULL << n;
        }
      }

      // Call instruction breaks registers which contain in their live interval (start < nip < end).
      if (ir->kind == IR_CALL) {
        // Non-saved registers on calling convention.
        const uint64_t ibroken = (1ULL << settings->phys_temporary_count) - 1;
        const uint64_t fbroken = (1ULL << settings->fphys_temporary_count) - 1;
        occupy_regs(ra, actives, ibroken, fbroken);
        iargset = fargset = 0;
      }

      // Activate registers after usage checked.
      while (inactives->len > 0) {
        LiveInterval *li = inactives->data[0];
        if (li->start > nip)
          break;
        vec_remove_at(inactives, 0);
        vec_push(actives, li);
      }
    }
  }

  free_vector(inactives);
  free_vector(actives);
}

static void linear_scan_register_allocation(RegAlloc *ra, LiveInterval **sorted_intervals,
                                            int vreg_count) {
  PhysicalRegisterSet iregset = {
    .active = ALLOCA(sizeof(LiveInterval*) * ra->settings->phys_max),
    .phys_max = ra->settings->phys_max,
    .phys_temporary = ra->settings->phys_temporary_count,
    .active_count = 0,
    .using_bits = 0,
    .used_bits = 0,
  };
  PhysicalRegisterSet fregset = {
    .active = ALLOCA(sizeof(LiveInterval*) * ra->settings->fphys_max),
    .phys_max = ra->settings->fphys_max,
    .phys_temporary = ra->settings->fphys_temporary_count,
    .active_count = 0,
    .using_bits = 0,
    .used_bits = 0,
  };

  for (int i = 0; i < vreg_count; ++i) {
    LiveInterval *li = sorted_intervals[i];
    if (ra->vregs->data[li->virt] == NULL)
      continue;
    if (li->state != LI_NORMAL)
      continue;
    expire_old_intervals(&iregset, li->start);
    expire_old_intervals(&fregset, li->start);

    PhysicalRegisterSet *prsp = &iregset;
    if (((VReg*)ra->vregs->data[li->virt])->flag & VRF_FLONUM)
      prsp = &fregset;
    int start_index = 0;
    int regno = -1;
    VReg *vreg = ra->vregs->data[li->virt];
    int ip = vreg->reg_param_index;
    uint64_t occupied = prsp->using_bits | li->occupied_reg_bit;
    if (ip >= 0) {
      if (vreg->flag & VRF_FLONUM) {
        // Assume floating-pointer parameter registers are same order,
        // and no mapping required.
      } else {
        ip = ra->settings->reg_param_mapping[ip];
      }

      if (ip >= 0 && !(occupied & (1ULL << ip)))
        regno = ip;
      else
        start_index = prsp->phys_temporary;
    }
    if (regno < 0) {
      for (int j = start_index; j < prsp->phys_max; ++j) {
        if (!(occupied & (1ULL << j))) {
          regno = j;
          break;
        }
      }
    }
    if (regno >= 0) {
      li->phys = regno;
      prsp->using_bits |= 1ULL << regno;

      insert_active(prsp->active, prsp->active_count, li);
      ++prsp->active_count;
    } else {
      split_at_interval(ra, prsp->active, prsp->active_count, li);
    }
    prsp->used_bits |= prsp->using_bits;
  }
  ra->used_reg_bits = iregset.used_bits;
  ra->used_freg_bits = fregset.used_bits;
}

static int insert_tmp_reg(RegAlloc *ra, Vector *irs, int j, VReg *spilled) {
  VReg *tmp = reg_alloc_spawn(ra, spilled->vsize, VRF_NO_SPILL | (spilled->flag & VRF_MASK));
  IR *ir = irs->data[j];
  VReg *opr = ir->opr1 == spilled ? ir->opr1 : ir->opr2 == spilled ? ir->opr2 : NULL;
  if (opr != NULL) {
    vec_insert(irs, j++, new_ir_load_spilled(tmp, opr, ir->flag));
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
  enum {
    OPR1 = 1 << 0,
    OPR2 = 1 << 1,
    DST  = 1 << 2,
    D12 = DST | OPR1 | OPR2,
    D__ = DST,
    ___ = -1,
  };
  static const int kSpillTable[] = {
    [IR_LOAD]    = D12, [IR_STORE]   = D12, [IR_ADD]     = D12, [IR_SUB]     = D12,
    [IR_MUL]     = D12, [IR_DIV]     = D12, [IR_MOD]     = D12, [IR_BITAND]  = D12,
    [IR_BITOR]   = D12, [IR_BITXOR]  = D12, [IR_LSHIFT]  = D12, [IR_RSHIFT]  = D12,
    [IR_NEG]     = D12, [IR_BITNOT]  = D12, [IR_COND]    = D12,
    [IR_JMP]     = D12, [IR_TJMP]    = D12, [IR_PRECALL] = D12, [IR_PUSHARG] = D12,
    [IR_CALL]    = D12, [IR_RESULT]  = D12, [IR_SUBSP]   = D12, [IR_CAST]    = D12,
    [IR_MOV]     = D12, [IR_KEEP]    = D12, [IR_ASM]     = D12,

    [IR_BOFS]    = D__, [IR_IOFS]    = D__, [IR_SOFS]    = D__,

    [IR_LOAD_S]  = ___, [IR_STORE_S] = ___,
  };

  int inserted = 0;
  for (int i = 0; i < bbcon->bbs->len; ++i) {
    BB *bb = bbcon->bbs->data[i];
    Vector *irs = bb->irs;
    for (int j = 0; j < irs->len; ++j) {
      IR *ir = irs->data[j];
      assert(ir->kind < (int)ARRAY_SIZE(kSpillTable));
      int flag = kSpillTable[ir->kind];
      assert(flag != 0);
      if (flag == ___)
        continue;

      if (ir->opr1 != NULL && (flag & OPR1) != 0 && (ir->opr1->flag & VRF_SPILLED)) {
        assert(!(ir->opr1->flag & VRF_CONST));
        j = insert_tmp_reg(ra, irs, j, ir->opr1);
        ++inserted;
      }

      if (ir->opr2 != NULL && (flag & OPR2) != 0 && (ir->opr2->flag & VRF_SPILLED)) {
        assert(!(ir->opr2->flag & VRF_CONST));
        j = insert_tmp_reg(ra, irs, j, ir->opr2);
        ++inserted;
      }

      if (ir->dst != NULL && (flag & DST) != 0 && (ir->dst->flag & VRF_SPILLED)) {
        assert(!(ir->dst->flag & VRF_CONST));
        j = insert_tmp_reg(ra, irs, j, ir->dst);
        ++inserted;
      }
    }
  }
  return inserted;
}

void alloc_physical_registers(RegAlloc *ra, BBContainer *bbcon) {
  assert(ra->settings->phys_max < (int)(sizeof(ra->used_reg_bits) * CHAR_BIT));
  assert(ra->settings->fphys_max < (int)(sizeof(ra->used_freg_bits) * CHAR_BIT));

  int vreg_count = ra->vregs->len;
  LiveInterval *intervals = malloc_or_die(sizeof(LiveInterval) * vreg_count);
  LiveInterval **sorted_intervals = malloc_or_die(sizeof(LiveInterval*) * vreg_count);

  for (;;) {
    check_live_interval(bbcon, vreg_count, intervals);

    for (int i = 0; i < vreg_count; ++i) {
      LiveInterval *li = &intervals[i];
      VReg *vreg = ra->vregs->data[i];
      if (vreg == NULL)
        continue;

      assert(!(vreg->flag & VRF_CONST));

      if (vreg->flag & VRF_SPILLED) {
        li->state = LI_SPILL;
        li->phys = vreg->phys;
      }
    }

    // Sort by start, end
    for (int i = 0; i < vreg_count; ++i)
      sorted_intervals[i] = &intervals[i];
    qsort(sorted_intervals, vreg_count, sizeof(LiveInterval*), sort_live_interval);
    ra->sorted_intervals = sorted_intervals;

    detect_live_interval_flags(ra, bbcon, vreg_count, sorted_intervals);
    linear_scan_register_allocation(ra, sorted_intervals, vreg_count);

    // Spill vregs.
    bool spilled = false;
    for (int i = 0; i < vreg_count; ++i) {
      LiveInterval *li = &intervals[i];
      if (li->state == LI_SPILL) {
        VReg *vreg = ra->vregs->data[i];
        if (vreg->flag & VRF_SPILLED)
          continue;
        spill_vreg(vreg);
        spilled = true;
      }
    }
    if (spilled)
      ra->flag |= RAF_STACK_FRAME;

    if (insert_load_store_spilled_irs(ra, bbcon) <= 0)
      break;

    if (vreg_count != ra->vregs->len) {
      vreg_count = ra->vregs->len;
      free(intervals);
      free(sorted_intervals);
      intervals = malloc_or_die(sizeof(LiveInterval) * vreg_count);
      sorted_intervals = malloc_or_die(sizeof(LiveInterval*) * vreg_count);
    }
  }

  ra->intervals = intervals;
  ra->sorted_intervals = sorted_intervals;
}
