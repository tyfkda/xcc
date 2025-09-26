// Register allocation

#pragma once

#include <stdbool.h>
#include <stddef.h>  // size_t

#include "ir.h"  // enum VRegSize

typedef struct Vector BBContainer;
typedef struct Function Function;
typedef struct IR IR;
typedef struct VReg VReg;
typedef struct Vector Vector;

enum LiveIntervalState {
  LI_NORMAL,
  LI_SPILL,
};

typedef struct LiveInterval {
  unsigned long occupied_reg_bit;  // Represent occupied registers in bit.
  enum LiveIntervalState state;
  int start;
  int end;
  int virt;  // Virtual register no.
  int phys;  // Mapped physical register no.
} LiveInterval;

typedef struct RegAllocSettings {
  unsigned long (*detect_extra_occupied)(RegAlloc *ra, IR *ir);
  const int *reg_param_mapping;
  struct {
    int phys_max;              // Max physical register count.
    int phys_temporary_count;  // Temporary register count (= start index for saved registers)
  } regset[2];
} RegAllocSettings;

#define RAF_STACK_FRAME  (1 << 0)  // Require stack frame

typedef struct RegAlloc {
  const RegAllocSettings *settings;
  Vector *vregs;   // <VReg*>, non-const vregs
  Vector *consts;  // <VReg*>, const vregs
  Vector **vreg_table;  // [original_vreg_count]
  LiveInterval *intervals;  // size=vregs->len
  LiveInterval **sorted_intervals;

  unsigned long used_reg_bits[2];
  int flag;
  int original_vreg_count;
} RegAlloc;

RegAlloc *new_reg_alloc(const RegAllocSettings *settings);
VReg *reg_alloc_spawn_raw(enum VRegSize vsize, int vflag);
VReg *reg_alloc_spawn(RegAlloc *ra, enum VRegSize vsize, int vflag);
VReg *reg_alloc_with_original(RegAlloc *ra, VReg *original);
VReg *reg_alloc_spawn_const(RegAlloc *ra, int64_t value, enum VRegSize vsize);
#ifndef __NO_FLONUM
VReg *reg_alloc_spawn_fconst(RegAlloc *ra, double value, enum VRegSize vsize);
#endif
void alloc_physical_registers(RegAlloc *ra, BBContainer *bbcon);
