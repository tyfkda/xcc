// Register allocation

#pragma once

#include <stdbool.h>
#include <stddef.h>  // size_t

typedef struct BBContainer BBContainer;
typedef struct Function Function;
typedef struct VReg VReg;
typedef struct VRegType VRegType;
typedef struct Vector Vector;

enum LiveIntervalState {
  LI_NORMAL,
  LI_SPILL,
  LI_CONST,
};

typedef struct LiveInterval {
  enum LiveIntervalState state;
  int start;
  int end;
  int virt;  // Virtual register no.
  int phys;  // Mapped physical register no.
} LiveInterval;

typedef struct RegAlloc {
  Vector *vregs;  // <VReg*>
  LiveInterval *intervals;  // size=vregs->len
  LiveInterval **sorted_intervals;

  int phys_max;  // Max physical register count.
  int fphys_max;  // Floating-point register.
  unsigned long used_reg_bits;
  unsigned long used_freg_bits;
} RegAlloc;

RegAlloc *new_reg_alloc(int phys_max);
VReg *reg_alloc_spawn(RegAlloc *ra, const VRegType *vtype, int flag);
void alloc_physical_registers(RegAlloc *ra, BBContainer *bbcon);
