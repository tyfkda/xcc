// Register allocation

#pragma once

#include <stdbool.h>
#include <stddef.h>  // size_t

typedef struct BBContainer BBContainer;
typedef struct Function Function;
typedef struct VReg VReg;
typedef struct VRegType VRegType;
typedef struct Vector Vector;

typedef struct RegAlloc {
  Vector *vregs;
  struct LiveInterval **sorted_intervals;

  size_t frame_size;
  int phys_max;  // Max physical register count.
  unsigned short used_reg_bits;
#ifndef __NO_FLONUM
  unsigned short used_freg_bits;
  int fphys_max;  // Floating-point register.
#endif
} RegAlloc;

RegAlloc *new_reg_alloc(int phys_max);
VReg *reg_alloc_spawn(RegAlloc *ra, const VRegType *vtype, int flag);

void prepare_register_allocation(Function *func);
void alloc_physical_registers(RegAlloc *ra, BBContainer *bbcon);

// Private

enum LiveIntervalState {
  LI_NORMAL,
  LI_SPILL,
  LI_CONST,
};

typedef struct LiveInterval {
  int virt;  // Virtual reg no.
  int start;
  int end;
  enum LiveIntervalState state;
  int phys;  // Mapped physical reg no.
} LiveInterval;
