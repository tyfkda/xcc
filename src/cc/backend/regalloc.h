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

#define LIF_CONTAINS_CALL  (1 << 0)

typedef struct LiveInterval {
  enum LiveIntervalState state;
  int flag;
  int start;
  int end;
  int virt;  // Virtual register no.
  int phys;  // Mapped physical register no.
} LiveInterval;

typedef struct RegAlloc {
  Vector *vregs;  // <VReg*>
  LiveInterval *intervals;  // size=vregs->len
  LiveInterval **sorted_intervals;

  const int *reg_param_mapping;
  int phys_max;              // Max physical register count.
  int phys_temporary_count;  // Temporary register count (= start index for saved registers)
  int fphys_max;             // Floating-point register.
  int fphys_temporary_count;
  unsigned long used_reg_bits;
  unsigned long used_freg_bits;
} RegAlloc;

RegAlloc *new_reg_alloc(const int *reg_param_mapping, int phys_max, int temporary_count);
VReg *reg_alloc_spawn(RegAlloc *ra, const VRegType *vtype, int flag);
void alloc_physical_registers(RegAlloc *ra, BBContainer *bbcon);
