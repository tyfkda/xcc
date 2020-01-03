// Register allocation

#pragma once

#include <stdbool.h>
#include <stddef.h>  // size_t

typedef struct BBContainer BBContainer;
typedef struct Function Function;
typedef struct Type Type;
typedef struct VReg VReg;
typedef struct Vector Vector;

typedef struct RegAlloc {
  Vector *vregs;
  struct LiveInterval **sorted_intervals;

  size_t frame_size;
  short used_reg_bits;
} RegAlloc;

RegAlloc *new_reg_alloc(void);
VReg *reg_alloc_spawn(RegAlloc *ra, const Type *type);

void prepare_register_allocation(Function *func);
void alloc_real_registers(RegAlloc *ra, BBContainer *bbcon);

// Private

typedef struct LiveInterval {
  int vreg;
  int rreg;
  int start;
  int end;
  bool spill;
} LiveInterval;
