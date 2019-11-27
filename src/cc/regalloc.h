// Register allocation

#pragma once

#include <stdbool.h>
#include <stddef.h>  // size_t

typedef struct Function Function;
typedef struct Type Type;
typedef struct VReg VReg;
typedef struct Vector Vector;

typedef struct RegAlloc {
  int regno;
  Vector *vregs;
  struct LiveInterval **sorted_intervals;
} RegAlloc;

RegAlloc *new_reg_alloc(void);
VReg *reg_alloc_spawn(RegAlloc *ra, const Type *type);

size_t alloc_real_registers(Function *func);

// Private

typedef struct LiveInterval {
  int vreg;
  int rreg;
  int start;
  int end;
  bool spill;
} LiveInterval;
