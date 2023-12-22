#pragma once

// Configuration for riscv64

#define MAX_REG_ARGS   (8)
#define MAX_FREG_ARGS  (8)

#define PHYSICAL_REG_TEMPORARY   (8)
#define PHYSICAL_REG_MAX         (PHYSICAL_REG_TEMPORARY + 18)
#define PHYSICAL_FREG_TEMPORARY  (8)
#define PHYSICAL_FREG_MAX        (PHYSICAL_FREG_TEMPORARY + 24)

#define GET_FPREG_INDEX()  18
