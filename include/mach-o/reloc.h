#pragma once

#include <stdint.h>

struct relocation_info {
  int32_t r_address;
  uint32_t
    r_symbolnum : 24,
    r_pcrel     : 1,
    r_length    : 2,
    r_extern    : 1,
    r_type      : 4;
};
#define R_ABS  0
