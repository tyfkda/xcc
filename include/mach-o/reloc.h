#pragma once

#include <stdint.h>

struct relocation_info {
  int32_t r_address;
#ifndef __NO_BITFIELD
  uint32_t
    r_symbolnum : 24,
    r_pcrel     : 1,
    r_length    : 2,
    r_extern    : 1,
    r_type      : 4;
#else
  uint32_t r_pack;
#endif
};
#define R_ABS  0
