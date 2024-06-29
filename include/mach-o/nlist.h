#pragma once

#include <stdint.h>

#define N_STAB  0xe0
#define N_PEXT  0x10
#define N_TYPE  0x0e
#define N_EXT   0x01

#define N_UNDF  0x0
#define N_ABS   0x2
#define N_SECT  0xe
#define N_PBUD  0xc
#define N_INDR  0xa

struct nlist_64 {
  union {
    uint32_t n_strx;
  } n_un;
  uint8_t n_type;
  uint8_t n_sect;
  uint16_t n_desc;
  uint64_t n_value;
};

#define GET_COMM_ALIGN(n_desc) (((n_desc) >> 8) & 0x0f)
#define SET_COMM_ALIGN(n_desc, align) (n_desc) = (((n_desc) & 0xf0ff) | (((align) & 0x0f) << 8))
