#pragma once

typedef long Align;

typedef union header {
  struct {
    union header *ptr;
    unsigned int size;
  } s;
  Align x;
} Header;
