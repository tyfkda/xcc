#pragma once

struct FILE;

typedef struct {
  struct FILE **opened;
  int length, capacity;
} FILEMAN;
