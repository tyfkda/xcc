#pragma once

#define FF_BINARY  (1 << 0)

struct FILE {
  int fd;
  int flag;
};
