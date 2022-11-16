#pragma once

#define FF_BINARY  (1 << 0)

struct FILE {
  int fd;
  int wp;
  int flag;

  unsigned char wbuf[32];
};
