#pragma once

#define FF_BINARY  (1 << 0)

struct FILE {
  int fd;
  int rp, rs;
  int wp;
  int flag;

  // TODO: allocate buffers only if required.
  unsigned char rbuf[256];
  unsigned char wbuf[32];
};
