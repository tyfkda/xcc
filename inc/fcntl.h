#pragma once

#define O_RDONLY  (0x000)
#define O_WRONLY  (0x001)
#define O_RDWR    (0x002)
#define O_TRUNC   (0x100)
#define O_CREAT   (0x200)
#define O_APPEND  (0x400)

int open(const char *fn, int flag);
