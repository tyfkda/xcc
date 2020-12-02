#pragma once

#define O_RDONLY  (0x000)
#define O_WRONLY  (0x001)
#define O_RDWR    (0x002)
#define O_TRUNC   (0x100)
#define O_CREAT   (0x200)
#define O_APPEND  (0x400)

#define S_IRUSR         (0400)
#define S_IWUSR         (0200)
#define S_IXUSR         (0100)
#define S_IRGRP         (0040)
#define S_IWGRP         (0020)
#define S_IXGRP         (0010)
#define S_IROTH         (0004)
#define S_IWOTH         (0002)
#define S_IXOTH         (0001)

int open(const char *fn, int flag, ...);
