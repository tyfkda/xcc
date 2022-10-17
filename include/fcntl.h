#pragma once

#define O_RDONLY  (00)
#define O_WRONLY  (01)
#define O_RDWR    (02)
#define O_CREAT   (0100)
#define O_EXCL    (0200)
#define O_TRUNC   (01000)
#define O_APPEND  (02000)

#define S_IRUSR         (0400)
#define S_IWUSR         (0200)
#define S_IXUSR         (0100)
#define S_IRGRP         (0040)
#define S_IWGRP         (0020)
#define S_IXGRP         (0010)
#define S_IROTH         (0004)
#define S_IWOTH         (0002)
#define S_IXOTH         (0001)

#define AT_FDCWD  -100

int open(const char *fn, int flag, ...);
