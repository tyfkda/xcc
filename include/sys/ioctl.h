#pragma once

#define NCC 8
struct termio {
        unsigned short c_iflag;         /* input mode flags */
        unsigned short c_oflag;         /* output mode flags */
        unsigned short c_cflag;         /* control mode flags */
        unsigned short c_lflag;         /* local mode flags */
        unsigned char c_line;           /* line discipline */
        unsigned char c_cc[NCC];        /* control characters */
};

// /usr/include/asm-generic/ioctls.h
#define TCGETA          (0x5405)

#ifdef __cplusplus
extern "C" {
#endif

int ioctl(int fd, int request, ...);

#ifdef __cplusplus
}  // extern "C"
#endif
