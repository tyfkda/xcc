#if defined(__riscv)
int errno;

#else

#if defined(__APPLE__)
#define __errno_location  __error
#endif
int *__errno_location(void) {
  static int value;
  return &value;
}
#endif
