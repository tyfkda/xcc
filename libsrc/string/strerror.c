#include "string.h"

static char *strerrors[] = {
  "Success",
  "Operation not permitted",
  "No such file or directory",
  "No such process",
  "Interrupted system call",
  "I/O error",
  "No such device or address",
  "Argument list too long",
  "Exec format error",
  "Bad file number",
  "No child processes",
  "Try again",
  "Out of memory",
  "Permission denied",
  "Bad address",
  "Block device required",
  "Device or resource busy",
  "File exists",
  "Cross-device link",
  "No such device",
  "Not a directory",
  "Is a directory",
  "Invalid argument",
  "File table overflow",
  "Too many open files",
  "Not a typewriter",
  "Text file busy",
  "File too large",
  "No space left on device",
  "Illegal seek",
  "Read-only file system",
  "Too many links",
  "Broken pipe",
  "Math argument out of domain of func",
  "Math result not representable",
};

char *strerror(int no) {
  if (no < 0 || no >= sizeof(strerrors) / sizeof(*strerrors))
    return "Unknown error";
  return strerrors[no];
}
