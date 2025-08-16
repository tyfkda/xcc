#include "limits.h"
#include "stdbool.h"
#include "string.h"
#include "sys/types.h"  // ssize_t

#define SEP  '/'
#define DOT  '.'
#define DELIMIT(c)  ((c) == SEP || (c) == '\0')

#define S_SEP  "/"
#define S_DOT  "."
#define S_PARENT  S_DOT S_DOT

static ssize_t move_parent(char *buf, ssize_t p) {
  if (p == 0) {
    strcpy(&buf[0], S_PARENT S_SEP);
    return 3;
  }
  if (buf[p - 1] == SEP) {
    if (--p == 0)  // Error: try to move parent of root.
      return -1;
  }
  ssize_t q = p;
  for (; q > 0; --q) {
    if (buf[q - 1] == SEP)
      break;
  }
  if (strncmp(&buf[q], S_PARENT, sizeof(S_PARENT) - 1) == 0 && DELIMIT(buf[q + 2])) {
    // Parent of parent.
    strcpy(&buf[p], S_SEP S_PARENT S_SEP);
    p += 4;
  } else if (q == 0 && buf[0] != SEP) {
    // Move to parent of relative base path: "./" => "../", otherwise "./"
    static const char rela[] = S_PARENT S_SEP;
    if (buf[0] != DOT || !DELIMIT(buf[1])) {
      buf[p = 0] = '\0';  // Relative top.
    } else {
      strcpy(&buf[0], rela);
      p = sizeof(rela) - 1;
    }
  } else {
    p = q;
  }
  return p;
}

ssize_t _canonicalize_path(char *buf, ssize_t size, const char *path) {
#define PUT(c)  do { if (p<size) buf[p++] = (c); } while (0)
  ssize_t p = strlen(buf);
  if (*path == SEP) {
    p = 0;
    PUT(SEP);
    ++path;
  } else {
    if (p == 0 || buf[p - 1] != SEP)
      PUT(SEP);
  }

  for (;;) {
    while (*path == SEP)  // Double slash: ignore.
      ++path;
    if (*path == '\0')
      break;

    const char *next = strchr(path, SEP);
    if (next == NULL) {
      next = path + strlen(path);
    }
    bool no_sep = false;
    if (*path == DOT && DELIMIT(path[1])) {  // '.'
      no_sep = true;
    } else if (*path == DOT && path[1] == DOT && DELIMIT(path[2])) {  // ".."
      p = move_parent(buf, p);
      if (p < 0)
        return -1;
      no_sep = true;  //!(*next == SEP && next[1] == '\0');

      if (*next == '\0' || next[1] == '\0') {
        if (p == 0) {  // Relative top.
          if (*next == '\0') {
            strncpy(buf, S_DOT, size);
            p = 1;
          } else {
            strncpy(buf, S_DOT S_SEP, size);
            p = 2;
          }
        }
        break;
      }
    } else {
      if (p == 2 && buf[0] == DOT) {  // Relative top: remove "."
        p = 0;
      }

      size_t len = next - path;
      size_t rest = size - p;
      if (len > rest)
        len = rest;
      memcpy(&buf[p], path, len);
      p += len;
    }

    if (*next == '\0')
      break;
    if (!no_sep)
      PUT(SEP);
    path = next + 1;
  }
  if (p < size)
    buf[p] = '\0';
  return p;
#undef PUT
}
