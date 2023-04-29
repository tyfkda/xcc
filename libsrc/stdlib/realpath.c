/* realpath.c - Return the canonicalized absolute pathname */
/* Written 2000 by Werner Almesberger */
#include <errno.h>
#include <limits.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <unistd.h>

#include <stdio.h>

#if !defined(__WASM)
/* FIXME: buffer overrun possible, loops forever on cyclic symlinks */
/*
 * Canonical name: never ends with a slash
 */
static int resolve_path(char *path, char *result, char *pos) {
  if (*path == '/') {
    *result = '/';
    pos = result + 1;
    ++path;
  }
  *pos = '\0';
  if (*path == '\0')
    return 0;

  for (;;) {
fprintf(stderr, "path=%s\n", path);
    char *slash = strchr(path, '/');
    if (slash != NULL)
      *slash = '\0';
    if (path[0] == '\0' || (path[0] == '.' &&
                            (path[1] == '\0' || (path[1] == '.' && path[2] == '\0')))) {
      --pos;
      if (pos != result && path[0] != '\0' && path[1] != '\0') {
        while (*--pos != '/')
          ;
      }
    } else {
      strcpy(pos, path);
fprintf(stderr, "  pos=%s\n", pos);
      struct stat st;
      if (lstat(result, &st) < 0)
        return -1;
      if (S_ISLNK(st.st_mode)) {
fprintf(stderr, "  islink: %s\n", result);
        char buf[PATH_MAX];
        if (readlink(result, buf, sizeof(buf)) < 0)
          return -1;
fprintf(stderr, "    readlink: result=%s\n", buf);
        *pos = '\0';
        if (slash != NULL) {
          *slash = '/';
          strcat(buf, slash);
        }
        strcpy(path, buf);
        if (*path == '/')
          result[1] = '\0';
        pos = strchr(result, '\0');
        continue;
      }
      pos = strchr(result, '\0');
    }
    if (slash != NULL) {
      *pos++ = '/';
      path = slash + 1;
    }
    *pos = '\0';
    if (slash == NULL)
      break;
  }
  return 0;
}

char *realpath(const char *path, char *resolved_path) {
  int res;
  if (*path == '\0') {
    errno = ENOENT; /* SUSv2 */
    return NULL;
  }

  void *allocated = NULL;
  if (resolved_path == NULL) {
    resolved_path = allocated = malloc(PATH_MAX);
    if (resolved_path == NULL)
      return NULL;
  }

  char cwd[PATH_MAX];
  if (!getcwd(cwd, sizeof(cwd)))
    return NULL;

  strcpy(resolved_path, "/");
  if (resolve_path(cwd, resolved_path, resolved_path) == 0) {
fprintf(stderr, "resolve_path: %s\n", resolved_path);
    strcat(resolved_path, "/");

    char *path_copy = strdup(path);
    if (path_copy != NULL) {
      res = resolve_path(path_copy, resolved_path, strchr(resolved_path, '\0'));
      free(path_copy);
      if (res == 0)
        return resolved_path;
    }
  }
  free(allocated);
  return NULL;
}
#endif
