#include "string.h"

char *strstr(const char *s1, const char *s2) {
  for  (size_t len = strlen(s2); *s1 != '\0'; ++s1) {
    if (strncmp(s1, s2, len) == 0)
      return (char*)s1;
  }
  return NULL;
}
