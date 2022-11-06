#include <stdlib.h>  // malloc

extern int args_sizes_get(int *pargc, int *plen);
extern int args_get(char **pargv, char *pstr);

int _start(void) {
  extern int main(int, char**);
  int argc, len = 0;
  int r = args_sizes_get(&argc, &len);
  if (r != 0)
    return(r);

  char **argv = malloc(sizeof(char*) * (argc + 1) + len);
  char *str = ((char*)argv) + sizeof(char*) * (argc + 1);
  args_get(argv, str);
  argv[argc] = NULL;

  int ec = main(argc, argv);
  return ec;
}
