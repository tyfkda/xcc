#pragma once

int args_sizes_get(int *pargc, int *plen);
int args_get(char **pargv, char *pstr);
void proc_exit(int);

int random_get(void *buf, size_t buf_len);
