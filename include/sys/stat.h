#pragma once

typedef int mode_t;

int chmod(const char *pathname, mode_t mode);
int fchmodat(int dirfd, const char *pathname, mode_t mode, int flags);
