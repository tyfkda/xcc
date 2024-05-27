#pragma once

#include <stdbool.h>
#include <stdio.h>
#include <stdint.h>
#include <sys/types.h>

//////////////////////////////////////////////////////////////////////////////
// Platform independent implementations
//////////////////////////////////////////////////////////////////////////////

// Modifies a path in-place to changes slashes to forward slashes only,
// so it doesn't generate escape sequences in the preprocessor include generator
char* platform_pathslashes(char* buf);

// Creates a temporary file with a given extension, opens it in write-binary mode, returns the path
FILE* platform_mktempfile2(const char* ext, char** path, const char* mode);

// Creates a temporary file, opens it in write-binary mode
FILE* platform_mktempfile(void);

// Flushes the temporary file and ensures the data pointer contains a pointer to the contents of the buffer if necessary
void flush_memstream(FILE* file, char** data, size_t* len);

char* platform_getcwd(char* buf, size_t size);
bool platform_is_fullpath(const char* path);
bool platform_is_rootpath(const char* path);
bool platform_cmp_path(const char* a, const char* b);

bool platform_wait_process(pid_t pid, int* result);
pid_t platform_wait_process_any(pid_t *pids, size_t pids_len, int* result);

void platform_kill(int pid);

//////////////////////////////////////////////////////////////////////////////
// Platform specific implementations
//////////////////////////////////////////////////////////////////////////////
//
// Win32
//
#ifdef _WIN32

#include <sys/stat.h>

// Replacements
FILE* fmemopen(void* buf, size_t len, const char* mode);
FILE* open_memstream(char** data, size_t* len);
char* strndup(const char* s1, unsigned long long n);

int platform_spawnvp(const char* cname, const char* const* argv, const int handle_in, const int handle_out, const int handle_err);

#endif
