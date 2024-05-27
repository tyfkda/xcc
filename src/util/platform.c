#include "platform.h"

#include <stdlib.h>
#include <string.h>
#include <assert.h>

//////////////////////////////////////////////////////////////////////////////
// Platform independent implementations
//////////////////////////////////////////////////////////////////////////////

// Modifies a path in-place to changes slashes to forward slashes only,
// so it doesn't generate escape sequences in the preprocessor include generator
char* platform_pathslashes(char* buf) {
  size_t i;
  for(i = 0; i < strlen(buf); ++i) {
    if (buf[i] == '\\') {
      buf[i] = '/';
    }
  }
  return buf;
}

//////////////////////////////////////////////////////////////////////////////
// Platform specific implementations
//////////////////////////////////////////////////////////////////////////////
//
// Win32
//
#ifdef _WIN32

#include <io.h>
#include <fcntl.h>
#include <windows.h>
#include <shlwapi.h>
#include <process.h>

char* strndup(const char* s1, unsigned long long n) {
    char* c = (char*)malloc(n + 1);
    memcpy(c, s1, n);
    c[n] = 0;
    return c;
}

// Creates a temporary file with a given extension and returns the path
FILE* platform_mktempfile2(const char* ext, char** path, const char* mode) {
  if (!path) {
    return NULL;
  }

  char tp[MAX_PATH + 1];
  char* fn = strdup("xcc-XXXXXX");

  // Temporary path
  if (!GetTempPath(sizeof(tp), tp)) {
    free(fn);
    return NULL;
  }
  tp[MAX_PATH] = 0;

  // Temporary filename
  mktemp(fn);
  if (!fn) {
    return NULL;
  }

  // Construct full path
  char fp[MAX_PATH + 1] = { 0 };
  snprintf(fp, (sizeof(char) * MAX_PATH), "%s%s%s", tp, fn, ext ? ext : "");
  fp[MAX_PATH] = 0;
  free(fn);

  *path = strdup(fp);

  // Open file
  return fopen(fp, mode);
}

// Opens a temporary file cached by the filesystem, shouldn't normally cache to disk
FILE* fmemopen(void* buf, size_t len, const char* mode) {
  // Uses a temporary file to simulate fmemopen, though FILE_ATTRIBUTE_TEMPORARY should keep it cached in RAM
  // Only supports 'b' in mode

  int fd;
  FILE *fp;
  char tp[MAX_PATH - 32];
  char fn[MAX_PATH + 1];
  HANDLE h;

  (void)mode;

  if (!GetTempPath(sizeof(tp), tp)) {
    return NULL;
  }

  if (!GetTempFileName(tp, "xcc", 0, fn)) {
    return NULL;
  }

  h = CreateFile(fn, GENERIC_READ | GENERIC_WRITE, 0, NULL, CREATE_ALWAYS, FILE_ATTRIBUTE_TEMPORARY | FILE_FLAG_DELETE_ON_CLOSE, NULL);
  if (INVALID_HANDLE_VALUE == h) {
    return NULL;
  }

  fd = _open_osfhandle((intptr_t)h, _O_APPEND);
  if (fd < 0) {
    CloseHandle(h);
    return NULL;
  }

  if (strchr(mode, 'b') != NULL) {
    fp = fdopen(fd, "wb+");
  } else {
    fp = fdopen(fd, "w+");
  }
  if (!fp) {
    CloseHandle(h);
    return NULL;
  }

  if (buf != NULL && len > 0) {
    fwrite(buf, len, 1, fp);
    rewind(fp);
  }

  return fp;
}

// Creates a temporary file, opens it in write-binary mode
FILE* platform_mktempfile(void) {
  // Opens a temporary file in write and binary mode cached by the filesystem, shouldn't normally cache to disk
  return fmemopen(NULL, 0, "wb");
}

FILE* open_memstream(char** data, size_t* len) {
  // Dummy, actually handled in flush_memstream on this platform, and wrap fmemopen instead
  (void)data;
  (void)len;
  // Opens a temporary file in write and binary mode cached by the filesystem, shouldn't normally cache to disk
  return platform_mktempfile();
}

// Flushes the temporary file and ensures the data pointer contains a pointer to the contents of the buffer if necessary
void flush_memstream(FILE* file, char** data, size_t* len) {
  if (!file || !data || !len) {
    return;
  }

  // Get file length
  if (fseek(file, 0, SEEK_END) != 0) {
    return;
  }
  const size_t file_len = ftell(file);

  // Allocate buffer
  char* buffer = (char*)malloc(file_len + 1);
  if (!buffer) {
    return;
  }

  // Read entire file into buffer
  if (fseek(file, 0, SEEK_SET) != 0) {
    free(buffer);
    return;
  }
  size_t nread = fread(buffer, 1, file_len, file);
  if (nread != file_len) {
    free(buffer);
    return;
  }
  buffer[file_len] = 0;

  *data = buffer;
  *len = file_len;
}

char* platform_getcwd(char* buf, size_t size) {
  return getcwd(buf, size);
}

bool platform_is_fullpath(const char *filename) {
  return !PathIsRelativeA(filename);
}

bool platform_is_rootpath(const char* path) {
  // Returns whether this path originates from the root
  return platform_is_fullpath(path);
}

bool platform_cmp_path(const char* str1, const char *str2) {
  // Consider forward and backward slashes identical
  while (*str1) {
    if (!((*str1 == *str2) || (*str1 == '/' && *str2 == '\\') || (*str1 == '\\' && *str2 == '/'))) {
      return false;
    }
    str1++;
    str2++;
  }
  return true;
}

bool platform_wait_process(pid_t pid, int *result) {
  if (!result) {
    return false;
  }
  
  *result = -1;
  if (!SUCCEEDED(_cwait(result, pid, WAIT_CHILD)))
    return false;
  return true;
}

pid_t platform_wait_process_any(pid_t *pids, size_t pids_len, int *result) {
  if (!result || !pids_len) {
    return -1;
  }

  // pids -> handles
  HANDLE* handles = (HANDLE*)malloc(sizeof(HANDLE) * pids_len);  
  if (!handles) {
    return -1;
  }
  memset(handles, 0, sizeof(HANDLE) * pids_len);

  // Wait for any process to signal
  DWORD res = WaitForMultipleObjects(pids_len, handles, FALSE, INFINITE);
  if (/*res >= WAIT_OBJECT_0 &&*/ res < (WAIT_OBJECT_0 + pids_len)) {
    DWORD dwResult = 0;
    GetExitCodeProcess(handles[res - WAIT_OBJECT_0], &dwResult);
    *result = dwResult;
    free(handles);
    return pids[res - WAIT_OBJECT_0];
  }

  // Failure
  free(handles);
  return -1;
}

int platform_spawnvp(const char* cname, const char* const* argv, const int handle_in, const int handle_out, const int handle_err) {
    int ret;
    STARTUPINFO StartupInfo;
    PROCESS_INFORMATION ProcessInformation;
    char *cmd;
    size_t clen = 0;

    // Count number of argv elements by using NULL terminator
    while(argv[clen] != NULL) {
      ++clen;
    }
    if (clen < 1) {
      // At least one argument (program name) required
      return -1;
    }

    // Join argv into full command line string
    {
      // Calculate total string length
      size_t total_length = 0;
      for (size_t i = 0; i < clen; ++i) {
        total_length += strlen(argv[i]);
      }

      // Add spaces (except last) and terminator
      total_length += (clen - 1) + 1;

      // Allocate
      cmd = (char*)malloc(total_length * sizeof(char));
      if (cmd == NULL) {
        return -1;
      }
      cmd[0] = 0;

      // Concatenate
      for (size_t i = 0; i < clen; ++i) {
        strcat(cmd, argv[i]);
        if (i < (clen - 1)) {
          strcat(cmd, " ");
        }
      }
    }

    memset(&StartupInfo, 0, sizeof(StartupInfo));
    StartupInfo.cb = sizeof(StartupInfo);
    StartupInfo.wShowWindow = FALSE;
    StartupInfo.hStdInput = handle_in != -1 ? (HANDLE)_get_osfhandle(handle_in) : GetStdHandle(STD_INPUT_HANDLE);
    StartupInfo.hStdOutput  = handle_out != -1 ? (HANDLE)_get_osfhandle(handle_out) : GetStdHandle(STD_OUTPUT_HANDLE);
    StartupInfo.hStdError = handle_err != -1 ? (HANDLE)_get_osfhandle(handle_err) : GetStdHandle(STD_ERROR_HANDLE);
    if (!(StartupInfo.hStdInput == INVALID_HANDLE_VALUE &&
        StartupInfo.hStdOutput == INVALID_HANDLE_VALUE &&
        StartupInfo.hStdError == INVALID_HANDLE_VALUE))
    {
        StartupInfo.dwFlags |= STARTF_USESTDHANDLES;
    }
    if (!CreateProcess(cname,   /* search PATH to find executable */
                       cmd,   /* executable, and its arguments */
                       NULL,    /* process attributes */
                       NULL,    /* thread attributes */
                       TRUE,    /* inherit handles */
                       CREATE_NO_WINDOW,    /* creation flags */
                       NULL, /* inherit environment */
                       NULL,   /* inherit cwd */
                       &StartupInfo,
                       &ProcessInformation))
    {
      return -1;
    }
    ret = (int)ProcessInformation.dwProcessId;
    CloseHandle(ProcessInformation.hThread);
    free(cmd);
    return ret;
}

void platform_kill(int pid) {
  HANDLE handle = OpenProcess(PROCESS_TERMINATE, FALSE, (DWORD)pid);
  if (handle == NULL) {
    // Failure
    return;
  }
  TerminateProcess(handle, 1);
}

#else
//////////////////////////////////////////////////////////////////////////////
// Platform specific implementations
//////////////////////////////////////////////////////////////////////////////
//
// POSIX
//
#include <unistd.h>

// Creates a temporary file with a given extension and returns the path
FILE* platform_mktempfile2(const char* ext, char** path, const char* mode) {
  if (!path) {
    return NULL;
  }

  // Construct full path template
  const size_t MAX_PATH = 255;
  char fp[MAX_PATH + 1];
  snprintf(fp, (sizeof(char) * MAX_PATH), "/tmp/xcc-XXXXXX%s", ext ? ext : "");
  fp[MAX_PATH] = 0;

  // Create temporary file
  int fd = mkstemps(fp, ext ? strlen(ext) : 0);
  *path = strdup(fp);
  return fdopen(fd, mode);
}

// Creates a temporary file, opens it in write-binary mode
FILE* platform_mktempfile(void) {
  return tmpfile();
}

// Flushes the temporary file and ensures the data pointer contains a pointer to the contents of the buffer if necessary
void flush_memstream(FILE* file, char** data, size_t* len) {
  (void)file;
  (void)data;
  (void)len;
  // This function is a nop because on POSIX, the memstream does this on open
}

char* platform_getcwd(char* buf, size_t size) {
  return getcwd(buf, size);
}

bool platform_is_fullpath(const char* filename) {
  // Path without any .. operators
  if (*filename != '/')
    return false;
  for (const char *p = filename;;) {
    p = strstr(p, "/..");
    if (p == NULL)
      return true;
    if (p[3] == '/' || p[3] == '\0')
      return false;
    p += 3;
  }
}

bool platform_is_rootpath(const char* path) {
  // Returns whether this path originates from the root
  return *path == '/';
}

bool platform_cmp_path(const char* a, const char* b) {
  return strcmp(a, b) == 0;
}

// Unsupported functions for WASM targets
#ifndef __WASM
#include <signal.h>
#include <sys/wait.h>

bool platform_wait_process(pid_t pid, int* result) {
  if (waitpid(pid, result, 0) < 0) {
    return false;
  }
  return true;
}

pid_t platform_wait_process_any(pid_t* pids, size_t pids_len, int* result) {
  (void)pids;
  (void)pids_len;

  // Assumes all processes have been forked, so waitpid will do
  *result = -1;
  return waitpid(0, result, 0);
}

void platform_kill(int pid) {
  kill(pid, SIGKILL);
}

#endif

#endif
