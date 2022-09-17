#include "../config.h"

#include <assert.h>
#include <fcntl.h>  // open
#include <libgen.h>  // dirname
#include <signal.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

#include "util.h"

#if !defined(__XV6) && !defined(__linux__)

#define AS_USE_CC

#endif

static pid_t fork1(void) {
  pid_t pid = fork();
  if (pid < 0)
    error("fork failed");
  return pid;
}

static int wait_process(pid_t pid) {
  int ec = -1;
  if (waitpid(pid, &ec, 0) < 0)
    error("wait failed");
  return ec;
}

static pid_t wait_child(int *result) {
  *result = -1;
  return waitpid(0, result, 0);
}

// command > ofd
static pid_t exec_with_ofd(char **command, int ofd) {
  pid_t pid = fork1();
  if (pid == 0) {
    if (ofd >= 0 && ofd != STDOUT_FILENO) {
      close(STDOUT_FILENO);
      if (dup(ofd) == -1)
        error("dup failed");
    }

    if (execvp(command[0], command) < 0) {
      perror(command[0]);
      exit(1);
    }
  }
  return pid;
}

// | command > ofd
static pid_t pipe_exec(char **command, int ofd, int fd[2]) {
  if (pipe(fd) < 0)
    error("pipe failed");

  pid_t pid = fork1();
  if (pid == 0) {
    close(STDIN_FILENO);
    if (dup(fd[0]) == -1)
      error("dup failed");

    if (ofd >= 0 && ofd != STDOUT_FILENO) {
      close(STDOUT_FILENO);
      if (dup(ofd) == -1)
        error("dup failed");
    }

    close(fd[0]);
    close(fd[1]);
    if (execvp(command[0], command) < 0) {
      perror(command[0]);
      exit(1);
    }
  }
  return pid;
}

static int compile(const char *src, Vector *cpp_cmd, Vector *cc1_cmd, int ofd) {
  int ofd2 = ofd;
  int cc_fd[2];
  pid_t cc_pid = -1;
  int running = 0;
  if (cc1_cmd != NULL) {
    cc_pid = pipe_exec((char**)cc1_cmd->data, ofd, cc_fd);
    ofd2 = cc_fd[1];
    ++running;
  }

  cpp_cmd->data[cpp_cmd->len - 2] = (void*)src;
  pid_t cpp_pid = exec_with_ofd((char**)cpp_cmd->data, ofd2);
  ++running;

  int res = 0;
  for (; running > 0; --running) {
    int r = 0;
    pid_t done = wait_child(&r);  // cpp or cc1
    if (done > 0) {
      res |= r;
      if (done == cpp_pid) {
        cpp_pid = -1;
        if (cc_pid != -1) {
          if (r != 0) {
            // Illegal: cpp exit with failure.
#if !defined(__XV6)
            kill(cc_pid, SIGKILL);
#endif
            break;
          }
          close(cc_fd[0]);
          close(cc_fd[1]);
        }
      } else if (done == cc_pid) {
        cc_pid = -1;
        if (cpp_pid != -1) {
          // Illegal: cc dies earlier than cpp.
#if !defined(__XV6)
          kill(cpp_pid, SIGKILL);
#endif
          break;
        }
      }
    } else {
      res |= 1;
    }
  }
  return res;
}

static void usage(FILE *fp) {
  fprintf(
      fp,
      "Usage: xcc [options] file...\n"
      "Options:\n"
      "  -I <path>           Add include path\n"
      "  -D <label[=value]>  Define label\n"
      "  -o <filename>       Set output filename (Default: a.out)\n"
      "  -c                  Output object file\n"
      "  -S                  Output assembly code\n"
      "  -E                  Output preprocess result\n"
  );
}

enum OutType {
  OutPreprocess,
  OutAssembly,
  OutObject,
  OutExecutable,
};

static int compile_csource(const char *source_fn, enum OutType out_type, const char *ofn, int ofd,
                           Vector *cpp_cmd, Vector *cc1_cmd, Vector *as_cmd, Vector *ld_cmd) {
  const char *objfn = NULL;
  int obj_fd = -1;
  if (out_type > OutAssembly) {
    if (ofn != NULL && out_type < OutExecutable) {
      objfn = ofn;
    } else {
      char template[] = "/tmp/xcc-XXXXXX.o";
      obj_fd = mkstemps(template, 2);
      if (obj_fd == -1) {
        perror("Failed to open output file");
        exit(1);
      }
      objfn = strdup(template);
    }
  }

  int as_fd[2];
  pid_t as_pid = -1;

  if (out_type > OutAssembly) {
    as_cmd->data[as_cmd->len - 2] = (void*)objfn;
    as_pid = pipe_exec((char**)as_cmd->data, -1, as_fd);
    ofd = as_fd[1];
  }

  int res = compile(source_fn, cpp_cmd, out_type == OutPreprocess ? NULL : cc1_cmd, ofd);

  if (out_type >= OutExecutable)
    vec_push(ld_cmd, objfn);

  if (res != 0 && as_pid != -1) {
#if !defined(__XV6)
    kill(as_pid, SIGKILL);
    remove(ofn);
#endif
  }
  if (as_pid != -1) {
    close(as_fd[0]);
    close(as_fd[1]);
    as_pid = -1;
    res |= wait_process(as_pid);
  }
  if (obj_fd != -1) {
    close(obj_fd);
  }
  return res;
}

static int compile_asm(const char *source_fn, enum OutType out_type, const char *ofn, int ofd,
                       Vector *as_cmd, Vector *ld_cmd) {
  const char *objfn = NULL;
  if (out_type > OutAssembly) {
    if (ofn != NULL && out_type < OutExecutable) {
      objfn = ofn;
    } else {
      size_t len = strlen(source_fn);
      char *p = malloc(len + 3);
      memcpy(p, source_fn, len);
      strcpy(p + len, ".o");
      objfn = p;
    }
    as_cmd->data[as_cmd->len - 2] = (void*)objfn;
  }

  vec_pop(as_cmd);
  vec_push(as_cmd, source_fn);
  vec_push(as_cmd, NULL);

  int res = 0;
  pid_t as_pid = exec_with_ofd((char**)as_cmd->data, ofd);
  waitpid(as_pid, &res, 0);

  vec_pop(as_cmd);
  vec_pop(as_cmd);
  vec_push(as_cmd, NULL);

  if (out_type >= OutExecutable)
    vec_push(ld_cmd, objfn);
  return res;
}

int main(int argc, char *argv[]) {
  const char *root = dirname(strdup(argv[0]));
  char *cpp_path = cat_path(root, "cpp");
  char *cc1_path = cat_path(root, "cc1");
#if !defined(AS_USE_CC)
  char *as_path = cat_path(root, "as");
  char *ld_path = cat_path(root, "ld");
#else
  char *as_path = "as";
  char *ld_path = "cc";
#endif
  bool nodefaultlibs = false, nostdlib = false;

  Vector *cpp_cmd = new_vector();
  vec_push(cpp_cmd, cpp_path);
  vec_push(cpp_cmd, "-D__LP64__");  // Memory model.
  vec_push(cpp_cmd, "-I");
  vec_push(cpp_cmd, cat_path(root, "include"));

  Vector *cc1_cmd = new_vector();
  vec_push(cc1_cmd, cc1_path);

  Vector *as_cmd = new_vector();
  vec_push(as_cmd, as_path);

  Vector *ld_cmd = new_vector();
  vec_push(ld_cmd, ld_path);

  enum OutType out_type = OutExecutable;

  const char *ofn = NULL;

  enum {
    OPT_HELP = 128,
    OPT_VERSION,
    OPT_NODEFAULTLIBS,
    OPT_NOSTDLIB,
  };

  static const struct option options[] = {
    {"c", no_argument},  // Output .o
    {"E", no_argument},  // Output preprocess result
    {"S", no_argument},  // Output assembly code
    {"I", required_argument},  // Add include path
    {"D", required_argument},  // Define macro
    {"o", required_argument},  // Specify output filename
    {"nodefaultlibs", no_argument, OPT_NODEFAULTLIBS},
    {"nostdlib", no_argument, OPT_NOSTDLIB},
    {"-help", no_argument, OPT_HELP},
    {"-version", no_argument, OPT_VERSION},
    {NULL},
  };
  int opt;
  while ((opt = optparse(argc, argv, options)) != -1) {
    switch (opt) {
    case OPT_HELP:
      usage(stdout);
      return 0;
    case OPT_VERSION:
      show_version("xcc");
      return 0;
    case 'I':
      vec_push(cpp_cmd, "-I");
      vec_push(cpp_cmd, optarg);
      break;
    case 'D':
      vec_push(cpp_cmd, "-D");
      vec_push(cpp_cmd, optarg);
      break;
    case 'o':
      ofn = optarg;
      break;
    case 'c':
      out_type = OutObject;
      // vec_push(as_cmd, "-c");
      break;
    case 'E':
      out_type = OutPreprocess;
      break;
    case 'S':
      out_type = OutAssembly;
      break;
    case OPT_NODEFAULTLIBS:
      nodefaultlibs = true;
      break;
    case OPT_NOSTDLIB:
      nostdlib = true;
      break;
    default:
      fprintf(stderr, "Warning: unknown option: %s\n", argv[optind - 1]);
      break;
    }
  }

  int iarg = optind;
  if (iarg >= argc) {
    fprintf(stderr, "No input files\n\n");
    usage(stderr);
    return 1;
  }

  if (ofn == NULL) {
    if (out_type == OutObject) {
      ofn = change_ext(basename(argv[iarg]), "o");
    } else if (out_type == OutAssembly) {
      ofn = change_ext(basename(argv[iarg]), "s");
    }
  }

  vec_push(cpp_cmd, NULL);  // Buffer for src.
  vec_push(cpp_cmd, NULL);  // Terminator.
  vec_push(cc1_cmd, NULL);  // Buffer for label prefix.
  vec_push(cc1_cmd, NULL);  // Terminator.
  vec_push(as_cmd, "-o");
  vec_push(as_cmd, ofn);
  vec_push(as_cmd, NULL);  // Terminator.
  vec_push(ld_cmd, "-o");
  vec_push(ld_cmd, ofn != NULL ? ofn : "a.out");

  int ofd = STDOUT_FILENO;

  if (out_type == OutAssembly) {
#if !defined(__XCC) && !defined(__XV6)
    close(STDOUT_FILENO);
    ofd = open(ofn, O_WRONLY | O_CREAT | O_TRUNC, S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH);
    if (ofd == -1) {
      perror("Failed to open output file");
      exit(1);
    }
#endif
  }

  Vector *sources = new_vector();
  for (int i = iarg; i < argc; ++i) {
    vec_push(sources, argv[i]);
  }
#ifndef AS_USE_CC
  if (out_type >= OutExecutable) {
    if (!nostdlib)
      vec_push(sources, cat_path(root, "lib/crt0.a"));
    if (!nodefaultlibs && !nostdlib)
      vec_push(sources, cat_path(root, "lib/libc.a"));

  }
#else
  UNUSED(nodefaultlibs);
  UNUSED(nostdlib);
#endif

  int res = 0;
  for (int i = 0; i < sources->len; ++i) {
    char *src = sources->data[i];
    char *ext = get_ext(src);
    if (strcasecmp(ext, "c") == 0) {
      res = compile_csource(src, out_type, ofn, ofd, cpp_cmd, cc1_cmd, as_cmd, ld_cmd);
    } else if (strcasecmp(ext, "s") == 0) {
      res = compile_asm(src, out_type, ofn, ofd, as_cmd, ld_cmd);
    } else if (strcasecmp(ext, "o") == 0 || strcasecmp(ext, "a") == 0)  {
      if (out_type >= OutExecutable)
        vec_push(ld_cmd, src);
    } else {
      fprintf(stderr, "Unsupported file type: %s\n", src);
      res = -1;
    }
    if (res != 0)
      break;
  }

  if (res == 0 && out_type >= OutExecutable) {
    vec_push(ld_cmd, NULL);
    pid_t ld_pid = exec_with_ofd((char**)ld_cmd->data, -1);
    waitpid(ld_pid, &res, 0);
  }

  return res == 0 ? 0 : 1;
}
