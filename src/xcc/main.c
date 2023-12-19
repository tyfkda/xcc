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

  // When src is NULL, no input file is given and cpp read from stdin.
  cpp_cmd->data[cpp_cmd->len - 2] = (void *)src;
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
            kill(cc_pid, SIGKILL);
            break;
          }
          close(cc_fd[0]);
          close(cc_fd[1]);
        }
      } else if (done == cc_pid) {
        cc_pid = -1;
        if (cpp_pid != -1) {
          // Illegal: cc dies earlier than cpp.
          kill(cpp_pid, SIGKILL);
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
    kill(as_pid, SIGKILL);
    remove(ofn);
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
      char *p = malloc_or_die(len + 3);
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

static const char *get_exe_prefix(const char *path) {
  static const char XCC[] = "xcc";
  size_t len = strlen(path);
  const char *p = &path[len];
  // endswith
  if (len > sizeof(XCC)) {
    const char *q = p - (sizeof(XCC) - 1);
    if (strcmp(q, XCC) == 0)
      p = q;
  }
  return p;
}

static char *join_exe_prefix(const char *xccpath, const char *prefix, const char *fn) {
  StringBuffer sb;
  sb_init(&sb);
  sb_append(&sb, xccpath, prefix);
  sb_append(&sb, fn, NULL);
  return sb_to_string(&sb);
}

int main(int argc, char *argv[]) {
  const char *xccpath = argv[0];
  const char *root = dirname(strdup(xccpath));
  const char *prefix = get_exe_prefix(xccpath);
  char *cpp_path = join_exe_prefix(xccpath, prefix, "cpp");
  char *cc1_path = join_exe_prefix(xccpath, prefix, "cc1");
#if !defined(AS_USE_CC)
  char *as_path = join_exe_prefix(xccpath, prefix, "as");
  char *ld_path = join_exe_prefix(xccpath, prefix, "ld");
#elif defined(HOST_CC_PREFIX)
#define S(x)   S2(x)
#define S2(x)  #x
  char *as_path = S(HOST_CC_PREFIX) "as";
  char *ld_path = S(HOST_CC_PREFIX) "gcc";
#undef S2
#undef S
#else
  char *as_path = "/usr/bin/as";
  char *ld_path = "/usr/bin/cc";
#endif
  bool nodefaultlibs = false, nostdlib = false, nostdinc = false;
  bool use_ld = false;

  Vector *cpp_cmd = new_vector();
  vec_push(cpp_cmd, cpp_path);
  vec_push(cpp_cmd, "-D__LP64__");  // Memory model.
#if XCC_TARGET_ARCH == XCC_ARCH_X64
  vec_push(cpp_cmd, "-D__x86_64__");
#elif XCC_TARGET_ARCH == XCC_ARCH_AARCH64
  vec_push(cpp_cmd, "-D__aarch64__");
#elif XCC_TARGET_ARCH == XCC_ARCH_RISCV64
  vec_push(cpp_cmd, "-D__riscv");
#endif
#if XCC_TARGET_PLATFORM == XCC_PLATFORM_APPLE
  vec_push(cpp_cmd, "-D__APPLE__");
#elif XCC_TARGET_PLATFORM == XCC_PLATFORM_POSIX
  vec_push(cpp_cmd, "-D__linux__");
#endif

  Vector *cc1_cmd = new_vector();
  vec_push(cc1_cmd, cc1_path);

  Vector *as_cmd = new_vector();
  vec_push(as_cmd, as_path);

  Vector *ld_cmd = new_vector();
  vec_push(ld_cmd, ld_path);

  enum OutType out_type = OutExecutable;

  enum SourceType {
    UnknownSource,
    Assembly,
    Clanguage,
    ObjectFile,
    ArchiveFile,
  };
  enum SourceType src_type = UnknownSource;

  const char *ofn = NULL;

  enum {
    OPT_HELP = 128,
    OPT_VERSION,
    OPT_NODEFAULTLIBS,
    OPT_NOSTDLIB,
    OPT_NOSTDINC,
    OPT_ISYSTEM,
    OPT_IDIRAFTER,
    OPT_LINKOPTION,

    OPT_ANSI,
    OPT_STD,
    OPT_PEDANTIC,
    OPT_MMD,
    OPT_NO_PIE,
  };

  static const struct option options[] = {
    {"c", no_argument},  // Output .o
    {"E", no_argument},  // Output preprocess result
    {"S", no_argument},  // Output assembly code
    {"I", required_argument},  // Add include path
    {"isystem", required_argument, OPT_ISYSTEM},  // Add system include path
    {"idirafter", required_argument, OPT_IDIRAFTER},  // Add include path (after)
    {"D", required_argument},  // Define macro
    {"o", required_argument},  // Specify output filename
    {"x", required_argument},  // Specify code type
    {"l", required_argument},  // Library
    {"nodefaultlibs", no_argument, OPT_NODEFAULTLIBS},
    {"nostdlib", no_argument, OPT_NOSTDLIB},
    {"nostdinc", no_argument, OPT_NOSTDINC},
    {"Xlinker", required_argument, OPT_LINKOPTION},
    {"-help", no_argument, OPT_HELP},
    {"-version", no_argument, OPT_VERSION},

    // Suppress warnings
    {"W", required_argument},
    {"O", optional_argument},
    {"g", optional_argument},  // Debug info
    {"f", required_argument},
    {"ansi", no_argument, OPT_ANSI},
    {"std", optional_argument, OPT_STD},
    {"pedantic", no_argument, OPT_PEDANTIC},
    {"MMD", no_argument, OPT_MMD},
    {"no-pie", no_argument, OPT_NO_PIE},

    {NULL},
  };
  Vector *sources = new_vector();
  Vector *linker_options = new_vector();
  for (;;) {
    int opt = optparse(argc, argv, options);
    if (opt == -1) {
      if (optind >= argc)
        break;

      vec_push(sources, argv[optind++]);
      continue;
    }

    switch (opt) {
    default: assert(false); break;
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
    case OPT_ISYSTEM:
      vec_push(cpp_cmd, "-isystem");
      vec_push(cpp_cmd, optarg);
      break;
    case OPT_IDIRAFTER:
      vec_push(cpp_cmd, "-idirafter");
      vec_push(cpp_cmd, optarg);
      break;
    case 'D':
      vec_push(cpp_cmd, "-D");
      vec_push(cpp_cmd, optarg);
      break;
    case 'o':
      ofn = optarg;
      vec_push(linker_options, "-o");
      vec_push(linker_options, ofn);
      break;
    case 'c':
      out_type = OutObject;
      // vec_push(as_cmd, "-c");
      break;
    case 'E':
      out_type = OutPreprocess;
      if (src_type == UnknownSource)
        src_type = Clanguage;
      break;
    case 'S':
      out_type = OutAssembly;
      break;
    case 'x':
      if (strcmp(optarg, "c") == 0) {
        src_type = Clanguage;
      } else if (strcmp(optarg, "assembler") == 0) {
        src_type = Assembly;
      } else {
        error("language not recognized: %s", optarg);
      }
      break;
    case 'W':
      if (strncmp(argv[optind - 1], "-Wl,", 4) == 0) {
        if (use_ld)
          vec_push(linker_options, argv[optind - 1]);
        else
          vec_push(ld_cmd, argv[optind - 1] + 4);
      } else {
        vec_push(cc1_cmd, "-W");
        vec_push(cc1_cmd, optarg);
      }
      break;
    case OPT_NODEFAULTLIBS:
      nodefaultlibs = true;
      vec_push(linker_options, "-nodefaultlibs");
      break;
    case OPT_NOSTDLIB:
      nostdlib = true;
      vec_push(linker_options, "-nostdlib");
      break;
    case OPT_NOSTDINC:
      nostdinc = true;
      break;
    case OPT_LINKOPTION:
      vec_push(ld_cmd, optarg);
      break;
    case 'f':
      if (strncmp(optarg, "use-ld", 6) == 0) {
        if (optarg[6] == '=') {
          ld_cmd->data[0] = &optarg[7];
        } else if (optarg[6] == '\0' && optind < argc) {
          ld_cmd->data[0] = argv[optind++];
        } else {
          fprintf(stderr, "extra argument required for '-fuse-ld");
        }
        use_ld = true;
      } else {
        vec_push(linker_options, argv[optind - 1]);
      }
      break;
    case 'l':
      if (strncmp(argv[optind - 1], "-l", 2) == 0) {
        // -lfoobar
        // file order matters, so add to sources.
        vec_push(sources, argv[optind - 1]);
      } else {
        assert(!"TODO");
      }
      break;
    case '?':
      if (strcmp(argv[optind - 1], "-") == 0) {
        if (src_type == UnknownSource) {
          error("-x required");
        }
        vec_push(sources, NULL);
      } else {
        fprintf(stderr, "Warning: unknown option: %s\n", argv[optind - 1]);
        vec_push(linker_options, argv[optind - 1]);
      }
      break;

    case 'O':
    case 'g':
    case OPT_ANSI:
    case OPT_STD:
    case OPT_PEDANTIC:
    case OPT_MMD:
    case OPT_NO_PIE:
      // Silently ignored.
      vec_push(linker_options, argv[optind - 1]);
      break;
    }
  }

  if (sources->len == 0) {
    fprintf(stderr, "No input files\n\n");
    usage(stderr);
    return 1;
  }

  if (!nostdinc) {
    vec_push(cpp_cmd, "-idirafter");
    vec_push(cpp_cmd, JOIN_PATHS(root, "include"));
  }

  vec_push(cpp_cmd, NULL);  // Buffer for src.
  vec_push(cpp_cmd, NULL);  // Terminator.
  vec_push(cc1_cmd, NULL);  // Buffer for label prefix.
  vec_push(cc1_cmd, NULL);  // Terminator.
  vec_push(as_cmd, "-o");
  vec_push(as_cmd, ofn);
  vec_push(as_cmd, NULL);  // Terminator.

  if (use_ld) {
    // Pass through command line options.
    for (int i = 0; i < linker_options->len; ++i)
      vec_push(ld_cmd, linker_options->data[i]);
  } else {
    vec_push(ld_cmd, "-o");
    vec_push(ld_cmd, ofn != NULL ? ofn : "a.out");
  }

  int ofd = STDOUT_FILENO;

  if (out_type >= OutExecutable && !use_ld) {
#if !defined(AS_USE_CC) || defined(NO_STD_LIB)
    if (!nostdlib)
      vec_push(sources, JOIN_PATHS(root, "lib/crt0.a"));
    if (!nodefaultlibs && !nostdlib)
      vec_push(sources, JOIN_PATHS(root, "lib/libc.a"));
# if defined(NO_STD_LIB)
    vec_push(ld_cmd, "-nostdlib");
# endif
#else
    UNUSED(nodefaultlibs);
    UNUSED(nostdlib);
#endif
  }

  int res = 0;
  for (int i = 0; i < sources->len; ++i) {
    char *src = sources->data[i];
    const char *outfn = ofn;
    if (src != NULL) {
      if (*src == '\0')
        continue;
      if (*src == '-') {
        assert(src[1] == 'l');
        vec_push(ld_cmd, src);
        continue;
      }

      if (outfn == NULL) {
        if (out_type == OutObject)
          outfn = change_ext(basename(src), "o");
        else if (out_type == OutAssembly)
          outfn = change_ext(basename(src), "s");
      }
    }

    if (out_type <= OutAssembly && outfn != NULL && strcmp(outfn, "-") != 0) {
      close(STDOUT_FILENO);
      ofd = open(outfn, O_WRONLY | O_CREAT | O_TRUNC, S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH);
      if (ofd == -1) {
        perror("Failed to open output file");
        exit(1);
      }
    }

    enum SourceType st = src_type;
    if (src != NULL) {
      char *ext = get_ext(src);
      if      (strcasecmp(ext, "c") == 0)  st = Clanguage;
      else if (strcasecmp(ext, "s") == 0)  st = Assembly;
      else if (strcasecmp(ext, "o") == 0)  st = ObjectFile;
      else if (strcasecmp(ext, "a") == 0)  st = ArchiveFile;
    }

    switch (st) {
    case UnknownSource:
      fprintf(stderr, "Unknown source type: %s\n", src);
      res = -1;
      break;
    case Clanguage:
      res = compile_csource(src, out_type, outfn, ofd, cpp_cmd, cc1_cmd, as_cmd, ld_cmd);
      break;
    case Assembly:
      res = compile_asm(src, out_type, outfn, ofd, as_cmd, ld_cmd);
      break;
    case ObjectFile:
    case ArchiveFile:
      if (out_type >= OutExecutable)
        vec_push(ld_cmd, src);
      break;
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
