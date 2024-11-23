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

  // Hack: AT_REMOVEDIR defined in riscv-gnu-toolchain differs on MacOS and Linux?
#if defined(__APPLE__) && !defined(__AT_REMOVEDIR)
#define __AT_REMOVEDIR  0x080
#endif

static pid_t fork1(void) {
  pid_t pid = fork();
  if (pid < 0)
    error("fork failed");
  return pid;
}

static int wait_process(pid_t pid) {
  int status = -1;
  if (waitpid(pid, &status, 0) < 0)
    error("wait failed");
  return status;
}

static pid_t wait_child(int *status) {
  *status = -1;
  return waitpid(0, status, 0);
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

static Vector remove_on_exit;

static void remove_tmp_files(void) {
  for (int i = 0; i < remove_on_exit.len; ++i) {
    const char *fn = remove_on_exit.data[i];
    remove(fn);
  }
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
  cpp_cmd->data[cpp_cmd->len - 2] = strcmp(src, "-") == 0 ? NULL : (void*)src;
  pid_t cpp_pid = exec_with_ofd((char**)cpp_cmd->data, ofd2);
  ++running;

  int res = 0;
  for (; running > 0; --running) {
    int status;
    pid_t done = wait_child(&status);  // cpp or cc1
    if (done > 0) {
      res |= status;
      if (done == cpp_pid) {
        cpp_pid = -1;
        if (cc_pid != -1) {
          if (status != 0) {
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
      vec_push(&remove_on_exit, objfn);
    }
  }

  int as_fd[2];
  pid_t as_pid = -1;

  if (out_type > OutAssembly) {
    assert(as_cmd->len >= 3);
    as_cmd->data[as_cmd->len - 3] = (void*)objfn;  // Overwrite output filename.
    as_cmd->data[as_cmd->len - 2] = "-";  // Overwrite source filename.
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

    assert(as_cmd->len >= 3);
    as_cmd->data[as_cmd->len - 3] = (void*)objfn;  // Overwrite output filename.
    as_cmd->data[as_cmd->len - 2] = (void*)source_fn;  // Overwrite source filename.
  }

  int status = 0;
  pid_t as_pid = exec_with_ofd((char**)as_cmd->data, ofd);
  waitpid(as_pid, &status, 0);

  if (out_type >= OutExecutable)
    vec_push(ld_cmd, objfn);
  return status;
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

enum SourceType {
  UnknownSource,
  Assembly,
  Clanguage,
  ObjectFile,
  ArchiveFile,
};

typedef struct {
  Vector *cpp_cmd;
  Vector *cc1_cmd;
  Vector *as_cmd;
  Vector *ld_cmd;
  Vector *sources;
  Vector *linker_options;
  const char *ofn;
  enum OutType out_type;
  enum SourceType src_type;
  bool nodefaultlibs, nostdlib, nostdinc;
  bool use_ld;
} Options;

static void parse_options(int argc, char *argv[], Options *opts) {
  enum {
    OPT_HELP = 128,
    OPT_VERSION,
    OPT_DUMP_VERSION,
    OPT_NODEFAULTLIBS,
    OPT_NOSTDLIB,
    OPT_NOSTDINC,
    OPT_ISYSTEM,
    OPT_IDIRAFTER,
    OPT_LINKOPTION,
    OPT_SHARED,

    OPT_ANSI,
    OPT_STD,
    OPT_PEDANTIC,
    OPT_MMD,
    OPT_NO_PIE,

    OPT_SSA,
  };

  static const struct option kOptions[] = {
    {"c", no_argument},  // Output .o
    {"E", no_argument},  // Output preprocess result
    {"S", no_argument},  // Output assembly code
    {"I", required_argument},  // Add include path
    {"isystem", required_argument, OPT_ISYSTEM},  // Add system include path
    {"idirafter", required_argument, OPT_IDIRAFTER},  // Add include path (after)
    {"D", required_argument},  // Define macro
    {"C", no_argument},  // Do not discard comments
    {"o", required_argument},  // Specify output filename
    {"x", required_argument},  // Specify code type
    {"O", optional_argument},  // Optimization level
    {"l", required_argument},  // Library
    {"L", required_argument},  // Add library path
    {"nodefaultlibs", no_argument, OPT_NODEFAULTLIBS},
    {"nostdlib", no_argument, OPT_NOSTDLIB},
    {"nostdinc", no_argument, OPT_NOSTDINC},
    {"Xlinker", required_argument, OPT_LINKOPTION},
    {"shared", no_argument, OPT_SHARED},
    {"-help", no_argument, OPT_HELP},
    {"-version", no_argument, OPT_VERSION},
    {"dumpversion", no_argument, OPT_DUMP_VERSION},

    // Sub command
    {"f", required_argument},
    {"W", required_argument},

    // Suppress warnings
    {"g", optional_argument},  // Debug info
    {"ansi", no_argument, OPT_ANSI},
    {"std", optional_argument, OPT_STD},
    {"pedantic", no_argument, OPT_PEDANTIC},
    {"MMD", no_argument, OPT_MMD},
    {"no-pie", no_argument, OPT_NO_PIE},

    // Feature flag.
    {"-apply-ssa", no_argument, OPT_SSA},

    {NULL},
  };

  for (;;) {
    int opt = optparse(argc, argv, kOptions);
    if (opt == -1) {
      if (optind >= argc)
        break;

      vec_push(opts->sources, argv[optind++]);
      continue;
    }

    switch (opt) {
    default: assert(false); break;
    case OPT_HELP:
      usage(stdout);
      exit(0);
    case OPT_VERSION:
      show_version("xcc");
      exit(0);
    case OPT_DUMP_VERSION:
      show_version(NULL);
      exit(0);
    case 'I':
      vec_push(opts->cpp_cmd, "-I");
      vec_push(opts->cpp_cmd, optarg);
      break;
    case OPT_ISYSTEM:
      vec_push(opts->cpp_cmd, "-isystem");
      vec_push(opts->cpp_cmd, optarg);
      break;
    case OPT_IDIRAFTER:
      vec_push(opts->cpp_cmd, "-idirafter");
      vec_push(opts->cpp_cmd, optarg);
      break;
    case 'D':
      vec_push(opts->cpp_cmd, "-D");
      vec_push(opts->cpp_cmd, optarg);
      break;
    case 'C':
      vec_push(opts->cpp_cmd, "-C");
      break;
    case 'o':
      opts->ofn = optarg;
      vec_push(opts->linker_options, "-o");
      vec_push(opts->linker_options, opts->ofn);
      break;
    case 'c':
      opts->out_type = OutObject;
      break;
    case 'E':
      opts->out_type = OutPreprocess;
      if (opts->src_type == UnknownSource)
        opts->src_type = Clanguage;
      break;
    case 'S':
      opts->out_type = OutAssembly;
      break;
    case 'x':
      if (strcmp(optarg, "c") == 0) {
        opts->src_type = Clanguage;
      } else if (strcmp(optarg, "assembler") == 0) {
        opts->src_type = Assembly;
      } else {
        error("language not recognized: %s", optarg);
      }
      break;
    case 'O':
      vec_push(opts->cc1_cmd, argv[optind - 1]);
      break;

    case OPT_NODEFAULTLIBS:
      opts->nodefaultlibs = true;
      vec_push(opts->linker_options, "-nodefaultlibs");
      break;
    case OPT_NOSTDLIB:
      opts->nostdlib = true;
      vec_push(opts->linker_options, "-nostdlib");
      break;
    case OPT_NOSTDINC:
      opts->nostdinc = true;
      break;
    case OPT_LINKOPTION:
      vec_push(opts->ld_cmd, optarg);
      break;
    case OPT_SHARED:
      vec_push(opts->linker_options, "-shared");
      break;

    case 'l':
      {
        char *p;
        if (strncmp(argv[optind - 1], "-l", 2) == 0) {
          // -lfoobar
          // file order matters, so add to sources.
          p = argv[optind - 1];
        } else {
          StringBuffer sb;
          sb_init(&sb);
          sb_append(&sb, "-l", optarg);
          sb_append(&sb, optarg, NULL);
          p = sb_to_string(&sb);
        }
        vec_push(opts->sources, p);
      }
      break;
    case 'L':
      vec_push(opts->ld_cmd, "-L");
      vec_push(opts->ld_cmd, optarg);
      break;
    case '?':
      if (strcmp(argv[optind - 1], "-") == 0) {
        if (opts->src_type == UnknownSource) {
          error("-x required");
        }
        vec_push(opts->sources, NULL);
      } else {
        fprintf(stderr, "Warning: unknown option: %s\n", argv[optind - 1]);
        vec_push(opts->linker_options, argv[optind - 1]);
      }
      break;
    case OPT_NO_PIE:
      vec_push(opts->ld_cmd, argv[optind - 1]);
      break;

    case 'f':
      if (strncmp(optarg, "use-ld", 6) == 0) {
        if (optarg[6] == '=') {
          opts->ld_cmd->data[0] = &optarg[7];
        } else if (optarg[6] == '\0' && optind < argc) {
          opts->ld_cmd->data[0] = argv[optind++];
        } else {
          fprintf(stderr, "extra argument required for '-fuse-ld");
        }
        opts->use_ld = true;
      } else {
        const char *opt = argv[optind - 1];  // TODO:
        vec_push(opts->cc1_cmd, opt);
        vec_push(opts->linker_options, opt);
      }
      break;

    case 'W':
      if (strncmp(argv[optind - 1], "-Wl,", 4) == 0) {
        if (opts->use_ld)
          vec_push(opts->linker_options, argv[optind - 1]);
        else
          vec_push(opts->ld_cmd, argv[optind - 1] + 4);
      } else {
        vec_push(opts->cc1_cmd, "-W");
        vec_push(opts->cc1_cmd, optarg);
      }
      break;

    case 'g':
    case OPT_ANSI:
    case OPT_STD:
    case OPT_PEDANTIC:
    case OPT_MMD:
      // Silently ignored.
      vec_push(opts->linker_options, argv[optind - 1]);
      break;

    case OPT_SSA:
      vec_push(opts->cc1_cmd, argv[optind - 1]);
      break;
    }
  }
}

static int do_compile(Options *opts, const char *root) {
  UNUSED(root);
  int ofd = STDOUT_FILENO;
  int res = 0;
  for (int i = 0; i < opts->sources->len; ++i) {
    char *src = opts->sources->data[i];
    const char *outfn = opts->ofn;
    if (src != NULL) {
      if (*src == '\0')
        continue;
      if (*src == '-' && src[1] != '\0') {
        assert(src[1] == 'l');
        // Pass to the linker.
        vec_push(opts->ld_cmd, src);
        continue;
      }

      if (outfn == NULL) {
        if (opts->out_type == OutObject)
          outfn = change_ext(basename(src), "o");
        else if (opts->out_type == OutAssembly)
          outfn = change_ext(basename(src), "s");
      }
    }

    if (opts->out_type <= OutAssembly && outfn != NULL && strcmp(outfn, "-") != 0) {
      close(STDOUT_FILENO);
      ofd = open(outfn, O_WRONLY | O_CREAT | O_TRUNC, S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH);
      if (ofd == -1) {
        perror("Failed to open output file");
        exit(1);
      }
    }

    enum SourceType st = opts->src_type;
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
      res = compile_csource(src, opts->out_type, outfn, ofd, opts->cpp_cmd, opts->cc1_cmd, opts->as_cmd, opts->ld_cmd);
      break;
    case Assembly:
      res = compile_asm(src, opts->out_type, outfn, ofd, opts->as_cmd, opts->ld_cmd);
      break;
    case ObjectFile:
    case ArchiveFile:
      if (opts->out_type >= OutExecutable)
        vec_push(opts->ld_cmd, src);
      break;
    }
    if (res != 0)
      break;
  }

  if (res == 0 && opts->out_type >= OutExecutable) {
    if (!opts->use_ld) {
#if !defined(USE_SYS_LD)
      if (!opts->nostdlib) {
        vec_push(opts->ld_cmd, JOIN_PATHS(root, "lib/crt0.a"));
        if (!opts->nodefaultlibs)
          vec_push(opts->ld_cmd, "-lc");
      }
#endif
    }

    vec_push(opts->ld_cmd, NULL);
    pid_t ld_pid = exec_with_ofd((char**)opts->ld_cmd->data, -1);
    int status;
    waitpid(ld_pid, &status, 0);
    res |= status;
  }

  return res == 0 ? 0 : 1;
}

#define STR(x)   STR2(x)
#define STR2(x)  #x

static void append_predefined_macros(Vector *cpp_cmd) {
  vec_push(cpp_cmd, "-D__XCC");
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

  vec_push(cpp_cmd, "-D__STDC__");
  vec_push(cpp_cmd, "-D__STDC_VERSION__=201112L");

  vec_push(cpp_cmd, "-D__SIZEOF_POINTER__=" STR(TARGET_POINTER_SIZE));
#if XCC_TARGET_PROGRAMMING_MODEL == XCC_PROGRAMMING_MODEL_ILP32
  vec_push(cpp_cmd, "-D__SIZEOF_INT__=4");
  vec_push(cpp_cmd, "-D__SIZEOF_LONG__=4");
  vec_push(cpp_cmd, "-D__SIZEOF_LONG_LONG__=8");
  vec_push(cpp_cmd, "-D__SIZEOF_SIZE_T__=4");
#elif XCC_TARGET_PROGRAMMING_MODEL == XCC_PROGRAMMING_MODEL_LP64
  vec_push(cpp_cmd, "-D__SIZEOF_INT__=4");
  vec_push(cpp_cmd, "-D__SIZEOF_LONG__=8");
  vec_push(cpp_cmd, "-D__SIZEOF_LONG_LONG__=8");
  vec_push(cpp_cmd, "-D__SIZEOF_SIZE_T__=8");
#else
# error "Unsupported programming model"
#endif

#if defined(__NO_FLONUM)
  vec_push(cpp_cmd, "-D__NO_FLONUM");
#endif
#if defined(__NO_BITFIELD)
  vec_push(cpp_cmd, "-D__NO_BITFIELD");
#endif
#if defined(__NO_VLA)
  vec_push(cpp_cmd, "-D__NO_VLA");
  vec_push(cpp_cmd, "-D__STDC_NO_VLA__");
#endif
#if defined(__NO_WCHAR)
  vec_push(cpp_cmd, "-D__NO_WCHAR");
#endif

#if defined(__AT_REMOVEDIR)
  vec_push(cpp_cmd, "-D__AT_REMOVEDIR=" STR(__AT_REMOVEDIR));
#endif
}

int main(int argc, char *argv[]) {
  const char *xccpath = argv[0];
  const char *root = dirname(strdup(xccpath));
  const char *prefix = get_exe_prefix(xccpath);
  char *cpp_path = join_exe_prefix(xccpath, prefix, "cpp");
  char *cc1_path = join_exe_prefix(xccpath, prefix, "cc1");
#if !defined(USE_SYS_AS)
  char *as_path = join_exe_prefix(xccpath, prefix, "as");
#elif defined(HOST_CC_PREFIX)
  char *as_path = STR(HOST_CC_PREFIX) "as";
#else
  char *as_path = "/usr/bin/as";
#endif
#if !defined(USE_SYS_LD)
  char *ld_path = join_exe_prefix(xccpath, prefix, "ld");
#elif defined(HOST_CC_PREFIX)
  char *ld_path = STR(HOST_CC_PREFIX) "gcc";
#else
  char *ld_path = "/usr/bin/cc";
#endif

  Vector *cpp_cmd = new_vector();
  vec_push(cpp_cmd, cpp_path);
  append_predefined_macros(cpp_cmd);

  Vector *cc1_cmd = new_vector();
  vec_push(cc1_cmd, cc1_path);

  Vector *as_cmd = new_vector();
  vec_push(as_cmd, as_path);

  Vector *ld_cmd = new_vector();
  vec_push(ld_cmd, ld_path);

  Options opts = {
    .cpp_cmd = cpp_cmd,
    .cc1_cmd = cc1_cmd,
    .as_cmd = as_cmd,
    .ld_cmd = ld_cmd,
    .sources = new_vector(),
    .linker_options = new_vector(),
    .ofn = NULL,
    .out_type = OutExecutable,
    .src_type = UnknownSource,
    .nodefaultlibs = false,
    .nostdlib = false,
    .nostdinc = false,
    .use_ld = false,
  };
  parse_options(argc, argv, &opts);

  if (opts.sources->len == 0) {
    fprintf(stderr, "No input files\n\n");
    usage(stderr);
    return 1;
  }

  if (!opts.nostdinc) {
    vec_push(cpp_cmd, "-idirafter");
    vec_push(cpp_cmd, JOIN_PATHS(root, "include"));
  }

  vec_push(cpp_cmd, NULL);  // Buffer for src.
  vec_push(cpp_cmd, NULL);  // Terminator.
  vec_push(cc1_cmd, "-");   // Read from cpp pipe.
  vec_push(cc1_cmd, NULL);  // Terminator.
  vec_push(as_cmd, "-o");
  vec_push(as_cmd, opts.ofn);
  vec_push(as_cmd, NULL);   // Placeholder for source filename.
  vec_push(as_cmd, NULL);   // Terminator.

  if (opts.use_ld) {
    // Pass through command line options.
    for (int i = 0; i < opts.linker_options->len; ++i)
      vec_push(ld_cmd, opts.linker_options->data[i]);
  } else if (opts.ofn != NULL) {
    vec_push(ld_cmd, "-o");
    vec_push(ld_cmd, opts.ofn);
  }

  if (opts.out_type >= OutExecutable && !opts.use_ld) {
#if !defined(USE_SYS_LD) || defined(NO_STD_LIB)
    vec_push(opts.ld_cmd, "-L");
    vec_push(opts.ld_cmd, JOIN_PATHS(root, "lib"));
#endif
  }

  atexit(remove_tmp_files);

  return do_compile(&opts, root);
}
