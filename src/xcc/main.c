#include <assert.h>
#include <fcntl.h>  // open
#include <libgen.h>  // dirname
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

#include "util.h"

static char *get_ext(const char *filename) {
  const char *last_slash = strrchr(filename, '/');
  if (last_slash == NULL)
    last_slash = filename;
  char *dot = strrchr(last_slash, '.');
  return dot != NULL ? (char*)&dot[1]: (char*)&last_slash[strlen(last_slash)];
}

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
pid_t pipe_exec(char **command, int ofd, int fd[2]) {
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

static int cat(const char *filename, int ofd) {
  int ifd = open(filename, O_RDONLY);
  if (ifd < 0)
    return 1;

  const int SIZE = 4096;
  char *buf = malloc(SIZE);
  for (;;) {
    ssize_t size = read(ifd, buf, SIZE);
    if (size < 0)
      return 1;
    if (size > 0)
      write(ofd, buf, size);
    if (size < SIZE)
      break;
  }
  free(buf);

  close(ifd);
  return 0;
}

static void create_local_label_prefix_option(int index, char *out, size_t n) {
  static const char LOCAL_LABEL_PREFIX[] = "--local-label-prefix=";
  static const char DIGITS[] = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";
  char prefix[8];
  char *p = &prefix[sizeof(prefix) - 1];
  *p-- = '\0';
  assert(index >= 0);
  if (index > 0) {
    --index;
    do {
      if (p <= prefix)
        error("Label prefix buffer overflow");
      *p-- = DIGITS[index % (sizeof(DIGITS) - 1)];
      index /= sizeof(DIGITS) - 1;
    } while (index > 0);
  }
  *p = 'L';

  snprintf(out, n, "%s%s", LOCAL_LABEL_PREFIX, p);
}

static int compile(const char *src, Vector *cpp_cmd, Vector *cc1_cmd, int ofd) {
  int ofd2 = ofd;
  int cc_fd[2];
  pid_t cc_pid = -1;
  if (cc1_cmd != NULL) {
    cc_pid = pipe_exec((char**)cc1_cmd->data, ofd, cc_fd);
    ofd2 = cc_fd[1];
  }

  cpp_cmd->data[cpp_cmd->len - 2] = (void*)src;
  pid_t cpp_pid = exec_with_ofd((char**)cpp_cmd->data, ofd2);
  int r = wait_process(cpp_pid);
  if (r == 0) {
    if (cc_pid != -1) {
      close(cc_fd[0]);
      close(cc_fd[1]);
      r = wait_process(cc_pid);
    }
  }
  return r;
}


int main(int argc, char* argv[]) {
  const char *ofn = "a.out";
  bool out_pp = false;
  bool run_asm = true;
  int iarg;

  char *cpp_path = cat_path(dirname(strdup_(argv[0])), "cpp");
  char *cc1_path = cat_path(dirname(strdup_(argv[0])), "cc1");
  char *as_path = cat_path(dirname(strdup_(argv[0])), "as");

  Vector *cpp_cmd = new_vector();
  vec_push(cpp_cmd, cpp_path);

  Vector *cc1_cmd = new_vector();
  vec_push(cc1_cmd, cc1_path);

  Vector *as_cmd = new_vector();
  vec_push(as_cmd, as_path);

  for (iarg = 1; iarg < argc; ++iarg) {
    char *arg = argv[iarg];
    if (*arg != '-')
      break;
    if (strncmp(arg, "-I", 2) == 0 ||
        strncmp(arg, "-D", 2) == 0)
      vec_push(cpp_cmd, arg);
    if (strncmp(arg, "-o", 2) == 0) {
      ofn = arg + 2;
      vec_push(as_cmd, arg);
    }
    if (strcmp(arg, "-E") == 0) {
      out_pp = true;
      run_asm = false;
    }
    if (strcmp(arg, "-S") == 0 ||
        strcmp(arg, "--dump-ir") == 0)
      run_asm = false;
    vec_push(cc1_cmd, arg);
  }

  vec_push(cpp_cmd, NULL);  // Buffer for src.
  vec_push(cpp_cmd, NULL);  // Terminator.
  vec_push(cc1_cmd, NULL);  // Buffer for label prefix.
  vec_push(cc1_cmd, NULL);  // Terminator.
  vec_push(as_cmd, NULL);  // Terminator.

  int ofd = STDOUT_FILENO;
  int as_fd[2];
  pid_t as_pid = -1;

  if (run_asm) {
    as_pid = pipe_exec((char**)as_cmd->data, -1, as_fd);
    ofd = as_fd[1];
  }

  int res = 0;
  if (iarg < argc) {
    for (int i = iarg; i < argc; ++i) {
      char *src = argv[i];
      char *ext = get_ext(src);
      if (strcasecmp(ext, "c") == 0) {
        char prefix_option[32];
        create_local_label_prefix_option(i - iarg, prefix_option, sizeof(prefix_option));
        cc1_cmd->data[cc1_cmd->len - 2] = prefix_option;

        res = compile(src, cpp_cmd, out_pp ? NULL : cc1_cmd, ofd);
      } else if (strcasecmp(ext, "s") == 0) {
        res = cat(src, ofd);
      } else {
        fprintf(stderr, "Unsupported file type: %s\n", src);
        res = -1;
      }
      if (res != 0)
        break;
    }
  } else {
    // cpp is read from stdin.
    res = compile(NULL, cpp_cmd, out_pp ? NULL : cc1_cmd, ofd);
  }

  if (res != 0 && as_pid != -1) {
#if !defined(__XV6)
    kill(as_pid, SIGKILL);
    remove(ofn);
#else
    (void)ofn;
#endif
    close(as_fd[0]);
    close(as_fd[1]);
    as_pid = -1;
  }

  if (as_pid != -1) {
    close(as_fd[0]);
    close(as_fd[1]);

    res = wait_process(as_pid);
  }
  return res == 0 ? 0 : 1;
}
