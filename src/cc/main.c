#include "libgen.h"  // dirname
#include "stdarg.h"
#include "stdint.h"
#include "stdio.h"
#include "stdlib.h"
#include "string.h"
#include "sys/wait.h"
#include "unistd.h"  // fork, execvp

#include "xcc.h"
#include "elfutil.h"
#include "expr.h"
#include "lexer.h"
#include "util.h"
#include "x86_64.h"

#define PROG_START   (0x100)

#if defined(__XV6)
// XV6
#include "../kernel/syscall.h"
#include "../kernel/traps.h"

#define START_ADDRESS    0x1000

#define SYSTEMCALL(no)  do { MOV_IM32_EAX(no); INT(T_SYSCALL); } while(0)

#define SYSCALL_EXIT   (SYS_exit)
#define SYSCALL_WRITE  (SYS_write)

#elif defined(__linux__)
// Linux

#include <sys/stat.h>

#define START_ADDRESS    (0x01000000 + PROG_START)

#define SYSTEMCALL(no)  do { MOV_IM32_EAX(no); SYSCALL(); } while(0)

#define SYSCALL_EXIT   (60 /*__NR_exit*/)
#define SYSCALL_WRITE  (1 /*__NR_write*/)

#else

#error Target not supported

#endif

#define LOAD_ADDRESS    START_ADDRESS

////////////////////////////////////////////////

static pid_t fork1(void) {
  pid_t pid = fork();
  if (pid < 0)
    error("fork failed");
  return pid;
}

static void init_compiler(uintptr_t adr) {
  struct_map = new_map();
  typedef_map = new_map();
  gvar_map = new_map();

  init_gen(adr);
}

static void compile(FILE *fp, const char *filename) {
  init_lexer(fp, filename);
  Vector *node_vector = parse_program();

  for (int i = 0, len = node_vector->len; i < len; ++i)
    gen(node_vector->data[i]);
}

// Pass preprocessor's output to this compiler
static int pipe_pp_xcc(char **pp_argv, char ** xcc_argv) {
  // cpp | xcc
  int fd[2];
  if (pipe(fd) < 0)
    error("pipe failed");
  pid_t pid1 = fork1();
  if (pid1 == 0) {
    close(STDOUT_FILENO);
    dup(fd[1]);
    close(fd[0]);
    close(fd[1]);
    if (execvp(pp_argv[0], pp_argv) < 0) {
      perror(pp_argv[0]);
      exit(1);
    }
  }
  pid_t pid2 = fork1();
  if (pid2 == 0) {
    close(STDIN_FILENO);
    dup(fd[0]);
    close(fd[0]);
    close(fd[1]);
    if (execvp(xcc_argv[0], xcc_argv) < 0) {
      perror(xcc_argv[0]);
      exit(1);
    }
  }
  close(fd[0]);
  close(fd[1]);

  int ec1 = -1, ec2 = -1;
  int r1 = waitpid(pid1, &ec1, 0);
  int r2 = waitpid(pid2, &ec2, 0);
  if (r1 < 0 || r2 < 0)
    error("wait failed");
  return ec1 != 0 ? ec1 : ec2;
}

static char *change_ext(const char *fn, const char *ext) {
  size_t fnlen = strlen(fn), extlen = strlen(ext);
  char *buf = malloc(fnlen + extlen + 2);  // dot + '\0'
  strcpy(buf, fn);
  char *p = strrchr(buf, '/');
  if (p == NULL)
    p = buf;
  p = strrchr(p, '.');
  if (p == NULL)
    p = buf + fnlen;
  *p++ = '.';
  strcpy(p, ext);
  return buf;
}

static void put_padding(FILE* fp, uintptr_t start) {
  long cur = ftell(fp);
   if (start > (size_t)cur) {
    size_t size = start - (uintptr_t)cur;
    char* buf = calloc(1, size);
    fwrite(buf, size, 1, fp);
    free(buf);
  }
}

int main(int argc, char* argv[]) {
  const char *ofn = "a.out";
  char *out_asm = NULL;
  int iarg;

  for (iarg = 1; iarg < argc; ++iarg) {
    if (*argv[iarg] != '-')
      break;
    if (strncmp(argv[iarg], "-o", 2) == 0)
      ofn = strdup_(argv[iarg] + 2);
    if (strncmp(argv[iarg], "-S", 2) == 0)
      out_asm = &argv[iarg][2];
  }

  if (argc > iarg) {
    // Pass sources to preprocessor.
    char **pp_argv = malloc(sizeof(char*) * (argc + 1));
    pp_argv[0] = cat_path(dirname(strdup_(argv[0])), "cpp");
    memcpy(&pp_argv[1], &argv[1], sizeof(char*) * argc);
    pp_argv[argc] = NULL;
    char **xcc_argv = argv;
    xcc_argv[iarg] = NULL;  // Destroy!
    return pipe_pp_xcc(pp_argv, xcc_argv) != 0;
  }

  // Compile.

  FILE *asm_fp = NULL;
  if (out_asm != NULL) {
    const char *name = *out_asm != '\0' ? out_asm : ofn;
    out_asm = change_ext(name, "s");
    asm_fp = fopen(out_asm, "w");
    if (asm_fp == NULL)
      error("Cannot open file for asm: %s", out_asm);
    set_asm_fp(asm_fp);
  }

  init_compiler(LOAD_ADDRESS);

  // Test.
  define_global(new_func_type(&tyVoid, NULL, true), 0, NULL, "__asm");
  define_global(new_func_type(&tyVoid, NULL, false), 0, NULL, "__rel32");

  compile(stdin, "*stdin*");

  fixup_locations();

  uintptr_t entry = label_adr("_start");
  if (entry == (uintptr_t)-1)
    error("Cannot find label: `%s'", "_start");

  FILE* fp = fopen(ofn, "wb");
  if (fp == NULL) {
    fprintf(stderr, "Failed to open output file: %s\n", ofn);
    return 1;
  }

  size_t codefilesz, codememsz;
  size_t datafilesz, datamemsz;
  uintptr_t codeloadadr, dataloadadr;
  get_section_size(0, &codefilesz, &codememsz, &codeloadadr);
  get_section_size(1, &datafilesz, &datamemsz, &dataloadadr);

  int phnum = datamemsz > 0 ? 2 : 1;

  out_elf_header(fp, entry, phnum);
  out_program_header(fp, 0, PROG_START, codeloadadr, codefilesz, codememsz);
  if (phnum > 1)
    out_program_header(fp, 1, ALIGN(PROG_START + codefilesz, 0x1000), dataloadadr, datafilesz, datamemsz);

  put_padding(fp, PROG_START);
  output_section(fp, 0);
  if (phnum > 1) {
    put_padding(fp, ALIGN(PROG_START + codefilesz, 0x1000));
    output_section(fp, 1);
  }
  fclose(fp);
  if (asm_fp != NULL)
    fclose(asm_fp);

#if !defined(__XV6) && defined(__linux__)
  if (chmod(ofn, 0755) == -1) {
    perror("chmod failed\n");
    return 1;
  }
#endif

  return 0;
}
