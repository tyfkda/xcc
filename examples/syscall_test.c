#include <unistd.h>
#include <sys/syscall.h>

int main(int argc, char *argv[]) {
  long exit_status = 42;

  syscall(__NR_exit, exit_status);
}
