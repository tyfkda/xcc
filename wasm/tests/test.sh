#!/bin/bash

WCC=${WCC:-../wcc}

XCC=$WCC
PTRSIZE=4
RUN_AOUT="node ../runtime/runwasm.js a.wasm"

PROLOGUE=$(cat <<EOS
extern void exit(int code);
extern long write(int fd, const char *str, long len);
int _start(int argc, char *argv[]) {
  extern int main(int, char**);
  return main(argc, argv);
}
EOS
)

RE_SKIP='\/\/-WCC'

. ../../tests/test.sh || exit 1
