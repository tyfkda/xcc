#!/bin/bash

WCC=${WCC:-../wcc}

XCC=$WCC
PTRSIZE=4
RUN_AOUT="node ../runtime/runwasm.js a.wasm"

PROLOGUE=$(cat <<EOS
extern long write(int fd, const void *str, unsigned long len);
EOS
)

RE_SKIP='\/\/-WCC'

export XCC
export PTRSIZE
export RUN_AOUT
export PROLOGUE
export RE_SKIP

. ../../tests/test.sh || exit 1
