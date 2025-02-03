#!/bin/bash

set -e

INC_PATH="-I $CSMITH_PATH"

CSMITH_OPT="
    --max-funcs 3
    --max-expr-complexity 3
    --no-volatiles
    --no-packed-struct
    --no-safe-math
    --no-jumps
    --no-bitfields
    "

RUN_AOUT="./a.out"
CC=gcc
XCC=./xcc
# RUN_AOUT="./tool/run-riscv64 ./a.out"
# CC=riscv64-unknown-elf-gcc

# wcc
# RUN_AOUT="./tool/runwasi ./a.wasm"
# CC="emcc -o a.wasm"
# XCC="./wcc --stack-size=0x10000"

for i in `seq 0 99`; do
    echo "Test $i"

    csmith $CSMITH_OPT > tmp.c
    # csmith creates platform.info
    rm -f platform.info

    $XCC $INC_PATH tmp.c > /dev/null 2>&1
    $RUN_AOUT > tmp_out.xcc.txt

    $CC $INC_PATH tmp.c > /dev/null 2>&1
    $RUN_AOUT > tmp_out.gcc.txt
    diff tmp_out.xcc.txt tmp_out.gcc.txt
done

rm tmp_out.*
echo "SUCCESS"
