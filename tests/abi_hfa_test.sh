#!/bin/bash
set -euo pipefail

SCRIPT_DIR=$(cd "$(dirname "$0")" && pwd)
# AArch64 HFA ABI check with mixed toolchain (xcc caller, system CC callee).
XCC=${XCC:-"$SCRIPT_DIR/../xcc"}
CC=${CC:-cc}
AOUT=${AOUT:-"$SCRIPT_DIR/tmp_hfa_abi"}

trap 'rm -f "$SCRIPT_DIR/tmp_hfa_caller.o" "$SCRIPT_DIR/tmp_hfa_callee.o" "$AOUT"' EXIT

$XCC -c -o "$SCRIPT_DIR/tmp_hfa_caller.o" -Wall -Werror "$SCRIPT_DIR/abi_hfa_caller.c"
$CC -c -o "$SCRIPT_DIR/tmp_hfa_callee.o" -Wall -Werror "$SCRIPT_DIR/abi_hfa_callee.c"
$CC -o "$AOUT" "$SCRIPT_DIR/tmp_hfa_caller.o" "$SCRIPT_DIR/tmp_hfa_callee.o"
"$AOUT"
