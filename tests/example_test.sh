#!/bin/bash

set -o pipefail

source ./test_sub.sh

function no_flonum() {
  echo -e "#include <stdio.h>\nint main(){\n#ifdef __NO_FLONUM\nputs(\"true\");\n#endif\nreturn 0;}" | \
    $XCC -xc - && ${RUN_EXE} ./a.out || exit 1
}

function test_all() {
  begin_test_suite "All"

  try_file 'hello' 'Hello, world!' ../examples/hello.c
  try_file 'fib' 832040 ../examples/fib.c
  try_file 'echo' 'foo bar baz' ../examples/echo.c foo bar baz

  if [ "$(no_flonum)" != "true" ]; then
    try_cmp 'mandelbrot' 'mandel256.ppm' 'mandelbrot.ppm' ../examples/mandelbrot.c 100 256 256
  fi

  end_test_suite
}

test_all

if [[ $FAILED_SUITE_COUNT -ne 0 ]]; then
  exit "$FAILED_SUITE_COUNT"
fi
