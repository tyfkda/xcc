#!/bin/bash

set -o pipefail

source ./test_sub.sh

XCC_TEST_EXAMPLES_DIR=${XCC_TEST_EXAMPLES_DIR:-../examples}

function no_flonum() {
  echo -e "#include <stdio.h>\nint main(){\n#ifdef __NO_FLONUM\nputs(\"true\");\n#endif\nreturn 0;}" | \
    $XCC -o "$AOUT" -xc - && ${RUN_AOUT} || exit 1
}

function test_all() {
  begin_test_suite "All"

  try_file 'hello' 'Hello, world!' "${XCC_TEST_EXAMPLES_DIR}"/hello.c
  try_file 'fib' 832040 "${XCC_TEST_EXAMPLES_DIR}"/fib.c
  try_file 'echo' 'foo bar baz' "${XCC_TEST_EXAMPLES_DIR}"/echo.c foo bar baz

  if [ "$(no_flonum)" != "true" ]; then
    try_cmp 'mandelbrot' 'mandel256.ppm' 'mandelbrot.ppm' "${XCC_TEST_EXAMPLES_DIR}"/mandelbrot.c \
        100 256 256
  fi

  end_test_suite
}

test_all

if [[ $FAILED_SUITE_COUNT -ne 0 ]]; then
  exit "$FAILED_SUITE_COUNT"
fi
