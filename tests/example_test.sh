#!/bin/bash

source ./test_sub.sh

AOUT=${AOUT:-$(basename "$(mktemp -u)")}
XCC=${XCC:-../xcc}
# RUN_EXE=${RUN_EXE:-}

try() {
  local title="$1"
  local expected="$2"
  local inputs="$3"

  begin_test "$title"

  $XCC -o "$AOUT" -Werror "$inputs" > /dev/null 2>&1 || {
    end_test 'Compile failed'
    return
  }

  declare -a args=( "$@" )
  local actual
  actual=$(${RUN_EXE} ./"$AOUT" "${args[@]:3}") > /dev/null 2>&1 || {
    end_test 'Exec failed'
    return
  }

  local err=''; [[ "$actual" == "$expected" ]] || err="${expected} expected, but ${actual}"
  end_test "$err"
}

try_cmp() {
  local title="$1"
  local expected="$2"
  local output="$3"
  local inputs="$4"

  begin_test "$title"

  $XCC -o "$AOUT" -Werror "$inputs" > /dev/null 2>&1 || {
    end_test 'Compile failed'
    return
  }

  declare -a args=( "$@" )
  ${RUN_EXE} ./"$AOUT" "${args[@]:4}" > /dev/null 2>&1 || {
    end_test 'Exec failed'
    return
  }

  local err=''; cmp "$expected" "$output" > /dev/null 2>&1 || err="Differ"
  end_test "$err"
}

no_flonum() {
  echo -e "#include <stdio.h>\nint main(){\n#ifdef __NO_FLONUM\nputs(\"true\");\n#endif\nreturn 0;}" > tmp.c
  $XCC tmp.c && ${RUN_EXE} ./a.out || exit 1
}

test_all() {
  begin_test_suite "All"

  try 'hello' 'Hello, world!' ../examples/hello.c
  try 'fib' 832040 ../examples/fib.c
  try 'echo' 'foo bar baz' ../examples/echo.c foo bar baz

  if [ "$(no_flonum)" != "true" ]; then
    try_cmp 'mandelbrot' 'mandel256.ppm' 'mandelbrot.ppm' ../examples/mandelbrot.c 100 256 256
  fi

  end_test_suite
}

test_all

if [[ $FAILED_SUITE_COUNT -ne 0 ]]; then
  exit "$FAILED_SUITE_COUNT"
fi
