#!/bin/bash

AOUT=${AOUT:-$(basename `mktemp -u`)}
XCC=${XCC:-../xcc}

try() {
  local title="$1"
  local expected="$2"
  local inputs="$3"

  echo -n "$title => "

  $XCC -o "$AOUT" "$inputs" || exit 1

  declare -a args=( "$@" )
  local actual
  actual=$(./"$AOUT" "${args[@]:3}") || exit 1

  if [ "$actual" = "$expected" ]; then
    echo "OK"
  else
    echo "NG: $expected expected, but got $actual"
    exit 1
  fi
}

try_cmp() {
  local title="$1"
  local expected="$2"
  local output="$3"
  local inputs="$4"

  echo -n "$title => "

  $XCC -o "$AOUT" "$inputs" || exit 1

  declare -a args=( "$@" )
  ./"$AOUT" "${args[@]:4}" || exit 1

  cmp "$expected" "$output" || {
    echo "NG"
    exit 1
  }
  echo "OK"
}

no_flonum() {
  echo -e "#include <stdio.h>\nint main(){\n#ifdef __NO_FLONUM\nputs(\"true\");\n#endif\nreturn 0;}" > tmp.c
  $XCC tmp.c && ./a.out || exit 1
}

try 'hello' 'Hello, world!' ../examples/hello.c
try 'fib' 832040 ../examples/fib.c
try 'echo' 'foo bar baz' ../examples/echo.c foo bar baz
try 'longjmp_test' '123' ../examples/longjmp_test.c

if [ "`no_flonum`" != "true" ]; then
try_cmp 'mandelbrot' '../examples/mandelbrot.ppm' 'mandelbrot.ppm' ../examples/mandelbrot.c 100 256 256
fi
