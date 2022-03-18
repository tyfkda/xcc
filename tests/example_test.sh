#!/bin/bash

XCC=${XCC:-../xcc}

try() {
  local title="$1"
  local expected="$2"
  local inputs="$3"

  echo -n "$title => "

  $XCC $inputs || exit 1

  local actual
  actual=`./a.out ${@:4}` || exit 1

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

  $XCC $inputs || exit 1

  `./a.out ${@:5}` || exit 1

  cmp $expected $output || {
    echo "NG"
    exit 1
  }
  echo "OK"
}

try 'hello' 'Hello, world!' ../examples/hello.c
try 'fib' 832040 ../examples/fib.c
try 'echo' 'foo bar baz' ../examples/echo.c foo bar baz
try 'longjmp_test' '123' "../examples/longjmp_test.c ../lib/setjmp.c"
try_cmp 'mandelbrot' '../examples/mandelbrot.ppm' 'mandelbrot.ppm' "../examples/mandelbrot.c ../lib/sprintf.c ../lib/umalloc.c ../lib/lib.c ../lib/crt0.c" 100 256 256
