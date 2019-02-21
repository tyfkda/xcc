#!/bin/bash

XCC=../xcc

try() {
  title="$1"
  expected="$2"
  inputs="$3"

  echo -n "$title => "

  $XCC $inputs > tmp || exit 1
  chmod +x tmp

  actual=`./tmp ${@:4}` || exit 1

  if [ "$actual" = "$expected" ]; then
    echo "OK"
  else
    echo "NG: $expected expected, but got $actual"
    exit 1
  fi
}

try 'hello' 'Hello, world!' '../examples/hello.c'
try 'fib' 832040 '../examples/util.c ../examples/fib.c'
try 'echo' 'foo bar baz' '../examples/util.c ../examples/echo.c' foo bar baz
