#!/bin/bash

try() {
  expected="$1"
  input="$2"

  ./9cc "$input" > tmp || exit 1
  chmod +x tmp
  ./tmp
  actual="$?"

  if [ "$actual" = "$expected" ]; then
    echo "$input => $actual"
  else
    echo "ERROR: $input => $expected expected, but got $actual"
    exit 1
  fi
}

try 0 0
try 42 42

echo OK
