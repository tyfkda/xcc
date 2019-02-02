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

try 0 '0;'
try 42 '42;'
try 21 '5+20-4;'
try 41 " 12 + 34 - 5 ;"
try 47 "5+6*7;"
try 15 "5*(9-6);"
try 4 "(3+5)/2;"
try 14 "a = 3; b = 5 * 6 - 8; a + b / 2;"
try 14 "foo = 3; bar = 5 * 6 - 8; foo + bar / 2;"

echo OK
