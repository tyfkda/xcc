#!/bin/bash

try_direct() {
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

try() {
  try_direct "$1" "main(){ $2 }"
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
try 1 "a = b = (c = 1) + 2; a == b;"
try 1 "123 != 456;"
try_direct 23 "foo(){ 123; } main(){ foo() - 100; }"
try_direct 9 "sqsub(x, y){ xx = x * x; yy = y * y; xx - yy; } main(){ sqsub(5, 4); }"
try 2 "if (1) 2;"
try 3 "if (0) 2; else 3;"

echo OK
