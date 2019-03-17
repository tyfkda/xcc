#!/bin/bash

CPP=../cpp

try() {
  title="$1"
  expected="$2"
  input="$3"

  echo -n "$title => "

  actual=$(echo -e "$input" | $CPP) || exit 1

  if [ "$actual" = "$expected" ]; then
    echo "OK"
  else
    echo "NG: $expected expected, but got $actual"
    exit 1
  fi
}

compile_error() {
  title="$1"
  input="$2"

  echo -n "$title => "

  echo -e "$input" | $CPP
  result="$?"

  if [ "$result" = "0" ]; then
    echo "NG: Compile error expected, but succeeded"
    exit 1
  fi
}

try 'NULL' '0 ' "#define NULL 0\nNULL"
try 'Param' '1+ 2' "#define ADD(x,y) x+y\nADD(1,2)"
try '#ifdef' 'x' "#define X\n#ifdef X\nx\n#else\ny\n#endif"
try '#ifdef else' 'y' "#ifdef X\nx\n#else\ny\n#endif"
compile_error '#error' '#error !!!\nvoid main(){}'
