#!/bin/bash

set -o pipefail

CPP=${CPP:-../cpp}

try() {
  local title="$1"
  local expected=$(echo -e "$2")
  local input="$3"

  echo -n "$title => "

  local actual
  actual=$(echo -e "$input" | $CPP | tr -d '\n')
  local exitcode=$?
  if [ $exitcode -ne 0 ]; then
    echo "NG: CPP failed"
    exit 1
  fi

  if [ "$actual" = "$expected" ]; then
    echo "OK"
  else
    echo "NG: $expected expected, but got $actual"
    exit 1
  fi
}

compile_error() {
  local title="$1"
  local input="$2"

  echo -n "$title => "

  echo -e "$input" | $CPP
  local result="$?"

  if [ $result -eq 0 ]; then
    echo "NG: Compile error expected, but succeeded"
    exit 1
  fi
}

try 'NULL' '0' "#define NULL 0\nNULL"
try 'Param' '((1) + (2))' "#define ADD(x,y)  ((x) + (y))\nADD(1, 2)"
try 'No arguments keeps as is' 'int MAX=123;' "#define MAX(a,b) ((a)>=(b)?(a):(b))\nint MAX=123;"
try 'Newline in macro' '1+2' "#define ADD(x,y) x+y\nADD(1,\n2)"
try 'Newline in macro2' '(1+ 2)' "#define FOO(x) (x)\nFOO( 1  \n + 2 )"
try 'Newline in macro3' '(123)' "#define FOO(x) (x)\nFOO\n(123)"
try '#ifdef' 'x' "#define X\n#ifdef X\nx\n#else\ny\n#endif"
try '#ifdef else' 'y' "#ifdef X\nx\n#else\ny\n#endif"
try '#if 1' '2' "#if 1\n2\n#else\n3\n#endif"
try '#if 0' '3' "#if 0\n3\n#else\n3\n#endif"
try '#elif-1' '2' "#if 1\n2\n#elif 1\n3\n#elif 1\n4#else\n5\n#endif"
try '#elif-01' '3' "#if 0\n2\n#elif 1\n3\n#elif 1\n4\n#else\n5\n#endif"
try '#elif-001' '4' "#if 0\n2\n#elif 0\n3\n#elif 1\n4\n#else\n5\n#endif"
try '#if defined' '3' "#if defined(XXX)\n2\n#else\n3\n#endif"
try '#if defined w/o ()' '3' "#if defined XXX\n2\n#else\n3\n#endif"
try '#if !defined' '2' "#if !defined(XXX)\n2\n#else\n3\n#endif"
try '#if &&' '3' "#if 1 && 2\n3\n#else\n4\n#endif"
try '#if &&-2' '4' "#if 1 && 0\n3\n#else\n4\n#endif"
try '#if ||' '3' "#if 0 || 1\n3\n#else\n4\n#endif"
try '#if ||-2' '4' "#if 0 || 0\n3\n#else\n4\n#endif"
try '#if exp' 'abc' "#if 4 + 5 > 6\nabc\n#else\nxyz\n#endif"
try 'Macro in #if' '111' "#define FOO(x) x/2\n#if FOO(2)==1\n111\n#else\n222\n#endif"
try '__FILE__' '"*stdin*"' "__FILE__"
try '__LINE__' "3" "\n\n__LINE__"
try 'Block comment' '/*block comment*/' "/*\nblock comment\n*/"
try 'Quote in comment' "/*I'm fine*/" "/*\nI'm fine\n*/"
try 'Concat' 'FOO_123' '#define FOO(x)  FOO_ ## x\nFOO( 123 )'
try 'Stringify' '"1 + 2"' '#define S(x)  #x\nS(1 + 2)'
try 'Stringify escaped' '"\"abc\""' '#define S(x)  #x\nS("abc")'

compile_error '#error' '#error !!!\nvoid main(){}'
