#!/bin/bash

set -o pipefail

CPP=${CPP:-../cpp}
XCC=${XCC:-../xcc}
RUN_AOUT=${RUN_AOUT:-./a.out}

try() {
  local title="$1"
  local expected
  expected=$(echo -e "$2")
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

try_run() {
  local title="$1"
  local expected
  expected=$(echo -e "$2")
  local input="$3"
  local tmpfile="$4"

  if [ "$tmpfile" = "" ]; then
    tmpfile=$(mktemp).c
  fi

  echo -n "$title => "

  echo -e "$input" > "$tmpfile"
  $XCC "$tmpfile" || exit 1

  $RUN_AOUT
  local actual="$?"

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

  echo -n "Error case: $title => "

  echo -e "$input" | $CPP | tr -d '\n'
  local result="$?"

  if [ $result -eq 0 ]; then
    echo "NG: Compile error expected, but succeeded"
    exit 1
  fi
}

try '#define' '0' "#define NULL 0\nNULL"
try '#undef' 'undefined' "#define FOO\n#undef FOO\n#ifdef FOO\ndefined\n#else\nundefined\n#endif"
try 'Param' '((1) + (2))' "#define ADD(x,y)  ((x) + (y))\nADD(1, 2)"
try 'No arguments keeps as is' 'int MAX=123;' "#define MAX(a,b) ((a)>=(b)?(a):(b))\nint MAX=123;"
try 'Newline in macro' '1+2' "#define ADD(x,y) x+y\nADD(1,\n2)"
try 'Newline in macro2' '(1 + 2)' "#define FOO(x) (x)\nFOO( 1  \n + 2 )"
try 'Newline in macro3' '(123)' "#define FOO(x) (x)\nFOO\n(123)"
try 'Block comment after #define' '88' '#define X 88/*block\ncomment*/\nX'
try 'Nothing' 'ABC ' '#define NOTHING /*nothing*/\nABC NOTHING'
try '#ifdef' 'x' "#define X\n#ifdef X\nx\n#else\ny\n#endif"
try '#ifdef else' 'y' "#ifdef X\nx\n#else\ny\n#endif"
try '#if 1' '2' "#if 1\n2\n#else\n3\n#endif"
try '#if 0' '3' "#if 0\n3\n#else\n3\n#endif"
try '#elif-1' '2' "#if 1\n2\n#elif 1\n3\n#elif 1\n4#else\n5\n#endif"
try '#elif-01' '3' "#if 0\n2\n#elif 1\n3\n#elif 1\n4\n#else\n5\n#endif"
try '#elif-001' '4' "#if 0\n2\n#elif 0\n3\n#elif 1\n4\n#else\n5\n#endif"
try 'non-defined-ident is zero' 'false' "#if UNDEF\ntrue\n#else\nfalse\n#endif"
try '#if defined' '3' "#if defined(XXX)\n2\n#else\n3\n#endif"
try '#if defined w/o ()' '3' "#if defined XXX\n2\n#else\n3\n#endif"
try '#if !defined' '2' "#if !defined(XXX)\n2\n#else\n3\n#endif"
try '#if defined reserved' 'NOT DEFINED' "#if !defined(int)\nNOT DEFINED\n#endif"
try '#if &&' '3' "#if 1 && 2\n3\n#else\n4\n#endif"
try '#if &&-2' '4' "#if 1 && 0\n3\n#else\n4\n#endif"
try '#if ||' '3' "#if 0 || 1\n3\n#else\n4\n#endif"
try '#if ||-2' '4' "#if 0 || 0\n3\n#else\n4\n#endif"
try '#if exp' 'abc' "#if 4 + 5 > 6\nabc\n#else\nxyz\n#endif"
try 'Macro in #if' '111' "#define FOO(x) x/2\n#if FOO(2)==1\n111\n#else\n222\n#endif"
try 'Direct comment' '/*comment*///comment' "#if 0\n#else/*comment*/\n#endif//comment"
try '#if w/ block comment' 'AAA' '#if 1==2/*\n*/-1\nAAA\n#else\nBBB\n#endif'
try '#else w/ block comment' '/**/BBB' '#if 0\nAAA\n#else/*\n*/\nBBB\n#endif'
try '#endif w/ block comment' '/**/CCC' '#if 0\nAAA\n#endif/*\n*/CCC'
try '__FILE__' '"*stdin*"' "__FILE__"
try '__LINE__' "3" "\n\n__LINE__"
try '#line' "# 123 \"foobar.p\" 1\"foobar.p\":123" "#line  123\t\"foobar.p\"\n__FILE__:__LINE__"
try '#line number only' "# 123 \"*stdin*\" 1\"dummy\"\"*stdin*\":124" "#line  123\n\"dummy\"\n__FILE__:__LINE__"
try 'Block comment' '/*block comment*/' "/*\nblock comment\n*/"
try 'Quote in comment' "/*I'm fine*/" "/*\nI'm fine\n*/"
try 'Concat' 'FOO_123' '#define FOO(x)  FOO_ ## x\nFOO( 123 )'
try 'Stringify' '"1 + 2"' '#define S(x)  #x\nS(1 + 2)'
try 'Stringify escaped' '"\"abc\""' '#define S(x)  #x\nS("abc")'
try 'recursive macro' 'SELF(123-1)' "#define SELF(n) SELF(n-1)\nSELF(123)"
try 'recursive macro in expr' 'false' "#define SELF SELF\n#if SELF\ntrue\n#else\nfalse\n#endif"
try 'Nested' 'foo' "#define F G\n#define G(p) p\nF(foo)"
try 'Empty arg' '"" ""' "#define F(x, y) #x #y\nF(  ,  )"
try 'vaarg' '1 2 3,4,5' "#define VAARG(x, y, ...)  x y __VA_ARGS__\nVAARG(1, 2, 3, 4, 5)"
try 'all vaarg' '{x,y,z};' "#define ALL(...)  {__VA_ARGS__};\nALL(x, y, z)"
try 'no vaarg' 'foo bar' "#define NOVAARG(x, y, ...)  x y\nNOVAARG(foo, bar)"

compile_error '#error' '#error !!!\nvoid main(){}'
compile_error '#if not closed' '#if 1'
compile_error '#elif not closed' '#if 0\n#elif 1'
compile_error 'Duplicate #else' '#if 0\n#else\n#else\n#endif'

# Include with macro
echo "#define FOO (37)" > tmp.h
try_run 'Include with macro' 37 "#define FILE  \"tmp.h\"\n#include FILE\nint main(){return FOO;}" tmp.c

# Block comment after include
echo "#define FOO (73)" > tmp.h
try_run 'Comment after include' 73 "#include \"tmp.h\" /*block\n*/ // line\nint main(){return FOO;}" tmp.c
compile_error 'Token after include comment' "#include \"tmp.h\" /*block\n*/ illegal-token\nint main(){return FOO;}" tmp.c
