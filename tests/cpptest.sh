#!/bin/bash

set -o pipefail

source ./test_sub.sh

function try_run() {
  local title="$1"
  local expected
  expected=$(echo -e "$2")
  local input="$3"
  local ppflags="$4"

  begin_test "$title"

  echo -e "$input" | $CPP $ppflags | $CC -o "$AOUT" -Wall -Werror -xc - || exit 1

  $RUN_AOUT
  local actual="$?"

  local err=''; [[ "$actual" == "$expected" ]] || err="${expected} expected, but ${actual}"
  end_test "$err"
}

function test_basic() {
  begin_test_suite "Basic"

  try_pp '#define' '0' "#define NULL 0\nNULL"
  try_pp 'empty' '' "#define EMPTY\nEMPTY"
  try_pp '#undef' 'undefined' "#define FOO\n#undef FOO\n#ifdef FOO\ndefined\n#else\nundefined\n#endif"
  try_pp 'Param' '((1) + (2))' "#define ADD(x,y)  ((x) + (y))\nADD(1, 2)"

  try_pp '#ifdef' 'x' "#define X\n#ifdef X\nx\n#else\ny\n#endif"
  try_pp '#ifdef else' 'y' "#ifdef X\nx\n#else\ny\n#endif"
  try_pp '#if 1' '2' "#if 1\n2\n#else\n3\n#endif"
  try_pp '#if 0' '3' "#if 0\n3\n#else\n3\n#endif"
  try_pp '#elif-1' '2' "#if 1\n2\n#elif 1\n3\n#elif 1\n4#else\n5\n#endif"
  try_pp '#elif-01' '3' "#if 0\n2\n#elif 1\n3\n#elif 1\n4\n#else\n5\n#endif"
  try_pp '#elif-001' '4' "#if 0\n2\n#elif 0\n3\n#elif 1\n4\n#else\n5\n#endif"
  try_pp 'non-defined-ident is zero' 'false' "#if UNDEF\ntrue\n#else\nfalse\n#endif"
  try_pp '#if defined' '3' "#if defined(XXX)\n2\n#else\n3\n#endif"
  try_pp '#if defined w/o ()' '3' "#if defined XXX\n2\n#else\n3\n#endif"
  try_pp '#if !defined' '2' "#if !defined(XXX)\n2\n#else\n3\n#endif"

  try_pp '__FILE__' '"*stdin*"' "__FILE__"
  try_pp '__LINE__' "3" "\n\n__LINE__"
  try_pp '#line' "# 123 \"foobar.p\" 1\"foobar.p\":123" "#line  123\t\"foobar.p\"\n__FILE__:__LINE__"
  try_pp '#line number only' "# 123 \"*stdin*\" 1\"dummy\"\"*stdin*\":124" "#line  123\n\"dummy\"\n__FILE__:__LINE__"
  try_pp 'Block comment' '/*block comment*/' "/*\nblock comment\n*/" '-C'
  try_pp 'Quote in comment' "/*I'm fine*/" "/*\nI'm fine\n*/" '-C'

  try_pp 'hash only' '' "#\n/* */ #"
  try_pp 'through illegal char' '\`@$#🤔' '\\`@$#🤔'

  end_test_suite
}

function test_if() {
  begin_test_suite "If"

  try_pp '#if defined reserved' 'NOT DEFINED' "#if !defined(int)\nNOT DEFINED\n#endif"
  try_pp '#if &&' '3' "#if 1 && 2\n3\n#else\n4\n#endif"
  try_pp '#if &&-2' '4' "#if 1 && 0\n3\n#else\n4\n#endif"
  try_pp '#if ||' '3' "#if 0 || 1\n3\n#else\n4\n#endif"
  try_pp '#if ||-2' '4' "#if 0 || 0\n3\n#else\n4\n#endif"
  try_pp '#if exp' 'abc' "#if 4 + 5 > 6\nabc\n#else\nxyz\n#endif"
  try_pp '#if VAR expr' 'FOUR' "#define X  -2 + 2\n#if X * 3 == 0\nZERO\n#elif X * 3 == 4\nFOUR\n#endif"
  try_pp '#if VAR expr2' 'equal' "#define UNIT 5\n#define NUM 3\n#define X  (UNIT * NUM)\n#if X / UNIT != NUM\nnot-equal\n#else\nequal\n#endif"
  try_pp 'Macro in #if' '111' "#define FOO(x) x/2\n#if FOO(2)==1\n111\n#else\n222\n#endif"
  try_pp 'func-macro in unmet' '' "#ifdef FOO\n#if FOO(123)\nfoo-123\n#endif\n#endif"
  try_pp 'Direct comment' '/*comment*///comment' "#if 0\n#else/*comment*/\n#endif//comment" '-C'
  try_pp '#if w/ block comment' 'AAA' '#if 1==2/*\n*/-1\nAAA\n#else\nBBB\n#endif' '-C'
  try_pp '#else w/ block comment' '/**/BBB' '#if 0\nAAA\n#else/*\n*/\nBBB\n#endif' '-C'
  try_pp '#endif w/ block comment' '/**/CCC' '#if 0\nAAA\n#endif/*\n*/CCC' '-C'
  try_pp 'block comment in #if 0' '' "#if 0\n/**/\n#endif" '-C'
  try_pp 'quote char in #if 0' '' "#if 0\nchar c='\"';\n#endif"

  try_pp 'Block comment hide #else' 'AAA /*#elseBBB */' '#if 1\nAAA /*\n#else\nBBB */\n#endif' '-C'
  try_pp 'Line comment hide #else' '' '#if 0\nAAA\n//#else\nBBB\n#endif' '-C'
  try_pp 'Double quote in #if' '' "#if 0\n// \"str not closed, but in comment'\n#endif" '-C'

  end_test_suite
}

function test_macro() {
  begin_test_suite "Macro"

  try_pp 'No arguments keeps as is' 'int MAX =123;' "#define MAX(a,b) ((a)>=(b)?(a):(b))\nint MAX=123;"
  try_pp '() macro and struct name' 'struct F f;' "#define F(a, b)  FF(a, b)\nstruct F f;"
  try_pp '()-ed macro insert space' 'void foo(){}' "#define EXTERN(x)  x\nEXTERN(void)foo(){}"
  try_pp 'Newline in macro' '1+2' "#define ADD(x,y) x+y\nADD(1,\n2)"
  try_pp 'Newline in macro2' '(1 + 2)' "#define FOO(x) (x)\nFOO( 1  \n + 2 )"
  try_pp 'Newline in macro3' '(123)' "#define FOO(x) (x)\nFOO\n(123)"
  try_pp 'Macro w/ str str' '"a" "b"' "#define M(x) x\nM(\"a\" \"b\")"
  try_pp 'Block comment after #define' '88' '#define X 88/*block\ncomment*/\nX'
  try_pp 'Nothing' 'ABC' '#define NOTHING /*nothing*/\nABC NOTHING'
  try_pp 'Block comment in macro body removed' 'FOO_abc' "#define M(x)  FOO_/**/x\nM(abc)"

  try_pp 'recursive macro' 'SELF(123-1)' "#define SELF(n) SELF(n-1)\nSELF(123)"
  try_pp 'recursive macro in expr' 'false' "#define SELF SELF\n#if SELF\ntrue\n#else\nfalse\n#endif"
  try_pp 'Nested' 'H(987)' "#define F(x) C(G(x))\n#define G(x) C(H(x))\n#define C(x) x\nF(987)"
  try_pp 'recursive in arg' 'SELF' "#define I(v)  v\n#define SELF  I(SELF)\nSELF"
  try_pp 'Empty arg' '"" ""' "#define F(x, y) #x #y\nF(  ,  )"
  try_pp 'Empty for single param' '""' "#define F(x) #x\nF()"
  try_pp 'vaarg' '1 2 (3, 4, 5)' "#define VAARG(x, y, ...)  x y (__VA_ARGS__)\nVAARG(1, 2, 3, 4, 5)"
  try_pp 'no vaarg' '1 2 ()' "#define VAARG(x, y, ...)  x y (__VA_ARGS__)\nVAARG(1, 2)"
  try_pp 'all vaarg' '{x, y, z};' "#define ALL(...)  {__VA_ARGS__};\nALL(x, y, z)"
  try_pp 'named vaarg' '{3, 4, 5}' "#define NAMED(x, y, rest...)  {rest}\nNAMED(1, 2, 3, 4, 5)"

  try_pp '#if in macro arguments' '{bar}' "#define FOO(x)  {x}\nFOO(\n#if 1\nbar\n#else\nqux\n#endif\n)"

  try_pp 'expand and stringify' 'a(m_z, "M(z)")' "#define MinM(x) a(x, # x)\n#define M(x) m_ ## x\nMinM(M(z))"

  try_pp ', ## empty' 'foo(fmt)'          "#define FOO(fmt, ...)  foo(fmt, ## __VA_ARGS__)\nFOO(fmt)"
  try_pp ', ## some'  'foo(fmt,1, 2, 3)'  "#define FOO(fmt, ...)  foo(fmt, ## __VA_ARGS__)\nFOO(fmt, 1, 2, 3)"
  try_pp '__VA_OPT__ empty' 'foo(fmt  )'          "#define FOO(fmt, ...)  foo(fmt __VA_OPT__(,) __VA_ARGS__)\nFOO(fmt)"
  try_pp '__VA_OPT__ some'  'foo(fmt , 1, 2, 3)'  "#define FOO(fmt, ...)  foo(fmt __VA_OPT__(,) __VA_ARGS__)\nFOO(fmt, 1, 2, 3)"

  end_test_suite
}

function test_cat() {
  begin_test_suite "Concat"

  local CATDEFS
  CATDEFS=$(cat <<EOS
#define CAT(x, y) x ## y
#define INDIRECT(x, y) CAT(x, y)
#define N 1
#define FOO1 MATCHED
#define X Y
#define _ /* empty */
EOS
)
  try_pp 'simple'   'ABCDEF'   "$CATDEFS\nCAT(ABC, DEF)"
  try_pp 'non-sym'  '123UL'    "$CATDEFS\nCAT(123, UL)"
  try_pp 'non-sym2' '123.f'    "$CATDEFS\nCAT(123, .f)"
  try_pp 'match'    'MATCHED'  "$CATDEFS\nCAT(FOO, 1)"
  try_pp 'unmatch'  'FOON'     "$CATDEFS\nCAT(FOO, N)"
  try_pp 'indirect' 'MATCHED'  "$CATDEFS\nINDIRECT(FOO, N)"
  try_pp 'unmatch2' 'XN'       "$CATDEFS\nCAT(X, N)"
  try_pp 'unmatch3' 'X_'       "$CATDEFS\nCAT(X, _)"
  try_pp 'unmatch4' '_X'       "$CATDEFS\nCAT(_, X)"

  try_pp 'with non-param' 'x y z_' "#define POST(x) x ## _\nPOST(x y z)"
  try_pp 'num with postfix' '123U' "# define UINT32_C(c) c ## U\nUINT32_C(123)"
  try_pp 'empty l' 'R' "#define CAT(x, y) x ## y\nCAT(, R)"
  try_pp 'empty r' 'L' "#define CAT(x, y) x ## y\nCAT(L, )"
  try_pp 'empty both' '' "#define CAT(x, y) x ## y\nCAT(, )"

  end_test_suite
}

function test_stringify() {
  begin_test_suite "Stringify"

  try_pp 'basic' '"1 + 2"' '#define S(x)  #x\nS(1 + 2)'
  try_pp 'escaped' '"\"abc\""' '#define S(x)  # x\nS("abc")'

  end_test_suite
}

function test_error() {
  begin_test_suite "Error"

  pp_error '#error' '#error !!!\nvoid main(){}'
  pp_error '#if not closed' '#if 1'
  pp_error '#elif not closed' '#if 0\n#elif 1'
  pp_error 'Duplicate #else' '#if 0\n#else\n#else\n#endif'
  pp_error 'undefiled func-like' '#if define(XXX)\n2\n#else\n3\n#endif'  # Confirm misspelled 'defined'
  pp_error 'less params' '#define FOO(x, y) x+y\nFOO(1)'
  pp_error 'more params' '#define FOO(x, y) x+y\nFOO(1, 2, 3)'
  pp_error 'Block comment not closed' 'AAA /* BBB'
  pp_error 'Double quote not closed' 'CCC " DDD'

  end_test_suite
}

function test_option() {
  begin_test_suite "Option"

  try_pp 'Define' '1' "Foobar" "-DFoobar"
  try_pp 'Define value' '42' "Value" "-DValue=42"
  try_pp 'Undef' 'Foobar' "Foobar" "-DFoobar -UFoobar"

  end_test_suite
}

function test_run() {
  begin_test_suite "Run"

  # Include with macro
  echo "#define FOO (37)" > tmp.h
  try_run 'Include with macro' 37 "#define FILE  \"tmp.h\"\n#include FILE\nint main(){return FOO;}"

  # Block comment after include
  echo "#define FOO (73)" > tmp.h
  try_run 'Comment after include' 73 "#include \"tmp.h\" /*block\n*/ // line\nint main(){return FOO;}"
  pp_error 'Token after include comment' "#include \"tmp.h\" /*block\n*/ illegal-token\nint main(){return FOO;}"

  mkdir -p tmp_include
  echo -e "#define BAR (13)" > tmp_include/tmp.h
  echo -e "#include_next <tmp.h>\n#define FOO (29)" > tmp.h
  try_run "Include with include_next" 42 "#include <tmp.h>\nint main(){return FOO+BAR;}"  "-I . -I tmp_include"

  end_test_suite
}

test_basic
test_if
test_macro
test_cat
test_stringify
test_error
test_option
test_run

if [[ $FAILED_SUITE_COUNT -ne 0 ]]; then
  exit "$FAILED_SUITE_COUNT"
fi
