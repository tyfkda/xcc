#!/bin/bash

source ./test_sub.sh

AOUT=${AOUT:-$(basename "$(mktemp -u)")}
XCC=${XCC:-../xcc}
RUN_AOUT=${RUN_AOUT:-./"$AOUT"}

echo "Compile=[$XCC], Run=[$RUN_AOUT]"

ARCH=$(arch)
if [[ -z "$RE_SKIP" ]]; then
  if [[ "$ARCH" = "arm64" ]] || [[ "$ARCH" = "aarch64" ]]; then
    RE_SKIP='\/\/-AARCH64'
  fi
fi

SILENT=' > /dev/null 2>&1'
if [[ "$VERBOSE" != "" ]]; then
  SILENT=''
fi

RE_WNOERR='\/\/-WNOERR'
MAKE_ERR='err'

try_direct() {
  local title="$1"
  local expected="$2"
  local input="$3"

  begin_test "$title"

  if [[ -n "$RE_SKIP" ]]; then
    echo -n "$input" | grep "$RE_SKIP" > /dev/null && {
      end_test
      return
    };
  fi
  local OPT=''
  echo -n "$input" | grep "$RE_WNOERR" > /dev/null || OPT=-Werror

  echo -e "$input" | eval "$XCC" $OPT -o "$AOUT" -xc - "$SILENT" ||  {
    end_test 'Compile failed'
    return
  }

  $RUN_AOUT
  local actual="$?"

  local err=''; [[ "$actual" == "$expected" ]] || err="${expected} expected, but ${actual}"
  end_test "$err"
}

try() {
  try_direct "$1" "$2" "int main(void){$3\n}"
}

compile_error() {
  local title="$1"
  local input="$2"

  begin_test "$title"

  if [[ -n "$RE_SKIP" ]]; then
    echo -n "$input" | grep "$RE_SKIP" > /dev/null && {
      end_test
      return
    };
  fi
  local OPT=''
  echo -n "$input" | grep "$RE_WNOERR" > /dev/null || OPT=-Werror

  echo -e "$input" | eval "$XCC" $OPT -o "$AOUT" -xc - "$SILENT"
  local exitcode="$?"

  local err=''; [[ "$exitcode" -ne 0 ]] || err="Compile error expected, but succeeded"
  end_test "$err"
}

check_error_line() {
  local title="$1"
  local expected
  expected=$(echo -e "$2")
  local input="$3"

  begin_test "$title"

  local actual
  actual=$(echo -e "$input" | $XCC -o "$AOUT" -Werror -xc - 2>&1 | \
      egrep -o '\([0-9]+\)' | sed 's/[()]//g' | head -n 1)

  local err=''; [[ "$actual" == "$expected" ]] || err="${expected} expected, but ${actual}"
  end_test "$err"
}

test_basic() {
  begin_test_suite "Basic"

  try 'data-bss alignment' 0 'static char data = 123; static int bss; return (long)&bss & 3;'
  try 'cast array to pointer' 44 'int a[3][2] = {11,22,33,44,55,66}; int *p = a; return p[3]; //-WNOERR'
  try_direct 'variable definition overwrite' 123 'int x; int x = 123; int main(void) {return x;}'
  compile_error 'variable definition conflict' 'int x = 0; int x = 123; int main(void) {return x;}'
  compile_error 'illegal type combination' 'int main(void) {long short x = 99; return x;}'
  compile_error 'assign array to struct' 'int main(void) {int a[3][2] = {11,22,33,44,55,66}; struct { int a[3][2]; } s; s = a; return s.a[1][1]; } //-WNOERR'
  try_direct 'direct addressing' 99 'int main(int argc) {if (argc < 0) { *(volatile short*)0x12 = 0x34; return *(volatile int*)0x5678; } return 99;}'
  try_direct 'restrict for array in funparam' 83 'int sub(int arr[restrict]) { return arr[0]; } int main(void) { int a[] = {83}; return sub(a); }'

  end_test_suite
}

test_struct() {
  begin_test_suite "Struct"

  compile_error 'init struct with variable on global' 'struct S {int x;}; const struct S s={123}; struct S a=s; int main(){return 0;}'

  compile_error 'same struct name' 'struct Foo{int x;}; struct Foo{int x;}; void main(){}'
  compile_error 'same struct name in scope' 'void main(){struct Foo{int x;}; struct Foo{int x;}; }'
  compile_error 'same struct/union name' 'struct Foo{int x;}; union Foo{int y;}; void main(){}'
  compile_error 'union for struct' 'struct Foo{int x;}; void main(){ union Foo foo; }'
  compile_error 'no member name' 'struct Foo{union{int anon;}; int;}; void main(){}'
  compile_error 'FAM must be last' 'struct Foo{int a; int b[]; int y;}; void main(){}'
  compile_error 'FAM cannot array' 'struct Foo{int a; int b[];}; struct Foo x[5]; void main(){}'
  compile_error 'FAM cannot struct' 'struct Foo{int a; int b[];}; struct Bar {struct Foo foo; int x;}; void main(){}'

  end_test_suite
}

test_bitfield() {
  begin_test_suite "Bitfield"

  compile_error 'bit width 0' 'void main(){struct {int x:0;} s;}'
  compile_error 'bit width neg' 'void main(){struct {int x:-1;} s;}'
  compile_error 'require bit width' 'void main(){struct {int x:;} s;}'
  compile_error 'bit size over' 'void main(){struct {int x:33;} s;}'
  compile_error 'prohibit &' 'void main(){struct {int x:1;} s; int *p = &s.x;}'
  compile_error 'prohibit sizeof' 'int main(){struct {int x:1;} s; return sizeof(s.x);}'

  end_test_suite
}

test_initializer() {
  begin_test_suite "Initializer"

  compile_error 'non exist field initializer' 'struct Foo{int x;}; void main(){ struct Foo foo = {.y=1}; }'
  compile_error 'initializer for empty struct' 'struct Foo{}; void main(){ struct Foo foo = {1}; }'
  compile_error 'array initializer (global)' 'int a[1] = 1; void main(){}'
  compile_error 'array initializer (local)' 'void main(){int a[1] = 1;}'
  compile_error 'struct initializer (global)' 'struct S {int x;} s = 1; void main(){}'
  compile_error 'struct initializer (local)' 'void main(){struct S {int x;} s = 1;}'
  compile_error 'excess elements for array' 'int a[3] = {1, 2, 3, 4}; int main(){return 0;}'
  compile_error 'excess elements for struct' 'struct {char x; short y; long z;} s = {5, 6, 7, 8}; int main(){return 0;}'
  compile_error 'undeclared struct init' 'struct Foo foo={12}; void main(){}'
  compile_error 'undeclared struct array init' 'struct Foo arr[]={{1}, {2}, {3}}; void main(){}'
  compile_error 'extern with init' 'extern int x = 123; void main(){}'
  compile_error 'char array init with ptr' 'char* foo = "foo"; void main(){ char bar[] = foo; }'
  compile_error 'global pointer init with undefined' 'char *p = &x; void main(){}'
  compile_error 'global pointer init with other type' 'int x; char *p = &x; void main(){}'
  compile_error 'global pointer init with fixnum' 'void *main = 1234;'
  compile_error 'const cast' 'int main(){const void *p = 0; void *q = p; return 0;}'
  compile_error 'no name nor defined struct ptr' 'void main(){ struct *p; }'
  compile_error 'refer undeclared struct member' 'void main(){ struct Foo *p; p->x; }'
  compile_error 'dot designator for array' 'void main(){ int a[] = {.x = 1}; }'
  compile_error 'bracket designator for struct' 'void main(){ struct {int x; int y;} s = {[1] = 2}; }'

  end_test_suite
}

test_function() {
  begin_test_suite "Function"

  compile_error 'few arg num' 'void foo(int x){} void main(){ foo(); }'
  compile_error 'many arg num' 'void foo(int x){} void main(){ foo(1, 2); }'
  compile_error 'zero arg num' 'void foo(void){} void main(){ foo(1); }'
  compile_error 'void param' 'void main(void x){}'
  compile_error 'void and param' 'void main(void, int x){}'
  compile_error 'return void' 'void main(){ return 1; }'
  compile_error 'return void2' 'int main(){ return (void)0; }'
  compile_error 'return non-void' 'int main(){ return; }'
  compile_error 'no return' 'int sub(){} int main(){return 0;}'
  try_direct 'no return in main' 0 'int main(){}'
  compile_error 'funparam static' 'void main(static int argc){}'
  compile_error 'funparam extern' 'void main(extern int argc){}'
  compile_error 'duplicate func' 'void main(){} void main(){}'
  compile_error 'conflict func' 'void main(); int main(int, char**){return 0;}'
  compile_error 'duplicate var & func' 'int main; int main(){return 0;}'
  compile_error 'duplicate func & var' 'int main(){return 0;} int main;'
  try_direct 'infinite loop and exit' 77 '#include <stdlib.h>\nint main(){for (int i = 0; ; ++i) if (i == 10) exit(77);}'
  try_direct 'multiple prototype' 22 'int foo(), bar=76, qux(); int main(){return foo() - qux();} int foo(){return 98;} int qux(){return bar;}'

  end_test_suite
}

test_error() {
  begin_test_suite "Error"

  compile_error 'no main' 'void foo(){}'
  compile_error 'comment not closed' 'void main(){} /*'
  compile_error 'undef varref' 'int main(){ return x; }'
  compile_error 'undef var assign' 'void main(){ x = 1; }'
  compile_error 'undef funcall' 'void foo(); void main(){ foo(); }  //-WCC'
  compile_error 'no proto def' 'void main(){ foo(); } void foo(){}'
  compile_error 'int - ptr' 'void main(){ int *p; p = (void*)1; 2 - p; }'
  compile_error 'pre inc non lval' 'void main(){ ++111; }'
  compile_error 'post dec non lval' 'void main(){ 23--; }'
  compile_error '*num' 'void main(){ *123; }'
  compile_error '&num' 'void main(){ &123; }'
  compile_error '&enum' 'enum Num { Zero }; void main(){ void *p = &Zero; }'
  compile_error 'scoped enum name' 'int sub(){enum Num{Zero}; return Zero;} int main(){enum Num n = 0; return n;}'
  compile_error 'scoped enum value' 'int sub(){enum{Zero}; return Zero;} int main(){return Zero;}'
  compile_error 'assign to non-lhs' 'void main(){ int x; x + 1 = 3; }'
  compile_error 'assign to array' 'void main(){ int a[3], b[3]; a = b; }'
  compile_error 'assign to func' 'void main(){ main = main; }'
  compile_error '+= to non-lhs' 'void main(){ int x; x + 1 += 3; }'
  compile_error 'implicit cast to ptr' 'void foo(int *p); void main(){ foo(123); }'
  compile_error 'cast to array' 'int sub(int a[][3]){return 0;} int main(){ int a[3][2]; return sub((int[][3])a); }'
  compile_error 'struct->' 'struct Foo{int x;}; void main(){ struct Foo foo; foo->x; }'
  compile_error 'struct*.' 'struct Foo{int x;}; void main(){ struct Foo* p; p.x; }'
  compile_error 'int*->' 'void main(){ int *p; p->x; }'
  compile_error 'void var' 'void main(){ void x; }'
  compile_error 'void expr' 'void main(){ 1 + (void)2; }'
  compile_error 'empty char' "int main() { return ''; }"
  compile_error 'no single char' "int main() { return '12'; }"
  compile_error '+ str' 'void main(){ +"foo"; }'
  compile_error '- str' 'void main(){ -"foo"; }'
  compile_error 'break outside loop' 'void main(){ break; }'
  compile_error 'continue outside loop' 'void main(){ continue; }'
  compile_error 'continue inside switch' 'void main(){ switch (0) {continue;} }'
  compile_error 'use before decl' 'void main(){ x = 0; int x; }'
  compile_error 'scope invisible' 'int main(){ {int x;} return x; }'
  compile_error 'array = ptr' 'void main(){ int a[1], *p; a = p; }'
  compile_error 'case w/o switch' 'int main(){ for (;;) { case 0: break; } return 0; }'
  compile_error 'case outside switch' 'int main(){ switch(0){} case 0: return 0; }'
  compile_error 'dup cases' 'void main(){ switch(0){case 1: break; case 1: break;} }'
  compile_error 'case is int only' 'void main(){ switch(0){case "foo": break;} }'
  compile_error 'default outside switch' 'int main(){ switch(0){} default: return 0; }'
  compile_error 'non-const case' 'void main(){int x=1; switch (x){case x: x=2;}}'
  compile_error 'vardecl is not stmt' 'void main(){ if (1) int x = 0; }'
  compile_error 'extern only' 'extern int x; void main(){ x = 123; }'
  compile_error 'for-var scoped' 'int main(){ for (int i = 0; i < 5; ++i) ; return i; }'
  compile_error 'use void' 'void func(){} void main(){ int a = (int)func(); }'

  compile_error 'goto no-label' 'void main(){ goto label; }  //-WCC'
  compile_error 'goto dup-label' 'void main(){ label: goto label; label:; }  //-WCC'
  compile_error 'unused label' 'void main(){ label:; }'

  # Reachability check.
  compile_error 'unreachable after return' 'int main(){ int x=0; return x; x=1; }'
  compile_error 'unreachable break' 'int main(){ for (;;) { break; break; } }'
  compile_error 'unreachable after if' 'int main(int x, char *argv[]){ for (;;) { if (x) break; else return 1; ++x; } }'
  compile_error 'unreachable after switch' 'int main(int x, char *argv[]){ switch (x){case 0: return 1; default: return 2;} return 3; }'
  compile_error 'unreachable after infinite loop' 'int main(int x, char *argv[]){ for (;;) { ++x; } return x; }'
  compile_error 'unreachable inner for(;0;)' 'int main(int x, char *argv[]){ for (;0;) { ++x; } return x; }'
  compile_error 'unreachable after while(1)' 'int main(int x, char *argv[]){ while (1) {++x;} return x; }'
  compile_error 'unreachable inner while(0)' 'int main(int x, char *argv[]){ while (0) {++x;} return x; }'
  compile_error 'unreachable after do-while(1)' 'int main(int x, char *argv[]){ do {++x;} while (1); return x; }'
  try 'allow switch break after block' 21 'int x=21; switch (x) {case 1: {return -1;} break; case 2: {break;} break;} return x;'
  try 'use goto to skip first' 54 'int acc=0, i=1; goto inner; for (; i<=10;) {acc += i; inner: ++i;} return acc;  //-WCC'
  compile_error 'after noreturn function' '#include <stdlib.h>\nint main(){ exit(0); return 1; }'
  compile_error 'noreturn should not return' 'void sub(void) __attribute__((noreturn));\nvoid sub(void){}\nint main(){ sub(); }'
  compile_error 'noreturn should be void' '#include <stdlib.h>\nint sub(void) __attribute__((noreturn));\nint sub(void){exit(0);}\nint main(){ sub(); }'

  compile_error 'enum and global' 'enum Foo { BAR }; int BAR; void main(){}'
  compile_error 'global and enum' 'int BAR; enum Foo { BAR }; void main(){}'
  compile_error 'dup enum elem' 'enum Foo { BAR, BAR }; void main(){}'
  compile_error '+x =' 'void main(){ int x; +x = 45; }'
  compile_error '(int)x = ' 'void main(){ int x; (int)x = 32; }'
  compile_error 'compound literal =' 'struct Foo {int x;}; void main(){ struct Foo foo = {1}; (struct Foo){66} = foo; }'
  compile_error 'compound literal w/o brace' 'void main(){ ++(int)55; }'
  compile_error 'param and first scope' 'void main(int x){ int x; }'
  compile_error 'conflict typedef' 'typedef int Foo; typedef long Foo; void main(){}'
  compile_error 'conflict struct typedef' 'typedef struct{int x;} Foo; typedef struct{int x;} Foo; void main(){}'
  compile_error 'no VLA in global' "#ifndef __NO_VLA\nint n = 3; int array[n];\n#else\n#error no VLA\n#endif\n void main(){}"
  compile_error 'no VLA in global typedef' "#ifndef __NO_VLA\nint n = 3; typedef int array[n];\n#else\n#error no VLA\n#endif\n void main(){}"
  compile_error 'negative array' "void main(){ int array[-1]; }"
  compile_error 'size unknown' 'extern char string[]; int main(){ return sizeof(string); } char string[] = "Hello";'
  compile_error 'scoped typedef' 'void sub(){typedef int T;} T g=123; int main(){return g;}'
  compile_error 'typedef and var' 'int main(){typedef int ttt; int ttt = 123; return ttt;}'
  compile_error 'var and typedef' 'int main(){int ttt = 123; typedef int ttt; return ttt;}'
  try_direct 'typedef and var in other scope' 123 'typedef int ttt; int main(){ttt ttt = 123; return ttt;}'
  compile_error 'func retval ref' 'typedef struct {int x;} S; S func() {return (S){111};} int main(){S *p = &func(); return s->x;}'
  compile_error 'if void' 'void main(){if ((void)0) {}}'
  compile_error 'while void' 'void main(){while ((void)1) {}}'
  compile_error 'do-while void' 'void main(){do {} while ((void)-2);}'
  compile_error 'for void' 'void main(){for (; (void)3; ) {}}'
  compile_error 'switch void' 'void main(){switch ((void)4) {}}'
  compile_error 'assign const' 'const int G = 0; void main(){G=1;}'
  compile_error 'assign const struct' 'const struct S {int x;} s = {100}; int main(){s.x = 1; return s.x;}'
  # flonum
  compile_error 'array[double]' 'void main(){int a[]={1, 2, 3}; double d=1; return a[d];}'
  compile_error 'ptr + f' 'void main(){int a[]={1, 2, 3}; double d=1; int *p=(a+3)-d; return *p;}'

  end_test_suite
}

test_error_line() {
  begin_test_suite "Error line no"

  check_error_line "Block comment" 5 "int main() {
      /*
        Block comment
      */
      ${MAKE_ERR};}"
  check_error_line "Backslash" 5 "int main() {
      int concat_line = 1 + \\
                        2 * \\
                        3;
      ${MAKE_ERR};}"
  check_error_line "newline in macro argument" 5 "#define FOO(a, b) \\
        (a + b)
    int main() {
      FOO(1, 2);
      ${MAKE_ERR};}"
  check_error_line "newline in macro argument" 5 "#define FOO(a, b)  (a + b)
    int main() {
      FOO(1,
          2);
      ${MAKE_ERR};}"
  check_error_line "Block comment in #if 0" 7 "int main() {
      #if 0
        /*
      #else
        */
      #endif
      ${MAKE_ERR};}"
  check_error_line "After include" 2 "#include <stdio.h>
    ${MAKE_ERR};}"

  end_test_suite
}

test_basic
test_struct
test_bitfield
test_initializer
test_function
test_error
test_error_line

if [[ $FAILED_SUITE_COUNT -ne 0 ]]; then
  exit "$FAILED_SUITE_COUNT"
fi
