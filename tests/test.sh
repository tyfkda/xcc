#!/bin/bash

XCC=../xcc

try_direct() {
  title="$1"
  expected="$2"
  input="$3"

  echo -n "$title => "

  echo -e "$input" | $XCC || exit 1

  ./a.out
  actual="$?"

  if [ "$actual" = "$expected" ]; then
    echo "OK"
  else
    echo "NG: $expected expected, but got $actual"
    exit 1
  fi
}

try() {
  try_direct "$1" "$2" "int main(void){$3}"
}

try_output_direct() {
  title="$1"
  expected="$2"
  input="$3"

  echo -n "$title => "

  echo -e "$input" | $XCC || exit 1

  actual=`./a.out` || exit 1

  if [ "$actual" = "$expected" ]; then
    echo "OK"
  else
    echo "NG: $expected expected, but got $actual"
    exit 1
  fi
}

try_output() {
  try_output_direct "$1" "$2" "int main(){ $3 return 0; }"
}

compile_error() {
  title="$1"
  input="$2"

  echo -n "$title => "

  echo -e "$input" | $XCC
  result="$?"

  if [ "$result" = "0" ]; then
    echo "NG: Compile error expected, but succeeded"
    exit 1
  fi
}

try_output 'write' 'hello' "_write(1, \"hello\\\\n\", 6);"
try_output 'char array' 123 "char s[16]; s[0] = '1'; s[1] = '2'; s[2] = '3'; s[3] = '\\\\n'; _write(1, s, 4);"
try_output 'string initializer' 'aBc' "char s[] = \"abc\\\\n\"; s[1] = 'B'; _write(1, s, 4);"
try_direct 'enum' 11 'enum Num { Zero, One, Two }; int main(){ return One + 10; }'
try_direct 'enum with trailing comma' 11 'enum Num { Zero, One, }; int main(){ return One + 10; }'
try_direct 'enum with assign' 11 'enum Num { Ten = 10, Eleven }; int main(){ return Eleven; }'
try_direct 'enum can use in case' 1 'enum Num { Zero, One, Two }; int main(){ switch (1) { case One: return 1; } return 0; }'
try_direct 'typedef' 123 'typedef struct {int x;} Foo; int main(){ Foo foo; foo.x = 123; return foo.x; }'
try_output_direct 'empty function' '' 'void main(){}'
try_direct 'Undeclared struct typedef' 8 'typedef struct FILE FILE; int main(){ return sizeof(FILE*); }'
try_direct 'late declare struct' 42 'struct Foo *p; struct Foo {int x;}; int main(){ struct Foo foo; p = &foo; p->x = 42; return p->x; }'
try_direct 'typedef func-ptr' 84 'typedef int (*Func)(int); int twice(Func f, int x) { return f(f(x)); } int double(int x) { return x * 2; } int main(){ return twice(&double, 21); }'
try_direct 'for-var' 55 'int main(){ int acc = 0; for (int i = 1, len = 10; i <= len; ++i) acc += i; return acc; }'

# error cases
echo ''
echo '### Error cases'
compile_error 'no main' 'void foo(){}'
compile_error 'undef varref' 'int main(){ return x; }'
compile_error 'undef var assign' 'void main(){ x = 1; }'
compile_error 'undef funcall' 'void foo(); void main(){ foo(); }'
compile_error 'no proto def' 'void main(){ foo(); } void foo(){}'
compile_error 'int - ptr' 'void main(){ int *p; p = (void*)1; 2 - p; }'
compile_error '*num' 'void main(){ *123; }'
compile_error '&num' 'void main(){ &123; }'
compile_error '&enum' 'enum Num { Zero }; void main(){ void *p = &Zero; }'
compile_error 'assign to non-lhs' 'void main(){ int x; x + 1 = 3; }'
compile_error '+= to non-lhs' 'void main(){ int x; x + 1 += 3; }'
compile_error 'implicit cast to ptr' 'void foo(int *p); void main(){ foo(123); }'
compile_error 'struct->' 'struct Foo{int x;}; void main(){ struct Foo foo; foo->x; }'
compile_error 'struct*.' 'struct Foo{int x;}; void main(){ struct Foo* p; p.x; }'
compile_error 'int*->' 'void main(){ int *p; p->x; }'
compile_error 'void var' 'void main(){ void x; }'
compile_error 'void param' 'void main(void x){}'
compile_error 'void and param' 'void main(void, int x){}'
compile_error 'void expr' 'void main(){ 1 + (void)2; }'
compile_error 'few arg num' 'void foo(int x){} void main(){ foo(); }'
compile_error 'many arg num' 'void foo(int x){} void main(){ foo(1, 2); }'
compile_error 'zero arg num' 'void foo(void){} void main(){ foo(1); }'
compile_error 'return void' 'void foo(){} void main(){ return foo(); }'
compile_error '+ str' 'void main(){ +"foo"; }'
compile_error '- str' 'void main(){ -"foo"; }'
compile_error 'break outside loop' 'void main(){ break; }'
compile_error 'continue outside loop' 'void main(){ continue; }'
compile_error 'return void' 'void main(){ return 1; }'
compile_error 'return non-void' 'int main(){ return; }'
compile_error 'use before decl' 'void main(){ x = 0; int x; }'
compile_error 'scope invisible' 'int main(){ {int x;} return x; }'
compile_error 'array = ptr' 'void main(){ int a[1], *p; a = p; }'
compile_error 'case outside switch' 'void main(){ switch(0){} case 0: return 0; }'
compile_error 'default outside switch' 'void main(){ switch(0){} default: return 0; }'
compile_error 'vardecl is not stmt' 'int main(){ if (1) int x = 0; return x; }'
compile_error 'static cannot use for local yet' 'void main(){ static int x = 123; }'
compile_error 'same struct name' 'struct Foo{int x;}; union Foo{int y;}; int main(){ return 0; }'
compile_error '`union` for struct' 'struct Foo{int x;}; void main(){ union Foo foo; }'
compile_error 'non exist field initializer' 'struct Foo{int x;}; void main(){ struct Foo foo = {.y=1}; }'
compile_error 'initializer for empty struct' 'struct Foo{}; void main(){ struct Foo foo = {1}; }'
compile_error 'no name nor defined struct ptr' 'void main(){ struct *p; }'
compile_error 'refer undeclared struct member' 'void main(){ struct Foo *p; p->x; }'
compile_error 'extern only' 'extern int x; void main(){ x = 123; }'
compile_error 'extern with init' 'extern int x = 123; void main(){}'
compile_error 'for-var scoped' 'int main(){ for (int i = 0; i < 5; ++i) ; return i; }'
