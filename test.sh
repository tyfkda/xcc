#!/bin/bash

try_direct() {
  title="$1"
  expected="$2"
  input="$3"

  echo -e "$input" | ./xcc > tmp || exit 1
  chmod +x tmp

  echo -n "$title => "

  ./tmp
  actual="$?"

  if [ "$actual" = "$expected" ]; then
    echo "OK"
  else
    echo "NG: $expected expected, but got $actual"
    exit 1
  fi
}

try() {
  try_direct "$1" "$2" "int func(){$3} void main(){ _exit(func()); }"
}

try_output_direct() {
  title="$1"
  expected="$2"
  input="$3"

  echo -e "$input" | ./xcc > tmp || exit 1
  chmod +x tmp

  echo -n "$title => "

  actual=`./tmp` || exit 1

  if [ "$actual" = "$expected" ]; then
    echo "OK"
  else
    echo "NG: $expected expected, but got $actual"
    exit 1
  fi
}

try_output() {
  try_output_direct "$1" "$2" "void main(){ $3 _exit(0); }"
}

compile_error() {
  title="$1"
  input="$2"

  echo -n "$title => "

  echo -e "$input" | ./xcc > tmp
  result="$?"

  if [ "$result" = "0" ]; then
    echo "NG: Compile error expected, but succeeded"
    exit 1
  fi
}

try 'zero' 0 'return 0;'
try 'decimal' 42 'return 42;'
try 'hex' 18 'return 0x12;'
try 'octal' 83 'return 0123;'
try '+-' 21 'return 5+20-4;'
try 'token' 41 ' return 12 + 34 - 5 ;'
try '*+' 47 'return 5+6*7;'
try '()' 15 'return 5*(9-6);'
try '/' 4 'return (3+5)/2;'
try '%' 3 'return 123%10;'
try 'variable' 14 'int a; int b; a = 3; b = 5 * 6 - 8; return a + b / 2;'
try 'variable2' 14 'int foo; int bar; foo = 3; bar = 5 * 6 - 8; return foo + bar / 2;'
try '==' 1 'int a; int b; int c; a = b = (c = 1) + 2; return a == b;'
try '!=' 1 'return 123 != 456;'
try 'preinc' 4 'int x; int y; x = 1; y = ++x; return x + y;'
try 'predec' 0 'int x; int y; x = 1; y = --x; return x + y;'
try 'postinc' 3 'int x; int y; x = 1; y = x++; return x + y;'
try 'postdec' 1 'int x; int y; x = 1; y = x--; return x + y;'
try_direct 'funcall' 23 'int foo(){ return 123; } void main(){ _exit(foo() - 100); }'
try_direct 'func var' 9 'int sqsub(int x, int y){ int xx; int yy; xx = x * x; yy = y * y; return xx - yy; } void main(){ _exit(sqsub(5, 4)); }'
try 'if' 2 'if (1) return 2;'
try 'if else' 3 'if (1 == 0) return 2; else return 3;'
try 'block statement' 3 'int a; int b; a = b = 0; if (1) { a = 1; b = 2; } return a + b;'
try 'while' 55 'int i; int acc; i = acc = 0; while (i <= 10) { acc = acc + i; ++i; } return acc;'
try 'for' 55 'int i; int acc; for (i = acc = 0; i <= 10; i++) acc = acc + i; return acc;'
try 'pointer' 11 'int *p; int x; x = 10; p = &x; ++(*p); return x;'
try 'array' 123 'int a[3]; int *p; p = a; ++p; *p = 123; return *(a + 1);'
try 'array access' 11 'int a[2]; *a = 1; a[1] = 10; return a[0] + 1[a];'
try 'pre-inc pointer' 20 'int a[2]; int *p; a[0] = 10; a[1] = 20; p = a; return *(++p);'
try 'post-inc pointer' 10 'int a[2]; int *p; a[0] = 10; a[1] = 20; p = a; return *p++;'
try_direct 'global' 11 'int x; void main(){ x = 1; _exit(x + 10); }'
try_output 'write' 'hello' "_write(1, \"hello\\\\n\", 6);"
try_output 'char array' 123 "char s[16]; s[0] = '1'; s[1] = '2'; s[2] = '3'; s[3] = '\\\\n'; _write(1, s, 4);"
try_output_direct 'putdeci' 12345 "void putdeci(int x) { char s[16]; char *p; p = s + 16; for (; x != 0; x = x / 10) { p = p - 1; *p = (x % 10) + '0'; } _write(1, p, (s + 16) - p); } void main() { putdeci(12345); _exit(0); }"
try_direct 'struct' 3 'struct Foo{char x; int y;}; void main(){ struct Foo foo; foo.x = 1; foo.y = 2; _exit(foo.x + foo.y);}'
try_direct 'struct pointer' 3 'struct Foo{char x; int y;}; void main(){ struct Foo foo; struct Foo *p; p = &foo; p->x = 1; p->y = 2; _exit(foo.x + foo.y);}'
try_direct 'func pointer' 9 'int sub(int x, int y){ return x - y; } int apply(void *f, int x, int y) { return f(x, y); } void main(){ _exit(apply(&sub, 15, 6)); }'
try 'block comment' 123 '/* comment */ return 123;'
try 'line comment' 123 "// comment\nreturn 123;"
try_direct 'proto decl' 123 'int foo(); void main(){ _exit(foo()); } int foo(){ return 123; }'

# error cases
echo '### Error cases'
compile_error 'no main' 'void foo(){}'
compile_error 'undef var' 'void main(){ x = 1; }'
compile_error 'undef funcall' 'void foo(); void main(){ foo(); }'
compile_error 'no proto def' 'void main(){ foo(); } void foo(){}'
compile_error 'int - ptr' 'void main(){ int *p; p = 1; 2 - p; }'
compile_error '*num' 'void main(){ *123; }'
compile_error '&num' 'void main(){ &123; }'
compile_error 'implicit cast to ptr' 'void foo(int *p); void main(){ foo(123); }'
compile_error 'struct->' 'struct Foo{int x;}; void main(){ struct Foo foo; foo->x; }'
compile_error 'struct*.' 'struct Foo{int x;}; void main(){ struct Foo* p; p.x; }'
compile_error 'int*->' 'void main(){ int *p; p->x; }'
compile_error 'void var' 'void main(){ void x; }'
compile_error 'void param' 'void main(void x){}'
compile_error 'void expr' 'void main(){ 1 + (void)2; }'
compile_error 'few arg num' 'void foo(int x); void main(){ foo(); }'
compile_error 'many arg num' 'void foo(int x); void main(){ foo(1, 2); }'

echo 'All tests PASS!'
