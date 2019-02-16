#!/bin/bash

try_direct() {
  title="$1"
  expected="$2"
  input="$3"

  echo -n "$title => "

  echo -e "$input" | ./xcc > tmp || exit 1
  chmod +x tmp

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

  echo -n "$title => "

  echo -e "$input" | ./xcc > tmp || exit 1
  chmod +x tmp

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
try 'negative' 214 'return -42;'
try '+-' 21 'return 5+20-4;'
try 'token' 41 ' return 12 + 34 - 5 ;'
try '*+' 47 'return 5+6*7;'
try '()' 15 'return 5*(9-6);'
try '/' 4 'return (3+5)/2;'
try '%' 3 'return 123%10;'
try 'variable' 14 'int a = 3; int b = 5 * 6 - 8; return a + b / 2;'
try 'variable2' 14 'int foo = 3; int bar = 5 * 6 - 8; return foo + bar / 2;'
try 'positive var' 42 'int x = 42; return +x;'
try 'negative var' 214 'int x = 42; return -x;'
try '==' 1 'int a; int b; int c; a = b = (c = 1) + 2; return a == b;'
try '!=' 1 'return 123 != 456;'
try 'not true' 0 'return !(1 == 1);'
try 'not false' 1 'return !(0 == 1);'
try 'preinc' 4 'int x = 1; int y = ++x; return x + y;'
try 'predec' 0 'int x = 1; int y = --x; return x + y;'
try 'postinc' 3 'int x = 1; int y = x++; return x + y;'
try 'postdec' 1 'int x = 1; int y = x--; return x + y;'
try '+=' 13 'int x = 10; x += 3; return x;'
try '-=' 7 'int x = 10; x -= 3; return x;'
try '*=' 30 'int x = 10; x *= 3; return x;'
try '/=' 3 'int x = 10; x /= 3; return x;'
try '%=' 1 'int x = 10; x %= 3; return x;'
try_direct 'funcall' 23 'int foo(){ return 123; } void main(){ _exit(foo() - 100); }'
try_direct 'func var' 9 'int sqsub(int x, int y){ int xx = x * x; int yy = y * y; return xx - yy; } void main(){ _exit(sqsub(5, 4)); }'
try 'if' 2 'if (1) return 2; return 3;'
try 'if-false' 3 'if (0) return 2; return 3;'
try 'if else' 2 'if (1 == 1) return 2; else return 3; return 4;'
try 'if else-false' 3 'if (1 == 0) return 2; else return 3; return 4;'
try 'block statement' 3 'int a = 0; int b = 0; if (1) { a = 1; b = 2; } return a + b;'
try 'empty statement' 2 'if (1) ; else return 1; return 2;'
try 'while' 55 'int i = 0; int acc = 0; while (i <= 10) { acc += i; ++i; } return acc;'
try 'for' 55 'int i; int acc; for (i = acc = 0; i <= 10; i++) acc += i; return acc;'
try 'pointer' 11 'int x = 10; int *p = &x; ++(*p); return x;'
try 'array' 123 'int a[3]; int *p = a; ++p; *p = 123; return *(a + 1);'
try 'array access' 11 'int a[2]; *a = 1; a[1] = 10; return a[0] + 1[a];'
try 'pre-inc pointer' 20 'int a[2]; int *p; a[0] = 10; a[1] = 20; p = a; return *(++p);'
try 'post-inc pointer' 10 'int a[2]; int *p; a[0] = 10; a[1] = 20; p = a; return *p++;'
try_direct 'global' 11 'int x; void main(){ x = 1; _exit(x + 10); }'
try_output 'write' 'hello' "_write(1, \"hello\\\\n\", 6);"
try_output 'char array' 123 "char s[16]; s[0] = '1'; s[1] = '2'; s[2] = '3'; s[3] = '\\\\n'; _write(1, s, 4);"
try_output_direct 'putdeci' 12345 "void putdeci(int x) { char s[16]; char *p = s + 16; for (; x != 0; x = x / 10) *(--p) = (x % 10) + '0'; _write(1, p, (s + 16) - p); } void main() { putdeci(12345); _exit(0); }"
try_direct 'struct' 3 'struct Foo{char x; int y;}; void main(){ struct Foo foo; foo.x = 1; foo.y = 2; _exit(foo.x + foo.y);}'
try_direct 'struct pointer' 3 'struct Foo{char x; int y;}; void main(){ struct Foo foo; struct Foo *p = &foo; p->x = 1; p->y = 2; _exit(foo.x + foo.y);}'
try_direct 'func pointer' 9 'int sub(int x, int y){ return x - y; } int apply(void *f, int x, int y) { return f(x, y); } void main(){ _exit(apply(&sub, 15, 6)); }'
try 'block comment' 123 '/* comment */ return 123;'
try 'line comment' 123 "// comment\nreturn 123;"
try_direct 'proto decl' 123 'int foo(); void main(){ _exit(foo()); } int foo(){ return 123; }'
try 'for-break' 10 'int i; int acc; for (i = acc = 0; i <= 10; i++) { if (i == 5) break; acc += i; } return acc;'
try 'for-continue' 50 'int i; int acc; for (i = acc = 0; i <= 10; i++) { if (i == 5) continue; acc += i; } return acc;'
try 'while-break' 10 'int i = 0; int acc = 0; while (++i <= 10) { if (i == 5) break; acc += i; } return acc;'
try 'while-continue' 50 'int i = 0; int acc = 0; while (++i <= 10) { if (i == 5) continue; acc += i; } return acc;'
try 'do-while-break' 10 'int i = 0; int acc = 0; do { if (i == 5) break; acc += i; } while (++i <= 10); return acc;'
try 'do-while-continue' 50 'int i = 0; int acc = 0; do { if (i == 5) continue; acc += i; } while (++i <= 10); return acc;'

# error cases
echo ''
echo '### Error cases'
compile_error 'no main' 'void foo(){}'
compile_error 'undef var' 'void main(){ x = 1; }'
compile_error 'undef funcall' 'void foo(); void main(){ foo(); }'
compile_error 'no proto def' 'void main(){ foo(); } void foo(){}'
compile_error 'int - ptr' 'void main(){ int *p; p = 1; 2 - p; }'
compile_error '*num' 'void main(){ *123; }'
compile_error '&num' 'void main(){ &123; }'
compile_error 'assign to non-lhs' 'void main(){ int x; x + 1 = 3; }'
compile_error '+= to non-lhs' 'void main(){ int x; x + 1 += 3; }'
compile_error 'implicit cast to ptr' 'void foo(int *p); void main(){ foo(123); }'
compile_error 'struct->' 'struct Foo{int x;}; void main(){ struct Foo foo; foo->x; }'
compile_error 'struct*.' 'struct Foo{int x;}; void main(){ struct Foo* p; p.x; }'
compile_error 'int*->' 'void main(){ int *p; p->x; }'
compile_error 'void var' 'void main(){ void x; }'
compile_error 'void param' 'void main(void x){}'
compile_error 'void expr' 'void main(){ 1 + (void)2; }'
compile_error 'few arg num' 'void foo(int x); void main(){ foo(); }'
compile_error 'many arg num' 'void foo(int x); void main(){ foo(1, 2); }'
compile_error '+ str' 'void main(){ +"foo"; }'
compile_error '- str' 'void main(){ -"foo"; }'
compile_error 'break outside loop' 'void main(){ break; }'
compile_error 'continue outside loop' 'void main(){ continue; }'
compile_error 'return void' 'void main(){ return 1; }'
compile_error 'return non-void' 'int main(){ return; }'

echo 'All tests PASS!'
