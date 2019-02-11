#!/bin/bash

try_direct() {
  expected="$1"
  input="$2"

  echo "$input" | ./xcc > tmp || exit 1
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
  try_direct "$1" "int func(){$2} int main(){ _exit(func()); }"
}

try_output_direct() {
  expected="$1"
  input="$2"

  echo "$input" | ./xcc > tmp || exit 1
  chmod +x tmp
  actual=`./tmp` || exit 1

  if [ "$actual" = "$expected" ]; then
    echo "$input => $actual"
  else
    echo "ERROR: $input => $expected expected, but got $actual"
    exit 1
  fi
}

try_output() {
  try_output_direct "$1" "int main(){ $2 _exit(0); }"
}

try 0 'return 0;'
try 42 'return 42;'
try 21 'return 5+20-4;'
try 41 ' return 12 + 34 - 5 ;'
try 47 'return 5+6*7;'
try 15 'return 5*(9-6);'
try 4 'return (3+5)/2;'
try 3 'return 123%10;'
try 14 'int a; int b; a = 3; b = 5 * 6 - 8; return a + b / 2;'
try 14 'int foo; int bar; foo = 3; bar = 5 * 6 - 8; return foo + bar / 2;'
try 1 'int a; int b; int c; a = b = (c = 1) + 2; return a == b;'
try 1 'return 123 != 456;'
try 4 'int x; int y; x = 1; y = ++x; x + y;'
try 0 'int x; int y; x = 1; y = --x; x + y;'
try 3 'int x; int y; x = 1; y = x++; x + y;'
try 1 'int x; int y; x = 1; y = x--; x + y;'
try_direct 23 'int foo(){ return 123; } int main(){ _exit(foo() - 100); }'
try_direct 9 'int sqsub(int x, int y){ int xx; int yy; xx = x * x; yy = y * y; return xx - yy; } int main(){ _exit(sqsub(5, 4)); }'
try 2 'if (1) return 2;'
try 3 'if (1 == 0) return 2; else return 3;'
try 3 'int a; int b; a = b = 0; if (1) { a = 1; b = 2; } return a + b;'
try 55 'int i; int acc; i = acc = 0; while (i <= 10) { acc = acc + i; ++i; } return acc;'
try 55 'int i; int acc; for (i = acc = 0; i <= 10; i++) acc = acc + i; return acc;'
try 11 'int *p; int x; x = 10; p = &x; ++(*p); return x;'
try 123 'int a[3]; int *p; p = a; ++p; *p = 123; return *(a + 1);'
try 11 'int a[2]; *a = 1; a[1] = 10; return a[0] + 1[a];'
try 20 'int a[2]; int *p; a[0] = 10; a[1] = 20; p = a; *(++p);'
try 10 'int a[2]; int *p; a[0] = 10; a[1] = 20; p = a; *p++;'
try_direct 11 'int x; int main(){ x = 1; _exit(x + 10); }'
try_output 'hello' '_write(1, "hello\n", 6);'
try_output 123 "char s[16]; s[0] = '1'; s[1] = '2'; s[2] = '3'; s[3] = '\n'; _write(1, s, 4);"
try_output_direct 12345 "void putdeci(int x) { char s[16]; char *p; p = s + 16; for (; x != 0; x = x / 10) { p = p - 1; *p = (x % 10) + '0'; } _write(1, p, (s + 16) - p); } int main() { putdeci(12345); _exit(0); }"
try_direct 3 'struct Foo{char x; int y;}; void main(){ struct Foo foo; foo.x = 1; foo.y = 2; _exit(foo.x + foo.y);}'
try_direct 3 'struct Foo{char x; int y;}; void main(){ struct Foo foo; struct Foo *p; p = &foo; p->x = 1; p->y = 2; _exit(foo.x + foo.y);}'
try_direct 9 'int sub(int x, int y){ return x - y; } int apply(void *f, int x, int y) { return f(x, y); } void main(){ _exit(apply(&sub, 15, 6)); }'

echo OK
