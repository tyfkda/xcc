#!/bin/bash

WCC=${WCC:-../wcc}

PROLOGUE=$(cat <<EOS
extern void exit(int code);
extern long write(int fd, const char *str, long len);
int _start(int argc, char *argv[]) {
  extern int main(int, char**);
  return main(argc, argv);
}
EOS
)
PTRSIZE=4

try_direct() {
  local title="$1"
  local expected="$2"
  local input="$PROLOGUE\n$3"

  echo -n "$title => "

  echo -e "$input" | $WCC || exit 1

  node ../runtime/runwasm.js a.wasm
  local actual="$?"

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
  local title="$1"
  local expected="$2"
  local input="$PROLOGUE\n$3"

  echo -n "$title => "

  echo -e "$input" | $WCC || exit 1

  local actual
  actual=`node ../runtime/runwasm.js a.wasm` || exit 1

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
  local title="$1"
  local input="$PROLOGUE\n$2"

  echo -n "$title => "

  echo -e "$input" | $WCC
  local result="$?"

  if [ "$result" = "0" ]; then
    echo "NG: Compile error expected, but succeeded"
    exit 1
  fi
}

try_direct '+*' 7 'int main(){return 1+2*3;}'
try_direct 'local var' 72 'int main(){int x=8, y=9; return x*y;}'
try_direct 'i64, f32, f64' 82 'float f32(int i){return i+1;} double f64(float f){return f*2;} long i64(double d){return d/3;} int main(){return i64(f64(f32(123)));}'
try_direct 'pre-inc' 55 'int main(){int x=4; int y=++x; return x*10+y;}'
try_direct 'pre-dec' 33 'int main(){int x=4; int y=--x; return x*10+y;}'
try_direct 'post-inc' 54 'int main(){int x=4; int y=x++; return x*10+y;}'
try_direct 'post-dec' 34 'int main(){int x=4; int y=x--; return x*10+y;}'
try_direct 'if-t' 11 'int main(){int x=0, y; if (x==0) y=11; else y=22; return y;}'
try_direct 'if-f' 22 'int main(){int x=1, y; if (x==0) y=11; else y=22; return y;}'
try_direct 'if-const' 11 'int main(){int x=1, y; if (x) y=11; else y=22; return y;}'
try_direct 'ternary' 0 'int main(){int a=33, b=44, c=22; return a>b ? a!=c : b<c;}'
try_direct 'switch match' 22 'int main(){int x=2; switch (x) {case 1: x=11; break; case 2: x=22; break; default: x=99;} return x;}'
try_direct 'switch default' 99 'int main(){int x=5; switch (x) {case 1: x=11; break; case 2: x=22; break; default: x=99;} return x;}'
try_direct 'switch no-match' 5 'int main(){int x=5; switch (x) {case 1: x=11; break; case 2: x=22; break;} return x;}'
try_direct 'switch fallthrough' 99 'int main(){int x=1; switch (++x) {case 1: x=11; break; case 2: x=22; /*break;*/ default: x=99;} return x;}'
try_direct 'while' 55 'int main(){int acc=0, i=1; while (i<=10) {acc+=i; ++i;} return acc;}'
try_direct 'do-while' 55 'int main(){int acc=0, i=1; do {acc+=i; ++i;} while (i<=10); return acc;}'
try_direct 'for' 55 'int main(){int acc=0; for (int i=1; i<=10; ++i) acc+=i; return acc;}'
try_direct 'break, continue' 30 'int main(){int s=0; for (int i=1; i<=10; ++i) {if ((i&1)!=0) continue; s+=i; if (i>=10) break;} return s;}'
try_direct 'return in block' 55 'int main(){int acc=0, i=1; for (;;) {acc+=i; if (i==10) return acc; ++i;}}'
try_output_direct 'call imported func' '12321' 'void puti(int); int sq(int x){return x*x;} int main(){puti(sq(111)); return 0;}'
try_direct 'global var' 24 'static int g; int main(){g+=10; g-=2; g*=3; return g;}'
try_direct 'global data' 66 'int a[]={11, 22, 33}; int main(){int s=0; for (int i=0; i<3; ++i) s+=a[i]; return s;}'
try_direct 'local array' 66 'int main(){short a[]={11, 22, 33}; int s=0; for (int i=0; i<3; ++i) s+=a[i]; return s;}'
try_direct 'struct ptr' 32 'typedef struct {int x;} S; S g; int main(){S l; S *p=&g; p->x=321; p=&l; p->x=10; return g.x/l.x;}'
try_direct 'take ref' 15 'int g=10; int sub(int p){int *pp=&p; return *pp/2;} int main(){int l=20; int *pl=&l, *pg=&g; return sub(*pl+*pg);}'
try_output_direct 'string literal' 'Hello, world!' 'void putstr(const char*); int main(){putstr("Hello, world!\\n"); return 0;}'
try_output_direct 'char-ptr' 'Hello, world!' 'void putstr(const char*); const char *gstr="Hello, world!\\n"; int main(){putstr(gstr); return 0;}'
try_output_direct 'char-ptr array' 'Hello, world!' 'void putstr(const char*); const char *gstr[]={"Hello, world!\\n"}; int main(){putstr(gstr[0]); return 0;}'
try_direct 'assign struct' 37 'int main(){typedef struct{int x;}S; S s={37}; S t; t=s; return s.x;}'
try_direct 'vaarg' 15 'double sub(double x, ...) {__builtin_va_list ap; __builtin_va_start(ap, x); int i=__builtin_va_arg(ap, int); double d=__builtin_va_arg(ap, double); __builtin_va_end(ap); return x*i/d;} int main(){return sub(2.0, (unsigned char)60, 8.0);}'
try_direct 'indirect funcall' 61 'int sub(void){return 61;} int main(){int (*f)(void)=sub; return f();}'

try_output 'write' 'hello' "write(1, \"hello\\\\n\", 6);"
try_output 'char array' 123 "char s[16]; s[0] = '1'; s[1] = '2'; s[2] = '3'; s[3] = '\\\\n'; write(1, s, 4);"
try_output 'string initializer' 'aBc' "char s[] = \"abc\\\\n\"; s[1] = 'B'; write(1, s, 4);"
try 'cast string' 120 'char *s = (char*)"x"; return s[0];'
try 'cast string static' 120 'static char *s = (char*)"x"; return s[0];'
try_direct 'brace initializer' 34 'int main(){ int x = {34}; return x; }'
try 'paren =' 98 'int x; (x) = 98; return x;'
try_direct 'enum' 11 'enum Num { Zero, One, Two }; int main(){ return One + 10; }'
try 'local enum' 23 'enum Num { Zero, One, Two }; return One + 22;'
try_direct 'enum with trailing comma' 11 'enum Num { Zero, One, }; int main(){ return One + 10; }'
try_direct 'enum with assign' 11 'enum Num { Ten = 10, Eleven }; int main(){ return Eleven; }'
try_direct 'enum can use in case' 1 'enum Num { Zero, One, Two }; int main(){ switch (1) { case One: return 1; } return 0; }'
try_direct 'compare enum' 0 'enum Num { Zero, One, Two }; int main(){ enum Num num = Zero; return num == One; }'
try_direct 'enum initializer' 0 'enum Num { Zero } num = Zero; int main(){ return num; }'
try_direct 'enum initializer2' 67 'enum Num { Zero } num = 67; int main(){ return num; }'
try_direct 'global enum variable' 1 'enum Foo { BAR, BAZ }; enum Foo foo; int main(){ foo = BAZ; return foo; }'
try_direct 'typedef' 123 'typedef struct {int x, y;} Foo; int main(){ Foo foo; foo.x = 123; return foo.x; }'
try_direct 'same typedef' 66 'typedef int Foo; typedef int Foo; int main(){ return 66; }'
try_output_direct 'empty function' '' 'void foo(void){} int main(){ foo(); return 0; }'
try_direct 'Undeclared struct typedef' $PTRSIZE 'typedef struct FILE FILE; int main(){ return sizeof(FILE*); }'
try_direct 'late declare struct' 42 'struct Foo *p; struct Foo {int x;}; int main(){ struct Foo foo; p = &foo; p->x = 42; return p->x; }'
try 'scoped struct' 5 'int size; struct S {int x;}; { struct S {char y;}; size = sizeof(struct S); } return size + sizeof(struct S);'
try 'scoped typedef' 5 'int size; typedef struct {int x;} S; { typedef struct {char y;} S; size = sizeof(S); } return size + sizeof(S);'
try_direct 'typedef func-ptr' 84 'typedef int (*Func)(int); int twice(Func f, int x) { return f(f(x)); } int mul2(int x) { return x * 2; } int main(){ return twice(&mul2, 21); }'
try_direct 'multi typedef' 4 'typedef char T1, T2[4]; int main() {return sizeof(T2);}'
try_direct 'old-style func' 93 'int sub(); int main(){ return sub(31); } int sub(int x) { return x * 3; }'
#try_direct 'old-func-ptr' 81 'int twice(int(*f)(), int x) { return f(f(x)); } int sqr(int x) { return x * x; } int main(){ return twice(sqr, 3); }'
try_direct 'global-func-var' 88 'int sub(void) { return 88; } int (*f)(void) = sub; int main(){ return f(); }'
try_direct 'static-func-var' 99 'int sub(void) { return 99; } int main(){ static int (*f)(void) = sub; return f(); }'
try_direct 'for-var' 55 'int main(){ int acc = 0; for (int i = 1, len = 10; i <= len; ++i) acc += i; return acc; }'
try_direct 'for-no-initial-val' 3 "int main(){ const char *p = \"abc\"; int len = 0; for (char c; (c = *p) != '\\\\0'; ++p) ++len; return len; }"
try_direct 'args' 51 'int func(int x, ...) { return x; } int main(){ return func(51, 1, 2); }'
try_output_direct 'global str-array init' 'StrArray' 'char g_str[] = "StrArray"; int main(){ write(1, g_str, sizeof(g_str) - 1); return 0; }'
try_output_direct 'global str-ptr init' 'StrPtr' 'char *g_str = "StrPtr"; int main(){ write(1, g_str, 6); return 0; }'
try_output_direct 'global str-array init' 'StrPtr' 'char *g_str[] = {"StrPtr"}; int main(){ write(1, g_str[0], 6); return 0; }'
try_output_direct 'global str-in-struct init' 'StrPtr' 'struct {char *s;} g_str[] = {{"StrPtr"}}; int main(){ write(1, g_str[0].s, 6); return 0; }'
try_output_direct 'global char-array-in-struct init' 'abc' 'struct {char s[4];} g_str[] = {{"abc"}}; int main(){ write(1, g_str[0].s, 3); return 0; }'
try_output_direct 'global ptr-ref1' '456' 'char str[] = "0123456789"; char *s = str + 4; int main(){ write(1, s, 3); return 0; }'
try_output_direct 'global ptr-ref2' '456' 'char str[] = "0123456789"; char *s = &str[4]; int main(){ write(1, s, 3); return 0; }'
try_direct 'global array' 42 'int array[] = {10,20,30}; int main(){ return sizeof(array) + array[2]; }'
try 'static ref' 22 'static const int array[] = {11,22,33}; static int *p = array; return p[1];'
try_direct 'local static array' 42 'int main(){ static int array[] = {10,20,30}; return sizeof(array) + array[2]; }'
try 'int static const' 34 'int static const a = 34; return a;'
try 'struct static const' 67 'struct {int x;} static const a[] = {{67}}; return a[0].x;'
try 'init struct contain union' 99 'struct { union { long a; char b; } x; int y; } static s = {.x={.b=88}, .y=99}; return s.y;'
try 'sizeof(self) in initializer' 4 'int x = sizeof(x); return x;'
try_direct '(void)x;' 0 'void func(int x) { (void)x; } int main(){ func(123); return 0; }'
try_output 'strings' 'hello world' "write(1, \"hello \" \"world\\\\n\", 12);"
try_direct 'init union' 77 'union { int x; struct { char a; short b; } y; } u = {.y={.b=77}}; int main(){ return u.y.b; }'
#try_direct 'goto' 1 'int main(){ int x = 1; goto label; x = 2; label: return x; }'
try_output '*const' foobar 'const char* const str = "foobar"; write(1, str, 6);'
try_direct 'switch w/o case' 1 'int main(){ int x = 0; switch (0) {default: x = 1; break;} return x; }'
try_direct 'switch w/o case & default' 0 'int main(){ int x = 0; switch (0) {x = 1;} return x; }'
try 'post inc pointer' 1 'char *p = (char*)(-1L); p++; return p == 0;'
try_direct 'more params' 36 'int func(int a, int b, int c, int d, int e, int f, char g, int h) { return a + b + c + d + e + f + g + h; } int main(){ return func(1, 2, 3, 4, 5, 6, 7, 8); }'
#try_direct 'more params w/ struct' 143 'typedef struct {int x;} S; S func(int a, int b, int c, int d, int e, int f, int g) { return (S){f + g}; } int main(){ S s = func(11, 22, 33, 44, 55, 66, 77); return s.x; }'
try 'shadow var' 10 'int x = 1; { x = 10; int x = 100; } return x;'
try_direct 'struct assign' 33 'struct Foo { int x; }; int main(){ struct Foo foo, bar; foo.x = 33; bar = foo; return bar.x; }'
try_direct 'struct initial assign' 55 'struct Foo { int x; }; int main(){ struct Foo foo = {55}, bar = foo; return bar.x; }'
try_direct 'struct deref' 44 'struct Foo { long x; }; int main(){ struct Foo foo, bar, *baz = &bar; baz->x = 44; foo = *baz; return foo.x; }'
try_direct 'struct copy' 51 'typedef struct {int x;} S; void copy(S *e1, S *e2){*e1=*e2;} int main(){S s={51},x; copy(&x,&s); return x.x;}'
try_direct 'empty struct size' 0 'struct empty {}; int main(){ return sizeof(struct empty); }'
try 'empty struct copy' 0 'struct empty {}; struct empty a = {}, b; b = a; return sizeof(b);'
try_direct 'typedef name can use in local' 61 'typedef int Foo; int main(){ int Foo = 61; return Foo; }'
try_direct 'proto in func' 78 'int main(){ int sub(int); return sub(77); } int sub(int x) { return x + 1; }'
try_direct 'extern in func' 45 'int main(){ extern int g; g = 45; return g; } int g;'
try_direct 'array arg w/o size' 22 'extern int array[]; int sub(int arg[]) { return arg[1]; } int main(){ return sub(array); } int array[] = {11, 22, 33};'
try_direct 'anonymous union init' 99 'struct {union {int x;};} a = {.x = 99}; int main(){ return a.x; }'
try 'func ref' 1 'return main == &main;'
try 'func deref' 1 'return (long)main == (long)*main;'
try 'implicit int' 92 'unsigned x = 92; return x;'
try_direct 'func-ptr-array' 30 'int mul2(int x) {return x*2;} int div2(int x) {return x/2;} int (*funcs[])(int)={mul2, div2}; int main() {int acc=0; for (int i=0; i<2; ++i) acc+=funcs[i](12); return acc;}'
try_direct 'func-ptr-array in local' 30 'int mul2(int x) {return x*2;} int div2(int x) {return x/2;} int main() {int (*funcs[])(int)={mul2, div2}; int acc=0; for (int i=0; i<2; ++i) acc+=funcs[i](12); return acc;}'
#try_direct 'struct args' 82 'typedef struct {int a; int b;} X; int sub(X x, int k) { return x.a * k + x.b; } int main() { X x = {12, 34}; return sub(x, 4); }'
try 'ternary string' 114 'int x = 1; const char *p = x ? "true" : "false"; return p[1];'
try 'ternary ptr:0' 98 'const char *p = "abc"; p = p != 0 ? p + 1 : 0; return *p;'
try_direct 'ternary w/ func' 53 'int f(void){return 27;} int g(void){return 53;} int main(void){return (0?f:g)();}'
try_output 'ternary void' 'false' "0 ? (void)write(1, \"true\", 4) : (void)write(1, \"false\", 5);"
#try_direct 'compound literal:array' 2 'int main(){ int *foo = (int[]){1, 2, 3}; return foo[1]; }'
#try_direct 'compound literal:struct' 66 'struct Foo {int x;}; int main(){ struct Foo *foo = &(struct Foo){66}; return foo->x; }'
#try_direct 'inc compound literal' 56 'int main(){ int i = ++(int){55}; return i; }'
try_direct '&()' 86 'void sub(int *p) {*p *= 2;} int main() {int x = 43; sub(&(x)); return x;}'
try 'pre-inc ()' 34 'int x = 33; return ++(x);'
try 'post-dec ()' 44 'int x = 44; return (x)--;'
#try_direct 'return struct' 46 'typedef struct { int x; int y; } S; S func(void) { S s = {.x = 12, .y = 34}; return s; } int main(){ S s = func(); return s.x + s.y; }'
#try_direct 'return struct not broken' 222 'typedef struct {long x; long y;} S; S sub(){S s={111, 222}; return s;} int main(){int dummy[1]; S s; s = sub(); return s.y;}'
#try_direct 'return struct member' 57 'typedef struct {int x;} S; S func() {return (S){57};} int main(){return func().x;}'
try_direct 'modify arg' 32 'int sub(int x, int y) {return x+y;} int main() {int w=0, x=0, y=5; int z=sub(++x, y+=10); return x+y+z+w;}'
try_direct 'long immediate' 240 'int sub(unsigned long x){return x;} int main(){ return sub(0x123456789abcdef0); }'
try 'can assign const ptr' 97 'const char *p = "foo"; p = "bar"; return p[1];'
try 'str' 75 '"use strict"; return 75;'
try 'str in comma' 117 'char *p = (1, "use strict", "dummy"); return p[1];'
try_direct 'return str' 111 'const char *foo(void){ return "foo"; } int main(){ return foo()[2]; }'
try 'deref str' 48 'return *"0";'

try_direct 'ref static' 29 'int main() {static int f=29; int *p = &f; return *p;}'

# error cases
echo ''
echo '### Error cases'
compile_error 'cannot use goto in wcc (yet)' 'void main(){ goto label; label:; }'
compile_error 'switch-if-default' 'int main(){switch(0){if(0){default: return 49;}} return 94;}'
compile_error 'cannot use old-style func' 'int apply(int(*f)(), int x){return f(x);} int sqr(int x){return x * x;} int main(){return apply(sqr, 3);}'
compile_error 'struct args' 'typedef struct{int a; int b;} X; int sub(X x){return x.a+x.b;} int main(){X x={12, 34}; return sub(x);}'
compile_error 'compound literal:struct' 'struct Foo{int x;}; int main(){struct Foo *foo=&(struct Foo){66}; return foo->x;}'
compile_error 'return struct' 'typedef struct{int x; int y;} S; S func(void){S s={.x=12, .y=34}; return s;} int main(){S s=func(); return s.x+s.y;}'
