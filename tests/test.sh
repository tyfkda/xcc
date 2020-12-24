#!/bin/bash

XCC=${XCC:-../xcc}

if [ "$(uname)" == 'Darwin' ]; then
  PROLOGUE=$(cat <<EOS
extern void exit(int code);
extern long write(int fd, const char *str, long len);
EOS
  )
else
  PROLOGUE=$(cat <<EOS
void _start() {
  __asm("mov (%rsp), %rdi");
  __asm("lea 8(%rsp), %rsi");
  __asm("call main");
  __asm("mov %eax, %edi");
  __asm("jmp exit");
}
void exit(int code) {
  __asm("mov \$60, %eax");  // __NR_exit
  __asm("syscall");
}
long write(int fd, const char *str, long len) {
  __asm("mov \$1, %eax");  // __NR_write
  __asm("syscall");
}
EOS
  )
fi


try_direct() {
  local title="$1"
  local expected="$2"
  local input="$PROLOGUE\n$3"

  echo -n "$title => "

  local tmpfile=$(mktemp).c
  echo -e "$input" > $tmpfile
  $XCC -I../include $tmpfile || exit 1

  ./a.out
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

  local tmpfile=$(mktemp).c
  echo -e "$input" > $tmpfile
  $XCC $tmpfile || exit 1

  local actual
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
  local title="$1"
  local input="$PROLOGUE\n$2"

  echo -n "$title => "

  local tmpfile=$(mktemp).c
  echo -e "$input" > $tmpfile
  $XCC $tmpfile
  local result="$?"

  if [ "$result" = "0" ]; then
    echo "NG: Compile error expected, but succeeded"
    exit 1
  fi
}

try_output 'write' 'hello' "write(1, \"hello\\\\n\", 6);"
try_output 'char array' 123 "char s[16]; s[0] = '1'; s[1] = '2'; s[2] = '3'; s[3] = '\\\\n'; write(1, s, 4);"
try_output 'string initializer' 'aBc' "char s[] = \"abc\\\\n\"; s[1] = 'B'; write(1, s, 4);"
try 'non nul-terminated str' 8 'static char s[4]="abcd"; char l[4]="efgh"; return sizeof(s) + sizeof(l);'
try 'non nul-terminated str in struct' 4 'struct S {char str[4];} static s={"abcd"}; return sizeof(s);'
try 'cast string' 120 'char *s = (char*)"x"; return s[0];'
try 'cast string static' 120 'static char *s = (char*)"x"; return s[0];'
try_direct 'cast str to int' 116 'long x = (long)"str"; int main(){ char *p = (char*)x; return p[1]; }'
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
try_output_direct 'empty function' '' 'void foo(){} int main(){ foo(); return 0; }'
try_output_direct 'empty block' '' 'int main(){ ; {} {;;;} return 0; }'
try_direct 'Undeclared struct typedef' 8 'typedef struct FILE FILE; int main(){ return sizeof(FILE*); }'
try_direct 'late declare struct' 42 'struct Foo *p; struct Foo {int x;}; int main(){ struct Foo foo; p = &foo; p->x = 42; return p->x; }'
try 'scoped struct' 5 'int size; struct S {int x;}; { struct S {char y;}; size = sizeof(struct S); } return size + sizeof(struct S);'
try 'scoped typedef' 5 'int size; typedef struct {int x;} S; { typedef struct {char y;} S; size = sizeof(S); } return size + sizeof(S);'
try_direct 'typedef func-ptr' 84 'typedef int (*Func)(int); int twice(Func f, int x) { return f(f(x)); } int mul2(int x) { return x * 2; } int main(){ return twice(&mul2, 21); }'
try_direct 'typedef func' 25 'typedef int Func(int); int twice(Func f, int x) { return f(f(x)); } int add2(int x) { return x + 2; } int main(){ return twice(add2, 21); }'
try_direct 'multi typedef' 4 'typedef char T1, T2[4]; int main() {return sizeof(T2);}'
try_direct 'typedef void' 91 'typedef void VOID; VOID sub(VOID){} int main(){sub(); return 91;}'
try_direct 'old-style func' 93 'int sub(); int main(){ return sub(31); } int sub(int x) { return x * 3; }'
try_direct 'old-func-ptr' 81 'int twice(int(*f)(), int x) { return f(f(x)); } int sqr(int x) { return x * x; } int main(){ return twice(sqr, 3); }'
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
try 'self referential struct' 1 'struct S {struct S *p;} *s = 0; return sizeof(s->p[0]) == sizeof(void*);'
try_direct '(void)x;' 0 'void func(int x) { (void)x; } int main(){ func(123); return 0; }'
try_output 'strings' 'hello world' "write(1, \"hello \" \"world\\\\n\", 12);"
try_direct 'init union' 77 'union { int x; struct { char a; short b; } y; } u = {.y={.b=77}}; int main(){ return u.y.b; }'
try_direct 'goto' 1 'int main(){ int x = 1; goto label; x = 2; label: return x; }'
try 'goto opt' 88 'j3: goto j1; goto j2; j2: goto j3; j1: return 88;'
try_output '*const' foobar 'const char* const str = "foobar"; write(1, str, 6);'
try_direct 'switch w/o case' 1 'int main(){ int x = 0; switch (0) {default: x = 1; break;} return x; }'
try_direct 'switch w/o case & default' 0 'int main(){ int x = 0; switch (0) {x = 1;} return x; }'
try 'switch-if-default' 49 'switch(0){if(0){default: return 49;}} return 94;'
try 'post inc pointer' 1 'char *p = (char*)(-1L); p++; return p == 0;'
try_direct 'more params' 36 'int func(int a, int b, int c, int d, int e, int f, char g, int h) { return a + b + c + d + e + f + g + h; } int main(){ return func(1, 2, 3, 4, 5, 6, 7, 8); }'
try_direct 'more params w/ struct' 143 'typedef struct {int x;} S; S func(int a, int b, int c, int d, int e, int f, int g) { return (S){f + g}; } int main(){ S s = func(11, 22, 33, 44, 55, 66, 77); return s.x; }'
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
try_direct 'struct args' 82 'typedef struct {int a; int b;} X; int sub(X x, int k) { return x.a * k + x.b; } int main() { X x = {12, 34}; return sub(x, 4); }'
try 'ternary string' 114 'int x = 1; const char *p = x ? "true" : "false"; return p[1];'
try 'ternary ptr:0' 98 'const char *p = "abc"; p = p != 0 ? p + 1 : 0; return *p;'
try_direct 'ternary w/ func' 53 'int f(){return 27;} int g(){return 53;} int main(){return (0?f:g)();}'
try_output 'ternary void' 'false' "0 ? (void)write(1, \"true\", 4) : (void)write(1, \"false\", 5);"
try_direct 'compound literal:array' 2 'int main(){ int *foo = (int[]){1, 2, 3}; return foo[1]; }'
try_direct 'compound literal:struct' 66 'struct Foo {int x;}; int main(){ struct Foo *foo = &(struct Foo){66}; return foo->x; }'
try_direct 'inc compound literal' 56 'int main(){ int i = ++(int){55}; return i; }'
try_direct '&()' 86 'void sub(int *p) {*p *= 2;} int main() {int x = 43; sub(&(x)); return x;}'
try 'pre-inc ()' 34 'int x = 33; return ++(x);'
try 'post-dec ()' 44 'int x = 44; return (x)--;'
try_direct 'return struct' 46 'typedef struct { int x; int y; } S; S func(void) { S s = {.x = 12, .y = 34}; return s; } int main(){ S s = func(); return s.x + s.y; }'
try_direct 'return struct not broken' 222 'typedef struct {long x; long y;} S; S sub(){S s={111, 222}; return s;} int main(){int dummy[1]; S s; s = sub(); return s.y;}'
try_direct 'return struct member' 57 'typedef struct {int x;} S; S func() {return (S){57};} int main(){return func().x;}'
try_direct 'modify arg' 32 'int sub(int x, int y) {return x+y;} int main() {int w=0, x=0, y=5; int z=sub(++x, y+=10); return x+y+z+w;}'
try_direct 'long immediate' 240 'int sub(unsigned long x){return x;} int main(){ return sub(0x123456789abcdef0); }'
try 'can assign const ptr' 97 'const char *p = "foo"; p = "bar"; return p[1];'
try 'str' 75 '"use strict"; return 75;'
try 'str in comma' 117 'char *p = (1, "use strict", "dummy"); return p[1];'
try_direct 'return str' 111 'const char *foo(){ return "foo"; } int main(){ return foo()[2]; }'
try 'deref str' 48 'return *"0";'

try_direct 'stdarg' 55 "#include <stdarg.h>
int f(int n, ...) {int a[14*2]; for (int i=0; i<14*2; ++i) a[i]=100+i; va_list ap; va_start(ap, n); int sum=0; for (int i=0; i<n; ++i) sum+=va_arg(ap, int); va_end(ap); return sum;}
int main(){return f(10, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10);}"

# error cases
echo ''
echo '### Error cases'
compile_error 'no main' 'void foo(){}'
compile_error 'comment not closed' 'void main(){} /*'
compile_error 'undef varref' 'int main(){ return x; }'
compile_error 'undef var assign' 'void main(){ x = 1; }'
compile_error 'undef funcall' 'void foo(); void main(){ foo(); }'
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
compile_error 'void param' 'void main(void x){}'
compile_error 'void and param' 'void main(void, int x){}'
compile_error 'void expr' 'void main(){ 1 + (void)2; }'
compile_error 'few arg num' 'void foo(int x){} void main(){ foo(); }'
compile_error 'many arg num' 'void foo(int x){} void main(){ foo(1, 2); }'
compile_error 'zero arg num' 'void foo(void){} void main(){ foo(1); }'
compile_error 'empty char' "int main() { return ''; }"
compile_error 'no single char' "int main() { return '12'; }"
compile_error '+ str' 'void main(){ +"foo"; }'
compile_error '- str' 'void main(){ -"foo"; }'
compile_error 'break outside loop' 'void main(){ break; }'
compile_error 'continue outside loop' 'void main(){ continue; }'
compile_error 'return void' 'void main(){ return 1; }'
compile_error 'return void2' 'int main(){ return (void)0; }'
compile_error 'return non-void' 'int main(){ return; }'
compile_error 'use before decl' 'void main(){ x = 0; int x; }'
compile_error 'scope invisible' 'int main(){ {int x;} return x; }'
compile_error 'array = ptr' 'void main(){ int a[1], *p; a = p; }'
compile_error 'case outside switch' 'int main(){ switch(0){} case 0: return 0; }'
compile_error 'dup cases' 'void main(){ switch(0){case 1: break; case 1: break;} }'
compile_error 'case is int only' 'void main(){ switch(0){case "foo": break;} }'
compile_error 'default outside switch' 'int main(){ switch(0){} default: return 0; }'
compile_error 'non-const case' 'void main(){int x=1; switch (x){case x: x=2;} return x;}'
compile_error 'vardecl is not stmt' 'void main(){ if (1) int x = 0; }'
compile_error 'same struct name' 'struct Foo{int x;}; struct Foo{int x;}; void main(){}'
compile_error 'same struct name in scope' 'void main(){struct Foo{int x;}; struct Foo{int x;}; }'
compile_error 'same struct/union name' 'struct Foo{int x;}; union Foo{int y;}; void main(){}'
compile_error '`union` for struct' 'struct Foo{int x;}; void main(){ union Foo foo; }'
compile_error 'non exist field initializer' 'struct Foo{int x;}; void main(){ struct Foo foo = {.y=1}; }'
compile_error 'initializer for empty struct' 'struct Foo{}; void main(){ struct Foo foo = {1}; }'
compile_error 'no name nor defined struct ptr' 'void main(){ struct *p; }'
compile_error 'refer undeclared struct member' 'void main(){ struct Foo *p; p->x; }'
compile_error 'undeclared struct init' 'struct Foo foo={12}; void main(){}'
compile_error 'undeclared struct array init' 'struct Foo arr[]={{1}, {2}, {3}}; void main(){}'
compile_error 'extern only' 'extern int x; void main(){ x = 123; }'
compile_error 'extern with init' 'extern int x = 123; void main(){}'
compile_error 'char array init with ptr' 'char* foo = "foo"; void main(){ char bar[] = foo; }'
compile_error 'for-var scoped' 'int main(){ for (int i = 0; i < 5; ++i) ; return i; }'
compile_error 'global poitner init with undefined' 'char *p = &x; void main(){}'
compile_error 'global poitner init with other type' 'int x; char *p = &x; void main(){}'
compile_error 'global poitner init with fixnum' 'void *main = 1234;'
compile_error 'use void' 'void func(){} void main(){ int a = (int)func(); }'
compile_error 'goto no-label' 'void main(){ goto label; }'
compile_error 'goto dup-label' 'void main(){ label: goto label; label:; }'
compile_error 'funparam static' 'void main(static int argc){}'
compile_error 'funparam extern' 'void main(extern int argc){}'
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
compile_error 'no VLA' 'void main(int x){ int array[x]; }'
compile_error 'negative array' "void main(){ int array[-1]; }"
compile_error 'zero array' "void main(){ int array[0]; }"
compile_error 'size unknown' 'extern char string[]; int main(){ return sizeof(string); } char string[] = "Hello";'
compile_error 'scoped typedef' 'int sub(){typedef int T;} T g=123; int main(void){return g;}'
compile_error 'no member name' 'struct Foo{union{int anon;}; int;}; void main(){}'
compile_error 'func retval ref' 'typedef struct {int x;} S; S func() {return (S){111};} int main(){S *p = &func(); return s->x;}'
compile_error 'if void' 'void main(){if ((void)0) {}}'
compile_error 'while void' 'void main(){while ((void)1) {}}'
compile_error 'do-while void' 'void main(){do {} while ((void)-2);}'
compile_error 'for void' 'void main(){for (; (void)3; ) {}}'
compile_error 'switch void' 'void main(){switch ((void)4) {}}'
compile_error 'assign const' 'const int G = 0; void main(){G=1;}'
compile_error 'assign const struct' 'const struct S {int x;} s = {100}; int main(){s.x = 1; return s.x;}'
compile_error 'duplicate func' 'void main(){} void main(){}'
compile_error 'conflict func' 'void main(); int main(int, char**){}'
compile_error 'duplicate var & func' 'int main; int main(){}'
compile_error 'duplicate func & var' 'int main(){} int main;'
# flonum
compile_error 'array[double]' 'void main(){int a[]={1, 2, 3}; double d=1; return a[d];}'
compile_error 'ptr + f' 'void main(){int a[]={1, 2, 3}; double d=1; int *p=(a+3)-d; return *p;}'
