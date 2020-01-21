#!/bin/bash

XCC=${XCC:-../xcc}

if [ "$(uname)" == 'Darwin' ]; then
  PROLOGUE=$(cat <<EOS
extern void exit(int code);
extern void write(int fd, const char *str, long len);
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
void write(int fd, const char *str, long len) {
  __asm("mov \$1, %eax");  // __NR_write
  __asm("syscall");
}
EOS
  )
fi


try_direct() {
  title="$1"
  expected="$2"
  input="$PROLOGUE\n$3"

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
  input="$PROLOGUE\n$3"

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
  input="$PROLOGUE\n$2"

  echo -n "$title => "

  echo -e "$input" | $XCC
  result="$?"

  if [ "$result" = "0" ]; then
    echo "NG: Compile error expected, but succeeded"
    exit 1
  fi
}

try_output 'write' 'hello' "write(1, \"hello\\\\n\", 6);"
try_output 'char array' 123 "char s[16]; s[0] = '1'; s[1] = '2'; s[2] = '3'; s[3] = '\\\\n'; write(1, s, 4);"
try_output 'string initializer' 'aBc' "char s[] = \"abc\\\\n\"; s[1] = 'B'; write(1, s, 4);"
try_direct 'enum' 11 'enum Num { Zero, One, Two }; int main(){ return One + 10; }'
try_direct 'enum with trailing comma' 11 'enum Num { Zero, One, }; int main(){ return One + 10; }'
try_direct 'enum with assign' 11 'enum Num { Ten = 10, Eleven }; int main(){ return Eleven; }'
try_direct 'enum can use in case' 1 'enum Num { Zero, One, Two }; int main(){ switch (1) { case One: return 1; } return 0; }'
try_direct 'compare enum' 0 'enum Num { Zero, One, Two }; int main(){ enum Num num = Zero; return num == One; }'
try_direct 'enum initializer' 0 'enum Num { Zero } num = Zero; int main(){ return num; }'
try_direct 'enum initializer2' 67 'enum Num { Zero } num = 67; int main(){ return num; }'
try_direct 'global enum variable' 1 'enum Foo { BAR, BAZ }; enum Foo foo; int main(){ foo = BAZ; return foo; }'
try_direct 'typedef' 123 'typedef struct {int x, y;} Foo; int main(){ Foo foo; foo.x = 123; return foo.x; }'
try_output_direct 'empty function' '' 'void foo(){} int main(){ foo(); return 0; }'
try_direct 'Undeclared struct typedef' 8 'typedef struct FILE FILE; int main(){ return sizeof(FILE*); }'
try_direct 'late declare struct' 42 'struct Foo *p; struct Foo {int x;}; int main(){ struct Foo foo; p = &foo; p->x = 42; return p->x; }'
try_direct 'typedef func-ptr' 84 'typedef int (*Func)(int); int twice(Func f, int x) { return f(f(x)); } int mul2(int x) { return x * 2; } int main(){ return twice(&mul2, 21); }'
try_direct 'old-style func' 93 'int sub(); int main(){ return sub(31); } int sub(int x) { return x * 3; }'
try_direct 'old-func-ptr' 81 'int twice(int(*f)(), int x) { return f(f(x)); } int sqr(int x) { return x * x; } int main(){ return twice(sqr, 3); }'
try_direct 'global-func-var' 88 'int sub(void) { return 88; } int (*f)(void) = sub; int main(){ return f(); }'
try_direct 'for-var' 55 'int main(){ int acc = 0; for (int i = 1, len = 10; i <= len; ++i) acc += i; return acc; }'
try_direct 'for-no-initial-val' 3 "int main(){ const char *p = \"abc\"; int len = 0; for (char c; (c = *p) != '\\\\0'; ++p) ++len; return len; }"
try_direct 'args' 51 'int func(int x, ...) { return x; } int main(){ return func(51, 1, 2); }'
try_output_direct 'global str-array init' 'StrArray' 'char g_str[] = "StrArray"; int main(){ write(1, g_str, sizeof(g_str) - 1); return 0; }'
try_output_direct 'global str-ptr init' 'StrPtr' 'char *g_str = "StrPtr"; int main(){ write(1, g_str, 6); return 0; }'
try_output_direct 'global str-array init' 'StrPtr' 'char *g_str[] = {"StrPtr"}; int main(){ write(1, g_str[0], 6); return 0; }'
try_output_direct 'global str-in-struct init' 'StrPtr' 'struct {char *s;} g_str[] = {{"StrPtr"}}; int main(){ write(1, g_str[0].s, 6); return 0; }'
try_output_direct 'global char-array-in-struct init' 'abc' 'struct {char s[4];} g_str[] = {{"abc"}}; int main(){ write(1, g_str[0].s, 3); return 0; }'
try_direct 'global array' 42 'int array[] = {10,20,30}; int main(){ return sizeof(array) + array[2]; }'
try_direct 'local static array' 42 'int main(){ static int array[] = {10,20,30}; return sizeof(array) + array[2]; }'
try 'int static const' 34 'int static const a = 34; return a;'
try 'struct static const' 67 'struct {int x;} static const a[] = {{67}}; return a[0].x;'
try 'self reference' 4 'int x = sizeof(x); return x;'
try_direct '(void)x;' 0 'void func(int x) { (void)x; } int main(){ func(123); return 0; }'
try_output 'strings' 'hello world' "write(1, \"hello \" \"world\\\\n\", 12);"
try_direct 'init union' 77 'union { int x; struct { char a; short b; } y; } u = {.y={.b=77}}; int main(){ return u.y.b; }'
try_direct 'goto' 1 'int main(){ int x = 1; goto label; x = 2; label: return x; }'
try_output '*const' foobar 'const char* const str = "foobar"; write(1, str, 6);'
try_direct 'switch w/o case' 1 'int main(){ int x = 0; switch (0) {default: x = 1; break;} return x; }'
try_direct 'switch w/o case & default' 0 'int main(){ int x = 0; switch (0) {x = 1;} return x; }'
try 'post inc pointer' 1 'char *p = (char*)(-1L); p++; return p == 0;'
try_direct 'more params' 36 'int func(int a, int b, int c, int d, int e, int f, char g, long h) { return a + b + c + d + e + f + g + h; } int main(){ return func(1, 2, 3, 4, 5, 6, 7, 8); }'
try 'shadow var' 10 'int x = 1; { x = 10; int x = 100; } return x;'
try_direct 'struct assign' 33 'struct Foo { int x; }; int main(){ struct Foo foo, bar; foo.x = 33; bar = foo; return bar.x; }'
try_direct 'struct initial assign' 55 'struct Foo { int x; }; int main(){ struct Foo foo = {55}, bar = foo; return bar.x; }'
try_direct 'struct deref' 44 'struct Foo { long x; }; int main(){ struct Foo foo, bar, *baz = &bar; baz->x = 44; foo = *baz; return foo.x; }'
try_direct 'typedef can use in local' 61 'typedef int Foo; int main(){ int Foo = 61; return Foo; }'
try_direct 'proto in func' 78 'int main(){ int sub(int); return sub(77); } int sub(int x) { return x + 1; }'
try_direct 'extern in func' 45 'int main(){ extern int g; g = 45; return g; } int g;'
try_direct 'anonymous union init' 99 'struct {union {int x;};} a = {.x = 99}; int main(){ return a.x; }'
try 'func ref' 1 'return main == &main;'
try 'func deref' 1 'return (long)main == (long)*main;'
try 'implicit int' 92 'unsigned x = 92; return x;'

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
compile_error '*num' 'void main(){ *123; }'
compile_error '&num' 'void main(){ &123; }'
compile_error '&enum' 'enum Num { Zero }; void main(){ void *p = &Zero; }'
compile_error 'assign to non-lhs' 'void main(){ int x; x + 1 = 3; }'
compile_error 'assign to array' 'void main(){ int a[3], b[3]; a = b; }'
compile_error 'assign to func' 'void main(){ main = main; }'
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
compile_error 'variadic param count limit' 'void foo(const char *fmt, ...){} void main(){ foo("fmt", 1,2,3,4,5,6); }'
compile_error 'empty char' "int main() { return ''; }"
compile_error 'no single char' "int main() { return '12'; }"
compile_error '+ str' 'void main(){ +"foo"; }'
compile_error '- str' 'void main(){ -"foo"; }'
compile_error 'break outside loop' 'void main(){ break; }'
compile_error 'continue outside loop' 'void main(){ continue; }'
compile_error 'return void' 'void main(){ return 1; }'
compile_error 'return non-void' 'int main(){ return; }'
compile_error 'use before decl' 'void main(){ x = 0; int x; }'
compile_error 'scope invisible' 'int main(){ {int x;} return x; }'
compile_error 'array = ptr' 'void main(){ int a[1], *p; a = p; }'
compile_error 'case outside switch' 'int main(){ switch(0){} case 0: return 0; }'
compile_error 'dup cases' 'void main(){ switch(0){case 1: break; case 1: break;} }'
compile_error 'case is int only' 'void main(){ switch(0){case "foo": break;} }'
compile_error 'default outside switch' 'int main(){ switch(0){} default: return 0; }'
compile_error 'vardecl is not stmt' 'void main(){ if (1) int x = 0; }'
compile_error 'same struct name' 'struct Foo{int x;}; union Foo{int y;}; void main(){}'
compile_error '`union` for struct' 'struct Foo{int x;}; void main(){ union Foo foo; }'
compile_error 'non exist field initializer' 'struct Foo{int x;}; void main(){ struct Foo foo = {.y=1}; }'
compile_error 'initializer for empty struct' 'struct Foo{}; void main(){ struct Foo foo = {1}; }'
compile_error 'no name nor defined struct ptr' 'void main(){ struct *p; }'
compile_error 'refer undeclared struct member' 'void main(){ struct Foo *p; p->x; }'
compile_error 'extern only' 'extern int x; void main(){ x = 123; }'
compile_error 'extern with init' 'extern int x = 123; void main(){}'
compile_error 'char array init with ptr' 'char* foo = "foo"; void main(){ char bar[] = foo; }'
compile_error 'for-var scoped' 'int main(){ for (int i = 0; i < 5; ++i) ; return i; }'
compile_error 'global poitner init with undefined' 'char *p = &x; void main(){}'
compile_error 'global poitner init with other type' 'int x; char *p = &x; void main(){}'
compile_error 'use void' 'void func(){} void main(){ int a = (int)func(); }'
compile_error 'goto no-label' 'void main(){ goto label; }'
compile_error 'goto dup-label' 'void main(){ label: goto label; label:; }'
compile_error 'funparam static' 'void main(static int argc){}'
compile_error 'funparam extern' 'void main(extern int argc){}'
compile_error 'enum and global' 'enum Foo { BAR }; int BAR; void main(){}'
compile_error 'global and enum' 'int BAR; enum Foo { BAR }; void main(){}'
compile_error 'paren =' 'void main(){ int x; (x) = 98; }'
compile_error 'param and first scope' 'void main(int x){ int x; }'

# TODO
compile_error 'more params vaargs (yet)' 'int func(int a, ...) { return a; } int main(){ return func(1, 2, 3, 4, 5, 6, 7, 8); }'
