#!/bin/bash

WCC=${WCC:-../wcc}

try_direct() {
  local title="$1"
  local expected="$2"
  local input="$3"

  echo -n "$title => "

  echo -e "$input" | $WCC -emain || exit 1

  node ../runtime/runwasm.js a.wasm main
  local actual="$?"

  if [ "$actual" = "$expected" ]; then
    echo "OK"
  else
    echo "NG: $expected expected, but got $actual"
    exit 1
  fi
}

try_output_direct() {
  local title="$1"
  local expected="$2"
  local input="$3"

  echo -n "$title => "

  echo -e "$input" | $WCC -emain || exit 1

  local actual
  actual=`node ../runtime/runwasm.js a.wasm main` || exit 1

  if [ "$actual" = "$expected" ]; then
    echo "OK"
  else
    echo "NG: $expected expected, but got $actual"
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
try_direct 'while' 55 'int main(){int acc=0, i=1; while (i<=10) {acc+=i; ++i;} return acc;}'
try_direct 'do-while' 55 'int main(){int acc=0, i=1; do {acc+=i; ++i;} while (i<=10); return acc;}'
try_direct 'for' 55 'int main(){int acc=0; for (int i=1; i<=10; ++i) acc+=i; return acc;}'
try_direct 'return in block' 55 'int main(){int acc=0, i=1; for (;;) {acc+=i; if (i==10) return acc; ++i;}}'
try_output_direct 'call imported func' '12321' 'void puti(int); int sq(int x){return x*x;} int main(){puti(sq(111)); return 0;}'
try_direct 'global var' 24 'static int g; int main(){g+=10; g-=2; g*=3; return g;}'
try_direct 'global data' 66 'int a[]={11, 22, 33}; int main(){int s=0; for (int i=0; i<3; ++i) s+=a[i]; return s;}'
