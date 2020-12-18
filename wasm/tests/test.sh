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

try_direct '+*' 7 'int main(){return 1+2*3;}'
try_direct 'local var' 72 'int main(){int x=8, y=9; return x*y;}'
try_direct 'if-t' 11 'int main(){int x=0, y; if (x==0) y=11; else y=22; return y;}'
try_direct 'if-f' 22 'int main(){int x=1, y; if (x==0) y=11; else y=22; return y;}'
try_direct 'if-const' 11 'int main(){int x=1, y; if (x) y=11; else y=22; return y;}'
try_direct 'while' 55 'int main(){int acc=0, i=1; while (i<=10) {acc=acc+i; i=i+1;} return acc;}'
try_direct 'do-while' 55 'int main(){int acc=0, i=1; do {acc=acc+i; i=i+1;} while (i<=10); return acc;}'
try_direct 'for' 55 'int main(){int acc=0; for (int i=1; i<=10; i=i+1) acc=acc+i; return acc;}'
