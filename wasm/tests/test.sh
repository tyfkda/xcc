#!/bin/bash

WCC=${WCC:-../wcc}

try_direct() {
  local title="$1"
  local expected="$2"
  local input="$3"

  echo -n "$title => "

  echo -e "$input" | $WCC || exit 1

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
