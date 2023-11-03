#!/bin/bash
set -e

repo='https://github.com/TinyCC/tinycc.git'
. tests/thirdparty/thirdpartycommon
git reset --hard df67d8617b7d1d03a480a28f9f901848ffbfb7ec

sed -i "s/tcc_error(\"can't cross compile long double constants\");/*(long double *)ptr = (double)vtop->c.ld;/" tccgen.c

./configure --cc=$xcc
$make clean
$make
$make CC=cc -j1 test
