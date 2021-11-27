#!/bin/bash
set -e

repo='https://github.com/python/cpython.git'
. tests/thirdparty/thirdpartycommon
git reset --hard c75330605d4795850ec74fdc4d69aa5d92f76c00

# Python's './configure' command misidentifies chibicc as icc
# (Intel C Compiler) because icc is a substring of chibicc.
# Modify the configure file as a workaround.
#sed -i -e 1996,2011d configure.ac
autoreconf

CC=$xcc ./configure
$make clean
$make
$make test
