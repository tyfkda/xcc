#!/bin/bash
set -e

repo='https://github.com/rui314/libpng.git'
. tests/thirdparty/thirdpartycommon
git reset --hard dbe3e0c43e549a1602286144d94b0666549b18e6

# zlib defines `voidp` as `unsigned char*` using macro if the compiler is not `STDC`
CFLAGS="-DSTDC"

CC="$xcc $CFLAGS" ./configure
sed -i 's/^wl=.*/wl=-Wl,/; s/^pic_flag=.*/pic_flag=-fPIC/' libtool
$make clean
$make
$make test
