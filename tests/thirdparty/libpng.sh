#!/bin/bash
set -e

repo='https://github.com/rui314/libpng.git'
. tests/thirdparty/thirdpartycommon
git reset --hard dbe3e0c43e549a1602286144d94b0666549b18e6

CC="$xcc" ./configure
sed -i 's/^wl=.*/wl=-Wl,/; s/^pic_flag=.*/pic_flag=-fPIC/' libtool
$make clean
$make -j1
$make test
