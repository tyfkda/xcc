#!/bin/bash
set -e

repo='https://github.com/sqlite/sqlite.git'
. tests/thirdparty/thirdpartycommon
git reset --hard 86f477edaa17767b39c7bae5b67cac8580f7a8c1

CFLAGS="-D_GNU_SOURCE -D__STDC_VERSION__=199901L -Drestrict="

CC="$xcc" CFLAGS="$CFLAGS" ./configure
sed -i 's/^wl=.*/wl=-Wl,/; s/^pic_flag=.*/pic_flag=-fPIC/' libtool
$make clean
$make -j1
$make test
