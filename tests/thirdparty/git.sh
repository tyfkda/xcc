#!/bin/bash
set -e

repo='https://github.com/git/git.git'
. tests/thirdparty/thirdpartycommon
git reset --hard 54e85e7af1ac9e9a92888060d6811ae767fea1bc

# CFLAGS="-D__STDC_VERSION__=199901L -Drestrict="

$make clean
$make V=1 CC="$xcc $CFLAGS"
$make V=1 CC="$xcc $CFLAGS" test
