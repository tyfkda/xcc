#!/bin/bash
set -e

repo='https://github.com/git/git.git'
. tests/thirdparty/thirdpartycommon
# git reset --hard 54e85e7af1ac9e9a92888060d6811ae767fea1bc  # 2020/9/9
git reset --hard 5fbd2fc5997dfa4d4593a862fe729b1e7a89bcf8  # 2021/11/4

# CFLAGS="-D__STDC_VERSION__=199901L -Drestrict="

# zlib removes `const` using macro if the compiler is not `STDC`
CFLAGS="-DSTDC"

$make clean
$make V=1 CC="$xcc $CFLAGS"
$make V=1 CC="$xcc $CFLAGS" test
