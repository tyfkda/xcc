#!/bin/bash
set -e

repo='https://github.com/git/git.git'
. tests/thirdparty/thirdpartycommon
git reset --hard 564d0252ca632e0264ed670534a51d18a689ef5d  # 2023/11/20 v2.43.0

$make clean
$make V=1 CC="$xcc"
$make V=1 CC="$xcc" test
