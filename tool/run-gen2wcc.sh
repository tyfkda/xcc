#!/bin/sh

set -e

PRJROOT=$(cd "$(dirname "$0")/..";pwd)
WCCWASM=$PRJROOT/cc.wasm

if [ ! -e "$WCCWASM" ]; then
    echo "Run 'make wcc-gen2' command to create executable" 1>&2
    exit 1
fi

"$PRJROOT/tool/runwasi" \
        --dir=. \
        --dir=/tmp \
        "--mapdir=/usr/include::$PRJROOT/include" \
        "--mapdir=/usr/lib::$PRJROOT/lib" \
    "$WCCWASM" -- "$@"
