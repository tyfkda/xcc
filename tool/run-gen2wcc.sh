#!/bin/sh

set -e

PRJROOT=$(cd "$(dirname "$0")/..";pwd)
TMPDIR=/tmp

# Platform dependent adjustments
case "$(uname -s)" in
    Linux*)     host=linux;;
    Darwin*)    host=macos;;
    MINGW*)     host=mingw;;
    *)          host="UNKNOWN:${unameOut}"
esac
if [ "$host" = "mingw" ]; then
    # Convert potential MSYS path to Windows path to avoid problems with NodeJS realpath
    PRJROOT=$(cygpath -w $PRJROOT)
    # Use alternative local temporary directory to prevent permission problems
    TMPDIR=$PRJROOT/.tmp
    # Make sure the temp dir exists
    mkdir -p $TMPDIR
fi

WCCWASM=$PRJROOT/cc.wasm

if [ ! -e "$WCCWASM" ]; then
    echo "Run 'make wcc-gen2' command to create executable" 1>&2
    exit 1
fi

"$PRJROOT/tool/runwasi" \
        --dir=. \
        "--mapdir=/tmp::$TMPDIR" \
        "--mapdir=/usr/include::$PRJROOT/include" \
        "--mapdir=/usr/lib::$PRJROOT/lib" \
    "$WCCWASM" -- "$@"
