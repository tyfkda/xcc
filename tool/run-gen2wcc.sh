#!/bin/sh

set -e

PRJROOT=$(cd "$(dirname "$0")/..";pwd)
WCCWASM=$PRJROOT/cc.wasm
CWD=.
EXTRA_OPTS=''

while [ $# -gt 0 ]  # (( $# > 0 ))
do
  case $1 in
    -C | --current-dir | --current-dir=*)
      if [ "$1" =~ ^--current-dir= ]; then
        CWD=${1#--current-dir=}
      elif [ -z "$2" ] || [ "$2" =~ ^-+ ]; then
        echo "'current-dir' requires an argument." 1>&2
        exit 1
      else
        CWD="$2"
        shift
      fi
      ;;
    --dir=* | --mapdir=*)
      EXTRA_OPTS="${EXTRA_OPTS} ""$1"  # Escape required?
      ;;
    --)
      shift
      break
      ;;
    -*)
      echo "$1: Unkonwn option." 1>&2
      exit 1
      ;;
    *)
      break
      ;;
  esac
  shift
done

if [ ! -e "$WCCWASM" ]; then
  echo "Run 'make wcc-gen2' command to create executable" 1>&2
  exit 1
fi

"$PRJROOT/tool/runwasi" \
  --dir="${CWD}" \
  --dir=/tmp \
  "--mapdir=/usr/include::$PRJROOT/include" \
  "--mapdir=/usr/lib::$PRJROOT/lib" \
  ${EXTRA_OPTS} \
  "$WCCWASM" -- "$@"
