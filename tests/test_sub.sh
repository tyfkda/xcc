#!/bin/bash

CLEAR_LINE="\033[1G\033[2K"
RED="\033[31m"
GREEN="\033[32m"
BOLD="\\033[1m"
RESET_COLOR="\033[0m"

FAILED_SUITE_COUNT=0

function begin_test_suite() {
  SUITE_NAME="$1"
  TEST_COUNT=0
  ERROR_COUNT=0
}

function end_test_suite() {
  if [[ $ERROR_COUNT -eq 0 ]]; then
    printf "${CLEAR_LINE}  ${SUITE_NAME}: ${GREEN}OK: ${TEST_COUNT}${RESET_COLOR}\n"
  else
    printf "${CLEAR_LINE}    ${RED}${BOLD}ERROR: ${ERROR_COUNT}/${TEST_COUNT}${RESET_COLOR}\n"
    FAILED_SUITE_COUNT=$((FAILED_SUITE_COUNT + 1))
  fi
}

function begin_test() {
  local title="$1"
  TEST_COUNT=$((TEST_COUNT + 1))
  printf "${CLEAR_LINE}  ${SUITE_NAME} ${TEST_COUNT}: $title"
}

function end_test() {
  local err="$1"
  if [[ "$err" != "" ]]; then
    printf "  ${RED}${BOLD}FAILED${RESET_COLOR}: ${err}\n"
    ERROR_COUNT=$((ERROR_COUNT + 1))
  fi
}

#

AOUT=${AOUT:-$(basename "$(mktemp -u)")}
XCC=${XCC:-../xcc}
CPP=${CPP:-../cpp}
RUN_AOUT=${RUN_AOUT:-./"$AOUT"}

CC=${CC:-cc}
REMOVE_TOP_LINEMARKER="tail -n +2"  # First line of preprocessor output is linemarker, so remove it.

echo "Compile=[$XCC], Run=[$RUN_AOUT]"

ARCH=$(arch)
if [[ -z "$RE_SKIP" ]]; then
  if [[ "$ARCH" = "arm64" ]] || [[ "$ARCH" = "aarch64" ]]; then
    RE_SKIP='\/\/-AARCH64'
  fi
fi

SILENT=' > /dev/null 2>&1'
if [[ "$VERBOSE" != "" ]]; then
  SILENT=''
fi

RE_WNOALL='\/\/-WNOALL'
RE_WNOERR='\/\/-WNOERR'
MAKE_ERR='err'

function try_direct() {
  local title="$1"
  local expected="$2"
  local input="$3"

  begin_test "$title"

  if [[ -n "$RE_SKIP" ]]; then
    echo -n "$input" | grep "$RE_SKIP" > /dev/null && {
      end_test
      return
    };
  fi
  local OPT=''
  echo -n "$input" | grep "$RE_WNOALL" > /dev/null || OPT+=' -Wall'
  echo -n "$input" | grep "$RE_WNOERR" > /dev/null || OPT+=' -Werror'

  echo -e "$input" | eval "$XCC" $OPT -o "$AOUT" -xc - "$SILENT" ||  {
    end_test 'Compile failed'
    return
  }

  $RUN_AOUT
  local actual="$?"

  local err=''; [[ "$actual" == "$expected" ]] || err="${expected} expected, but ${actual}"
  end_test "$err"
}

function try() {
  try_direct "$1" "$2" "int main(void){$3\n}"
}

function try_file() {
  local title="$1"
  local expected="$2"
  local inputs="$3"

  begin_test "$title"

  $XCC -o "$AOUT" -Wall -Werror "$inputs" > /dev/null 2>&1 || {
    end_test 'Compile failed'
    return
  }

  declare -a args=( "$@" )
  local actual
  actual=$(${RUN_EXE} ./"$AOUT" "${args[@]:3}") > /dev/null 2>&1 || {
    end_test 'Exec failed'
    return
  }

  local err=''; [[ "$actual" == "$expected" ]] || err="${expected} expected, but ${actual}"
  end_test "$err"
}

function try_cmp() {
  local title="$1"
  local expected="$2"
  local output="$3"
  local inputs="$4"

  begin_test "$title"

  $XCC -o "$AOUT" -Wall -Werror "$inputs" > /dev/null 2>&1 || {
    end_test 'Compile failed'
    return
  }

  declare -a args=( "$@" )
  ${RUN_EXE} ./"$AOUT" "${args[@]:4}" > /dev/null 2>&1 || {
    end_test 'Exec failed'
    return
  }

  local err=''; cmp "$expected" "$output" > /dev/null 2>&1 || err="Differ"
  end_test "$err"
}

function compile_error() {
  local title="$1"
  local input="$2"

  begin_test "$title"

  if [[ -n "$RE_SKIP" ]]; then
    echo -n "$input" | grep "$RE_SKIP" > /dev/null && {
      end_test
      return
    };
  fi
  local OPT=''
  echo -n "$input" | grep "$RE_WNOALL" > /dev/null || OPT+=' -Wall'
  echo -n "$input" | grep "$RE_WNOERR" > /dev/null || OPT+=' -Werror'

  echo -e "$input" | eval "$XCC" $OPT -o "$AOUT" -xc - "$SILENT"
  local exitcode="$?"

  local err=''; [[ "$exitcode" -ne 0 ]] || err="Compile error expected, but succeeded"
  end_test "$err"
}

function check_error_line() {
  local title="$1"
  local expected
  expected=$(echo -e "$2")
  local input="$3"

  begin_test "$title"

  local actual
  actual=$(echo -e "$input" | $XCC -o "$AOUT" -Wall -Werror -xc - 2>&1 | \
      grep -E -o '^<.*>:[0-9]+:' | sed 's/[^0-9]//g' | head -n 1)
      # grep -E -o '\([0-9]+\)' | sed 's/[()]//g' | head -n 1)

  local err=''; [[ "$actual" == "$expected" ]] || err="${expected} expected, but ${actual}"
  end_test "$err"
}

function link_success() {
  local title="$1"
  shift
  local input=$*

  begin_test "${title}"

  if [[ -n "$RE_SKIP" ]]; then
    echo -n "$title" | grep "$RE_SKIP" > /dev/null && {
      end_test
      return
    };
  fi

  local err=''
  eval "$XCC" -o "$AOUT" -Wall -Werror "${input[@]}" "$SILENT" || {
    end_test 'Compile failed'
    return
  }

  local RE_NOEXEC='NOEXEC'
  echo -n "$title" | grep "$RE_NOEXEC" > /dev/null && {
    end_test
    return
  };

  $RUN_AOUT
  local actual="$?"
  local err=''; [[ "$actual" == "0" ]] || err="exit with ${actual}"
  end_test "$err"
}

function link_error() {
  local title="$1"
  shift
  local input=$*

  begin_test "${title}"

  if [[ -n "$RE_SKIP" ]]; then
    echo -n "$title" | grep "$RE_SKIP" > /dev/null && {
      end_test
      return
    };
  fi

  eval "$XCC" -o "$AOUT" -Wall -Werror "${input[@]}" "$SILENT"
  local exitcode="$?"
  local err=''; [[ "$exitcode" -ne 0 ]] || err="Compile error expected, but succeeded"
  end_test "$err"
}

trapped=''
function trap_handler() {
  trapped='TRAPPED'
}

segfault() {
  local title="$1"
  local input="$2"

  begin_test "$title"

  if [[ -n "$RE_SKIP" ]]; then
    echo -n "$input" | grep "$RE_SKIP" > /dev/null && {
      end_test
      return
    };
  fi
  local OPT=''
  echo -n "$input" | grep "$RE_WNOALL" > /dev/null || OPT+=' -Wall'
  echo -n "$input" | grep "$RE_WNOERR" > /dev/null || OPT+=' -Werror'

  echo -e "$input" | eval "$XCC" $OPT -o "$AOUT" -xc - "$SILENT" ||  {
    end_test 'Compile failed'
    return
  }

  trap trap_handler ERR
  trapped=''
  $RUN_AOUT > /dev/null 2>&1
  trap - ERR

  local err=''; [[ "$trapped" == "TRAPPED" ]] || err="trap expected, but succeeded"
  end_test "$err"
}

function try_pp() {
  local title="$1"
  local expected
  expected=$(echo -e "$2")
  local input="$3"
  local ppflags="$4"

  begin_test "$title"

  local actual
  actual=$(echo -e "$input" | $CPP $ppflags 2>/dev/null | $REMOVE_TOP_LINEMARKER | tr -d '\n' | sed 's/ $//')
  local exitcode=$?
  if [ $exitcode -ne 0 ]; then
    end_test "CPP failed: exitcode=${exitcode}"
    return
  fi

  local err=''; [[ "$actual" == "$expected" ]] || err="${expected} expected, but ${actual}"
  end_test "$err"
}

function pp_error() {
  local title="$1"
  local input="$2"

  begin_test "$title"

  echo -e "$input" | $CPP $ppflags > /dev/null 2>&1 | tr -d '\n'
  local result="$?"

  local err=''; [[ "$result" -ne 0 ]] || err="Compile error expected, but succeeded"
  end_test "$err"
}
