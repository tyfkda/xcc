#!/bin/bash

CLEAR_LINE="\033[1G\033[2K"
RED="\033[31m"
GREEN="\033[32m"
BOLD="\\033[1m"
RESET_COLOR="\033[0m"

FAILED_SUITE_COUNT=0

begin_test_suite() {
  SUITE_NAME="$1"
  TEST_COUNT=0
  ERROR_COUNT=0
}

end_test_suite() {
  if [[ $ERROR_COUNT -eq 0 ]]; then
    printf "${CLEAR_LINE}  ${SUITE_NAME}: ${GREEN}OK: ${TEST_COUNT}${RESET_COLOR}\n"
  else
    printf "${CLEAR_LINE}    ${RED}${BOLD}ERROR: ${ERROR_COUNT}/${TEST_COUNT}${RESET_COLOR}\n"
    FAILED_SUITE_COUNT=$(($FAILED_SUITE_COUNT + 1))
  fi
}

begin_test() {
  local title="$1"
  TEST_COUNT=$(($TEST_COUNT + 1))
  printf "${CLEAR_LINE}  ${SUITE_NAME} ${TEST_COUNT}: $title"
}

end_test() {
  local err="$1"
  if [[ "$err" != "" ]]; then
    printf "  ${RED}${BOLD}FAILED${RESET_COLOR}: ${err}\n"
    ERROR_COUNT=$(($ERROR_COUNT + 1))
  fi
}
