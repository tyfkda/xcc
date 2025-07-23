#!/bin/bash

set -o pipefail

source ./test_sub.sh

function test_successful_goto_cases() {
  begin_test_suite "Successful Goto Cases"
  
  try_direct 'break from nested loops with goto' 5 '
#include <stdio.h>

int exampleBreakFromTwoLoops() {
    int x = 0;
    for (int i = 0; i < 2; i++) {
        for (int j = 0; j < 3; j++) {
            if (i + j == 3) {
                goto end;
            }
            x++;
        }
    }
end:
    return x;
}

int main() {
    return exampleBreakFromTwoLoops();
}'

  try_direct 'goto with do-while(0) pattern - normal case' 0 '
#include <stdio.h>
#include <stdint.h>

int64_t squareThrowCatch(int64_t x) {
    do {
        if (x > 3000000000) {
            goto catch;
        }
        // Return 0 if square calculation would succeed
        return 0;
    } while(0);
catch:
    return 1;  // Return 1 if catch block reached
}

int main() {
    return squareThrowCatch(2000000000);  // Should return 0 (success)
}'

  try_direct 'goto with do-while(0) pattern - catch case' 1 '
#include <stdio.h>
#include <stdint.h>

int64_t squareThrowCatch(int64_t x) {
    do {
        if (x > 3000000000) {
            goto catch;
        }
        // Return 0 if no goto executed
        return 0;
    } while(0);
catch:
    return 1;  // Return 1 if catch block reached
}

int main() {
    return squareThrowCatch(4000000000);  // Should return 1 (catch reached)
}'

  try_direct 'forward goto with label after block' 42 '
int main() {
    {
        int x = 1;
        if (x) {
            goto after_block;
        }
    }
after_block:
    return 42;  // Should reach here
}'

  try_direct 'goto to exit from nested blocks' 0 '
int main() {
    int flag = 1;
    do {
        {
            {
                if (flag)
                    goto exit_all;
            }
            if (!flag) return 1;
        }
        if (!flag) return 2;
    } while(0);
exit_all:
    return 0;  // Should reach here
}'

  try_direct 'goto for early function exit' 99 '
int main() {
    int result = 0;
    for (int i = 0; i < 10; i++) {
        if (i == 5) {
            result = 99;
            goto cleanup;
        }
        result += i;
    }
cleanup:
    return result;  // Should return 99
}'

  end_test_suite
}

function test_all() {
  test_successful_goto_cases
}

test_all

if [[ $FAILED_SUITE_COUNT -ne 0 ]]; then
  exit "$FAILED_SUITE_COUNT"
fi 