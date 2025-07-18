#!/bin/bash

set -o pipefail

source ./test_sub.sh

function test_backward_goto() {
  begin_test_suite "Backward Goto"
  
  compile_error 'simple backward goto' '
int main() {
start_label:
    int x = 1;
    if (x) {
        goto start_label;  // BACKWARD GOTO - should fail
    }
    return 0;
}'

  compile_error 'backward goto in nested loops' '
int main() {
outer_loop:
    for (int i = 0; i < 2; i++) {
        for (int j = 0; j < 2; j++) {
            if (i == 1 && j == 1) {
                goto outer_loop;  // BACKWARD GOTO - should fail
            }
        }
    }
    return 0;
}'

  compile_error 'backward goto with conditional' '
int main() {
retry:
    int x = 5;
    if (x > 0) {
        x--;
        if (x > 0) {
            goto retry;  // BACKWARD GOTO - should fail
        }
    }
    return x;
}'

  end_test_suite
}

function test_cross_branch_goto() {
  begin_test_suite "Cross-Branch Goto"

  compile_error 'goto from if to else branch' '
int main() {
    int x = 1;
    if (x == 1) {
        goto else_label;  // CROSS-BRANCH GOTO - should fail
    } else {
else_label:
        return 0;
    }
    return 1;
}'

  compile_error 'goto between switch cases' '
int main() {
    int x = 1;
    switch (x) {
        case 1:
            goto case2_label;  // CROSS-BRANCH GOTO - should fail
            break;
        case 2:
case2_label:
            break;
        default:
            break;
    }
    return 0;
}'

  compile_error 'goto from one loop to another' '
int main() {
    for (int i = 0; i < 3; i++) {
        if (i == 1) {
            goto second_loop_label;  // CROSS-BRANCH GOTO - should fail
        }
    }
    for (int j = 0; j < 3; j++) {
second_loop_label:
        continue;
    }
    return 0;
}'

  compile_error 'goto into nested block from outside' '
int main() {
    int x = 1;
    if (x) {
        goto nested_label;  // CROSS-BRANCH GOTO - should fail
    }
    {
        int y = 2;
        {
nested_label:
            return 0;
        }
    }
    return 1;
}'

  end_test_suite
}

function test_invalid_label_placement() {
  begin_test_suite "Invalid Label Placement"

  compile_error 'label not immediately after block' '
int main() {
    {
        int x = 1;
    }
    int y = 2;  // Statement between block and label
invalid_label:
    goto invalid_label;  // Should fail due to invalid label placement
    return 0;
}'

  compile_error 'label inside block with goto from outside' '
int main() {
    goto inside_label;  // INVALID - jumping into block
    {
inside_label:
        return 0;
    }
    return 1;
}'

  compile_error 'multiple statements before label' '
int main() {
    {
        int x = 1;
    }
    int a = 1;        // Statement 1
    int b = 2;        // Statement 2
problematic_label:    // Invalid - not immediately after block
    goto problematic_label;
    return 0;
}'

  end_test_suite
}

function test_all() {
  test_backward_goto
  test_cross_branch_goto
  test_invalid_label_placement
}

test_all

if [[ $FAILED_SUITE_COUNT -ne 0 ]]; then
  exit "$FAILED_SUITE_COUNT"
fi 