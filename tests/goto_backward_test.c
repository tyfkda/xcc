// Goto cases that should fail compilation with WASM
// This is to demonstrate unsupported goto functionality is detected

#include <stdio.h>

// Backward goto should fail compilation
int test_simple_backward() {
start_label:
    for (int i = 0; i < 3; i++) {
        if (i == 2) {
            goto start_label;
        }
    }
    return 0;
}

// Backward goto in nested loops should fail
int test_backward_nested_loops() {
outer_loop:
    for (int i = 0; i < 2; i++) {
        for (int j = 0; j < 2; j++) {
            if (i == 1 && j == 1) {
                goto outer_loop;
            }
        }
    }
    return 0;
}

// Backward goto with conditional should fail
int test_backward_conditional() {
    int x = 5;
retry:
    if (x > 0) {
        x--;
        if (x > 0) {
            goto retry;
        }
    }
    return x;
}

int main() {
    test_simple_backward();
    test_backward_nested_loops();
    test_backward_conditional();
    return 0;
}
