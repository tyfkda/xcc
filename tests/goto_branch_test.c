// Goto cases that should fail compilation with WASM
// This is to demonstrate unsupported goto functionality is detected

#include <stdio.h>

// Goto from if to else branch should fail
int test_goto_if_to_else() {
    int x = 1;
    if (x == 1) {
        printf("In if branch\n");
        goto else_label;
    } else {
else_label:
        printf("In else branch\n");
    }
    return 0;
}

// Goto between switch cases should fail
int test_goto_switch_cases() {
    int x = 1;
    switch (x) {
        case 1:
            printf("Case 1\n");
            goto case2_label;
            break;
        case 2:
case2_label:
            printf("Case 2\n");
            break;
        default:
            printf("Default\n");
            break;
    }
    return 0;
}

// Goto from one loop to another should fail
int test_goto_between_loops() {
    // First loop
    for (int i = 0; i < 3; i++) {
        if (i == 1) {
            goto second_loop_label;
        }
    }
    // Second loop
    for (int j = 0; j < 3; j++) {
second_loop_label:
        printf("In second loop: %d\n", j);
    }
    return 0;
}

// Goto into nested block from outside should fail
int test_goto_into_nested_block() {
    int x = 1;
    if (x) {
        goto nested_label;
    }
    {
        int y = 2;
        {
nested_label:
            printf("In nested block\n");
        }
    }
    return 0;
}

// Goto between different conditional branches should fail
int test_goto_between_conditionals() {
    int a = 1, b = 2;
    if (a == 1) {
        printf("First condition\n");
        goto second_cond_label;
    }
    if (b == 2) {
second_cond_label:
        printf("Second condition\n");
    }
    return 0;
}

// Goto from try-like block to catch-like block should fail
int test_goto_try_catch_pattern() {
    int error = 0;
    // Simulate try block
    {
        error = 1;
        if (error) {
            goto catch_label;
        }
    }
    // Simulate catch block  
    if (error) {
catch_label:
        printf("Handling error\n");
    }
    return 0;
}

int main() {
    test_goto_if_to_else();
    test_goto_switch_cases();
    test_goto_between_loops();
    test_goto_into_nested_block();
    test_goto_between_conditionals();
    test_goto_try_catch_pattern();
    return 0;
}
