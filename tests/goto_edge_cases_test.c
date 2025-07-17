// Goto cases that should fail compilation with WASM
// This is to demonstrate unsupported goto functionality is detected

#include <stdio.h>

// Label not immediately after block should fail
int test_invalid_label_placement() {
    {
        printf("In block\n");
    }
    int x = 0;
    printf("Statement between block and label\n");
invalid_label:
    printf("After invalid label\n");
    if (x++ < 1) goto invalid_label;
    return 0;
}

// Label inside block should fail
int test_label_inside_block() {
    goto inside_label;
    {
inside_label:
        printf("Inside block\n");
    }
    return 0;
}

// Multiple statements before label
int test_multiple_statements_before_label() {
    {
        printf("In block\n");
    }
    int x = 1;
    x = x + 1;
problematic_label:
    printf("After problematic label\n");
    if (x++ < 3) goto problematic_label;
    return 0;
}

// Goto with complex control flow: valid case
int test_complex_valid_goto() {
    int state = 1;
    switch (state) {
        case 1:
            for (int i = 0; i < 1; i++) {
                if (i == 0) {
                    goto end_processing;
                }
            }
            break;
        case 2:
            printf("Case 2\n");
            break;
    }
end_processing:
    printf("End processing\n");
    return 0;
}

int main() {
    test_complex_valid_goto(); 
    test_invalid_label_placement();
    test_label_inside_block();
    test_multiple_statements_before_label();
    return 0;
} 