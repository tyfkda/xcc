// Goto cases that should work with WASM

#include <stdio.h>
#include <stdint.h>

#define XTEST_NO_EXPECT_NEAR
#include "./xtest.h"

// break from multiple
int exampleBreakFromTwoLoops() {
    int x = 0;
    for (int i = 0; i < 2; i++) {
        for (int j = 0; j < 3; j++) {
            // printf("i = %d, j = %d; i + j = %d\n", i, j, i + j);
            if (i + j == 3) {
                goto end;
            }
            x++;
        }
    }
end:
    return x;
}

// break from do { ... } while(0) 
int64_t squareThrowCatch(int64_t x) {
    do {
        if (x > 3000000000) {
            goto catch;
        }
        return x * x;
    } while(0);
catch:
    return -1;
}

TEST(all) {
    EXPECT_EQ(5, exampleBreakFromTwoLoops());
    EXPECT_EQ(4000000000000000000, squareThrowCatch(2000000000));
    EXPECT_EQ(9000000000000000000, squareThrowCatch(3000000000));
    EXPECT_EQ(-1, squareThrowCatch(4000000000));
}

XTEST_MAIN();
