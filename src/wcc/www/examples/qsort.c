// Function pointer example

#include <alloca.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

void dump(const char *title, int *array, int n) {
    printf("%s", title);
    for (int i = 0; i < n; ++i)
        printf("%d ", array[i]);
    printf("\n");
}

int compare(const void *va, const void *vb) {
    const int *pa = va;
    const int *pb = vb;
    return *pa - *pb;
}

int main(int argc, char *argv[]) {
    int N = 10;
    if (argc > 1)
        N = atoi(argv[1]);

    srand(time(NULL));

    int *array = alloca(sizeof(*array) * N);
    for (int i = 0; i < N; ++i) {
        array[i] = rand() % N;
    }

    dump("Before:", array, N);
    qsort(array, N, sizeof(*array), compare);
    dump("After :", array, N);

    return 0;
}
