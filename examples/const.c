// int sum(unsigned char n) {
//     long acc = 0;
//     for (int i = 1; i <= n; ++i) {
//         switch (i) {
//         case 1: acc += 11; break;
//         case 2: acc += 22; break;
//         case 3: acc += 33; break;
//         case 4: acc += 44; break;
//         case 5: acc += 55; break;
//         default: acc += i; break;
//         }
//     }
//     return acc;
// }

// int x = 100;

int main(int argc, char *argv[]) {
    // static int n = 10;
    // return sum(n) + (x > 0);
    // int x = 55;
    // int *p = &x;
    // return *p;
    int a[5];
    for (int i = 0; i < 5; ++i)
        a[i] = i * i;
    int acc = 0;
    for (int i = 0; i < 5; ++i)
        acc += a[i];
    return acc;
}
