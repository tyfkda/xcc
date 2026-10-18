int sum(unsigned char n) {
    long acc = 0;
    for (int i = 1; i <= n; ++i)
        acc += i;
    return acc;
}

int x = 100;

int main(int argc, char *argv[]) {
    static int n = 10;
    return sum(n) + x;
}
