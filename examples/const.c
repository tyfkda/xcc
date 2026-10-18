int sum(unsigned char n) {
    long acc = 0;
    for (int i = 1; i <= n; ++i)
        acc += i;
    return acc;
}

int main(int argc, char *argv[]) {
    return sum(10);
}
