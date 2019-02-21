// #include "./utils.c"

int fib(int n) {
  if (n < 2)
    return n;
  else
    return fib(n - 1) + fib(n - 2);
}

int main() {
  putdeci(fib(30));
  puts("\n");
  return 0;
}
