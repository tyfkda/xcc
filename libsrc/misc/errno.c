int *__errno_location(void) {
  static int value;
  return &value;
}
