int fib_v1(unsigned n) {
  int a = 1, b = 1;
  for (unsigned i = 0u; i < n; i += 1u) {
    int tmp = b;
    b = a + b;
    a = tmp;
  }
  return a;
}

void fib_v2(unsigned n, int *fibs) {
  fibs[0] = 1;
  fibs[1] = 1;
  for (unsigned i = 2u; i < n; i += 1u) {
    fibs[i] = fibs[i - 1u] + fibs[i - 2u];
  }
}

int fibs_v3(unsigned n) {
  if (n < 2u)
    return 1;
  else
    return fibs_v3(n - 1u) + fibs_v3(n - 2u);
}
