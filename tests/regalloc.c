#if 0

int fib_v1(unsigned n) {
  int a = 1, b = 1;
  for (unsigned i = 0u; i < n; i += 1u) {
    int tmp = b;
    b = a + b;
    a = tmp;
  }
  return a;
}

#endif

#if 0

int sign(int n) {
  int sign = 0;
  if (n < 0)
    sign = -1;
  else if (n > 0)
    sign = 1;
  return sign;
}

#endif

#if 1

int sum2d(int n, int m) {
  int sum = 0;
  for (int i = 0; i < n; i += 1)
    for (int j = 0; j < m; j += 1)
      sum += i * j;
  return sum;
}

#endif
