#if 1
int sign(int n) {
  int sign = 0;
  if (n < 0)
    sign = -1;
  else if (n > 0)
    sign = 1;
  return sign;
}

int bar() {
  int a;
  if (1 == 2)
    a = 42;
  return a;
}

int foo() {
  int a;
  int b;
  int c;
  a = 1;
  b = a + 2;
  c = b + 3;
  return a + b + c;
}

int sum() {
  int i = 1;
  int sum = 0;
  while (i < 100) {
    sum += i;
    i += 1;
  }
  return sum;
}
#endif

int sum2d(int n, int m) {
  int sum = 0;
  for (int i = 0; i < n; i += 1)
    for (int j = 0; j < m; j += 1)
      sum += i * j;
  return sum;
}
