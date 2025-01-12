void idaxpy(unsigned n, int *x, int *y, int a) {
  for (unsigned i = 0u; i < n; i += 1u) {
    y[i] = y[i] + x[i] * a;
  }
}
