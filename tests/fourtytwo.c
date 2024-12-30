unsigned fourtytwo_v1(int unused) {
  return 42u;
}

unsigned fourtytwo_v2() {
  int unused;
  if (true)
    return 42;
  else
    return 100u;
  return 200u;
}
