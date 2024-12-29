extern int fib_v1(unsigned n)
   {
    int a = 0x1;
    int b = 0x1;
    for (unsigned i = 0x0u;
         (i) < (n); (i) += (0x1u))
     {
      int tmp = b;
      (b) = ((a) + (b));
      (a) = (tmp);
     }
    return a;
   }

extern void fib_v2(unsigned n, int *fibs)
   {
    (*((int *)((const char *)fibs + (0x0 * 4)))) = (0x1);
    (*((int *)((const char *)fibs + (0x1 * 4)))) = (0x1);
    for (unsigned i = 0x2u;
         (i) < (n); (i) += (0x1u))
     {
      (*((int *)((const char *)fibs + (i * 4)))) = ((*((int *)((const char *)fibs + ((i) - (0x1u) * 4)))) + (*((int *)((const char *)fibs + ((i) - (0x2u) * 4)))));
     }
   }

extern int fibs_v3(unsigned n)
   {
    if ((n) < (0x2u))
     return 0x1;
    else
     return (fibs_v3((n) - (0x1u))) + (fibs_v3((n) - (0x2u)));
   }

