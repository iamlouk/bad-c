#ifndef TESTS_FOO_H
#define TESTS_FOO_H
/* A header used for unit tests in lex.rs */

#ifdef __SHITTYC
extern unsigned fib(unsigned n);
#else
#error "Expected to be compiled using the shitty C compiler!"
#endif
#endif
