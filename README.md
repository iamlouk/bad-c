# Bad C Compiler

__*A work-in-progress compiler (written from scratch) for a subset of C*__

Currently, only the preprocessor works, but is lacking some features: It does not support `#if` yet, but most other directives, including `#ifdef`.

- TODOs:
  - [ ] Implicit casts and proper type-checking, proper non-hacky pointer arithmetic desugaring
  - [ ] Parse `float *a, b;` as `float *a; float b;` and not `float *a, *b;`
  - [ ] Parsing and codegen of `if`/`while`/`for`/...
  - [ ] Support for structures in codegen (but not as SSA values, only as local allocations?!)
