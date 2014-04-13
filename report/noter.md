# Language design
## Functional paradigm
HoF as central part of language. All functions take one argument. 


# Parsing
Shift-reduce parsing via Happy. Grammar in BNF format.


# Type inference
## Hindley-Milner
Basically uses extended lambda calculus. Easier to understand and implement if using one-parameter functions, thus using higher-order functions to simulate multi-parameter functions.

Functional style implementation uses substitutions instead of mutable global state.


# Codegen
Can use different backends. Uses the analyzed and annoted AST given by the type checker. The AST is assumed to be correct.

## LLVM
### Partial application
Partial application possible by using an aggregate type with supplied arguments and a function ptr. This way partially applied functions are represented as a list of arguments and a function ptr until the last argument is supplied.

If using closures, partial application is given "for free" as the supplied argument(s) are simply captured in the closure of the function being applied and the partially applied function that is returned references those arguments.


# Standard library development
Minimum built-in functions.

## List functions
map, fold, head, tail, etc
