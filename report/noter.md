# Language design
## Functional paradigm
Functions as first-class citizens. All functions take one argument. No named functions, only lambdas bound to names.

## Minimal built-ins
<=, ==, basic arithmetic, list index, list cons

## Infixes are just functions
Succint partial application of infixes

## Standard library (Foundation) development
What functions are defined. Important ones: equalities, composition, concatenation, map, fold, range, etc

# Process
Started with a mix of syntactical and semantic design. Semantics changed much. Such syntactical revolt.

# Parsing
Shift-reduce parsing via Happy. Grammar in BNF format.
The responsibilities of the parser (desugaring).


# Static analysis

## Variable usage

## Type inference

### Hindley-Milner

Basically uses extended lambda calculus. Easier to understand and implement if using one-parameter functions, thus using higher-order functions to simulate multi-parameter functions. Invites us to allow partial application.

Functional style implementation uses substitutions instead of mutable global state.


# Codegen
Can use different backends. Uses the analyzed and annoted AST given by the type checker. The AST is assumed to be correct.

## JavaScript
Closely related semantics makes it an easy compile target. Laziness implemented with function wrapping.

### The JavaScript runtime

## LLVM
### Partial application
Partial application possible by using an aggregate type with supplied arguments and a function ptr. This way partially applied functions are represented as a list of arguments and a function ptr until the last argument is supplied.

If using closures, partial application is given "for free" as the supplied argument(s) are simply captured in the closure of the function being applied and the partially applied function that is returned references those arguments.
