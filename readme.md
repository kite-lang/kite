# The Kite Programming Language

[![Build Status](https://travis-ci.org/altschuler/kite.png?branch=master)](https://travis-ci.org/altschuler/kite)

Kite is language with a strong emphasis on functional development. It is statically and strongly typed.

We use [alex](https://github.com/simonmar/alex) for lexical analyzing, [happy](https://github.com/simonmar/happy) for parsing, and lastly, [LLVM](http://llvm.org/) for code generation.


## Semantics

### Types
There are five basic types, namely `Boolean`, `Int`, `Float`, `String`, `List` and `Function`.


### Recursion
Recursion is supported.


## Syntax
Kite uses mandatory semicolons to mark the end of an expression.


### Variable definitions
The basic variable definition is defined as follows

    <Type> identifier = <Expression>;

All types are capitalized to make it easy to differentiate between types and identifiers.

    Int one = 1;
    Float two = 2.0;
    Bool truth = True;

Strings are enclosed in *double* quotes. Single quotes are valid identifiers.

    String foo = "Hello, world!";
    String bar = "It's \"funny\"";

Function signatures are defined by the `->` operator, where `(a) -> b` defines a function that maps type `a` to `b`. The argument list can be empty.

    ([<ArgType>, ...]) -> <ReturnType>

Function literals (or expressions) are the corresponding implementations matching a function signature

    ([<ArgType> <ArgName>, ...]) -> <ReturnType>

Thus, the only difference between the signature and the literal is the addition of named arguments. The following are all valid function definitions:

    () -> Int one = () -> Int { return 1; };

    (String, String) -> String fullName = (String first, String, last) -> String {
        return first + " " + last;
    };

Kite supports higher-order functions:

    (() -> Int) -> ((Int) -> Int) foo = (() -> Int bar) -> ((Int) -> Int) {
        return (Int baz) -> Int {
            bar () + baz;
        };
    };

The above expressions creates a function, bound to variable `foo`, that takes a function of type `() -> Int` as it's only argument, and returns another function of type `(Int) -> Int`.


### Comments
Comments are marked by `#` and go to end of line. Comment blocks (possibly multiline) are enclosed by `#-` and `-#`.

    # This is a single line comment

    #- This is a block comment -#

    #-
        This is a
        multi line
        comment.
    -#


### Importing code files
Use the `import` keyword to include one file in another.


### Foreign Function Interface
Kite has a FFI which lets you call native Haskell functions.
