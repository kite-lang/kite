# The Kite Programming Language

Kite is language with a strong emphasis on functional development. It is statically and strongly typed.

## Semantics
### Types
There are five basic types, namely `Int`, `Float`, `String`, `List` and `Function`.

### Recursion

## Syntax
Kite uses mandatory semicolons to mark the end of an expression.

### Variable definitions
The basic variable definition is defined as follows

    <Type> identifier = <Expression>;

All types are capitalized to make it easy to differentiate between types and identifiers.

    Int one = 1;
    Float two = 2.0;

Strings are enclosed in *double* quotes. Single quotes are valid identifiers.

    String foo = "Hello, world!";
    String bar = "It's \"funny\"";

Function signatures are defined by the following syntax

    <ReturnType> ([<Arg1Type>, ...])

And function literals

    <ReturnType> ([<Arg1Type> arg, ...])

Thus, the only difference between the signature and the literal is the addition of named arguments. The following are all valid function definitions

    Int () one = Int () { return 1; };

    String (String, String) fullName = String (String first, String, last) {
        return first + " " + last;
    };

    Int fib (Int) = Int (Int n) {
        if n = 0; then return 0;
        else if n = 1; then return 1;
        else return fib (n - 1) + fib (n - 2);
    }


### Comments
Comments are marked by `#` and go to end of line. Comment blocks (possibly multiline) are enclosed by `#-` and `-#`.

    # this is one thousand

    #- This is a block -#

    #-
        This is a comment
        on multiple lines
    -#


### Importing code files

### Modules
