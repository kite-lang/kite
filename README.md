The Kite Programming Language
=============================

[![Build Status](https://travis-ci.org/altschuler/kite.png?branch=master)](https://travis-ci.org/altschuler/kite)

<p align="center">
  <img width="100" src="https://rawgit.com/altschuler/kite/master/logo/logo.svg" alt="Kite logo"/>
</p>

Kite is language with a strong emphasis on functional development. It is statically and strongly typed.

Wanna know more about the design and other features? See the [wiki](https://github.com/altschuler/kite/wiki).

## Installation
 * Get the source:

        $ git clone https://github.com/altschuler/kite.git

 * Make sure you have `cabal` installed, and then install dependencies:

        $ cabal install

 * Build with `make`. This will generate an executable in `dist/build/kite/`.

        $ make

 * To install an executable in `~/.cabal/bin`, which will be accessible for your local user:

        $ cabal install

   Make sure that you have the `~/.cabal/bin` directory in your `PATH`.

 * Optionally, install globally by putting an executable in `/usr/local/bin` (requires root privileges).

        # make install

## Tests
There are two test-suites:

 * One in Haskell, which tests each component of the compiler:

        $ make test

 * And one written in Kite, testing itself:

        $ kite tests/kunit/Runner.kite
        $ ./main

## Examples
For a quick example, try running one of the examples in `example/`.

    $ kite examples/Test.kite

The compiler only targets JavaScript, as of now, so make sure you have [`node`](https://github.com/joyent/node) installed.

The name of the output file defaults to `main`, which executes with `node`.

    $ ./main

Read more about the syntax in the [wiki](https://github.com/altschuler/kite/wiki/Syntax) or look at some of the [examples](https://github.com/altschuler/kite/tree/master/examples).

## Contribution
Feel free to submit a pull request or issue! We are open to ideas and all kinds of feedback.

## Authors
 * [Simon Altschuler](https://github.com/altschuler)
 * [Markus Færevaag](https://github.com/mfaerevaag)
 * [Patrick Gadd](https://github.com/patrickgadd)
 * [Christian Mathias Rohde Kiær](https://github.com/kiaer)

## License
This project is licensed under the MIT-license. [Read more](https://github.com/altschuler/kite/blob/master/LICENSE).
