# The Kite Programming Language

[![Build Status](https://travis-ci.org/altschuler/kite.png?branch=master)](https://travis-ci.org/altschuler/kite)

Kite is language with a strong emphasis on functional development. It is statically and strongly typed.

Wanna know more about the design and other features? See the [wiki](https://github.com/altschuler/kite/wiki)!

## Installation
 * Get the source.

        # git clone https://github.com/altschuler/kite.git

 * Make sure you have `cabal` installed, and then install dependencies.

        # cabal install

 * Build files with `make`. This will put an executable in `~/.cabal/bin`, which will work for your user, given that you have the directory in your `PATH`.

        # make

 * Optionally, install globally by putting an executable in `/usr/local/bin` (requires root privileges).

        $ make install

## Examples
For a quick example, try running one of the test files by emitting `JavaScript` and piping it to `Node.js`:

    # kite -j examples/Test.kite | node

Read more about the syntax in the [wiki](https://github.com/altschuler/kite/wiki/Syntax) and see example files under the directory [examples](https://github.com/altschuler/kite/tree/master/examples).

## Known issues
 * The type checker is a little buggy. See [issue](https://github.com/altschuler/kite/issues/18).

## Contribution
Feel free to submit a pull request or issue! We are open to ideas and all kinds of feedback.

## Authors
 * [Simon Altschuler](https://github.com/altschuler)
 * [Markus Færevaag](https://github.com/mfaerevaag)
 * [Patrick Gadd](https://github.com/patrickgadd)
 * [Christian Mathias Rohde Kiær](https://github.com/kiaer)

## License
This project is licensed under the MIT-license. [Read more](https://github.com/altschuler/kite/blob/master/LICENSE).
