#!/bin/sh
# git linting hook
# setup with:
# ln -s $PWD/utils/linting-hook.sh $PWD/.git/hooks/pre-commit

# redirect output to stderr
exec 1>&2

# cd to repo base dir
cd "$(git rev-parse --show-toplevel)"

# lint work tree
hlint -c src
