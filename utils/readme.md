# Utilities

## Linting Hook
To setup the git linting hook to lint your `src` folder before commit, run the following from the repo's base directory:

```bash
ln -s $PWD/utils/linting-hook.sh $PWD/.git/hooks/pre-commit
```

## Emacs Mode
To bind your `.kite` files to `kite-mode` in Emacs, do the following:

* Add or link `kite-mode.el` to your `~/.emacs.d/` or anywhere else in it's load path.

* Add the following to your `~/.emacs`:

    ```lisp
    (require 'kite-mode)
    (add-to-list 'auto-mode-alist '("\\.kite\\'" . kite-mode))
    ```
