;;; kite-mode.el --- Minor mode for editing kite source code
;; Version: 0.0.1
;;
;;; Commentary:
;; Provides the 'kite-mode' mode which does syntax highlighting for the kite programming language
;;
;;; Code:

;; Syntax highlighting
(defvar kite-keywords
  '(("return\\|import\\|if\\|then\\|else" . font-lock-keyword-face)
    ("False\\|True\\|[0-9]\\(\\.[0-9]*\\)?" . font-lock-constant-face)
    ("->" . font-lock-function-name-face)
    ("#.*" . font-lock-comment-face)
    ("[A-Z][a-zA-Z0-9_']*" . font-lock-type-face)
    ("[a-z][a-zA-Z0-9_']*" . font-lock-variable-name-face)))

;; Syntax table
(defvar kite-syntax-table (make-syntax-table) "Syntax table for `kite-mode'.")

; multiline comment
(modify-syntax-entry ?\# ". 14" kite-syntax-table)
(modify-syntax-entry ?- ". 23" kite-syntax-table)

(define-derived-mode kite-mode fundamental-mode
  "Major mode for editing Kite source code"
  :syntax-table kite-syntax-table

  (setq font-lock-defaults '(kite-keywords))
  (setq mode-name "kite"))

(provide 'kite-mode)

;;; kite-mode.el ends here
