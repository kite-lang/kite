;;; kite-mode.el --- Minor mode for editing kite source code
;; Version: 0.2.0
;;
;;; Commentary:
;; Provides the 'kite-mode' mode which does syntax highlighting
;; and compilation facilities for the Kite programming language
;;
;;; Code:

(defvar kite-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'kite-compile-current-buffer)
    (define-key map (kbd "C-c C-p C-c")
      (lambda () (interactive) (kite-compile-current-buffer "-p")))
    (define-key map (kbd "C-c C-r") 'kite-compile-region)
    (define-key map (kbd "C-c C-p C-r")
      (lambda (start end) (interactive "r") (kite-compile-region start end "-p")))
    (define-key map (kbd "C-c C-j")
      (lambda () (interactive) (kite-compile-current-buffer " && ./main")))
    (define-key map (kbd "C-c C-d C-j")
      (lambda () (interactive) (kite-compile-current-buffer " -d && ./main")))
    map)

  "Keymap for Kite major mode.")

(defcustom kite-compiled-buffer-name "*kite-compiled*"
  "The name of the scratch buffer used for compiled Kite source."
  :type 'string
  :group 'kite)

(defcustom kite-command "kite"
  "The command used to compile Kite source code."
  :type 'string
  :group 'kite)

(defun kite-compile-current-buffer (extra)
  "Compile current buffer and show output in buffer named `kite-compiled-buffer-name'."
  (interactive)
  (save-excursion
    (kite-compile-region extra)))

(defun kite-compile-region (extra)
  "Compile current region and show output in buffer named `kite-compiled-buffer-name'."
  (interactive "r")

  (get-buffer-create kite-compiled-buffer-name)

  (let ((result (shell-command-to-string
                 (format "%s %s %s"
                         kite-command
                         (buffer-file-name)
                         extra)))
        (buffer (get-buffer kite-compiled-buffer-name)))
    (display-buffer buffer)
    (with-current-buffer buffer
      (erase-buffer)
      (insert result)))

  (message "Kite finished"))

;; Menu bar

(easy-menu-define coffee-mode-menu kite-mode-map
  "Menu for Kite mode"
  '("Kite"
    ["Compile File" kite-compile-current-buffer]
    ))

;; keywords
(defvar kite-keywords
  '(("return\\|type\\|import\\|if\\|then\\|else\\|match" . font-lock-keyword-face)
    ;("[A-Z][a-zA-Z0-9_']*" . font-lock-type-face)
    ("False\\|True\\|Void" . font-lock-constant-face)
    ("[0-9]+f" . font-lock-constant-face) ; float constants
    ("[a-zA-Z][a-zA-Z0-9_']*" . font-lock-variable-name-face)
    ("[0-9]+\\(\\.[0-9]+\\)?" . font-lock-constant-face) ; number constants
    ("->" . font-lock-function-name-face)))

;; syntax table
(defvar kite-syntax-table nil "Syntax table for `kite-mode'.")
(setq kite-syntax-table
      (let ((syntax-table (make-syntax-table)))

        ;; singleline comments
        (modify-syntax-entry ?\{  "(}1nb" syntax-table)
        (modify-syntax-entry ?\}  "){4nb" syntax-table)
        (modify-syntax-entry ?-  "_ 123" syntax-table)
        (modify-syntax-entry ?\n ">" syntax-table)

        syntax-table))

;; comment/uncomment text
(defun kite-comment-dwim (arg)
  "Comment or uncomment current line or region.
For detail, see `comment-dwim'."
  (interactive "*P")
  (require 'newcomment)
  (let ((comment-start "--") (comment-end ""))
    (comment-dwim arg)))

;; define mode
(define-derived-mode kite-mode fundamental-mode
  "Major mode for editing Kite source code"
  :syntax-table kite-syntax-table

  (setq font-lock-defaults '(kite-keywords))
  (setq mode-name "kite")

  (define-key kite-mode-map [remap comment-dwim] 'kite-comment-dwim))


(provide 'kite-mode)

;;; kite-mode.el ends here
