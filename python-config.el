;;; python-config.el --- uv + REPL-first Python setup -*- lexical-binding: t; -*-

;; ---------------------------------------------
;; Tree-sitter (Emacs 29+)
;; ---------------------------------------------
(when (fboundp 'python-ts-mode)
  (add-to-list 'major-mode-remap-alist
               '(python-mode . python-ts-mode)))

;; ---------------------------------------------
;; Eglot + ty via uv
;; ---------------------------------------------
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '((python-base-mode :language-id "python")
                 . ("uv" "run" "--quiet" "ty" "server"))))

(add-hook 'python-base-mode-hook #'eglot-ensure)
