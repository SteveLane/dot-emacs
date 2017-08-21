;; Time-stamp: <2017-08-21 14:14:17 (slane)>
;; Split out package loading into a separate file.
;; Now using use-package
;; autoinsert
(use-package autoinsert
  :ensure t
  :init
  (setq auto-insert-query nil)
  (setq auto-insert-directory (locate-user-emacs-file "templates"))
  (add-hook 'find-file-hook 'auto-insert)
  (auto-insert-mode 1)
  :config
  (defun autoinsert-yas-expand()
    "Replace text in yasnippet template."
    (yas-expand-snippet (buffer-string) (point-min) (point-max)))
  (define-auto-insert "\\.R$" [ "default-R.R" autoinsert-yas-expand ])
  (define-auto-insert "\\.Rmd$" [ "default-Rmd.Rmd" autoinsert-yas-expand ])
  )
