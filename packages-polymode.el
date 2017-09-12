;; Time-stamp: <2017-09-12 13:51:58 (slane)>
;; Split out package loading into a separate file.
;; Code highlighting via polymode
(use-package markdown-mode
  :ensure t
  )

(use-package polymode
  :ensure markdown-mode
  :init
  (require 'poly-R)
  (require 'poly-noweb)
  :config
  ;; R/tex polymodes
  (add-to-list 'auto-mode-alist '("\\.Rnw" . poly-noweb+r-mode))
  (add-to-list 'auto-mode-alist '("\\.rnw" . poly-noweb+r-mode))
  (add-to-list 'auto-mode-alist '("\\.Rmd" . poly-markdown+r-mode))
  ;; Make sure r-mode is loaded
  (autoload 'r-mode "ess-site.el" "Major mode for editing R source." t)
  )

(use-package poly-markdown
  :ensure polymode
  :defer t
  :mode
  ("\\.md" . poly-markdown-mode)
  :config
  ;; Wrap lines at column limit, but don't put hard returns in
  (add-hook 'markdown-mode-hook (lambda () (visual-line-mode 1)))
  ;; Flyspell on
  (add-hook 'markdown-mode-hook (lambda () (flyspell-mode 1)))
  )

;; Add yaml to markdown an .yml files
(use-package yaml-mode
  :ensure t
  :mode (("\\.yml\\'" . yaml-mode)))
