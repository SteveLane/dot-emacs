;; Time-stamp: <2018-10-09 09:52:23 (slane)>
;; Split out package loading into a separate file.
;; Code highlighting via polymode
(use-package markdown-mode
  :ensure t
  :mode
  (("README\\.md\\'" . gfm-mode)
   ("\\.md\\'" . markdown-mode)
   ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-command "markdown")
  )

(use-package polymode
  :ensure markdown-mode
  :ensure poly-R
  :ensure poly-noweb
  :config
  ;; R/tex polymodes
  (add-to-list 'auto-mode-alist '("\\.Rnw" . poly-noweb+r-mode))
  (add-to-list 'auto-mode-alist '("\\.rnw" . poly-noweb+r-mode))
  (add-to-list 'auto-mode-alist '("\\.Rmd" . poly-markdown+r-mode))
  ;; org-mode poly (not working at the moment)
  ;; (add-to-list 'auto-mode-alist '("\\.org" . poly-org-mode))
  ;; Make sure r-mode is loaded
  ;; (autoload 'r-mode "ess-site.el" "Major mode for editing R source." t)

  ;; Add a chunk for rmarkdown
  ;; Need to add a keyboard shortcut
  ;; https://emacs.stackexchange.com/questions/27405/insert-code-chunk-in-r-markdown-with-yasnippet-and-polymode
  ;; (defun insert-r-chunk (header) 
  ;;   "Insert an r-chunk in markdown mode. Necessary due to interactions between polymode and yas snippet" 
  ;;   (interactive "sHeader: ") 
  ;;   (insert (concat "```{r " header "}\n\n\n```")) 
  ;;   (forward-line -2))
  ;; (define-key poly-markdown+r-mode-map (kbd "M-c") #'insert-r-chunk)
  )

(use-package poly-markdown
  :ensure polymode
  :defer t
  :config
  ;; Wrap lines at column limit, but don't put hard returns in
  (add-hook 'markdown-mode-hook (lambda () (visual-line-mode 1)))
  ;; Flyspell on
  (add-hook 'markdown-mode-hook (lambda () (flyspell-mode 1)))
  )

;; poly-R
(use-package poly-R
  :ensure polymode
  :ensure poly-markdown
  :ensure poly-noweb
  :defer t
  :config
  ;; Add a chunk for rmarkdown
  ;; Need to add a keyboard shortcut
  ;; https://emacs.stackexchange.com/questions/27405/insert-code-chunk-in-r-markdown-with-yasnippet-and-polymode
  ;; (defun insert-r-chunk (header) 
  ;;   "Insert an r-chunk in markdown mode. Necessary due to interactions between polymode and yas snippet" 
  ;;   (interactive "sHeader: ") 
  ;;   (insert (concat "```{r " header "}\n\n\n```")) 
  ;;   (forward-line -2))
  ;; (define-key poly-markdown+r-mode-map (kbd "M-c") #'insert-r-chunk)
)

;; Add yaml to markdown an .yml files
(use-package yaml-mode
  :ensure t
  :mode (("\\.yml\\'" . yaml-mode)))
