;; markdown customs for emacs

;; Markdown mode
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.Rmd\\'" . markdown-mode))
;; Wrap lines at column limit, but don't put hard returns in
(add-hook 'markdown-mode-hook (lambda () (visual-line-mode 1)))
;; Flyspell on
(add-hook 'markdown-mode-hook (lambda () (flyspell-mode 1)))
;; Make sure markdown-mode uses pandoc for previewing
;; Use this one if you have a css file
;; (setq markdown-command "pandoc -c file:///home/beaujean/.emacs.d/github-pandoc.css --from markdown_github -t html5 --mathjax --highlight-style pygments --standalone")
;; Without a css file:
(setq markdown-command "pandoc --from markdown_github -t html5 --mathjax --highlight-style pygments --standalone")

;; Add extra reftex commands for pandoc/knitcitations
;; (setq reftex-cite-format
;;       '(
;; 	(?P . "[@%l]")
;; 	(?T . "@%l [p. ]")
;; 	))

;; Do this in a function so that it can be only loaded in markdown
(defun markdown-mode-reftex-setup ()
  (interactive)
  (and (buffer-file-name) (file-exists-p (buffer-file-name))
       (progn
	 ;; Reftex should use the org file as master file. See C-h v TeX-master for infos.
	 (setq TeX-master t)
	 (turn-on-reftex)
	 (setq reftex-cite-format
	       '(
		 ;; These next two were for using knitcitations
		 ;; (?p . "`r knitcitations::citep(bib[['%l']])`")
		 ;; (?t . "`r knitcitations::citet(bib[['%l']])`")
		 ;; These use pandoc and work fine using bookdown/render
		 (?t . "@%l")
		 (?p . "[@%l]")
		 )))))

(add-hook 'markdown-mode-hook 'markdown-mode-reftex-setup)
