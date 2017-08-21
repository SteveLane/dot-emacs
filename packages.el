;; Time-stamp: <2017-08-21 10:58:10 (slane)>
;; Split out package loading into a separate file.
;; Now using use-package

;; These are all the packages I need (there may be more)
;; (yasnippet stan-mode solarized-theme polymode ox-twbs ox-pandoc ox-ioslide ox-impress-js org-ref org-octopress markdown-mode+ magit ess dracula)

;; ESS
(use-package ess
	     ;; installs if not already installed
	     :ensure t
	     :init (require 'ess-site)
	     ;; Set ESS up the way you like it
	     :config
	     (setq-default inferior-R-args "--no-restore-history --no-restore --no-save")
	     (add-hook 'ess-mode-hook (lambda () (auto-fill-mode 1)))
	     (setq ess-ask-for-ess-directory nil)
	     (setq inferior-R-program-name "/usr/local/bin/R") 
	     (setq ess-local-process-name "R")
	     ;; indentation etc (can sort this out later)
	     ;; (add-hook 'ess-mode-hook (lambda () (ess-set-style 'RRR)))
	     (setq ess-default-style 'RRR)
	     (setq ess-nuke-trailing-whitespace t)
	     (setq ess-eval-visibly 'nowait)

	     (defun my/add-pipe ()
	       "Adds a pipe operator %>% with one space to the left and then starts a newline with proper indentation"
	       (interactive)
	       (just-one-space 1)
	       (insert "%>%")
	       (ess-newline-and-indent))
	     (define-key ess-mode-map (kbd "M-p") #'my/add-pipe)

	     ;; Add a chunk for rmarkdown
	     ;; Need to add a keyboard shortcut
	     ;; https://emacs.stackexchange.com/questions/27405/insert-code-chunk-in-r-markdown-with-yasnippet-and-polymode
	     (defun tws-insert-r-chunk (header) 
	       "Insert an r-chunk in markdown mode. Necessary due to interactions between polymode and yas snippet" 
	       (interactive "sHeader: ") 
	       (insert (concat "```{r " header "}\n\n```")) 
	       (forward-line -1))
	     )

;; Code highlighting via polymode
(use-package polymode
	     :ensure t
	     :init
	     (require 'poly-R)
	     (require 'poly-markdown)
	     (require 'poly-noweb)
	     :config
	     ;; Markdown polymode
	     (add-to-list 'auto-mode-alist '("\\.md" . poly-markdown-mode))
	     ;; R/tex polymodes
	     (add-to-list 'auto-mode-alist '("\\.Rnw" . poly-noweb+r-mode))
	     (add-to-list 'auto-mode-alist '("\\.rnw" . poly-noweb+r-mode))
	     (add-to-list 'auto-mode-alist '("\\.Rmd" . poly-markdown+r-mode))
	     )

;; Theming
(use-package dracula
	     :ensure t
	     :init
	     (load-theme 'dracula t)
	     )


;; ;; Comment out whilst I still need to install them...
;; ;; ;; Include stan highlighting
;; ;; (require 'stan-mode)
;; ;; ;; Make sure bugs mode is loaded
;; ;; (require 'ess-bugs-d)
;; ;; ;; Require php-mode
;; ;; (require 'php-mode)


;; (add-hook 'bibtex-mode-hook 'turn-on-auto-revert-mode) ; Automatically detects changes to bibtex.

;; ;; R details
;;  ; Make autofill work for R files.
;; ;; R details, from zmjones.com/mac-setup
;; ;; Don't want it to ask where to create the directory; do it where I currently am.

;; ;;; Perl
;; (add-hook 'perl-mode-hook
;;           (lambda () (setq perl-indent-level 4)))

;; ;; Magit status bind global key
;; (global-set-key (kbd "C-x g") 'magit-status)
;; ;; Magit commit messages have line wrap at 72 not 80 chars
;; (add-hook 'git-commit-mode-hook (lambda () (setq fill-column 72)))
;; (magit-log-arguments (quote ("--graph" "--color" "--decorate" "-n256")))
;; (magit-tag-arguments (quote ("--annotate")))

;; ;; Require ess-jags-mode
;; (require 'ess-jags-d)
