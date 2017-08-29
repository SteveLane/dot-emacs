;; Time-stamp: <2017-08-29 11:57:30 (slane)>
;; Split out package loading into a separate file.
;; ESS
(use-package ess-site
  ;; installs if not already installed
  :ensure ess
  :diminish eldoc-mode
  :defer 1
  ;; :init (require 'ess-site)
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
