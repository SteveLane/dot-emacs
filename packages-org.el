;; Time-stamp: <2018-07-02 15:11:30 (slane)>
;; Commands to load org related packages
(use-package org
  :ensure ox-pandoc
  ;; :init
  ;; (require 'ox-pandoc)
  :mode (("\\.org$" . poly-org-mode))
  ;; keybindings
  :bind (
	 ;; Open the agenda
	 ("C-c a" . org-agenda)
	 ;; Capture something
	 ("C-c c" . org-capture)
	 ;; Store a link
	 ("C-c l" . org-store-link)
	 )
  :config
  (progn
    ;; soft line wrapping
    (add-hook 'org-mode-hook (lambda () (visual-line-mode 1)))
    ;; Disable whitespace mode in org mode
    (add-hook 'org-mode-hook (lambda () (whitespace-mode -1)))
    ;; Flyspell on
    (add-hook 'org-mode-hook (lambda () (flyspell-mode 1)))
    )

  ;; Set out some defaults
  (setq
   ;; Default directory for org files
   org-directory "~/org"
   ;; Directory for notes/tasks to be refiled
   org-default-notes-file "~/org/refile.org"
   ;; fontify code blocks
   org-src-fontify-natively t
   ;; Org files are stored in ~/org by default
   ;; Mark todo items with the time they are done
   org-log-done t
   ;; Allows to store agenda files in their appropriate files.
   ;; This is useful when per project task lists are used.
   org-refile-targets (quote ((nil :maxlevel . 9)
                              (org-agenda-files :maxlevel . 9)))
   )
  
  )
