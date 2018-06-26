;; Time-stamp: <2018-06-22 12:46:16 (slane)>
;; Commands to load org related packages
(use-package org
  :ensure ox-pandoc
  ;; :init
  ;; (require 'ox-pandoc)
  :mode (("\\.org$" . poly-org-mode))
  :config
  (progn
    ;; soft line wrapping
    (add-hook 'org-mode-hook (lambda () (visual-line-mode 1)))
    ;; Disable whitespace mode in org mode
    (add-hook 'org-mode-hook (lambda () (whitespace-mode -1)))    
    ))
