;; Time-stamp: <2017-08-21 12:07:00 (slane)>
;; Split out package loading into a separate file.
;; Now using use-package
;; magit
(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status))
  :config
  (add-hook 'git-commit-mode-hook (lambda () (setq fill-column 72)))
  (setq
   magit-log-arguments (quote ("--graph" "--color" "--decorate" "-n256"))
   magit-tag-arguments (quote ("--annotate"))
   )
  )
