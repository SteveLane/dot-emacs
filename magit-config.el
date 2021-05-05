;; Time-stamp: <2019-03-27 16:30:14 (slane)>
;; Split out package loading into a separate file.
;; Now using use-package
;; magit
(with-eval-after-load 'magit
  (add-hook 'git-commit-mode-hook (lambda () (setq fill-column 72)))
  (setq
   magit-log-arguments (quote ("--graph" "--color" "--decorate" "-n256"))
   magit-tag-arguments (quote ("--annotate"))
   )
  )
