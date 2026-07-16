;; Time-stamp: <2026-07-16 10:58:38 (lanes1)>
;; Split out package loading into a separate file.
;; Now using use-package
;; magit
(with-eval-after-load 'magit
  (add-hook 'git-commit-mode-hook (lambda () (setq fill-column 72)))
  (setq
   magit-log-arguments (quote ("--graph" "--color" "--decorate" "-n256"))
   magit-tag-arguments (quote ("--annotate"))
   magit-diff-refine-hunk t
   magit-show-long-lines-warning nil
   )
  )
