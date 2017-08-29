;; Time-stamp: <2017-08-28 11:13:04 (slane)>
;; Split out package loading into a separate file.
;; Now using use-package
;; multi-term
(use-package multi-term
  :ensure t
  :config
  (setq multi-term-program "/bin/bash")
  )
