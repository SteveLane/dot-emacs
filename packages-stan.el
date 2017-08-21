;; Time-stamp: <2017-08-21 11:43:58 (slane)>
;; Split out package loading into a separate file.
;; Now using use-package
;; stan-mode
(use-package stan-mode
  :ensure t
  :mode ("\\.stan\\'" . stan-mode)
  )
