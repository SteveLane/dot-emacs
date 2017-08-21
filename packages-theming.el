;; Time-stamp: <2017-08-21 11:56:21 (slane)>
;; Split out package loading into a separate file.
;; Now using use-package
;; Theming
(use-package dracula
  :ensure t
  :init
  (load-theme 'dracula t)
  )
