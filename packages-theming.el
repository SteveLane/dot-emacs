;; Time-stamp: <2017-08-21 13:10:45 (slane)>
;; Split out package loading into a separate file.
;; Now using use-package
;; Theming
(use-package dracula-theme
  :ensure t
  :init
  (load-theme 'dracula t)
  )
