;; Time-stamp: <2017-08-22 20:24:04 (slane)>
;; Split out package loading into a separate file.
;; Now using use-package
;; parentheses
(use-package highlight-parentheses
  :ensure t
  :config
  (progn
    (highlight-parentheses-mode)
    (global-highlight-parentheses-mode))
  )
