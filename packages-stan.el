;; Time-stamp: <2018-08-21 10:18:54 (slane)>
;; Split out package loading into a separate file.
;; Now using use-package
;; stan-mode
(use-package stan-mode
  :ensure t
  :mode ("\\.stan\\'" . stan-mode)
  )

(use-package stan-snippets
  :ensure t
  )
