;; Time-stamp: <2017-09-14 13:48:19 (slane)>
;; Split out package loading into a separate file.
;; Now using use-package
;; Theming
;; (use-package dracula-theme
;;   :ensure t
;;   :init
;;   (load-theme 'dracula t)
;;   )
;; (use-package monokai-theme
;;   :ensure t
;;   :init
;;   (load-theme 'monokai t)
;;   )
(use-package spacemacs-theme
  :ensure t
  :init
  (load-theme 'spacemacs-dark t)
  )
