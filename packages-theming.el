;; Time-stamp: <2018-04-16 11:23:22 (slane)>
;; Split out package loading into a separate file.
;; Now using use-package
;; Theming
;; (use-package dracula-theme
;;   :ensure t
;;   :defer t
;;   :init (progn (load-theme 'dracula t)
;; 	       (enable-theme 'dracula))
;;   )
;; (use-package monokai-theme
;;   :ensure t
;;   :defer t
;;   :init (progn (load-theme 'monokai t)
;; 	       (enable-theme 'monokai))
;;   )
(use-package spacemacs-theme
  :ensure t
  :defer t
  :init (progn (load-theme 'spacemacs-dark t)
	       (enable-theme 'spacemacs-dark))
  )
