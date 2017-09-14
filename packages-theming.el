;; Time-stamp: <2017-09-14 10:01:32 (slane)>
;; Split out package loading into a separate file.
;; Now using use-package
;; Theming
;; (use-package dracula-theme
;;   :ensure t
;;   :init
;;   (load-theme 'dracula t)
;;   )

;; Try a couple of others
;; (use-package noctilux-theme
;;   :ensure t
;;   :init
;;   (load-theme 'noctilux t)
;;   )
;; (use-package atom-one-dark-theme
;;   :ensure t
;;   :init
;;   (load-theme 'atom-one-dark t)
;;   )
(use-package monokai-theme
  :ensure t
  :init
  (load-theme 'monokai t)
  )
;; (use-package zenburn-theme
;;   :ensure t
;;   :init
;;   (load-theme 'zenburn t)
;;   )
;; (use-package ubuntu-theme
;;   :ensure t
;;   :init
;;   (load-theme 'ubuntu t)
;;   )
