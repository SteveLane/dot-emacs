;; Time-stamp: <2018-06-28 15:30:05 (slane)>
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
;; (use-package spacemacs-theme
;;   :ensure t
;;   :defer t
;;   :init (progn (load-theme 'spacemacs-dark t)
;; 	       (enable-theme 'spacemacs-dark))
;;   )
;; (use-package anti-zenburn-theme
;;   :ensure t
;;   :defer t
;;   :init (progn (load-theme 'anti-zenburn t)
;; 	       (enable-theme 'anti-zenburn))
;;   )
;; (use-package heroku-theme
;;   :ensure t
;;   :defer t
;;   :init (progn (load-theme 'heroku t)
;; 	       (enable-theme 'heroku))
;;   )
;; (use-package ample-theme
;;   :defer t
;;   :ensure t
;;   :init (progn (load-theme 'ample t t)
;;                (load-theme 'ample-flat t t)
;;                (load-theme 'ample-light t t)
;;                (enable-theme 'ample-light))
;;   )
;; Use the  built-in dichromacy
(load-theme 'dichromacy t)
(enable-theme 'dichromacy)
