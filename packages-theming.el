;; Time-stamp: <2017-08-21 11:04:43 (slane)>
;; Split out package loading into a separate file.
;; Now using use-package
;; Theming
(use-package dracula
	     :ensure t
	     :init
	     (load-theme 'dracula t)
	     )
