;; Time-stamp: <2017-08-21 11:04:13 (slane)>
;; Split out package loading into a separate file.
;; Code highlighting via polymode
(use-package polymode
	     :ensure t
	     :init
	     (require 'poly-R)
	     (require 'poly-markdown)
	     (require 'poly-noweb)
	     :config
	     ;; Markdown polymode
	     (add-to-list 'auto-mode-alist '("\\.md" . poly-markdown-mode))
	     ;; R/tex polymodes
	     (add-to-list 'auto-mode-alist '("\\.Rnw" . poly-noweb+r-mode))
	     (add-to-list 'auto-mode-alist '("\\.rnw" . poly-noweb+r-mode))
	     (add-to-list 'auto-mode-alist '("\\.Rmd" . poly-markdown+r-mode))
	     )

;; ;; Comment out whilst I still need to install them...
;; ;; ;; Include stan highlighting
;; ;; (require 'stan-mode)
;; ;; ;; Make sure bugs mode is loaded
;; ;; (require 'ess-bugs-d)
;; ;; ;; Require php-mode
;; ;; (require 'php-mode)
;; ;; Require ess-jags-mode
;; (require 'ess-jags-d)
