;; Time-stamp: <2018-12-13 12:16:56 (slane)>
;; Split out package loading into a separate file.
;; Now using use-package
;; flycheck
(use-package flycheck
  ;; add flycheck and linting
  :ensure t
  :ensure ess
  :config
  (add-hook 'ess-mode-hook (lambda () (flycheck-mode t)))
  (add-hook 'LaTeX-mode-hook (lambda () (flycheck-mode t)))
  (add-hook 'sh-mode-hook (lambda () (flycheck-mode t)))

  ;; Change defaults...
  ;; Only check on save, or if idle for longer than 5 seconds.
  (setq flycheck-check-syntax-automatically
	'(save
	  idle-change
	  mode-enabled)
	)
  (setq flycheck-idle-change-delay 5.0)
  )
