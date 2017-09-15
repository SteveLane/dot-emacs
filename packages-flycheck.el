;; Time-stamp: <2017-09-15 10:33:28 (slane)>
;; Split out package loading into a separate file.
;; Now using use-package
;; flycheck
(use-package flycheck
  ;; add flycheck and linting
  :ensure ess
  :config
  (add-hook 'ess-mode-hook (lambda () (flycheck-mode t)))
  )
