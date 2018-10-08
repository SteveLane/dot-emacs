;; Time-stamp: <2018-10-09 10:06:21 (slane)>
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
  )
