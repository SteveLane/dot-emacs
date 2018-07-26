;; Time-stamp: <2018-07-24 10:05:38 (slane)>
;; Split out package loading into a separate file.

;; Icons!
(use-package all-the-icons
  :ensure t
  ;; Nothing else here, but there's a hook to install fonts
  )

;; neo-tree (cool foldering...)
(use-package neotree
  :ensure t
  :ensure all-the-icons
  :config
  ;; Use it (F8)
  (global-set-key [f8] 'neotree-toggle)
  ;; Use icons
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  )

;; This dims the non-active buffers
(use-package dimmer
  :ensure t
  :config
  (add-hook 'after-init-hook 'dimmer-mode)
  (setq-default dimmer-fraction 0.5)
  )

;; Try minions and moody from magit author
;; (use-package minions
;;   :ensure t
;;   :init (minions-mode)
;;   :config
;;   (setq
;;    minions-mode-line-lighter "#"
;;    minions-direct '(flycheck-mode))
;;   )

;; (use-package moody
;;   :ensure t
;;   :config
;;   (moody-replace-mode-line-buffer-identification)
;;   (moody-replace-vc-mode)
;;   )
