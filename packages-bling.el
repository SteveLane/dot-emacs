;; Time-stamp: <2018-07-05 20:06:44 (slane)>
;; Split out package loading into a separate file.

;; Icons!
(use-package all-the-icons
  :ensure t
  ;; Nothing else here, but there's a hook to install fonts
  )

;; powerline
(use-package powerline
  :ensure t
  ;; :defer t
  :config
  (powerline-center-theme)
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
