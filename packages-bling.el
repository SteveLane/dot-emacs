;; Time-stamp: <2018-07-24 07:47:40 (slane)>
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

;; ;; powerline
;; (use-package powerline
;;   :ensure t
;;   ;; :defer t
;;   :config
;;   (powerline-center-theme)
;;   )

;; Use the spaceline version...
;; (use-package spaceline
;;   :ensure t
;;   :init
;;   (setq powerline-default-separator 'wave)
;;   )

;; (use-package spaceline-config
;;   :ensure spaceline
;;   :config
;;   (spaceline-emacs-theme)
;;   )

;; (use-package doom-modeline
;;   :ensure t
;;   :defer t
;;   :hook
;;   (after-init . doom-modeline-init)
;;   )

(use-package spaceline
  :ensure t
  :init 
  (progn 
    ;; size of modeline
    (setq powerline-height 18)
    ;; (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
    
    ;; slant (requires srbg support)
    (setq-default powerline-default-separator 'chamfer) 
    (setq spaceline-separator-dir-left '(right . right))
    (setq spaceline-separator-dir-right '(right . right))
    )
  )

(use-package spaceline-config
  :ensure spaceline
  :config
  (spaceline-toggle-buffer-size-off)
  (spaceline-spacemacs-theme)
  (setq spaceline-buffer-encoding-abbrev-p nil
	spaceline-window-numbers-unicode t
	spaceline-line-column-p nil
	spaceline-buffer-id-p nil
	spaceline-minor-modes-separator nil)
  (powerline-reset)
  )
