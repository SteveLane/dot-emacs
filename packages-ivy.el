;; Time-stamp: <2018-07-05 21:31:13 (slane)>
;; Commands to load ivy/swiper

(use-package ivy
  :ensure t
  :config
  (ivy-mode 1)

  ;; recommended for beginners by https://oremacs.com/swiper/
  (setq
   ivy-use-virtual-buffers t
   ivy-count-format "(%d/%d) "
   )
  )

(use-package swiper
  :ensure t

  :config
  ;; this will replace my regex search...
  (global-set-key (kbd "C-s") 'swiper)
  )
