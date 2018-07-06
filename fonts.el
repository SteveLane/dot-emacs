;; Set up fonts for use
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8-unix)

;; set a default font
(when (member "Iosevka" (font-family-list))
  (set-face-attribute 'default nil :font "Iosevka"
		      :weight 'light :height 200))

;; (as above, but when starting a new frame, e.g. when using daemon)
(when (member "Iosevka" (font-family-list))
  (add-to-list 'default-frame-alist '(font . "-*-Iosevka-light-normal-normal-*-20-*-*-*-m-0-iso10646-1"))
  (set-face-attribute 'default (selected-frame) :font "-*-Iosevka-light-normal-normal-*-20-*-*-*-m-0-iso10646-1")
  )
