;; Set up fonts for use
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8-unix)

;; set a default font
(when (member "Iosevka" (font-family-list))
  (set-face-attribute 'default nil :font "Iosevka"
		      :weight 'light :height 200))
