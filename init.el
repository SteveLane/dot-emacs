;; Time-stamp: <2017-07-27 10:09:59 (slane)>
;; init.el for emacs setup
;; separate files are provided that do different things for easy maintaining

;; Make sure if I double click a file, it is opened instead of the scratch buffer

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(setq inhibit-startup-screen t)
(setq initial-buffer-choice nil)

;; Get rid of OSX native fullscreen (use f11 or M-x toggle-frame-fullscreen)
(setq ns-use-native-fullscreen nil)

;; Set column fill to 80
(setq-default fill-column 80)

;; Change option and meta keys around.
(when (eq system-type 'darwin)
  (setq maq-option-key-is-meta nil
	mac-command-key-is-meta t
	mac-command-modifier 'meta
	mac-option-modifier 'none))

;; For resizing screens between external monitor and retina
(defun fontify-frame (frame)
  (interactive)
  (if window-system
      (progn
        (if (> (x-display-pixel-width) 2000)
	    ;; For the larger external display
	    (set-face-attribute
	     'default nil :family "Hack" :foundry "simp" :slant 'normal :weight 'normal :height 180 :width 'normal)
	  ;; For the smaller retina
	  (set-face-attribute
	   'default nil :family "Hack" :foundry "simp" :slant 'normal :weight 'normal :height 120 :width 'normal)))))

;; Fontify current frame
(fontify-frame nil)

;; Fontify any future frames
(push 'fontify-frame after-make-frame-functions)

;; Fonts on my linux box
(when (eq system-type 'gnu/linux)
  (set-face-attribute
   'default nil :family "Hack" :foundry "simp" :slant 'normal :weight 'normal :height 113 :width 'normal))


;; Set the default directory
(setq default-directory "~/Documents")

;; Turn off the toolbar
(tool-bar-mode -1)

;; Macros for sectioning in R mode.
;; This macro inserts section comments as a header
(fset 'header
   [?\C-u ?8 ?0 ?# return ?\C-u ?8 ?0 ?# return ?\M-\; ?B ?e ?g ?i ?n ?  ?S ?e ?c ?t ?i ?o ?n ?: ?  return ?\C-u ?8 ?0 ?# return ?\C-u ?8 ?0 ?# return up up left])

;; This macro inserts a section footer
(fset 'footer
   [?\C-u ?8 ?0 ?# return ?\C-u ?8 ?0 ?# return ])

;; Default dictionary
(setq default-dictionary "british")
(setq ispell-dictionary "british")

;; Column number mode
(setq column-number-mode t)

;; Show parentheses matching
(setq show-paren-mode t)

;; Make sure that ediff doesn't start in windowed mode
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; I'm sick of backups... at least in my working directory.
;; Let's place them somewhere else (plus some other cool stuff:
(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.saves"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)       ; use versioned backups

;; Add a timestamp to files (and on save)
;; http://emacs-fu.blogspot.com.au/2008/12/automatic-timestamps.html
(setq 
 time-stamp-active t          ; do enable time-stamps
 time-stamp-line-limit 20     ; check first 10 buffer lines for Time-stamp: 
 time-stamp-format "%04y-%02m-%02d %02H:%02M:%02S (%u)") ; date format
(add-hook 'write-file-hooks 'time-stamp) ; update when saving

;; Load separated customisation files.
;; (load "~/.emacs.d/packages.el")
(load "~/.emacs.d/tex-customs.el")
(load "~/.emacs.d/markdown-customs.el")
;; (load "~/.emacs.d/org-customs.el")
(load "~/.emacs.d/yas-customs.el")
