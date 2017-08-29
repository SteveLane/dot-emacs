;; Time-stamp: <2017-08-30 09:17:06 (slane)>
;; init.el for emacs setup
;; separate files are provided that do different things for easy maintaining

;; move customs away.
(setq custom-file "~/.emacs.d/custom.el")

;; Where to save abbrevs (and silently)
(setq abbrev-file-name "~/.emacs.d/.abbrev_defs")
(setq save-abbrevs 'silently)

;; Set up packages
(require 'package)
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
	("melpa-stable" . "https://stable.melpa.org/packages/")))
(package-initialize)
;; Bootstrap use-package
;; Install use-package if it's not already installed.
;; use-package is used to configure the rest of the packages.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(let ((default-directory "~/.emacs.d/elpa/"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))
(eval-when-compile
  (require 'use-package))

;; Make sure if I double click a file, it is opened instead of the scratch buffer
(setq inhibit-startup-screen t)
(setq initial-buffer-choice nil)

;; Get rid of OSX native fullscreen (use f11 or M-x toggle-frame-fullscreen)
(setq ns-use-native-fullscreen nil)

;; Set column fill to 80
(setq-default fill-column 80)

;; Change option and meta keys around.
(when (eq system-type 'darwin)
  (setq mac-option-key-is-meta nil
	mac-command-key-is-meta t
	mac-command-modifier 'meta
	mac-option-modifier 'none))

;; Set fonts based on system/screensize
(if (eq window-system nil)
    ;; if no window/no X                                                        
    (when (member "Hack" (font-family-list))
      (add-to-list 'initial-frame-alist '(font . "Hack-10"))
      (add-to-list 'default-frame-alist '(font . "Hack-10")))
    ;; else if windowed system
    ;; and its an x system (which has different sizes... why?
    (if (eq window-system 'x)
      ;; Bigger external screen
      (if (> (x-display-pixel-width) 2000)
	  ;; Bigger external screen
	  (when (member "Hack" (font-family-list))
	    (add-to-list 'initial-frame-alist '(font . "Hack-18"))
	    (add-to-list 'default-frame-alist '(font . "Hack-18")))
	  (if (< (x-display-pixel-width) 1300)
	      ;; smaller retina
	      (when (member "Hack" (font-family-list))
		(add-to-list 'initial-frame-alist '(font . "Hack-9"))
		(add-to-list 'default-frame-alist '(font . "Hack-9")))
	      (when (member "Hack" (font-family-list))
		(add-to-list 'initial-frame-alist '(font . "Hack-14"))
		(add-to-list 'default-frame-alist '(font . "Hack-14"))))
	  )))

;; For resizing screens between external monitor and retina
(defun fontify-frame (frame)
  (interactive)
  (if window-system
      (progn
        (if (> (x-display-pixel-width) 2000)
	    ;; For the larger external display
	    (set-face-attribute
	     'default nil :height 180)
	    ;; For the smaller retina
	    (set-face-attribute
	     'default nil :height 120)))))

;; Fontify current frame
(fontify-frame nil)

;; Fontify any future frames
(push 'fontify-frame after-make-frame-functions)

;; Set the default directory
(setq default-directory "~/")

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
(setq show-paren-delay 0)
(setq show-paren-mode 1)

;; Change default searching to regexp
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

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

;; ;; Load separated customisation files.
(load "~/.emacs.d/packages-ess.el")
(load "~/.emacs.d/packages-polymode.el")
(load "~/.emacs.d/packages-theming.el")
(load "~/.emacs.d/packages-latex.el")
(load "~/.emacs.d/packages-stan.el")
(load "~/.emacs.d/packages-yas.el")
(load "~/.emacs.d/packages-magit.el")
(load "~/.emacs.d/packages-autoinsert.el")
(load "~/.emacs.d/packages-company.el")
(load "~/.emacs.d/packages-parentheses.el")
(load "~/.emacs.d/packages-elpy.el")
(load "~/.emacs.d/packages-multiterm.el")
(load "~/.emacs.d/packages-execpath.el")
