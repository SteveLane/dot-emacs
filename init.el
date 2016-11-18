;; .emacs file that has been adapted from my linux one. I think there's a whole heap of stuff that I don't need...

;; Make sure if I double click a file, it is opened instead of the scratch buffer
(setq inhibit-startup-screen t)
(setq initial-buffer-choice nil)

;; Get rid of OSX native fullscreen (use f11 or M-x toggle-frame-fullscreen)
(setq ns-use-native-fullscreen nil)

;; Set column fill to 80
(setq-default fill-column 80)

;; Change option and meta keys around.
(when (eq system-type 'darwin)
  ;; (set-default-font "-*-Hack-normal-normal-normal-*-18-*-*-*-m-0-iso10646-1")
  (setq maq-option-key-is-meta nil
	mac-command-key-is-meta t
	mac-command-modifier 'meta
	mac-option-modifier 'none))
;; Use this next when on the large screen - need to figure out how to identify it
;; -*-Hack-normal-normal-normal-*-18-*-*-*-m-0-iso10646-1

;; This gets the fonts right depending on whether I'm on the external screen or the retina...
;; Gist-ed from in https://github.com/arnab/emacs-starter-kit
(defun fontify-frame (frame)
  (interactive)
  (if window-system
      (progn
        (if (> (x-display-pixel-width) 2000)
            (set-frame-parameter frame 'font "-*-Hack-normal-normal-normal-*-18-*-*-*-m-0-iso10646-1") ;; Cinema Display
         (set-frame-parameter frame 'font "-*-Hack-normal-normal-normal-*-12-*-*-*-m-0-iso10646-1")))))

;; Fontify current frame
(fontify-frame nil)

;; Fontify any future frames
(push 'fontify-frame after-make-frame-functions)

;; Set the default directory
(setq default-directory "~/Documents")

;; Turn off the toolbar
(tool-bar-mode -1)

;; Load packages from elpa
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(when (not package-archive-contents) (package-refresh-contents))
(let ((default-directory "~/.emacs.d/elpa/"))
    (normal-top-level-add-to-load-path '("."))
    (normal-top-level-add-subdirs-to-load-path))
;; Comment out whilst I still need to install them...
;; ;; Include stan highlighting
;; (require 'stan-mode)
;; ;; Make sure bugs mode is loaded
;; (require 'ess-bugs-d)
;; ;; Require php-mode
;; (require 'php-mode)
;; Load ess
(require 'ess-site)

;; Solarized
(load-theme 'solarized-dark t)

;; Make sure that ediff doesn't start in windowed mode
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(add-hook 'bibtex-mode-hook 'turn-on-auto-revert-mode) ; Automatically detects changes to bibtex.

;; R details
(setq-default inferior-R-args "--no-restore-history --no-restore --no-save")
(add-hook 'ess-mode-hook (lambda () (auto-fill-mode 1))) ; Make autofill work for R files.
;; R details, from zmjones.com/mac-setup
;; Don't want it to ask where to create the directory; do it where I currently am.
(setq ess-ask-for-ess-directory nil)
(setq inferior-R-program-name "/usr/local/bin/R") 
(setq ess-local-process-name "R")

;; Macro section
;; This macro inserts section comments as a header
(fset 'header
   [?\C-u ?8 ?0 ?# return ?\C-u ?8 ?0 ?# return ?\M-\; ?B ?e ?g ?i ?n ?  ?S ?e ?c ?t ?i ?o ?n ?: ?  return ?\C-u ?8 ?0 ?# return ?\C-u ?8 ?0 ?# return up up left])

;; This macro inserts a section footer
(fset 'footer
   [?\C-u ?8 ?0 ?# return ?\C-u ?8 ?0 ?# return ])

;; This macro inserts a title comment
(fset 'title
   [?\C-u ?8 ?0 ?# return ?\C-u ?8 ?0 ?# return ?\M-\; ?T ?i ?t ?l ?e ?: return ?\M-\; ?A ?u ?t ?h ?o ?r ?: return ?\M-\; ?D ?a ?t ?e ?: ?  ?\C-u ?\M-\! ?d ?a ?t ?e ?  ?\" ?\+ ?\% ?A ?, ?  ?\% ?d ?  ?\% ?B ?  ?\% ?Y ?\" return backspace ?  ?\C-e return ?\M-\; ?S ?y ?n ?o ?p ?s ?i ?s ?: return ?\C-u ?8 ?0 ?# return ?\C-u ?8 ?0 ?# return up up up up up ?\C-e ?  ?S ?t ?e ?v ?e ?  ?L ?a ?n ?e up ? ])

;; This macro inserts a title comment (for stan code)
(fset 'title-stan
   [?\C-u ?8 ?0 ?/ return ?\C-u ?8 ?0 ?/ return ?\M-\; ?T ?i ?t ?l ?e ?: return ?\M-\; ?A ?u ?t ?h ?o ?r ?: return ?\M-\; ?D ?a ?t ?e ?: ?  ?\C-u ?\M-\! ?d ?a ?t ?e ?  ?\" ?\+ ?\% ?A ?, ?  ?\% ?d ?  ?\% ?B ?  ?\% ?Y ?\" return backspace ?  ?\C-e return ?\M-\; ?S ?y ?n ?o ?p ?s ?i ?s ?: return ?\C-u ?8 ?0 ?/ return ?\C-u ?8 ?0 ?/ return up up up up up ?\C-e ?  ?S ?t ?e ?v ?e ?  ?L ?a ?n ?e up ? ])

;; This macro, adds an 'edited' comment
(fset 'edited
   [?# ?# ?  ?E ?d ?i ?t ?e ?d ?: ?  ?\C-u ?\M-! ?d ?a ?t ?e ?  ?\" ?+ ?% ?A ?, ?  ?% ?B ?  ?% ?d ?  ?% ?Y ?\" return ?\C-e ?. delete ? ])

;; Default dictionary
(setq default-dictionary "british")
(setq ispell-dictionary "british")

;; Column number mode
(setq column-number-mode t)

(setq ess-default-style 'RRR)
(setq ess-nuke-trailing-whitespace t)

;; Show parentheses matching
(show-paren-mode t)

;;; Perl
(add-hook 'perl-mode-hook
          (lambda () (setq perl-indent-level 4)))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(magit-tag-arguments (quote ("--annotate")))
 '(package-selected-packages
   (quote
    (yasnippet stan-mode solarized-theme polymode ox-twbs ox-pandoc ox-ioslide ox-impress-js org-ref org-octopress markdown-mode+ magit ess))))

;; I'm sick of backups... at least in my working directory. Let's place them somewhere else (plus some other cool stuff:
(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.saves"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)       ; use versioned backups

;; Magit status bind global key
(global-set-key (kbd "C-x g") 'magit-status)

;; Add a timestamp to files (and on save)
;; http://emacs-fu.blogspot.com.au/2008/12/automatic-timestamps.html
(setq 
 time-stamp-active t          ; do enable time-stamps
 time-stamp-line-limit 10     ; check first 10 buffer lines for Time-stamp: 
 time-stamp-format "%04y-%02m-%02d %02H:%02M:%02S (%u)") ; date format
(add-hook 'write-file-hooks 'time-stamp) ; update when saving

;; Load separated customisation files.
(load "~/.emacs.d/tex-customs.el")
(load "~/.emacs.d/markdown-customs.el")
(load "~/.emacs.d/org-customs.el")
(load "~/.emacs.d/yas-customs.el")

;; Require ess-jags-mode
(require 'ess-jags-d)

;; Adding in polymode stuff
(require 'poly-R)
(require 'poly-markdown)
(require 'poly-noweb)
;; Markdown polymode
(add-to-list 'auto-mode-alist '("\\.md" . poly-markdown-mode))
;; R/tex polymodes
(add-to-list 'auto-mode-alist '("\\.Rnw" . poly-noweb+r-mode))
(add-to-list 'auto-mode-alist '("\\.rnw" . poly-noweb+r-mode))
(add-to-list 'auto-mode-alist '("\\.Rmd" . poly-markdown+r-mode))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
