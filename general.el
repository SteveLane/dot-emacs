;; This is a general settings file for top-level settings

;; Where to save abbrevs (and silently)
(setq abbrev-file-name "~/.emacs.d/.abbrev_defs")
(setq save-abbrevs 'silently)

;; Here I'm changing my option and meta keys to where I would normally have them.
(setq mac-option-key-is-meta nil
	    mac-command-key-is-meta t
	    mac-command-modifier 'meta
	    mac-option-modifier 'none)

;; OK, now I'm going to remap escaping (are you jk'ing? haha)
(setq-default
 evil-escape-key-sequence "jk"
 evil-escape-unordered-key-sequence "true"
)

;; I'm sick of backups... at least in my working directory.
;; Let's place them somewhere else (plus some other cool stuff:
(setq
 backup-by-copying t      ; don't clobber symlinks
 backup-directory-alist '(("." . "~/.saves"))    ; don't litter my fs tree
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)       ; use versioned backups

;; Add a timestamp to files (and on save)
;; http://emacs-fu.blogspot.com.au/2008/12/automatic-timestamps.html
(setq 
 time-stamp-active t          ; do enable time-stamps
 time-stamp-line-limit 20     ; check first 10 buffer lines for Time-stamp: 
 time-stamp-format "%Y-%m-%d %02H:%02M:%02S (%u)") ; date format
(add-hook 'write-file-hooks 'time-stamp) ; update when saving

;; Make sure that [mM]akefile's with an 'extension' are opened in makefile-mode
(add-to-list 'auto-mode-alist '("[mM]akefile\\.[a-zA-Z]*\\'" . makefile-mode))

;; make spacemacs the global git editor
(global-git-commit-mode t)

;; make sure that ess-mode has linting
(add-to-list 'flycheck-global-modes 'ess-mode)

;; try and speed projectile up by using alien indexing
;; https://docs.projectile.mx/en/latest/configuration/#alien-indexing
(setq projectile-indexing-method 'alien)

;; Set default enconding to unix utf 8
(setq-default buffer-file-coding-system 'utf-8-unix)
