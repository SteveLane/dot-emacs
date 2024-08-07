;; This is a general settings file for top-level settings

;; Where to save abbrevs (and silently)
(setq abbrev-file-name "~/.emacs.d/.abbrev_defs")
(setq save-abbrevs 'silently)

;; Here I'm changing my option and meta keys to where I would normally have them.
(when (spacemacs/system-is-mac)
  (setq mac-option-key-is-meta nil
        mac-command-key-is-meta t
        mac-command-modifier 'meta
        mac-option-modifier 'none)
  )

;; OK, now I'm going to remap escaping (are you jk'ing? haha)
(setq-default
 evil-escape-key-sequence "jk"
 evil-escape-unordered-key-sequence "true"
 )

;; I'm sick of backups... at least in my working directory.
;; Let's place them somewhere else (plus some other cool stuff:
(setq backup-by-copying t      ; don't clobber symlinks
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
;; (global-git-commit-mode t)

;; make sure that ess-mode has linting
(add-to-list 'flycheck-global-modes 'ess-mode)

;; try and speed projectile up by using alien indexing
;; https://docs.projectile.mx/en/latest/configuration/#alien-indexing
(setq projectile-indexing-method 'alien)

(when (spacemacs/system-is-mswindows)
  ;; projectile hack to stop tr issue on windows
  ;; https://github.com/bbatsov/projectile/issues/1302
  (setq projectile-git-submodule-command nil)

  ;; make the default directory where I want it
  (setq default-directory "~/")
  )

;; Ensure that plink is used as default tramp on windows
;; And don't use authinfo - always ask for a password
(when (spacemacs/system-is-mswindows)
  (setq tramp-default-method "plink")
  (setq auth-sources nil)
  (setq password-cache-expiry nil)
  )

;; Prefer utf-8 linux, so we don't get crazy linefeeds
(prefer-coding-system 'utf-8-unix)

;; Make dired lists nicer and stop warnings
(setq ls-lisp-use-insert-directory-program t)
(setq dired-listing-switches "-alh")

;; Set environment variable for display of unicode in ipython
(setenv "PYTHONIOENCODING" "utf-8")

;; Also on windows, make sure to use bash versions of commands
(when (eq system-type 'windows-nt)

  ;; Make sure Unix tools are in front of `exec-path'
  (let ((bash (executable-find "bash")))
    (when bash
      (push (file-name-directory bash) exec-path)))

  ;; Update PATH from exec-path
  (let ((path (mapcar 'file-truename
                      (append exec-path
                              (split-string (getenv "PATH") path-separator t)))))
    (setenv "PATH" (mapconcat 'identity (delete-dups path) path-separator))))
