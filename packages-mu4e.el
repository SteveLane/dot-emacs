;; Time-stamp: <2018-06-27 20:04:30 (slane)>
;; Commands to load mu4e related stuff
;; Need to tell emacs about mu4e...
;; (add-to-list 'load-path
;; 	     (expand-file-name "/usr/local/Cellar/mu/1.0/share/emacs/site-lisp/mu/mu4e"))

(use-package mu4e
  ;; Must be installed first...
  :load-path "/usr/local/Cellar/mu/1.0/share/emacs/site-lisp/mu/mu4e"
  :config
  (setq mu4e-sent-folder "/unimelb/Sent Items"
	mu4e-drafts-folder "/unimelb/Drafts"
	mu4e-trash-folder "/unimelb/Deleted Items/")
  ;; :ensure ox-pandoc
  ;; ;; :init
  ;; ;; (require 'ox-pandoc)
  ;; :mode (("\\.org$" . poly-org-mode))
  ;; :config
  ;; (progn
  ;;   ;; soft line wrapping
  ;;   (add-hook 'org-mode-hook (lambda () (visual-line-mode 1)))
  ;;   ;; Disable whitespace mode in org mode
  ;;   (add-hook 'org-mode-hook (lambda () (whitespace-mode -1)))    
  ;;   )
  )
