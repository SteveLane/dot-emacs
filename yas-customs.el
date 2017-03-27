;; Emacs customisations for yasnippet
;; Many ideas from https://github.com/fniessen/emacs-leuven/blob/master/emacs-leuven.el
(require 'yasnippet)

;; Get rid of the default snippets
(setq yas-snippet-dirs "~/.emacs.d/snippets")

(with-eval-after-load "yasnippet"
  ;; No global mode (just yet)
  ;; (yas-global-mode 1)

  ;; Load snippet tables
  (yas-reload-all)
  ;; Make sure yasnippet is on for org files
  (add-hook 'org-mode-hook 'yas/minor-mode-on)
  ;; Make sure yasnippet is on for R files
  (add-hook 'ess-mode-hook 'yas/minor-mode-on)
  ;; Make sure yasnippet is on for stan files
  (add-hook 'stan-mode-hook 'yas/minor-mode-on)
  ;; Make sure yasnippet is on for jags files
  (add-hook 'ess-jags-mode-hook 'yas/minor-mode-on)
  ;; Make sure yasnippet is on for rmarkdown files
  (add-hook 'markdown-mode-hook 'yas/minor-mode-on)

  ;; Bind `yas-expand' to SPC. This is because it plays up with org-mode
  ;; (define-key yas-minor-mode-map (kbd "<tab>") nil)
  ;; (define-key yas-minor-mode-map (kbd "TAB") nil)
  ;; (define-key yas-minor-mode-map (kbd "SPC") #'yas-expand)

  ;; However, you then don't want other things to stuff up?
  ;; Don't expand when you are typing in a string or comment.
  (add-hook 'prog-mode-hook
	    (lambda ()
	      (setq yas-buffer-local-condition
		    '(if (nth 8 (syntax-ppss))
                                        ; Non-nil if in a string or comment.
			 '(require-snippet-condition . force-in-comment)
		       t))))
  )
