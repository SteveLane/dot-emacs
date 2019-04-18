;; Time-stamp: <2019-04-05 13:46:42 (slane)>
;; yasnippet
;; Many ideas from https://github.com/fniessen/emacs-leuven/blob/master/emacs-leuven.el
(with-eval-after-load 'yasnippet
  (setq yas-snippet-dirs '("~/github/emacs-config/snippets")
        ;; Fix indentation
        (setq yas-indent-line 'fixed)
        )
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
  ;; Don't expand when you are typing in a string or comment.
  (add-hook 'prog-mode-hook
	    (lambda ()
	      (setq yas-buffer-local-condition
		    '(if (nth 8 (syntax-ppss))
                                        ; Non-nil if in a string or comment.
			 '(require-snippet-condition . force-in-comment)
		       t))))
  )
