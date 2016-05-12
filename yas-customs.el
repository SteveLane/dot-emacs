;; Emacs customisations for yasnippet
;; Many ideas from https://github.com/fniessen/emacs-leuven/blob/master/emacs-leuven.el
(with-eval-after-load "yasnippet"
  (yas-global-mode 1)

  ;; Load snippet tables
  (yas-reload-all)

  ;; Bind `yas-expand' to SPC. This is because it plays up with org-mode
  (define-key yas-minor-mode-map (kbd "<tab>") nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  (define-key yas-minor-mode-map (kbd "SPC") #'yas-expand)

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
