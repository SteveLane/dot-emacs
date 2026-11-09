;; -*- lexical-binding: t; -*-
;; Time-stamp: <2026-11-09 13:15:20 (lanes1)>
;; yasnippet
;; Many ideas from https://github.com/fniessen/emacs-leuven/blob/master/emacs-leuven.el
(with-eval-after-load 'yasnippet
  ;; Just to get my version of sphinx doc functions working...
  (load-file "~/github/emacs-config/snippets/python-mode/.yas_setup.el")
  (setq yas-snippet-dirs
        '("~/github/emacs-config/snippets/")
        )
  )
