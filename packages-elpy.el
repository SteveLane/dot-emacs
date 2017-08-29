;; Time-stamp: <2017-08-30 09:34:42 (slane)>
;; Split out package loading into a separate file.
;; elpy for python
(use-package elpy
  ;; For code formating to pep8
  :ensure py-autopep8
  ;; For python REPL
  :ensure ein
  ;; For shell variables
  :ensure exec-path-from-shell
  :defer 2
  :init
  (progn
    ;; Use Flycheck instead of Flymake
    (when (require 'flycheck nil t)
      (remove-hook 'elpy-modules 'elpy-module-flymake)
      (remove-hook 'elpy-modules 'elpy-module-yasnippet)
      (remove-hook 'elpy-mode-hook 'elpy-module-highlight-indentation)
      (add-hook 'elpy-mode-hook 'flycheck-mode)
      (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save))
    (elpy-enable)
    (elpy-use-ipython)
    ;; jedi is great
    (setq elpy-rpc-backend "jedi")))
