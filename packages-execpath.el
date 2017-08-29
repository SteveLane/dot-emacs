;; Time-stamp: <2017-08-30 09:16:47 (slane)>
;; Split out package loading into a separate file.
;; inherit shell environment
(use-package exec-path-from-shell
  ;; For code formating to pep8
  :ensure t
  :init
  (progn
    ;; initialise when mac, ns, x
    (when (memq window-system '(mac ns x))
      (exec-path-from-shell-initialize)))
  )
