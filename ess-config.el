;; Time-stamp: <2021-05-03 15:06:56 (sprazza)>
;; Extra config for ESS that's required as spacemacs has some weird defaults.
(with-eval-after-load 'ess-mode
  (define-key ess-mode-map ";" 'ess-insert-assign)
  (define-key inferior-ess-mode-map ";" 'ess-insert-assign)
  ;; Set ESS up the way you like it
  (setq-default inferior-R-args "--no-restore-history --no-restore --no-save")
  (add-hook 'ess-mode-hook (lambda () (auto-fill-mode 1)))
  (setq ess-ask-for-ess-directory t)
  ;; (when (spacemacs/system-is-mac)
  ;;   ((setq inferior-ess-r-program "/usr/local/bin/R")))
  ;; this should only need setting in windows
  (when (spacemacs/system-is-mswindows)
    ((setq inferior-R-program-name "c:/Program Files/R/R-4.0.2/bin/x64/Rterm.exe")))
  (setq ess-local-process-name "R")
  ;; Default indentation style as RStudio (spacemacs sets a bunch of dumb stuff)
  (add-hook 'ess-mode-hook (lambda ()
                             (setq
                              ess-first-continued-statement-offset 'straight
                              ess-continued-statement-offset 'straight
                              ess-default-style 'RStudio-
                              ess-indent-offset 4
                              ess-offset-arguments 'prev-line
                              ess-align-blocks nil
                              ess-indent-with-fancy-comments nil)
                             )
            )
  (setq ess-indent-offset 4)
  (setq ess-nuke-trailing-whitespace t)
  (setq ess-eval-visibly 'nowait)
  ;; Remove old _ mapping
  (setq ess-smart-S-assign-key nil)
  ;; Similarly for bugs/jags mode
  ;; (define-key ess-jags-mode-map "_" nil)
  ;; (define-key ess-jags-mode-map ";" #'ess-bugs-hot-arrow)
  
  ;; Function to add the pipe operator (set in map above)
  (defun my-add-pipe ()
    "Adds a pipe operator %>% with one space to the left and then starts a newline with proper indentation"
    (interactive)
    (just-one-space 1)
    (insert "%>%")
    (reindent-then-newline-and-indent)
    )
  (when (spacemacs/system-is-mac)
    (define-key ess-mode-map (kbd "M-S-m") 'my-add-pipe))
  (when (spacemacs/system-is-mswindows)
    (define-key ess-mode-map (kbd "C-S-m") 'my-add-pipe))

  ;; Add in company-mode helpers
  (defun my-ess-company-hook ()
    ;; ensure company-R-library is in ESS backends
    (make-variable-buffer-local 'company-backends)
    (cl-delete-if (lambda (x) (and (eq (car-safe x) 'company-R-args))) company-backends)
    (add-to-list 'company-backends
                 '(company-R-args company-R-objects company-R-library
                                  company-dabbrev-code :separate)))
  (add-hook 'ess-mode-hook #'my-ess-company-hook)
  ;; (setq ess-use-company t)

  )
