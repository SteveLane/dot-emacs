;; Time-stamp: <2020-04-21 10:18:57 (lanes1)>
;; Extra config for ESS that's required as spacemacs has some weird defaults.
(with-eval-after-load 'ess-mode
  (define-key ess-mode-map ";" 'ess-insert-assign)
  (define-key inferior-ess-mode-map ";" 'ess-insert-assign)
  ;; Set ESS up the way you like it
  (setq-default inferior-R-args "--no-restore-history --no-restore --no-save")
  (add-hook 'ess-mode-hook (lambda () (auto-fill-mode 1)))
  (setq ess-ask-for-ess-directory nil)
  (setq inferior-R-program-name "c:/Program Files/R/R-3.5.0/bin/x64/Rterm.exe")
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
  (define-key ess-mode-map (kbd "M-p") 'my-add-pipe)
)
