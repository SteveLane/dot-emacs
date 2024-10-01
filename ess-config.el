;; Time-stamp: <2024-10-01 10:58:49 (lanes1)>
;; Extra config for ESS that's required as spacemacs has some weird defaults.
(with-eval-after-load 'ess-mode
  (define-key ess-mode-map ";" 'ess-insert-assign)
  (define-key inferior-ess-mode-map ";" 'ess-insert-assign)
  ;; Set ESS up the way you like it
  (setq-default inferior-R-args "--no-restore-history --no-restore --no-save")
  (add-hook 'ess-mode-hook (lambda () (auto-fill-mode 1)))
  (when (spacemacs/system-is-mswindows)
    (setq inferior-R-program-name "c:/Program Files/R/R-4.4.1/bin/x64/R.exe"))
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
  ;; Make some nicer settings
  (setq
   ess-ask-for-ess-directory t
   ess-local-process-name "R"
   ess-nuke-trailing-whitespace t
   ess-eval-visibly 'nowait
   ;; Remove old _ mapping
   ess-smart-S-assign-key nil
   ;; Turn off flymake (will use flycheck)
   ess-use-flymake nil
   )

  ;; Function to add the pipe operator (set in map above)
  (defun my-add-pipe ()
    "Adds a pipe operator %>% with one space to the left and then starts a newline with proper indentation"
    (interactive)
    (just-one-space 1)
    (insert "|>")
    (reindent-then-newline-and-indent)
    )
  (when (spacemacs/system-is-mac)
    (define-key ess-mode-map (kbd "M-S-m") 'my-add-pipe)
    (define-key inferior-ess-mode-map (kbd "M-S-m") " |> ")
    )
  (when (spacemacs/system-is-mswindows)
    (define-key ess-mode-map (kbd "C-S-m") 'my-add-pipe)
    (define-key inferior-ess-mode-map (kbd "C-S-m") " |> ")
    )

  )

;; Make sure quarto-mode is loaded
(require 'quarto-mode)
