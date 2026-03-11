;; Time-stamp: <2026-03-11 17:18:45 (sprazza)>
;; Extra config for ESS that's required as spacemacs has some weird defaults.

(with-eval-after-load 'ess-mode

  ;; Keybindings
  (define-key ess-mode-map ";" 'ess-insert-assign)
  (define-key inferior-ess-mode-map ";" 'ess-insert-assign)

  ;; R startup args
  (setq-default inferior-R-args "--no-restore-history --no-restore --no-save")

  ;; Auto-fill in ESS buffers
  (add-hook 'ess-mode-hook (lambda () (auto-fill-mode 1)))

  ;; OS‑specific R binary
  (cond
   ((spacemacs/system-is-mswindows)
    (setq inferior-R-program-name "c:/Program Files/R/R-4.5.2/bin/x64/R.exe"))
   ((spacemacs/system-is-linux)
    (setq inferior-R-program-name "/usr/bin/R"))
   ((spacemacs/system-is-mac)
    (setq inferior-R-program-name "/usr/local/bin/R")))

  ;; RStudio‑like indentation
  (add-hook 'ess-mode-hook
            (lambda ()
              (setq ess-first-continued-statement-offset 'straight
                    ess-continued-statement-offset 'straight
                    ess-default-style 'RStudio-
                    ess-indent-offset 4
                    ess-offset-arguments 'prev-line
                    ess-align-blocks nil
                    ess-indent-with-fancy-comments nil)))

  ;; General ESS behaviour
  (setq ess-ask-for-ess-directory t
        ess-local-process-name "R"
        ess-nuke-trailing-whitespace t
        ess-eval-visibly 'nowait
        ess-smart-S-assign-key nil   ;; disable old _ assignment
        ess-use-flymake nil)         ;; use flycheck instead

  ;; Pipe operator helper
  (defun my-add-pipe ()
    "Insert |> with spacing and newline."
    (interactive)
    (just-one-space 1)
    (insert "|>")
    (reindent-then-newline-and-indent))

  ;; OS‑specific pipe keybindings
  (cond
   ((spacemacs/system-is-mac)
    (define-key ess-mode-map (kbd "M-S-m") 'my-add-pipe)
    (define-key inferior-ess-mode-map (kbd "M-S-m") " |> "))
   ((spacemacs/system-is-mswindows)
    (define-key ess-mode-map (kbd "C-S-m") 'my-add-pipe)
    (define-key inferior-ess-mode-map (kbd "C-S-m") " |> "))))

;; Skip code chunks when spell-checking in quarto/markdown
(defun my/quarto-ispell-skip-code ()
  "Skip fenced code blocks (everything between triple backticks) when spell-checking in Quarto/Markdown."
  (setq-local ispell-skip-region-alist
              (append '(("```" . "```"))
                      ispell-skip-region-alist))
  )

(with-eval-after-load 'quarto-mode
  (add-hook 'quarto-mode-hook #'my/quarto-ispell-skip-code))
(with-eval-after-load 'markdown-mode
  (add-hook 'markdown-mode-hook #'my/quarto-ispell-skip-code))
