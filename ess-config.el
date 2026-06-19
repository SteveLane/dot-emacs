;; Time-stamp: <2026-06-19 16:27:29 (lanes1)>
;; Extra config for ESS that's required as spacemacs has some weird defaults.

(defun sprazza/eglot-start-if-available ()
  ;; Now make sure that eglot is started if available
  (when (fboundp 'eglot-ensure)
    (eglot-ensure)))

;; And now ESS specific settings
(with-eval-after-load 'ess

  ;; Remove any ESS-injected arguments
  (setq ess-R-args nil)

  ;; Where is my R? I haven't set it properly on the PATH with winblows...
  ;; OS‑specific R binary
  (cond
   ((spacemacs/system-is-mswindows)
    (setq inferior-ess-r-program "R.bat"))
   ((spacemacs/system-is-linux)
    (setq inferior-ess-r-program "/usr/bin/R"))
   ((spacemacs/system-is-mac)
    (setq inferior-ess-r-program "/usr/local/bin/R")))
  ;; R startup args
  (setq inferior-R-args "--no-restore-history --no-restore --no-save")

  ;; Keybindings
  (define-key ess-mode-map ";" 'ess-insert-assign)
  (define-key inferior-ess-mode-map ";" 'ess-insert-assign)

  ;; Auto-fill in ESS buffers
  (add-hook 'ess-mode-hook (lambda () (auto-fill-mode 1)))

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
        ess-use-flymake nil
        ess-use-eldoc nil
        )

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
    (define-key inferior-ess-mode-map (kbd "C-S-m") " |> ")))

  (add-hook 'ess-r-mode-hook #'sprazza/eglot-start-if-available)
  )

;; Ensure no spell-check in code blocks
(with-eval-after-load 'markdown-mode
  (add-hook 'markdown-mode-hook
            (lambda ()
              (setq-local flyspell-generic-check-word-predicate
                          #'markdown-flyspell-check-word-p))))

(with-eval-after-load 'quarto-mode
  ;; Spelling
  (add-hook 'quarto-mode-hook
            (lambda ()
              (setq-local flyspell-generic-check-word-predicate
                          #'markdown-flyspell-check-word-p)))
  (add-hook 'quarto-mode-hook #'sprazza/eglot-start-if-available)
  )

;; Ensure that the language server is started
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(ess-r-mode . ("R.bat" "--slave" "-e" "languageserver::run()"))))
