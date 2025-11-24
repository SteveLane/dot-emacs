;;; python-config.el --- Project-scoped Python REPL via uv -*- lexical-binding: t; -*-

(with-eval-after-load 'python
  ;; ---------------------------------------------
  ;; Helper: get current Projectile project name
  ;; ---------------------------------------------
  (defun sl/project-name ()
    "Return the current Projectile project name or 'unknown'."
    (let* ((root (ignore-errors (projectile-project-root)))
           (name (and root (file-name-nondirectory (directory-file-name root)))))
      (or name "unknown")))

  ;; ---------------------------------------------
  ;; Make REPL buffer name project-specific
  ;; ---------------------------------------------
  (defun sl/python-shell-buffer-name-per-project ()
    "Set buffer-local `python-shell-buffer-name` to *Python: <project>*."
    (let ((name (sl/project-name)))
      (setq-local python-shell-buffer-name (format "*Python: %s*" name))
      ;; Ensure we don't create per-buffer dedicated shells
      ;; (we want one REPL per project shared across buffers)
      (setq-local python-shell-dedicated nil)))
  (add-hook 'python-mode-hook #'sl/python-shell-buffer-name-per-project)

  ;; Also set the name inside inferior Python buffers, so switching works cleanly.
  (add-hook 'inferior-python-mode-hook
            (lambda ()
              (let ((name (sl/project-name)))
                (setq-local python-shell-buffer-name (format "*Python: %s*" name)))))

  ;; ---------------------------------------------
  ;; Start IPython via `uv` in the project root
  ;; ---------------------------------------------
  (defun sl/run-python-in-projectile-root (&optional arg)
    "Start IPython via `uv run` in the Projectile project root.
With prefix ARG, prompt for the command line."
    (interactive "P")
    ;; Respect the project-scoped shell name before starting
    (sl/python-shell-buffer-name-per-project)
    (let ((default-directory (or (ignore-errors (projectile-project-root))
                                 default-directory)))
      (when (bound-and-true-p pet-mode)
        (ignore-errors (pet-activate)))
      (run-python (when arg (python-shell-parse-command)) t t)))

  ;; Force ALL run-python calls to start in the project root
  (defun sl--run-python-in-project-root-advice (orig &rest args)
    (let ((default-directory (or (ignore-errors (projectile-project-root))
                                 default-directory)))
      ;; Ensure the buffer name is set before the REPL is created
      (sl/python-shell-buffer-name-per-project)
      (apply orig args)))
  (advice-add 'run-python :around #'sl--run-python-in-project-root-advice)

  ;; ---------------------------------------------
  ;; Auto-start REPL before send commands (line/region/buffer/etc.)
  ;; ---------------------------------------------
  (defun sl/ensure-python-repl (&rest _)
    "Ensure a Python REPL is running via uv for the current project."
    ;; Make sure buffer-local name is set so we attach to the project shell
    (sl/python-shell-buffer-name-per-project)
    (let ((proc (ignore-errors (python-shell-get-process))))
      (unless (and proc (process-live-p proc))
        (sl/run-python-in-projectile-root)
        ;; Small delay avoids a race on first send
        (sleep-for 0.05))))
  (dolist (fn '(python-shell-send-string
                python-shell-send-buffer
                python-shell-send-defun
                python-shell-send-region
                python-shell-send-statement
                python-shell-send-file
                python-shell-send-line))
    (when (fboundp fn)
      (advice-add fn :before #'sl/ensure-python-repl)))
  (dolist (fn '(spacemacs/python-send-line
                spacemacs/python-send-region
                spacemacs/python-send-buffer
                spacemacs/python-send-defun
                spacemacs/python-exec
                spacemacs/python-start-or-switch-repl))
    (when (fboundp fn)
      (advice-add fn :before #'sl/ensure-python-repl)))

  ;; ---------------------------------------------
  ;; Prompts: avoid autodetect + define IPython regexps
  ;; ---------------------------------------------
  (setq python-shell-prompt-detect-enabled nil)
  (dolist (re '("^In \\[[0-9]+\\]: *"
                "^\\s-*\\.\\.\\.: *"))
    (add-to-list 'python-shell-prompt-input-regexps re))
  (add-to-list 'python-shell-prompt-output-regexps "^Out\\[[0-9]+\\]: *")

  ;; ---------------------------------------------
  ;; Optional: non-blocking matplotlib backend
  ;; ---------------------------------------------
  (defun sl/python-ensure-mpl-nonblocking ()
    "Use a non-blocking matplotlib backend in comint IPython."
    (when-let ((proc (python-shell-get-process)))
      (comint-send-string proc "import matplotlib; matplotlib.use('Agg')\n")))
  (advice-add 'sl/run-python-in-projectile-root :after #'sl/python-ensure-mpl-nonblocking)

  ;; ---------------------------------------------
  ;; Optional: silence 'No readline support' warning
  ;; ---------------------------------------------
  ;; If you prefer no noise, uncomment the next two lines:
  ;; (defun sl/python-silence-readline-warning ()
  ;;   (when-let ((proc (python-shell-get-process)))
  ;;     (comint-send-string proc "import warnings; warnings.filterwarnings('ignore', message='.*readline.*')\n")))
  ;; (advice-add 'sl/run-python-in-projectile-root :after #'sl/python-silence-readline-warning)
  )
