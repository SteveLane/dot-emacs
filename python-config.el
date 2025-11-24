;;; python-config.el --- Project-scoped Python REPL (uv) + robust send -*- lexical-binding: t; -*-

(with-eval-after-load 'python
  ;; ---------------------------------------------
  ;; Helper: project name & shell buffer name
  ;; ---------------------------------------------
  (defun sl/project-name ()
    "Return current Projectile project name (or 'unknown')."
    (let* ((root (ignore-errors (projectile-project-root)))
           (name (and root (file-name-nondirectory (directory-file-name root)))))
      (or name "unknown")))

  (defun sl/project-shell-name ()
    "Return the project-scoped Python shell buffer name."
    (format "*Python: %s*" (sl/project-name)))

  ;; Ensure buffers default to project shell (shared across buffers)
  (defun sl/python-shell-buffer-config ()
    "Set buffer-local shell vars for a shared per-project REPL."
    (setq-local python-shell-buffer-name (sl/project-shell-name))
    (setq-local python-shell-dedicated nil)) ;; never create per-buffer shells

  (add-hook 'python-mode-hook #'sl/python-shell-buffer-config)
  (add-hook 'inferior-python-mode-hook #'sl/python-shell-buffer-config)

  ;; ---------------------------------------------
  ;; Force run-python to use project root & name
  ;; ---------------------------------------------
  (defun sl/run-python-in-projectile-root (&optional arg)
    "Start IPython via `uv run` in the Projectile project root.
With prefix ARG, prompt for the command line."
    (interactive "P")
    ;; set shell name in the current buffer before creating the REPL
    (sl/python-shell-buffer-config)
    (let ((default-directory (or (ignore-errors (projectile-project-root))
                                 default-directory)))
      (when (bound-and-true-p pet-mode)
        (ignore-errors (pet-activate)))
      (run-python (when arg (python-shell-parse-command)) t t)))

  ;; Advice: every run-python call starts in project root and uses our name
  (defun sl--run-python-in-project-root-advice (orig &rest args)
    (let ((default-directory (or (ignore-errors (projectile-project-root))
                                 default-directory)))
      (sl/python-shell-buffer-config)
      (apply orig args)))
  (advice-add 'run-python :around #'sl--run-python-in-project-root-advice)

  ;; ---------------------------------------------
  ;; VERY IMPORTANT: ensure project shell vars during send
  ;; ---------------------------------------------
  (defun sl/with-project-shell (orig &rest args)
    "Around advice to force project shell name + non-dedicated during send."
    (let ((python-shell-buffer-name (sl/project-shell-name))
          (python-shell-dedicated nil))
      (apply orig args)))

  ;; Wrap core python.el send functions
  (dolist (fn '(python-shell-send-string
                python-shell-send-buffer
                python-shell-send-defun
                python-shell-send-region
                python-shell-send-statement
                python-shell-send-file
                python-shell-send-line))
    (when (fboundp fn)
      (advice-add fn :around #'sl/with-project-shell)))

  ;; Wrap Spacemacs layer helpers (if present)
  (dolist (fn '(spacemacs/python-send-line
                spacemacs/python-send-region
                spacemacs/python-send-buffer
                spacemacs/python-send-defun
                spacemacs/python-exec
                spacemacs/python-start-or-switch-repl))
    (when (fboundp fn)
      (advice-add fn :around #'sl/with-project-shell)))

  ;; ---------------------------------------------
  ;; Auto-start REPL before sending
  ;; ---------------------------------------------
  (defun sl/ensure-python-repl (&rest _)
    "Ensure a Python REPL is running via uv for the current project."
    ;; also set shell config in case the current buffer missed the hook
    (sl/python-shell-buffer-config)
    (let ((proc (ignore-errors (python-shell-get-process))))
      (unless (and proc (process-live-p proc))
        (sl/run-python-in-projectile-root)
        (sleep-for 0.05)))) ;; small delay to avoid first-send race

  ;; Ensure before sends (python.el + Spacemacs)
  (dolist (fn '(python-shell-send-string
                python-shell-send-buffer
                python-shell-send-defun
                python-shell-send-region
                python-shell-send-statement
                python-shell-send-file
                python-shell-send-line
                spacemacs/python-send-line
                spacemacs/python-send-region
                spacemacs/python-send-buffer
                spacemacs/python-send-defun
                spacemacs/python-exec
                spacemacs/python-start-or-switch-repl))
    (when (fboundp fn)
      (advice-add fn :before #'sl/ensure-python-repl)))

  ;; ---------------------------------------------
  ;; Prompt handling: disable auto-detect & define IPython regexps
  ;; ---------------------------------------------
  (setq python-shell-prompt-detect-enabled nil)
  (dolist (re '("^In \\[[0-9]+\\]: *" "^\\s-*\\.\\.\\.: *"))
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
  ;; (defun sl/python-silence-readline-warning ()
  ;;   (when-let ((proc (python-shell-get-process)))
  ;;     (comint-send-string proc
  ;;       "import warnings; warnings.filterwarnings('ignore', message='.*readline.*')\n")))
  ;; (advice-add 'sl/run-python-in-projectile-root :after #'sl/python-silence-readline-warning)
  )
