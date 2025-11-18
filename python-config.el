;;; python-config.el --- Ensure REPL exists before all send commands -*- lexical-binding: t; -*-

(with-eval-after-load 'python
  ;; ---------------------------------------------
  ;; Prompts: avoid autodetect + define IPython regexps
  ;; ---------------------------------------------
  (setq python-shell-prompt-detect-enabled nil)
  (dolist (re '("^In \\[[0-9]+\\]: *"
                "^\\s-*\\.\\.\\.: *"))
    (add-to-list 'python-shell-prompt-input-regexps re))
  (add-to-list 'python-shell-prompt-output-regexps "^Out\\[[0-9]+\\]: *")

  ;; ---------------------------------------------
  ;; Start IPython via `uv run` in the project root
  ;; ---------------------------------------------
  (defun sl/run-python-in-projectile-root (&optional arg)
    "Start IPython via `uv run` in the Projectile project root.
With prefix ARG, prompt for the command line."
    (interactive "P")
    (let ((default-directory (or (ignore-errors (projectile-project-root))
                                 default-directory)))
      (when (bound-and-true-p pet-mode)
        (ignore-errors (pet-activate)))
      (run-python (when arg (python-shell-parse-command)) t t)))

  ;; Make ALL run-python calls use the project root
  (defun sl--run-python-in-project-root-advice (orig &rest args)
    (let ((default-directory (or (ignore-errors (projectile-project-root))
                                 default-directory)))
      (apply orig args)))
  (advice-add 'run-python :around #'sl--run-python-in-project-root-advice)

  ;; ---------------------------------------------
  ;; Ensure a REPL exists before *any* send command
  ;; ---------------------------------------------
  (defun sl/ensure-python-repl (&rest _)
    "Ensure a Python REPL is running via uv before sending."
    (let ((proc (ignore-errors (python-shell-get-process))))
      (unless (and proc (process-live-p proc))
        (sl/run-python-in-projectile-root)
        ;; Tiny pause to avoid race conditions on first send.
        (sleep-for 0.05))))

  ;; Advise core python.el send functions
  (dolist (fn '(python-shell-send-string
                python-shell-send-buffer
                python-shell-send-defun
                python-shell-send-region
                python-shell-send-statement
                python-shell-send-file
                python-shell-send-line))        ;; present in newer python.el
    (when (fboundp fn)
      (advice-add fn :before #'sl/ensure-python-repl)))

  ;; Also advise Spacemacs layer helpers if they exist
  (dolist (fn '(spacemacs/python-send-line
                spacemacs/python-send-region
                spacemacs/python-send-buffer
                spacemacs/python-send-defun
                spacemacs/python-exec
                spacemacs/python-start-or-switch-repl))
    (when (fboundp fn)
      (advice-add fn :before #'sl/ensure-python-repl)))

  ;; ---------------------------------------------
  ;; Optional: non-blocking matplotlib backend
  ;; ---------------------------------------------
  (defun sl/python-ensure-mpl-nonblocking ()
    "Use a non-blocking matplotlib backend in comint IPython."
    (when-let ((proc (python-shell-get-process)))
      (comint-send-string proc "import matplotlib; matplotlib.use('Agg')\n")))
  (advice-add 'sl/run-python-in-projectile-root :after #'sl/python-ensure-mpl-nonblocking)

  ;; ---------------------------------------------
  ;; Optional: per-project shell buffer name
  ;; ---------------------------------------------
  (defun sl/python-shell-name-per-project ()
    "Name Python shell buffer after the current Projectile project."
    (when-let* ((root (ignore-errors (projectile-project-root)))
                (name (file-name-nondirectory (directory-file-name root))))
      (setq-local python-shell-buffer-name (format "*Python: %s*" name))))
  (add-hook 'python-mode-hook #'sl/python-shell-name-per-project)
  )
