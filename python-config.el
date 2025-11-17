;;; python-config.el --- Steve's Python shell config (uv + projectile root) -*- lexical-binding: t; -*-

;; This block assumes you have set the layer variables:
;;   python-shell-interpreter "uv"
;;   python-shell-interpreter-args "run -q ipython -i --simple-prompt"
;; in dotspacemacs-configuration-layers for the python layer.

(with-eval-after-load 'python
  ;; -------------------------------
  ;; Prompt handling (IPython via uv)
  ;; -------------------------------
  ;; Avoid prompt auto-detection warnings/hangs when the interpreter is `uv`.
  (setq python-shell-prompt-detect-enabled nil)

  ;; Tell python.el what IPython prompts look like when using --simple-prompt
  ;; Inputs: "In [1]: " and continuation "...: "
  ;; Outputs: "Out[1]: "
  (dolist (re '("^In \\[[0-9]+\\]: *"
                "^\\s-*\\.\\.\\.: *")) ;; continuation line
    (add-to-list 'python-shell-prompt-input-regexps re))
  (add-to-list 'python-shell-prompt-output-regexps "^Out\\[[0-9]+\\]: *")

  ;; Optionally, prefer interactive-arg route (uncomment if you want this style).
  ;; If you use this, clear python-shell-interpreter-args in layer variables.
  ;; (setq python-shell-interpreter "uv"
  ;;       python-shell-interpreter-args nil
  ;;       python-shell-interpreter-interactive-arg "run -q ipython -i --simple-prompt")

  ;; ---------------------------------------------
  ;; Start IPython via uv in the Projectile project root
  ;; ---------------------------------------------
  (defun sl/run-python-in-projectile-root (&optional arg)
    "Start IPython via `uv run` in the Projectile project root.
With prefix ARG, prompt for the command line."
    (interactive "P")
    (let ((default-directory (or (ignore-errors (projectile-project-root))
                                 default-directory)))
      ;; Optional: if pet-mode is enabled, let it adjust env; harmless with uv.
      (when (bound-and-true-p pet-mode)
        (ignore-errors (pet-activate)))
      (run-python (when arg (python-shell-parse-command)) t t)))

  ;; Make *all* `run-python` calls use the project root (advice)
  (defun sl--run-python-in-project-root-advice (orig &rest args)
    (let ((default-directory (or (ignore-errors (projectile-project-root))
                                 default-directory)))
      (apply orig args)))
  (advice-add 'run-python :around #'sl--run-python-in-project-root-advice)

  ;; ---------------------------------------------
  ;; Spacemacs leader key: SPC m s r
  ;; ---------------------------------------------
  ;; Commented out at the moment, as this conflicts with standard spacemacs python layer
  ;; (when (featurep 'spacemacs)
  ;;   (spacemacs/set-leader-keys-for-major-mode 'python-mode
  ;;     "sr" #'sl/run-python-in-projectile-root))

  ;; ---------------------------------------------
  ;; Optional: per-project shell buffer name
  ;; ---------------------------------------------
  (defun sl/python-shell-name-per-project ()
    "Name Python shell buffer after the current Projectile project."
    (when-let* ((root (ignore-errors (projectile-project-root)))
                (name (file-name-nondirectory (directory-file-name root))))
      (setq-local python-shell-buffer-name (format "*Python: %s*" name))))
  (add-hook 'python-mode-hook #'sl/python-shell-name-per-project))

(provide 'python-config)
;;; python-config.el ends here
