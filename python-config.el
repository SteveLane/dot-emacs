;;; python-config.el --- uv + REPL-first Python setup -*- lexical-binding: t; -*-

;; ---------------------------------------------
;; Tree-sitter (Emacs 29+)
;; ---------------------------------------------
(when (fboundp 'python-ts-mode)
  (add-to-list 'major-mode-remap-alist
               '(python-mode . python-ts-mode)))

;; ---------------------------------------------
;; Eglot + ty via uv
;; ---------------------------------------------
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '((python-base-mode :language-id "python")
                 . ("uv" "run" "--quiet" "ty" "server"))))

(add-hook 'python-base-mode-hook #'eglot-ensure)

;; ---------------------------------------------
;; Detect uv project
;; ---------------------------------------------
(defun sprazza/uv-project-root ()
  "Return project root if pyproject.toml exists, else nil."
  (when-let ((root (ignore-errors (projectile-project-root))))
    (when (file-exists-p (expand-file-name "pyproject.toml" root))
      root)))

(defun sprazza/ensure-uv-project ()
  "Fail if not inside a uv project."
  (unless (sprazza/uv-project-root)
    (user-error "❌ Not inside a uv project (missing pyproject.toml)")))

;; ---------------------------------------------
;; REPL naming
;; ---------------------------------------------
(defun sprazza/python-shell-name ()
  "Consistent project-scoped REPL buffer name."
  (format "*Python: %s*"
          (file-name-nondirectory
           (directory-file-name
            (or (sprazza/uv-project-root) default-directory)))))

;; ---------------------------------------------
;; Start REPL (uv + project root)
;; ---------------------------------------------
(defun sprazza/run-python ()
  "Start uv-managed IPython REPL in project root."
  (interactive)
  (sprazza/ensure-uv-project)
  (let ((default-directory (sprazza/uv-project-root))
        (python-shell-buffer-name (sprazza/python-shell-name))
        (python-shell-dedicated nil))
    (run-python (python-shell-parse-command) nil t)))

;; ---------------------------------------------
;; Ensure REPL exists (core reliability improvement)
;; ---------------------------------------------
(defun sprazza/python-proc ()
  "Return active Python process, starting one if needed."
  (or (python-shell-get-process)
      (progn
        (sprazza/run-python)
        (python-shell-get-process))))

;; ---------------------------------------------
;; Send functions (REPL-first, uv-safe)
;; ---------------------------------------------
(defun sprazza/python-send-line ()
  (interactive)
  (sprazza/ensure-uv-project)
  (sprazza/python-proc)
  (python-shell-send-region
   (line-beginning-position)
   (line-end-position)))

(defun sprazza/python-send-region (start end)
  (interactive "r")
  (sprazza/ensure-uv-project)
  (sprazza/python-proc)
  (python-shell-send-region start end))

(defun sprazza/python-send-buffer ()
  (interactive)
  (sprazza/ensure-uv-project)
  (sprazza/python-proc)
  (python-shell-send-buffer))

(defun sprazza/python-send-defun ()
  (interactive)
  (sprazza/ensure-uv-project)
  (sprazza/python-proc)
  (python-shell-send-defun))

;; ---------------------------------------------
;; Spacemacs keybinding override (safe + explicit)
;; ---------------------------------------------
(with-eval-after-load 'python
  (spacemacs/set-leader-keys-for-major-mode 'python-base-mode
    ;; send group
    "s l" #'sprazza/python-send-line
    "s r" #'sprazza/python-send-region
    "s b" #'sprazza/python-send-buffer
    "s f" #'sprazza/python-send-defun

    ;; REPL control
    "s i" #'sprazza/run-python
    "s n" #'sprazza/run-python))  ;; restart = fresh uv session

;; ---------------------------------------------
;; IPython prompt handling (important for stability)
;; ---------------------------------------------
(setq python-shell-prompt-detect-enabled nil)

(add-to-list 'python-shell-prompt-input-regexps "^In \\[[0-9]+\\]: *")
(add-to-list 'python-shell-prompt-input-regexps "^\\s-*\\.\\.\\.: *")
(add-to-list 'python-shell-prompt-output-regexps "^Out\\[[0-9]+\\]: *")

;; ---------------------------------------------
;; Optional: nicer REPL window behaviour
;; ---------------------------------------------
;; (add-to-list 'display-buffer-alist
;;              '("\\*Python:"
;;                (display-buffer-at-bottom)
;;                (window-height . 0.3)))
