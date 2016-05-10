;; This is a separate emacs customisation file for org-mode customisations
;; Add in org-mode loading of languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)
   (ruby . t)
   (R . t)
   (sh . t)
   (ledger . t)
   (org . t)
   (latex . t)))

;; Don't ask to execute R code:
(defun my-org-confirm-babel-evaluate (lang body)
  (not (string= lang "R")))  ; don't ask for R
(setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)

;; Load org-mode exporters (on first call to exporter per session
(setq org-export-backends
      (
       quote (
	      beamer
	      md
	      odt
	      twbs
	      )))

;; fontify the code blocks in org-mode
(setq org-src-fontify-natively t)

;; This is for nicer css...
;; (setq org-html-head-include-default-style nil)
;; Do not generate internal css formatting for HTML exports
;; (setq org-export-htmlize-output-type 'css)

;; This makes setting up an org file easy.
(define-skeleton org-skeleton
  "Header info for a emacs-org file."
  "Title: "
  "#+TITLE:" str " \n"
  "#+AUTHOR: Stephen E Lane\n"
  "#+email: lane.s@unimelb.edu.au\n"
  "#+INFOJS_OPT: \n"
  "#+BABEL: :session *R* :cache yes :results output graphics :exports both :tangle yes \n"
  "-----"
  )
;; This sets the shortcut
(global-set-key [C-S-f4] 'org-skeleton)

;; Wrap lines at column limit, but don't put hard returns in
(add-hook 'org-mode-hook (lambda () (visual-line-mode 1)))

;; Function to update tags for auto-numbering latex equations.
;; See http://stackoverflow.com/questions/26090651/emacs-org-mode-increment-equation-numbers-with-latex-preview
(defun update-tag ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((count 1))
      (while (re-search-forward "\\tag{\\([0-9]+\\)}" nil t)
        (replace-match (format "%d" count) nil nil nil 1)
        (setq count (1+ count)))))
  )

;; Set up syntax highlighting for latex (use minted)
(setq org-latex-listings 'minted)
(add-to-list 'org-latex-packages-alist '("" "minted") t)
;; To use minted, need to use shell escape
(setq org-latex-pdf-process
      (list "latexmk --shell-escape -pdf %f"))
