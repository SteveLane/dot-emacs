;; Time-stamp: <2017-08-21 13:51:17 (slane)>
;; Split out package loading into a separate file.
;; Now using use-package
;; AucTex and Preview-Latex
(use-package tex
  :ensure auctex
  :defer t
  :init
  (setq reftex-plug-into-AUCTeX t)
  (setq TeX-view-program-selection '((output-pdf "PDF Viewer")))
  (setq TeX-view-program-list
	'(("PDF Viewer" "/Users/slane/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b")))
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)
  (add-to-list 'ispell-skip-region-alist '("^<<.*>>=" . "^@"))
  :config
  (use-package preview)
  (add-hook 'TeX-mode-hook '(lambda () (setq TeX-command-default "latexmk")))
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  ;; Make autofill work for tex mode
  (add-hook 'LaTeX-mode-hook (lambda () (visual-line-mode 1)))
  ;; Flyspell on
  (add-hook 'LaTeX-mode-hook (lambda () (flyspell-mode 1)))
  (add-hook 'LaTeX-mode-hook (lambda ()
			       (push
				'("latexmk" "latexmk --shell-escape -xelatex -pvc %s" TeX-run-TeX nil t
				  :help "Run latexmk on file")
				TeX-command-list)))
  (defun flyspell-eligible ()
    (let ((p (point)))
      (save-excursion
	(cond ((re-search-backward (ispell-begin-skip-region-regexp) nil t)
	       (ispell-skip-region (match-string-no-properties 0))
	       (< (point) p))
	      (t)))))
  (put 'latex-mode 'flyspell-mode-predicate 'flyspell-eligible)
  (add-hook 'bibtex-mode-hook 'turn-on-auto-revert-mode)
  )
