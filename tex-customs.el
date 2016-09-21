;; Emacs init file for tex customs

;; AucTex and Preview-Latex
(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)
;; use Skim as default pdf viewer
;; Skim's displayline is used for forward search (from .tex to .pdf)
;; option -b highlights the current line; option -g opens Skim in the background  
(setq TeX-view-program-selection '((output-pdf "PDF Viewer")))
(setq TeX-view-program-list
     '(("PDF Viewer" "/Users/slane/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b")))
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)
;; Make autofill work for tex mode
(add-hook 'LaTeX-mode-hook (lambda () (visual-line-mode 1)))
;; Flyspell on
(add-hook 'LaTeX-mode-hook (lambda () (flyspell-mode 1)))

(add-hook 'LaTeX-mode-hook (lambda ()
  (push
    '("latexmk" "latexmk --shell-escape -xelatex -pvc %s" TeX-run-TeX nil t
      :help "Run latexmk on file")
    TeX-command-list)))
(add-hook 'TeX-mode-hook '(lambda () (setq TeX-command-default "latexmk")))

;; Make ispell/flyspell ignore code chunks in Rnw
;; From: http://stackoverflow.com/questions/8287330/exempt-code-chunks-in-an-sweave-document-from-emacs-spell-check
(add-to-list 'ispell-skip-region-alist '("^<<.*>>=" . "^@"))
(defun flyspell-eligible ()
  (let ((p (point)))
    (save-excursion
      (cond ((re-search-backward (ispell-begin-skip-region-regexp) nil t)
             (ispell-skip-region (match-string-no-properties 0))
             (< (point) p))
            (t)))))

(put 'latex-mode 'flyspell-mode-predicate 'flyspell-eligible)
