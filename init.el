;; .emacs file that has been adapted from my linux one. I think there's a whole heap of stuff that I don't need...

;; Make sure if I double click a file, it is opened instead of the scratch buffer
(setq inhibit-startup-screen t)
(setq initial-buffer-choice nil)

;; Set column fill to 80
(setq-default fill-column 80)

;; Change option and meta keys around.
(when (eq system-type 'darwin)
  ;; (set-default-font "-*-Hack-normal-normal-normal-*-18-*-*-*-m-0-iso10646-1")
  (setq maq-option-key-is-meta nil
	mac-command-key-is-meta t
	mac-command-modifier 'meta
	mac-option-modifier 'none))
;; Use this next when on the large screen - need to figure out how to identify it
;; -*-Hack-normal-normal-normal-*-18-*-*-*-m-0-iso10646-1

;; This gets the fonts right depending on whether I'm on the external screen or the retina...
;; Gist-ed from in https://github.com/arnab/emacs-starter-kit
(defun fontify-frame (frame)
  (interactive)
  (if window-system
      (progn
        (if (> (x-display-pixel-width) 2000)
            (set-frame-parameter frame 'font "-*-Hack-normal-normal-normal-*-18-*-*-*-m-0-iso10646-1") ;; Cinema Display
         (set-frame-parameter frame 'font "-*-Hack-normal-normal-normal-*-12-*-*-*-m-0-iso10646-1")))))

;; Fontify current frame
(fontify-frame nil)

;; Fontify any future frames
(push 'fontify-frame after-make-frame-functions)

;; Set the default directory
(setq default-directory "~/Documents")

;; Turn off the toolbar
(tool-bar-mode -1)

;; Load packages from elpa
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(when (not package-archive-contents) (package-refresh-contents))
(let ((default-directory "~/.emacs.d/elpa/"))
    (normal-top-level-add-to-load-path '("."))
    (normal-top-level-add-subdirs-to-load-path))
;; Comment out whilst I still need to install them...
;; ;; Include stan highlighting
;; (require 'stan-mode)
;; ;; Make sure bugs mode is loaded
;; (require 'ess-bugs-d)
;; ;; Require php-mode
;; (require 'php-mode)
;; Load ess
(require 'ess-site)

;; Solarized
(load-theme 'solarized-dark t)

;; Make sure that ediff doesn't start in windowed mode
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(add-hook 'bibtex-mode-hook 'turn-on-auto-revert-mode) ; Automatically detects changes to bibtex.

;; R details
(setq-default inferior-R-args "--no-restore-history --no-restore --no-save")
(add-hook 'ess-mode-hook (lambda () (auto-fill-mode 1))) ; Make autofill work for R files.
;; R details, from zmjones.com/mac-setup
;; Don't want it to ask where to create the directory; do it where I currently am.
(setq ess-ask-for-ess-directory nil)
(setq inferior-R-program-name "/usr/local/bin/R") 
(setq ess-local-process-name "R")

;; Macro section
;; This macro inserts section comments as a header
(fset 'header
   [?\C-u ?8 ?0 ?# return ?\C-u ?8 ?0 ?# return ?\M-\; ?B ?e ?g ?i ?n ?  ?S ?e ?c ?t ?i ?o ?n ?: ?  return ?\C-u ?8 ?0 ?# return ?\C-u ?8 ?0 ?# return up up left])

;; This macro inserts a section footer
(fset 'footer
   [?\C-u ?8 ?0 ?# return ?\C-u ?8 ?0 ?# return ])

;; This macro inserts a title comment
(fset 'title
   [?\C-u ?8 ?0 ?# return ?\C-u ?8 ?0 ?# return ?\M-\; ?T ?i ?t ?l ?e ?: return ?\M-\; ?A ?u ?t ?h ?o ?r ?: return ?\M-\; ?D ?a ?t ?e ?: ?  ?\C-u ?\M-\! ?d ?a ?t ?e ?  ?\" ?\+ ?\% ?A ?, ?  ?\% ?d ?  ?\% ?B ?  ?\% ?Y ?\" return backspace ?  ?\C-e return ?\M-\; ?S ?y ?n ?o ?p ?s ?i ?s ?: return ?\C-u ?8 ?0 ?# return ?\C-u ?8 ?0 ?# return up up up up up ?\C-e ?  ?S ?t ?e ?v ?e ?  ?L ?a ?n ?e up ? ])

;; This macro, adds an 'edited' comment
(fset 'edited
   [?# ?# ?  ?E ?d ?i ?t ?e ?d ?: ?  ?\C-u ?\M-! ?d ?a ?t ?e ?  ?\" ?+ ?% ?A ?, ?  ?% ?B ?  ?% ?d ?  ?% ?Y ?\" return ?\C-e ?. delete ? ])

;; This macro inserts a title comment for matlab (which is now wrong!
(fset 'title-matlab
   [?\C-u ?8 ?0 ?% return ?\C-u ?8 ?0 ?% return ?% ?  ?T ?i ?t ?l ?e ?: return ?% ?  ?A ?u ?t ?h ?o ?r ?: return ?% ?  ?D ?a ?t ?e ?: ?  ?\C-u ?\M-\! ?d ?a ?t ?e ?  ?\" ?\+ ?\% ?d ?  ?\% ?B ?  ?\% ?Y ?\" ?\/ ?t return backspace ?  ?\C-e return ?% ?  ?S ?y ?n ?o ?p ?s ?i ?s ?: return ?\C-u ?8 ?0 ?% return ?\C-u ?8 ?0 ?% return up up up up up ?\C-e ?  ?S ?t ?e ?v ?e ?  ?L ?a ?n ?e up ? ])



;; Default dictionary
(setq default-dictionary "british")
(setq ispell-dictionary "british")

;; Markdown mode
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.Rmd\\'" . markdown-mode))
;; Wrap lines at column limit, but don't put hard returns in
(add-hook 'markdown-mode-hook (lambda () (visual-line-mode 1)))
;; Flyspell on
(add-hook 'markdown-mode-hook (lambda () (flyspell-mode 1)))
;; Make sure markdown-mode uses pandoc for previewing
;; Use this one if you have a css file
;; (setq markdown-command "pandoc -c file:///home/beaujean/.emacs.d/github-pandoc.css --from markdown_github -t html5 --mathjax --highlight-style pygments --standalone")
;; Without a css file:
(setq markdown-command "pandoc --from markdown_github -t html5 --mathjax --highlight-style pygments --standalone")

;; Column number mode
(setq column-number-mode t)

;; ;; R indentation from (http://stackoverflow.com/questions/12805873/changing-indentation-in-emacs-ess)
;; ;;; ESS
;; (add-hook 'ess-mode-hook
;;           (lambda ()
;;             (ess-set-style 'C++ 'quiet)
;;             ;; Because
;;             ;;                                 DEF GNU BSD K&R C++
;;             ;; ess-indent-level                  2   2   8   5   4
;;             ;; ess-continued-statement-offset    2   2   8   5   4
;;             ;; ess-brace-offset                  0   0  -8  -5  -4
;;             ;; ess-arg-function-offset           2   4   0   0   0
;;             ;; ess-expression-offset             4   2   8   5   4
;;             ;; ess-else-offset                   0   0   0   0   0
;;             ;; ess-close-brace-offset            0   0   0   0   0
;; 	    (setq ess-first-continued-statement-offset 4)
;;             (setq ess-continued-statement-offset 0)
;;             (add-hook 'local-write-file-hooks
;;                       (lambda ()
;;                         (ess-nuke-trailing-whitespace)))))
;; ;; (setq ess-nuke-trailing-whitespace-p 'ask)
;; ;; or even

(setq ess-default-style 'RRR)
(setq ess-nuke-trailing-whitespace t)

;; Show parentheses matching
(show-paren-mode t)

;;; Perl
(add-hook 'perl-mode-hook
          (lambda () (setq perl-indent-level 4)))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

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
(add-hook 'LaTeX-mode-hook (lambda () (auto-fill-mode 1)))
;; Flyspell on
(add-hook 'LaTeX-mode-hook (lambda () (flyspell-mode 1)))

;; make latexmk available via C-c C-c
;; The -c tells latexmk to clean up regeneratble files (log and aux files)
;; (add-hook 'LaTeX-mode-hook (lambda ()
;;   (push
;;     '("latexmk" "latexmk -c -interaction=nonstopmode -file-line-error -synctex=1 -pdf %s" TeX-run-TeX nil t
;;       :help "Run latexmk on file")
;;     TeX-command-list)))
;; (add-hook 'TeX-mode-hook '(lambda () (setq TeX-command-default "latexmk")))

(add-hook 'LaTeX-mode-hook (lambda ()
  (push
    '("latexmk" "latexmk -pvc %s" TeX-run-TeX nil t
      :help "Run latexmk on file")
    TeX-command-list)))
(add-hook 'TeX-mode-hook '(lambda () (setq TeX-command-default "latexmk")))

;; Add extra reftex commands for pandoc/knitcitations
;; (setq reftex-cite-format
;;       '(
;; 	(?P . "[@%l]")
;; 	(?T . "@%l [p. ]")
;; 	))

;; Do this in a function so that it can be only loaded in markdown
(defun markdown-mode-reftex-setup ()
  (interactive)
  (and (buffer-file-name) (file-exists-p (buffer-file-name))
       (progn
	 ;; Reftex should use the org file as master file. See C-h v TeX-master for infos.
	 (setq TeX-master t)
	 (turn-on-reftex)
	 (setq reftex-cite-format
	       '(
		 ;; These next two were for using knitcitations
		 ;; (?p . "`r knitcitations::citep(bib[['%l']])`")
		 ;; (?t . "`r knitcitations::citet(bib[['%l']])`")
		 ;; These use pandoc and work fine using bookdown/render
		 (?t . "@%l")
		 (?p . "[@%l]")
		 )))))

(add-hook 'markdown-mode-hook 'markdown-mode-reftex-setup)

;; I'm sick of backups... at least in my working directory. Let's place them somewhere else (plus some other cool stuff:
(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.saves"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)       ; use versioned backups

;; Magit status bind global key
(global-set-key (kbd "C-x g") 'magit-status)

;; Load separated customisation files.
(load "~/.emacs.d/org-customs.el")
