;; org-capture custom declarations
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cc" 'org-capture)
;; Mark todo items with time
(setq org-log-done t)

;; Set default notes file (private)
(setq general-notes (expand-file-name "~/org/defaultNotes.org"))
;; Find all agenda files (doesn't do if new file added to the current emacs session)
(setq org-agenda-files
      '("~/org"
	"~/github/researchNotes2016"))

;; I base my default notes file for my current days research notes
;; Filenames are set to YYYY-mm-dd in case I want to publish on jekyll
;; This function returns the current one
;; First, set the base directory string
(setq notes-dir "~/github/researchNotes")
(defun get-research-notes-file ()
  "Return filename for today's research notes."
  (let ((day-name (format-time-string "%Y-%m-%d"))
	(year-name (format-time-string "%Y")))
    (expand-file-name (concat notes-dir year-name "/" day-name "-notes.org"))))

;; Set default notes file
(setq org-default-notes-file (get-research-notes-file))

(defun research-notes-file ()
  "Create and load a research notes file based on today's date."
  (interactive)
  (find-file (get-research-notes-file)))
(define-key global-map "\C-cfr" 'research-notes-file)

;; Set org-capture templates
(setq org-capture-templates
      (quote (("t" "Tasks")
	      ("tr" "Tasks (research, public on github)" entry (file+headline (get-research-notes-file) "Tasks") "* TODO %?\n%T\n")
	      ("tc" "Tasks (research, public on github, include clipboard)" entry (file+headline (get-research-notes-file) "Tasks") "* TODO %?\n%T\n- %x\n")
	      ("tg" "Tasks (general, local only)" entry (file+headline (general-notes) "Tasks") "* TODO %?\n%T\n")
	      ("n" "Notes")
              ("nr" "Notes (research, public on github)" entry (file+headline (get-research-notes-file) "Notes") "* %? %^G\n%T\n- \n")
	      ("ng" "Notes (general, local only)" entry (file+headline (general-notes) "Notes") "* %? %^G\n%T\n- \n")
	      ("l" "Readings (research, public on github)" entry (file+headline (get-research-notes-file) "Readings") "* %? %^G\n%T\n- \n")
              )))
