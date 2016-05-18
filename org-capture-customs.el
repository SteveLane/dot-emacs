;; org-capture custom declarations
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cc" 'org-capture)
;; Mark todo items with time
(setq org-log-done t)

;; I base my default notes file for my current months research
;; This function returns the current one
;; First, set the base directory string
(setq notes-dir "~/github/researchNotes")
(defun get-research-notes-file ()
  "Return filename for this month's research notes."
  (let ((month-name (format-time-string "%m-%B"))
	(year-name (format-time-string "%Y")))
    (expand-file-name (concat notes-dir year-name "/" month-name ".org"))))

;; Set default notes file
(setq org-default-notes-file (get-research-notes-file))

(defun research-notes-file ()
  "Create and load a research notes file based on today's date."
  (interactive)
  (find-file (get-research-notes-file)))
(define-key global-map "\C-cfr" 'research-notes-file)
