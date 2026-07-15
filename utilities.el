;; General is getting too large
;; Set up some simple utility work here.

(defun sprazza/open-wezterm-project-root ()
  "Open wezterm in the project directory."
  (interactive)
  (let ((default-directory
         (or (projectile-project-root)
             default-directory)))
    (start-process
     "wezterm" nil
     "wezterm" "start" "--cwd" default-directory)))

;; Ensure wezterm is available in the "apps leader"
(spacemacs/set-leader-keys
  "aW" 'sprazza/open-wezterm-project-root
  )
