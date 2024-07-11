;;; spaceline-extra-segments.el --- Additional segments for spaceline all the icons theme -*- lexical-binding: t -*-

;; Author: Steve Lane
;; Maintainer: Steve Lane
;; Version: 0.1
;; Package-Requires: (spaceline, all-the-icons)
;; Homepage: homepage
;; Keywords: keywords


;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; These segments are inspired by both the spacemacs spaceline, and the
;; all-the-icons spaceline.

;;; Code:

(defvar sprazza/org-clock-format-function
  'org-clock-get-clock-string
  "The function called by the `org-clock' segment to determine what to show.")

(spaceline-define-segment sprazza/org-clock
  "Show information about the current org clock task.  Configure
`sprazza/org-clock-format-function' to configure. Requires a currently running
org clock.

This segment overrides the modeline functionality of `org-mode-line-string'."
  (when (and (fboundp 'org-clocking-p)
             (org-clocking-p))
    ;; Throw in here all-the-icons stuff
    (let ((face `(:height ,(spaceline-all-the-icons--height 0.9) :inherit)))
      (propertize
       (concat
        (propertize (all-the-icons-faicon "check-circle" :v-adjust 0.1)
                    'face `(:height ,(spaceline-all-the-icons--height 1.1) :family ,(all-the-icons-faicon-family) :inherit))
        " "
        (propertize (truncate-string-to-width (funcall spaceline-org-clock-format-function) 50 nil nil "…")
                    'face face
                    'display '(raise 0.1)))
       'help-echo "Go to task"
       'mouse-face (spaceline-all-the-icons--highlight)
       'local-map (make-mode-line-mouse-map 'mouse-1 #'org-clock-goto)))
    )
  :global-override org-mode-line-string)

;; Then add it to the segments (this function expects to be called later in config)
(spaceline-all-the-icons-theme 'sprazza/org-clock)

;;; spaceline-extra-segments.el ends here
