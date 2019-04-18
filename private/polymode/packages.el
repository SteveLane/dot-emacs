;;; packages.el --- polymode layer packages file for Spacemacs.
;;
;; Comes from Miles McBain: https://github.com/MilesMcBain/spacemacs_cfg/tree/master/private
;;; Code:

(defconst polymode-packages
  '(polymode
    poly-R
    poly-markdown))

(defun polymode/init-poly-R ())

(defun polymode/init-poly-markdown ())

(defun polymode/init-polymode ()
  (use-package polymode
    :mode (("\\.Rmd"   . Rmd-mode))
    :init
    (progn
      (defun Rmd-mode ()
        "ESS Markdown mode for Rmd files"
        (interactive)
        (require 'poly-R)
        (require 'poly-markdown)
        (R-mode)
        (poly-markdown+r-mode))
      ))
  )

;;; packages.el ends here
