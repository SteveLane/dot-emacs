;;; packages.el --- stan mode layer packages file for Spacemacs.
;;
;; Author: Steve Lane
;;
;;; Code:

(defconst stan-mode-packages
  '(stan-mode
    stan-snippets))

(defun stan-mode/init-stan-mode ()
  (use-package stan-mode :defer t))


(defun stan-mode/init-stan-snippets ()
  (use-package stan-snippets :defer t))
