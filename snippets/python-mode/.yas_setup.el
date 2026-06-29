;;; -*- lexical-binding: t -*-

                                        ; Copyright (C) miscellaneous contributors, see git history
                                        ; Copyright (C) 2024 Daniel Hornung <d.hornung@indiscale.com>
                                        ;
                                        ; This program is free software: you can redistribute it and/or modify
                                        ; it under the terms of the GNU General Public License as
                                        ; published by the Free Software Foundation, either version 3 of the
                                        ; License, or (at your option) any later version.
                                        ;
                                        ; This program is distributed in the hope that it will be useful,
                                        ; but WITHOUT ANY WARRANTY; without even the implied warranty of
                                        ; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
                                        ; GNU General Public License for more details.
                                        ;
                                        ; You should have received a copy of the GNU General Public License
                                        ; along with this program. If not, see <https://www.gnu.org/licenses/>.

(require 'yasnippet)
(defvar yas-text)

(defun sprazza/yas-args-to-google ()
  "Convert yas TEXT (argument string) into Google-style Args block."
  (let* ((args (split-string yas-text "," t "[[:space:]]*"))
         (args (seq-remove
                (lambda (x)
                  (or (string-match-p "^\\s-*self\\s-*$" x)
                      (string-match-p "^\\s-*\\*$" x)))
                args)))
    (mapconcat
     (lambda (arg)
       (let ((name (car (split-string arg ":" t "[[:space:]]*"))))
         (format "%s: " (string-trim name))))
     args
     "\n        ")))
