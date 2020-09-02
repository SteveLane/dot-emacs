(setq
 ;; Define custom org-roam capture templates
 org-roam-capture-templates
 '(
   ("p" "person" plain (function org-roam--capture-get-point)
    "%?"
    :file-name "%<%Y%m%d%H%M%S>-${slug}"
    :head "#+title: ${title}
#+roam_tags: person\n"
    :unnarrowed t)
   ("c" "code" plain (function org-roam--capture-get-point)
    "%?"
    :file-name "%<%Y%m%d%H%M%S>-${slug}"
    :head "#+title: ${title}
#+roam_tags: code\n"
    :unnarrowed t)
   ("P" "process" plain (function org-roam--capture-get-point)
    "%?"
    :file-name "%<%Y%m%d%H%M%S>-${slug}"
    :head "#+title: ${title}
#+roam_tags: process\n"
    :unnarrowed t)
   )
 )
