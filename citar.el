(use-package citar
  :custom
  (citar-bibliography '("~/Development/references.bib"))
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (org-cite-global-bibliography citar-bibliography)
  :hook
  (LaTeX-mode . citar-capf-setup)
  (org-mode . citar-capf-setup)
  (markdown-mode . citar-capf-setup)
  )

(use-package citar-embark
  :after citar embark
  :no-require
  :config (citar-embark-mode)
  )

(use-package citar-org-roam
  :after (citar org-roam)
  :config (citar-org-roam-mode)
  )
