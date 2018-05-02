(use-package org
  :mode (("\\.org$" . org-mode))
  :ensure org-plus-contrib)

(use-package smartparens-org)

(use-package ox-hugo
  :ensure t
  :after ox
  :commands (org-hugo-slug)
  :bind (:map modi-mode-map
         ("C-c G" . org-hugo-export-wim-to-md)))

(provide 'vde-org)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
