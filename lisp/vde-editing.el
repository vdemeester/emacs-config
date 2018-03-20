(use-package undo-tree                  ; Show buffer changes as a tree
  :ensure t
  :pin melpa
  :init (global-undo-tree-mode)
  :config (setq undo-tree-visualizer-timestamps t))

(provide 'vde-editing)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
