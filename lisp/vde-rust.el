(use-package rust-mode
  :mode "\\.rs\\'"
  :init
  (setq rust-format-on-save t))

(use-package lsp-rust
  :defer 2
  :after lsp-mode)

(provide 'vde-rust)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
