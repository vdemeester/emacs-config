(use-package rust-mode
  :ensure t
  :mode "\\.rs\\'"
  :init
  (setq rust-format-on-save t))

(use-package lsp-rust
  :pin melpa
  :ensure t
  :after lsp-mode)

(provide 'vde-rust)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
