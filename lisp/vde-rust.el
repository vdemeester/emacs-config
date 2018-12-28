;;; -*- lexical-binding: t; -*-
(use-package rust-mode
  :mode "\\.rs\\'"
  :init
  (setq rust-format-on-save t))

(use-package racer
  :after rust-mode
  :init
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'rust-mode-hook #'eldoc-mode)
  :config
  (setq racer-cmd (executable-find "racer")))

(use-package company-racer  
  :after company
  :config
  (push 'company-racer company-backends))

(use-package flycheck-rust
  :init
  (add-hook 'rust-mode-hook #'flycheck-rust-setup))

;; (use-package lsp-rust
;;   :defer 2
;;   :after lsp-mode
;;   :hook ((rust-mode . lsp-rust-enable)
;;          (rust-mode . flycheck-mode)))

(provide 'vde-rust)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
