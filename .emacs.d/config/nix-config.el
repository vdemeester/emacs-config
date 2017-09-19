
(use-package nix-mode
  :ensure t)

(use-package direnv
  :ensure t
  :init
  (setq direnv-always-show-summary t)
  (direnv-mode t))

(provide 'nix-config)
