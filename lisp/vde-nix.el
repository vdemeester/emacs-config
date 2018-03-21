(use-package nix-mode
  :ensure t
  :mode "\\.nix\\'"
  :pin melpa
  :config
  (use-package nix-repl)
  (use-package nix-format)
  (use-package nix-shell
    :commands (nix-shell nix-unpack)))

(use-package nixos-options
  :ensure t
  :pin melpa)
(use-package company-nixos-options
  :ensure t
  :pin melpa
  :hook
  (nix-mode . (lambda ()
                (set (make-local-variable 'company-backends) '(company-nixos-options))
                  (company-mode))))

(provide 'vde-nix)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
