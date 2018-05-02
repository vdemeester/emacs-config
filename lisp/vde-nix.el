(use-package nix-mode
  :mode "\\.nix\\'"
  :config
  (use-package nix-repl)
  (use-package nix-format)
  (use-package nix-shell
    :commands (nix-shell nix-unpack)))

(use-package nixos-options)

(use-package company-nixos-options
  :hook
  (nix-mode . (lambda ()
                (set (make-local-variable 'company-backends) '(company-nixos-options))
                  (company-mode))))

(provide 'vde-nix)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
