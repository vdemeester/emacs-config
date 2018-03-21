(use-package nix-mode
  :ensure t
  :mode "\\.nix\\'"
  :pin melpa
  :config
  (use-package nix-repl)
  (use-package nix-format)
  (use-package nix-shell
    :commands (nix-shell nix-unpack)))

(use-package company-nixos-options
  :ensure t
  :pin melpa
  :config
  (add-hook 'nix-mode-hook
	    (lambda ()
	      (add-to-list 'company-backends 'company-nixos-options))))

(provide 'vde-nix)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
