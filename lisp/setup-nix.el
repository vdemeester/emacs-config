;;; setup-nix.el --- setup nix modes
;;; Commentary:
;;; Code:
;;; -*- lexical-binding: t; -*-

(use-package nix-mode
  :mode ("\\.nix\\'" "\\.nix.in\\'"))

(use-package nix-drv-mode
  :ensure nix-mode
  :mode "\\.drv\\'")
(use-package nix-shell
  :ensure nix-mode
  :commands (nix-shell-unpack nix-shell-configure nix-shell-build))
(use-package nix-repl
  :ensure nix-mode
  :commands (nix-repl))

(use-package nixos-options)

(use-package nix-update
  :ensure nix-mode)

(provide 'setup-nix)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
