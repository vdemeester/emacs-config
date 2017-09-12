
(defun vde:config-evil ()
  "Configure evil mode the way I want"
  (vde:evil:configure-emacs-mode)
  (vde:evil:configure-bepo))

(defun vde:evil:configure-emacs-mode ()
  "configure evil emacs mode"
  (dolist (mode '(custom-mode
                  dired-mode
                  eshell-mode
                  term-mode
                  grep-mode))
    (add-to-list 'evil-emacs-state-modes mode)))

(defun vde:evil:configure-bepo ()
  "Remap default bindings to bépo one *if* the computer is
  running on a bépo keyboard layout.")

(use-package evil
  :ensure t
  :config
  (add-hook 'evil-mode-hook 'vde:config-evil)
  (evil-mode 1)
  (use-package evil-jumper
    :ensure t
    :config
    (global-evil-jumper-mode))
  (use-package evil-surround
    :ensure t
    :config
    (global-evil-surround-mode))
  (use-package evil-indent-textobject
    :ensure t))

(provide 'navigation-config)
