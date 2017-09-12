
(use-package dashboard
  :ensure t
  :config
  (setq dashboard-banner-logo-title "Welcome to Emacs, Vincent !"
        dashboard-startup-banner (expand-file-name "images/okumura_rin_4_by_naruto_lover16-d4ktg50.png" user-emacs-directory))
  (dashboard-setup-startup-hook))

(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bolt t)
  (setq doom-themes-enable-italic t)
  (load-theme 'doom-one t))

(use-package solaire-mode
  :ensure t
  :config
  (setq solaire-mode-remap-modeline nil)
  (add-hook 'after-change-major-mode-hook #'turn-on-solaire-mode)
  (add-hook 'after-revert-hook #'turn-on-solaire-mode)
  (add-hook 'minibuffer-setup-hook #'solaire-mode-in-minibuffer)
  (add-hook 'ediff-prepare-buffer-hook #'solaire-mode)
  (advice-add #'persp-load-state-from-file :after #'solaire-mode-restore-persp-mode-buffers))

(line-number-mode 1)
(column-number-mode 1)
(global-hl-line-mode 1)

(setq font-lock-maximum-decoration 2)

(setq-default indicate-buffer-boundaries 'left)
(setq-default indicate-empty-lines +1)

(defun vde/byte-recompile ()
  (interactive)
  (byte-recompile-directory user-emacs-directory 0)
  (byte-recompile-directory (expand-file-name "lisp" user-emacs-directory) 0)
  (byte-recompile-directory (expand-file-name "config" user-emacs-directory) 0)
  (byte-recompile-directory (expand-file-name "lisp/use-package" user-emacs-directory) 0))

(defvar vde/fixed-font-family
  (cond ((x-list-fonts "Ubuntu Mono") "Ubuntu Mono-14")
        ((x-list-fonts "Hasklig") "Hasklig-14")
        ((x-list-fonts "Consolas") "Consolas-14"))
  "Fixed width font based on what is install")

;; FIXME(vdemeester) extract the condition out 👼
(unless (eq system-configuration "aarch64-unknown-linux-android")
  (set-frame-font vde/fixed-font-family)
  (set-face-attribute 'default nil :font vde/fixed-font-family :height 110)
  (set-face-font 'default vde/fixed-font-family))

(line-number-mode t)
(column-number-mode t)

(global-linum-mode -1)
(add-hook 'prog-mode-hook (lambda () (linum-mode t)))

(provide 'visual-config)
