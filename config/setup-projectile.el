;;; -*- lexical-binding: t; -*-
(use-package projectile                 ; Project management
  :init (projectile-mode)
  :bind-keymap ("C-c p" . projectile-command-map)
  :config
  ;; Remove dead projects when Emacs is idle
  (run-with-idle-timer 10 nil #'projectile-cleanup-known-projects)
  (setq
   ;; Custom compilation buffer name function
   compilation-buffer-name-function (lambda (mode) (concat "*" (downcase mode) ": " (projectile-project-name) "*"))
   projectile-completion-system 'ivy
   projectile-find-dir-includes-top-level t
   projectile-switch-project-action #'projectile-commander
   projectile-create-missing-test-files t
   projectile-mode-line '(:eval (format " Proj[%s]" (projectile-project-name))))
  (def-projectile-commander-method ?s
    "Open a *shell* buffer for the project"
    (projectile-run-eshell))
  (def-projectile-commander-method ?c
    "Run `compile' in the project"
    (projectile-compile-project nil)))

(use-package counsel-projectile         ; Ivy integration for Projectile
  :bind (:map projectile-command-map
              ("p" . counsel-projectile-switch-project)
              ("r" . counsel-projectile-rg))
  :init (counsel-projectile-mode))

(provide 'setup-projectile)
