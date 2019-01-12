;;; setup-projectile.el --- setup projectile
;;; Commentary:
;;; Code:
;;; -*- lexical-binding: t; -*-

(use-package projectile                 ; Project management
  :init (projectile-mode)
  :chords (("pf" . projectile-find-file)
           ("pb" . projectile-switch-to-buffer)
           ("pv" . projectile-vc))
  :bind-keymap ("C-c p" . projectile-command-map)
  :config
  ;; Remove dead projects when Emacs is idle
  (run-with-idle-timer 10 nil #'projectile-cleanup-known-projects)

  (setq
   projectile-completion-system 'ivy
   projectile-find-dir-includes-top-level t
   projectile-mode-line '(:eval (format " Proj[%s]" (projectile-project-name)))))

(use-package counsel-projectile         ; Ivy integration for Projectile
  :bind (:map projectile-command-map
              ("p" . counsel-projectile-switch-project)
              ("r" . counsel-projectile-rg))
  :chords (("pg" . counsel-projectile-rg))
  :init (counsel-projectile-mode))

(provide 'setup-projectile)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
