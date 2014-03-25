(require 'deft)

;; We are going to use org-mode for deft
(setq deft-extension "org")
(setq deft-text-mode 'org-mode)
(setq deft-directory "~/desktop/org/notes")
(setq deft-use-filename-as-title t)         ;; Use filename as title

;; keybinding
(global-set-key (kbd "<f9>") 'deft)

