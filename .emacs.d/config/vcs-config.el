
(use-package magit
  :ensure t
  :diminish "🔮 "
  :commands magit-status
  :config
  (setq magit-completing-read-function 'ivy-completing-read)
  (add-to-list 'magit-no-confirm 'stage-all-changes)
  (setq magit-push-always-verify nil)
  (setq magit-last-seen-setup-instructions "2.1.0"))

(use-package git-annex
  :ensure t)
(use-package magit-annex
  :ensure t)

(provide 'vcs-config)
