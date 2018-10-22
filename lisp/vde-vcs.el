;;; -*- lexical-binding: t; -*-
(use-package vc-hooks                   ; Simple version control
  :bind (("S-<f5>" . vc-revert)
         ("C-c v r" . vc-refresh-state))
  :config
  ;; Always follow symlinks to files in VCS repos
  (setq vc-follow-symlinks t))

(use-package magit                      ; The best Git client out there
  :bind (("C-c v c" . magit-clone)
         ("C-c v C" . magit-checkout)
         ("C-c v d" . magit-dispatch-popup)
         ("C-c v g" . magit-blame)
         ("C-c v l" . magit-log-buffer-file)
         ("C-c v p" . magit-pull)
         ("C-c v v" . magit-status)) 
  :config
  (setq
   magit-save-repository-buffers 'dontask
   magit-refs-show-commit-count 'all
   magit-branch-prefer-remote-upstream '("master")
   ;; magit-branch-adjust-remote-upstream-alist '(("origin/master" "master"))
   magit-completing-read-function 'ivy-completing-read
   magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)

  (magit-define-popup-option 'magit-rebase-popup
    ?S "Sign using gpg" "--gpg-sign=" #'magit-read-gpg-secret-key) 
  (magit-define-popup-switch 'magit-log-popup
    ?m "Omit merge commits" "--no-merges")
  
  ;; Hide "Recent Commits"
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-unpushed-to-upstream
                          'magit-insert-unpushed-to-upstream-or-recent
                          'replace)

  (use-package magit-files
    :config
    (global-magit-file-mode))
  
  ;; Show refined hunks during diffs
  (set-default 'magit-diff-refine-hunk t)

  (add-hook 'projectile-switch-project-hook
            #'mu-magit-set-repo-dirs-from-projectile)

  ;; Refresh `magit-status' after saving a buffer
  (add-hook 'after-save-hook #'magit-after-save-refresh-status)

  ;; Free C-c C-w for Eyebrowse
  (unbind-key "C-c C-w" git-commit-mode-map)

  (defun mu-magit-kill-buffers ()
    "Restore window configuration and kill all Magit buffers."
    (interactive)
    (let ((buffers (magit-mode-get-buffers)))
      (magit-restore-window-configuration)
      (mapc #'kill-buffer buffers)))

  (bind-key "q" #'mu-magit-kill-buffers magit-status-mode-map))

(use-package magit-gitflow              ; gitflow extension for Magit
  :after magit
  :config
  ;; Free C-f and use a more suitable key binding
  (unbind-key "C-f" magit-gitflow-mode-map)
  (bind-key "C-c v f" #'magit-gitflow-popup magit-gitflow-mode-map)

  (add-hook 'magit-mode-hook 'turn-on-magit-gitflow))

(use-package magit-todos
  :after magit
  :config
  (magit-todos-mode 1))

(use-package git-commit                 ; Git commit message mode
  :defer 2
  :init (global-git-commit-mode)
  :config
  (remove-hook 'git-commit-finish-query-functions
               #'git-commit-check-style-conventions))

(use-package gitconfig-mode             ; Git configuration mode
  :defer 2)

(use-package gitignore-mode             ; .gitignore mode
  :defer 2)

(use-package gitattributes-mode         ; Git attributes mode
  :defer 2)

(use-package diff-hl
  :commands (diff-hl-mode diff-hl-dired-mode)
  :hook (magit-post-refresh . diff-hl-magit-post-refresh))

(use-package diffview
  :commands (diffview-current diffview-region diffview-message))

(provide 'vde-vcs)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
