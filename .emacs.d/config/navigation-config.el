
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

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :init
  (setq which-key-sort-order 'which-key-key-order-alpha)
  :config
  (which-key-mode))

(use-package ivy
  :ensure t
  :diminish ivy-mode
  :config
  (use-package ivy-hydra
    :ensure t)
  (ido-mode -1)
  (ivy-mode 1)  ;; Show recently killed buffers when calling `ivy-switch-buffer'
  (setq ivy-use-virtual-buffers t)
  (defun modi/ivy-kill-buffer ()
    (interactive)
    (ivy-set-action 'kill-buffer)
    (ivy-done))
  (bind-keys
   :map ivy-switch-buffer-map
   ("C-k" . modi/ivy-kill-buffer))
  (bind-keys
   :map ivy-minibuffer-map
   ;; Exchange the default bindings for C-j and C-m
   ("C-m" . ivy-alt-done) ; RET, default C-j
   ("C-j" . ivy-done) ; default C-m
   ("C-S-m" . ivy-immediate-done)
   ("C-t" . ivy-toggle-fuzzy)
   ("C-o" . hydra-ivy/body))
  ;; version of ivy-yank-word to yank from start of word
  (defun bjm/ivy-yank-whole-word ()
    "Pull next word from buffer into search string."
    (interactive)
    (let (amend)
  (with-ivy-window
        ;;move to last word boundary
        (re-search-backward "\\b")
        (let ((pt (point))
          (le (line-end-position)))
          (forward-word 1)
          (if (> (point) le)
          (goto-char pt)
            (setq amend (buffer-substring-no-properties pt (point))))))
  (when amend
        (insert (replace-regexp-in-string " +" " " amend)))))

  ;; bind it to M-j
  (define-key ivy-minibuffer-map (kbd "M-j") 'bjm/ivy-yank-whole-word))

(use-package counsel
  :ensure t
  :bind*                    
  (("M-x"     . counsel-M-x)
   ("M-y"     . counsel-yank-pop)
   ("C-x C-f" . counsel-find-file)
   ("C-x C-r" . counsel-recentf)
   ("C-c f"   . counsel-git)
   ("C-c s"   . counsel-git-grep)
   ("C-c /"   . counsel-ag))
  :config
  (progn
    (ivy-set-actions
     'counsel-find-file
     '(("d" (lambda (x) (delete-file (expand-file-name x)))
        "delete"
        )))
    (ivy-set-actions
     'ivy-switch-buffer
     '(("k" (lamba (x)
                   (kill-buffer x)
                   (ivy--reset-state ivy-last))
        "kill")
   ("j"
        ivy--switch-buffer-other-window-action
        "other window")))
    )
  )

(use-package projectile
  :ensure t
  :init
  (setq projectile-enable-caching t
        projectile-verbose nil
        projectile-completion-system 'ivy)
  (put 'projectile-project-run-cmd 'safe-local-variable #'stringp)
  (defun projectile-do-invalidate-cache (&rest _args)
    (projectile-invalidate-cache nil))
  (advice-add 'rename-file :after #'projectile-do-invalidate-cache))

(use-package counsel-projectile
  :ensure t
  :after projectile
  :config (counsel-projectile-on))

(use-package persp-projectile
  :ensure t
  :after projectile)

(use-package general
  :ensure t
  :config
  (let ((leader "SPC")
        (emacs-leader "M-m"))
    (general-define-key
     :states '(normal visual insert emacs)
     :prefix leader
     :non-normal-prefix emacs-leader
     "p" 'projectile-command-map
     "TAB" '(ivy-switch-buffer :which-key "switch buffer")
     "/" '(:ignore t :which-key "search")
     "/a" '(counsel-ag :which-key "ag")
     "/g" '(counsel-git-grep :which-key "git grep")
     "y" '(counsel-yank-pop :which-key "yank-pop")
     "SPC" '(counsel-M-x :which-key "M-x")
     "f" '(:ignore t :which-key "Files")
     "ff" '(counsel-find-file :which-key "find file")
     "fd" '(counsel-git :which-key "find file in git")
     "r" '(counsel-recentf :which-key "recent file"))))

(provide 'navigation-config)
