;;; vde-shells.el --- setup shells related mode
;;; Commentary:
;;; Code:
;;; -*- lexical-binding: t; -*-

(use-package shell                 ; Specialized comint.el for running the shell
  :bind (("<f1>"      . vde-shell-open)
         ("C-c a s s" . vde-shell-open)
         (:map shell-mode-map
               ("<tab>" . completion-at-point)))
  :config
  (defun vde-shell-open ()
    "Save window configuration and call `shell'."
    (interactive)
    (vde-save-wins-then-call 'shell))

  ;; Use a single full frame for shell
  (with-eval-after-load 'shell
    (fullframe shell vde-pop-window-configuration))

  (bind-key "C-c C-q" #'vde-pop-window-configuration shell-mode-map)
  
  (unbind-key "C-c C-l" shell-mode-map)
  (bind-key "C-c C-l" #'counsel-shell-history shell-mode-map)

  (defun vde-comint-delchar-or-eof-or-kill-buffer (arg)
    "Restore window configuration if process is dead, otherwise delete ARG."
    (interactive "p")
    (if (null (get-buffer-process (current-buffer)))
        (vde-pop-window-configuration)
      (comint-delchar-or-maybe-eof arg)))
  
  (setq
   ;; Prefer Bash to Fish for compatibility reasons
   explicit-shell-file-name "bash"
   ;; Fix find-dired
   shell-file-name "bash")
  (add-hook 'shell-mode-hook
            (lambda ()
              (bind-key "C-d" #'vde-comint-delchar-or-eof-or-kill-buffer
                        shell-mode-map)))
  )

(use-package shx                        ; Enhance comint-mode
  :defer 2
  :init (shx-global-mode 1))

(use-package eshell                     ; Emacs command shell
  :bind* ("C-x m t" . eshell-here)
  :config
  (defun eshell-here ()
    "Open EShell in the directory associated with the current buffer's file.
The EShell is renamed to match that directory to make multiple windows easier."
    (interactive)
    (let* ((parent (if (buffer-file-name)
                       (file-name-directory (buffer-file-name))
                     default-directory))
           (name   (car (last (split-string parent "/" t)))))
      (eshell "new")
      (rename-buffer (concat "*eshell: " name "*"))))

  ;; Handy aliases
  (defalias 'ff 'find-file)
  (defun eshell/l (&rest args) "Same as `ls -lah'"
         (apply #'eshell/ls "-lah" args))

  (defun eshell/d ()
    "Open a dired instance of the current working directory."
    (dired "."))

  (defun eshell/gs (&rest args)
    (magit-status (pop args) nil)
    (eshell/echo))                      ; The echo command suppresses output

  (defun eshell/clear ()
    "Clear `eshell' buffer, comint-style."
    (interactive)
    (let ((input (eshell-get-old-input)))
      (eshell/clear-scrollback)
      (eshell-emit-prompt)
      (insert input)))

  (add-hook
   'eshell-mode-hook
   (lambda ()
     (bind-keys :map eshell-mode-map
                ("C-c C-l"                . counsel-esh-history)
                ([remap eshell-pcomplete] . completion-at-point))))

  ;; Use system su/sudo
  (with-eval-after-load "em-unix"
    '(progn
       (unintern 'eshell/su nil)
       (unintern 'eshell/sudo nil)))

  (add-hook 'eshell-mode-hook #'with-editor-export-editor))

(use-package em-prompt                  ; EShell command prompts
  :defer 2
  :config
  (defun vde-eshell-quit-or-delete-char (arg)
    "Use C-d to either delete forward char or exit EShell."
    (interactive "p")
    (if (and (eolp) (looking-back eshell-prompt-regexp nil nil))
        (progn
          (eshell-life-is-too-much)
          (ignore-errors
            (delete-window)))
      (delete-char arg)))

  (add-hook 'eshell-mode-hook
            (lambda ()
              (bind-key "C-d"
                        #'vde-eshell-quit-or-delete-char eshell-mode-map))))

(use-package esh-mode                   ; EShell UI customizations
  :ensure eshell
  :config (setq eshell-scroll-to-bottom-on-input 'all))

(use-package em-cmpl                    ; EShell TAB completion
  :ensure eshell
  :config
  (add-hook 'eshell-mode-hook #'eshell-cmpl-initialize)

  (add-to-list 'eshell-command-completions-alist
               '("gunzip" "gz\\'"))
  (add-to-list 'eshell-command-completions-alist
               '("tar" "\\(\\.tar|\\.tgz\\|\\.tar\\.gz\\)\\'")))

(use-package em-hist                    ; EShell History management
  :ensure eshell
  :config (setq eshell-hist-ignoredups t))

(use-package em-term                    ; Handle visual commands in EShell
  :ensure eshell
  :config
  (add-to-list 'eshell-visual-commands "ssh")
  (add-to-list 'eshell-visual-commands "htop")
  (add-to-list 'eshell-visual-commands "top")
  (add-to-list 'eshell-visual-commands "tail")
  (add-to-list 'eshell-visual-commands "npm"))

(use-package fish-completion            ; Add Fish completion to EShell
  :defer 2
  :when (executable-find "fish")
  :config (add-hook 'eshell-mode-hook #'fish-completion-mode))

(use-package fish-mode                  ; Handle Fish shell scripts
  :mode ("\\.fish\\'" . fish-mode)
  :config
  ;; Run fish_indent before save
  (add-hook 'fish-mode-hook
            (lambda ()
              (add-hook 'before-save-hook 'fish_indent-before-save))))

(use-package eshell-prompt-extras
  :defer 1
  :init
  (progn
    (setq eshell-highlight-prompt nil
	  eshell-prompt-function 'epe-theme-lambda)))

(use-package esh-autosuggest
  :defer 1
  :hook (eshell-mode . esh-autosuggest-mode))

(use-package eshell-bookmark
  :config
  (add-hook 'eshell-mode-hook 'eshell-bookmark-setup))

;; for fish in ansi-term
(add-hook 'term-mode-hook 'toggle-truncate-lines)

(provide 'setup-shells)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
