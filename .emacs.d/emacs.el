(defun my/edit-emacs-configuration ()
  (interactive)
  (find-file "~/.emacs.d/emacs.org"))

(global-set-key "\C-ce" 'my/edit-emacs-configuration)

(setq
 ;; General
 ;; TODO use xdg to get these
 org-root-directory (substitute-env-in-file-name "$HOME/desktop/org")
 desktop-folder (substitute-env-in-file-name "$HOME/desktop")
 videos-folder (expand-file-name "videos" desktop-folder)
 downloads-folder (expand-file-name "downloads" desktop-folder)
 music-folder (expand-file-name "music" desktop-folder)
 pictures-folder (expand-file-name "pictures" desktop-folder)
 ;; Orgmode related
 my-org-file "emacs.org"
 org-root-directory (substitute-env-in-file-name "$HOME/desktop/org")
 org-todos-directory-name "todos"
 org-notes-directory-name "notes"
 org-sites-directory-name "sites"
 org-archive-directory-name "archive"
 org-archive-file-pattern "/%s_archive::"
 org-inbox-file "inbox.org"
 org-main-file "personal.org"
 org-journal-file "journal.org"
 org-stackoverflow-file "stack.org"
 org-web-article-file "ent.org"
 org-publish-folder (substitute-env-in-file-name "$HOME/var/public_html")
 sites-folder (substitute-env-in-file-name "$HOME/src/sites/")
 ;; Github related
 github-general-folder (substitute-env-in-file-name "$HOME/src/github")
 github-username "vdemeester")

(when (file-readable-p "~/.emacs.d/user.el")
  (load "~/.emacs.d/user.el"))

(setq FULLHOSTNAME (format "%s" system-name))
(setq HOSTNAME (substring (system-name) 0 (string-match "\\." (system-name))))

(setq HOSTNAME-FILE
      (expand-file-name
       (format "hosts/%s.el" HOSTNAME)
       "~/.emacs.d"))

(when (file-readable-p HOSTNAME-FILE)
  (load HOSTNAME-FILE))

(setq
 ;; Orgmode related
 org-todos-directory (expand-file-name org-todos-directory-name org-root-directory)
 org-notes-directory (expand-file-name org-notes-directory-name org-root-directory)
 org-sites-directory (expand-file-name org-sites-directory-name org-root-directory)
 org-archive-directory (expand-file-name org-archive-directory-name org-root-directory)
 ;; Github related
 github-personal-folder (expand-file-name github-username github-general-folder))

(when window-system
     (menu-bar-mode -1)
     (tool-bar-mode -1)
     (scroll-bar-mode -1)
     (blink-cursor-mode -1))

(setq inhibit-splash-screen t)

(line-number-mode 1)
(column-number-mode 1)
(global-hl-line-mode 1)

(setq font-lock-maximum-decoration 2)

(setq-default indicate-buffer-boundaries 'left)
(setq-default indicate-empty-lines +1)

(defconst emacs-tmp-dir (format "%s/%s%s/" temporary-file-directory "emacs" (user-uid)))
(setq backup-directory-alist
      `((".*" . ,emacs-tmp-dir))
      auto-save-file-name-transforms
      `((".*" ,emacs-tmp-dir t))
      auto-save-list-file-prefix emacs-tmp-dir)

(setq delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)
(setq-default buffer-file-coding-system 'utf-8-auto-unix)

(fset 'yes-or-no-p 'y-or-n-p)

(setq echo-keystrokes 0.1)

(global-set-key "\C-c\C-m" 'execute-extended-command)

(setq save-abbrevs 'silently)
(setq-default abbrev-mode t)

(when (fboundp 'winner-mode)
  (winner-mode 1))

(add-hook 'diff-mode-hook (lambda ()
                            (setq-local whitespace-style
                                        '(face
                                          tabs
                                          tab-mark
                                          spaces
                                          space-mark
                                          trailing
                                          indentation::space
                                          indentation::tab
                                          newline
                                          newline-mark))
                            (whitespace-mode 1)))

(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)
(add-hook 'ediff-after-quit-hook-internal 'winner-undo)

(setq tramp-default-method "ssh"
      tramp-backup-directory-alist backup-directory-alist)

(use-package dired-x)
(setq dired-guess-shell-alist-user
         '(("\\.pdf\\'" "evince" "okular")
           ("\\.\\(?:djvu\\|eps\\)\\'" "evince")
           ("\\.\\(?:jpg\\|jpeg\\|png\\|gif\\|xpm\\)\\'" "geeqie")
           ("\\.\\(?:xcf\\)\\'" "gimp")
           ("\\.csv\\'" "libreoffice")
           ("\\.tex\\'" "pdflatex" "latex")
           ("\\.\\(?:mp4\\|mkv\\|avi\\|flv\\|ogv\\)\\(?:\\.part\\)?\\'"
            "mpv")
           ("\\.\\(?:mp3\\|flac\\)\\'" "mpv")
           ("\\.html?\\'" "firefox")
           ("\\.cue?\\'" "audacious")))
(put 'dired-find-alternate-file 'disabled nil)

(setq diredp-hide-details-initially-flag nil) ;
(use-package dired+
             :ensure t
             :init)

(use-package dired-aux)

(defvar dired-filelist-cmd
  '(("vlc" "-L")))

(defun dired-start-process (cmd &optional file-list)
  (interactive
   (let ((files (dired-get-marked-files
                 t current-prefix-arg)))
     (list
      (dired-read-shell-command "& on %s: "
                                current-prefix-arg files)
      files)))
  (let (list-switch)
    (start-process
     cmd nil shell-file-name
     shell-command-switch
     (format
      "nohup 1>/dev/null 2>/dev/null %s \"%s\""
      (if (and (> (length file-list) 1)
               (setq list-switch
                     (cadr (assoc cmd dired-filelist-cmd))))
          (format "%s %s" cmd list-switch)
        cmd)
      (mapconcat #'expand-file-name file-list "\" \"")))))

(define-key dired-mode-map "c" 'dired-start-process)

(defun dired-get-size ()
  (interactive)
  (let ((files (dired-get-marked-files)))
    (with-temp-buffer
      (apply 'call-process "du" nil t nil "-schL" files) ;; -L to dereference (git-annex folder)
      (message
       "Size of all marked files: %s"
       (progn
         (re-search-backward "\\(^[ 0-9.,]+[A-Za-z]+\\).*total$")
         (match-string 1))))))
(define-key dired-mode-map (kbd "z") 'dired-get-size)

(define-key dired-mode-map "F" 'find-name-dired)

(define-key dired-mode-map "e" 'wdired-change-to-wdired-mode)

(setq dired-listing-switches "-laGh1v --group-directories-first")

(use-package peep-dired
  :ensure t
  :defer t ; don't access `dired-mode-map' until `peep-dired' is loaded
  :bind (:map dired-mode-map
              ("P" . peep-dired)))

(use-package dired-narrow
  :ensure t
  :defer t
  :bind (:map dired-mode-map
              ("/" . dired-narrow)))

(use-package diminish
  :ensure t
  :demand t
  :diminish (visual-line-mode . "ω")
  :diminish hs-minor-mode
  :diminish abbrev-mode
  :diminish auto-fill-function
  :diminish subword-mode)

(defun sk/diminish-org-indent ()
  (interactive)
  (diminish 'org-indent-mode ""))
(add-hook 'org-indent-mode-hook 'sk/diminish-org-indent)

(defun sk/diminish-auto-revert ()
  (interactive)
  (diminish 'auto-revert-mode ""))
(add-hook 'auto-revert-mode-hook 'sk/diminish-auto-revert)

(defun sk/diminish-eldoc ()
  (interactive)
  (diminish 'eldoc-mode ""))
(add-hook 'eldoc-mode-hook 'sk/diminish-eldoc)

(defun sk/diminish-subword ()
  (interactive)
  (diminish 'subword-mode ""))
(add-hook 'subword-mode-hook 'sk/diminish-subword)

(subword-mode t)

(put 'narrow-to-region 'disabled nil)

(setq recentf-max-saved-items 1000
      recentf-exclude '("/tmp/" "/ssh:")
      ;; disable recentf-cleanup on Emacs start, because it can cause
      ;; problems with remote files
      recentf-auto-cleanup 'never)
(recentf-mode)

(set-default-font "Ubuntu Mono-12")
(set-frame-font "Ubuntu Mono-12")
(set-face-attribute 'default nil :family "Ubuntu Mono" :height 110)

;;  (set-fontset-font t 'unicode "Symbola" nil 'prepend)

(use-package uniquify)
(setq uniquify-buffer-name-style 'forward)

(defun kill-default-buffer ()
  "Kill the currently active buffer"
  (interactive)
  (let (kill-buffer-query-functions) (kill-buffer)))

(global-set-key (kbd "C-x k") 'kill-default-buffer)

(defalias 'list-buffers 'ibuffer) ; make ibuffer default

(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

(use-package flyspell
  :ensure t
  :init
  (progn
    (use-package flyspell-lazy
      :ensure t)
    (add-hook 'text-mode-hook 'flyspell-mode)
    (add-hook 'prog-mode-hook 'flyspell-prog-mode))
  :config
  (progn
    (setq ispell-program-name "aspell")
    (setq ispell-local-dictionary "en_US")
    (setq ispell-local-dictionary-alist
          '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil nil nil utf-8)
            ("fr_FR" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil nil nil utf-8)))))

(defadvice server-ensure-safe-dir (around
                                   my-around-server-ensure-safe-dir
                                   activate)
  "Ignores any errors raised from server-ensure-safe-dir"
  (ignore-errors ad-do-it))
(unless (string= (user-login-name) "root")
  (require 'server)
  (when (or (not server-process)
            (not (eq (process-status server-process)
                     'listen)))
    (unless (server-running-p server-name)
   (server-start))))

(when (eq system-type 'darwin)
  (setq ns-right-alternate-modifier nil)
  (setq mac-right-option-modifier 'none))

(use-package which-key
  :ensure t
  :defer t
  :diminish which-key-mode
  :init
  (setq which-key-sort-order 'which-key-key-order-alpha)
  :bind* (("M-m ?" . which-key-show-top-level))
  :config
  (which-key-mode)
  (which-key-add-key-based-replacements
    "M-m ?" "top level bindings"))

(use-package discover-my-major
  :ensure t
  :bind (("C-h C-m" . discover-my-major)
         ("C-h M-m" . discover-my-mode)))

(use-package hydra
  :ensure t)

(use-package ivy
  :ensure t
  :diminish ivy-mode
  :bind (("C-c C-r" . ivy-resume))
  :config
  (use-package ivy-hydra
    :ensure t)
  (ido-mode -1)
  ;; Enable ivy
  (ivy-mode 1)
  ;; Show recently killed buffers when calling `ivy-switch-buffer'
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
  (define-key ivy-minibuffer-map (kbd "M-j") 'bjm/ivy-yank-whole-word)
  )

(use-package counsel
  :ensure t
  :bind* (("M-m f f" . counsel-find-file)
          ("M-m f r" . counsel-recentf)
          ("M-m f g" . counsel-git)

          ("M-m i" . counsel-imenu)

          ("C-x C-f" . counsel-find-file)
          ("C-h f" . counsel-describe-function)
          ("C-h v" . counsel-describe-variable)
          ("C-h i" . counsel-info-lookup-symbol)
          ("C-c C-u" . counsel-unicode-char)
          ("C-c s g" . counsel-git-grep)
          ("C-c s s" . counsel-pt)
          ("M-y" . counsel-yank-pop)
          ("M-x" . counsel-M-x))
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

(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :bind (("C-*" . undo-tree-undo))
  :init
  (progn
    (defalias 'redo 'undo-tree-redo)
    (defalias 'undo 'undo-tree-undo)
    (global-undo-tree-mode)
    )
  :config
  (progn
    (setq undo-tree-auto-save-history t)
    (let ((undo-dir (expand-file-name "undo" user-emacs-directory)))
      (setq undo-tree-history-directory-alist (list (cons "." undo-dir))))))

(use-package avy
  :ensure t
  :init
  (setq avy-keys-alist
        `((avy-goto-char-timer . (?j ?k ?l ?f ?s ?d ?e ?r ?u ?i))
          (avy-goto-line . (?j ?k ?l ?f ?s ?d ?e ?r ?u ?i))))
  :bind* (("M-m a f" . avy-goto-char-timer)
          ("M-m a g" . avy-goto-line)
          ("M-g g" . avy-goto-line)))

(use-package ace-window
  :ensure t
  :bind* (("M-m w o" . ace-window)))

(use-package popwin
  :ensure t
  :config
  (progn
    (add-to-list 'popwin:special-display-config `("*Swoop*" :height 0.5 :position bottom))
    (add-to-list 'popwin:special-display-config `("*Warnings*" :height 0.5 :noselect t))
    (add-to-list 'popwin:special-display-config `("*Process List*" :height 0.5))
    (add-to-list 'popwin:special-display-config `("*Messages*" :height 0.5 :noselect t))
    (add-to-list 'popwin:special-display-config `("*Backtrace*" :height 0.5))
    (add-to-list 'popwin:special-display-config `("*Compile-Log*" :height 0.3 :noselect t))
    (add-to-list 'popwin:special-display-config `("*Remember*" :height 0.5))
    (add-to-list 'popwin:special-display-config `("*All*" :height 0.5))
    (add-to-list 'popwin:special-display-config `("*Go Test*" :height 0.3))
    (add-to-list 'popwin:special-display-config `("*Async Shell Command*" :height 0.3))
    (add-to-list 'popwin:special-display-config `(flycheck-error-list-mode :height 0.5 :regexp t :position bottom))
    (popwin-mode 1)
    (global-set-key (kbd "C-z") popwin:keymap)))

(use-package fullframe
  :ensure t
  :config
  (fullframe magit-status magit-mode-quit-window)
  (fullframe ibuffer ibuffer-quit)
  (fullframe list-packages quit-window))

(use-package perspective
  :ensure t
  :bind* (("M-m SPC c" . persp-switch)
    ("M-m SPC n" . persp-next)
    ("M-m SPC p" . persp-prev)
    ("M-m SPC r" . persp-rename)
    ("M-m SPC k" . pers-kill)
    ("M-m SPC o o" . custom-persp/org)
    ("M-m SPC o d d" . custom-persp/org-docker)
    ("M-m SPC o d p" . custom-persp/org-docker-pipeline)
    ("M-m SPC o m" . custom-persp/org-moby)
    ("M-m SPC s d d" . custom-persp/magit-docker)
    ("M-m SPC s d p" . custom-persp/magit-docker-pipeline)
    ("M-m SPC s d i i" . custom-persp/magit-docker-infrakit)
    ("M-m SPC s d i d" . custom-persp/magit-docker-infrakit-do)
    ("M-m SPC s d i g" . custom-persp/magit-docker-infrakit-gcp)
    ("M-m SPC s l" . custom-persp/magit-linuxkit)
    ("M-m SPC d d" . custom-persp/docker)
    ("M-m SPC d p" . custom-persp/docker-pipeline)
    ("M-m SPC d i i" . custom-persp/docker-infrakit)
    ("M-m SPC d i d" . custom-persp/docker-infrakit-do)
    ("M-m SPC d i g" . custom-persp/docker-infrakit-gcp)
    ("M-m SPC l" . custom-persp/linuxkit))
  :config
  (persp-mode t)
  (defmacro custom-persp (name &rest body)
    `(let ((initialize (not (gethash ,name perspectives-hash)))
           (current-perspective persp-curr))
    (persp-switch ,name)
    (when initialize ,@body)
    (setq persp-last current-perspective)))
  (defun custom-persp/org ()
    (interactive)
    (custom-persp "org"
                  (find-file (expand-file-name "todos/personal.org" org-root-directory))))
  (defun custom-persp/org-docker ()
    (interactive)
    (custom-persp "org-docker"
                  (find-file (expand-file-name "todos/docker.org" org-root-directory))))
  (defun custom-persp/org-docker-pipeline ()
    (interactive)
    (custom-persp "org-pipeline"
                  (find-file (expand-file-name "todos/docker-pipeline.org" org-root-directory))))
  (defun custom-persp/org-moby ()
    (interactive)
    (custom-persp "org-moby"
                  (find-file (expand-file-name "todos/moby.org" org-root-directory))))
  (defun custom-persp/docker ()
    (interactive)
    (custom-persp "docker"
                  (find-file (substitute-env-in-file-name "$HOME/go/src/github.com/docker/docker"))))
  (defun custom-persp/docker-pipeline ()
    (interactive)
    (custom-persp "pipeline"
                  (find-file (substitute-env-in-file-name "$HOME/go/src/github.com/docker/pipeline"))))
  (defun custom-persp/docker-infrakit ()
    (interactive)
    (custom-persp "infrakit"
                  (find-file (substitute-env-in-file-name "$HOME/go/src/github.com/docker/infrakit"))))
  (defun custom-persp/docker-infrakit-do ()
    (interactive)
    (custom-persp "infrakit.digitalocean"
                  (find-file (substitute-env-in-file-name "$HOME/go/src/github.com/docker/infrakit.digitalocean"))))
  (defun custom-persp/docker-infrakit-gcp ()
    (interactive)
    (custom-persp "infrakit.gcp"
                  (find-file (substitute-env-in-file-name "$HOME/go/src/github.com/docker/infrakit.gcp"))))
  (defun custom-persp/linuxkit ()
    (interactive)
    (custom-persp "linuxkit"
                  (find-file (substitute-env-in-file-name "$HOME/go/src/github.com/linuxkit/linuxkit"))))
  (defun custom-persp/magit-docker ()
    (interactive)
    (custom-persp "magit-docker"
   	   (magit-status (substitute-env-in-file-name "$HOME/go/src/github.com/docker/docker"))))
  (defun custom-persp/magit-docker-pipeline ()
    (interactive)
    (custom-persp "magit-pipeline"
                  (magit-status (substitute-env-in-file-name "$HOME/go/src/github.com/docker/pipeline"))))
  (defun custom-persp/magit-docker-infrakit ()
    (interactive)
    (custom-persp "magit-infrakit"
                  (magit-status (substitute-env-in-file-name "$HOME/go/src/github.com/docker/infrakit"))))
  (defun custom-persp/magit-docker-infrakit-do ()
    (interactive)
    (custom-persp "magit-infrakit.digitalocean"
                  (magit-status (substitute-env-in-file-name "$HOME/go/src/github.com/docker/infrakit.digitalocean"))))
  (defun custom-persp/magit-docker-infrakit-gcp ()
    (interactive)
    (custom-persp "magit-infrakit.gcp"
                  (magit-status (substitute-env-in-file-name "$HOME/go/src/github.com/docker/infrakit.gcp"))))
  (defun custom-persp/magit-linuxkit ()
    (interactive)
    (custom-persp "magit-linuxkit"
                  (magit-status (substitute-env-in-file-name "$HOME/go/src/github.com/linuxkit/linuxkit"))))
  )

(use-package apropospriate-theme
  :ensure t
  :config
  (load-theme 'apropospriate-dark t))

(use-package all-the-icons
  :ensure t)
(use-package all-the-icons-dired
  :ensure t
  :config (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

(use-package spaceline-config
  :ensure spaceline
  :config
  (setq powerline-default-separator 'slant
        spaceline-workspace-numbers-unicode t
        spaceline-window-numbers-unicode t)
  (spaceline-spacemacs-theme)
  (spaceline-info-mode))

(defun cycle-powerline-separators (&optional reverse)
  "Set Powerline separators in turn.  If REVERSE is not nil, go backwards."
  (interactive)
  (let* ((fn (if reverse 'reverse 'identity))
         (separators (funcall fn '("arrow" "arrow-fade" "slant"
                                   "chamfer" "wave" "brace" "roundstub" "zigzag"
                                   "butt" "rounded" "contour" "curve")))
         (found nil))
    (while (not found)
   (progn (setq separators (append (cdr separators) (list (car separators))))
             (when (string= (car separators) powerline-default-separator)
   	    (progn (setq powerline-default-separator (cadr separators))
   		   (setq found t)
   		   (redraw-display)))))))

(use-package spaceline-all-the-icons
  :ensure t
  :after spaceline
  :config
  (spaceline-all-the-icons-theme)
  (spaceline-all-the-icons--setup-git-ahead))

(use-package highlight-symbol
  :ensure t
  :init
  (progn
    (setq highlight-symbol-on-navigation-p t)
    (add-hook 'prog-mode-hook 'highlight-symbol-mode))
  :bind (("C-<f3>" . highlight-symbol-at-point)
         ("<f3>" . highlight-symbol-next)
         ("S-<f3>" . highlight-symbol-prev)
         ("M-<f3>" . highlight-symbol-query-replace)))

(use-package volatile-highlights
  :ensure t
  :demand t
  :diminish volatile-highlights-mode
  :config
  (volatile-highlights-mode t))

(use-package highlight-indentation
  :ensure t
  :commands (highlight-indentation-mode))

(use-package fill-column-indicator
  :ensure t
  :commands (fci-mode)
  :init
  (setq fci-rule-width 3
        fci-rule-column 79))

(use-package rainbow-identifiers
  :ensure t
  :init (add-hook 'prog-mode-hook
                  'rainbow-identifiers-mode))

(use-package command-log-mode
  :ensure t)

(add-hook 'text-mode-hook
          (lambda()
            (turn-on-auto-fill)
            (setq show-trailing-whitespace 't))
          )

(use-package comment-dwim-2
  :ensure t
  :bind* (("C-M-/" . comment-dwim-2)))

(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))

(use-package yasnippet
  :ensure t
  :diminish (yas-minor-mode . "γ")
  :config
  (setq yas-verbosity 1
        yas/triggers-in-field t ; enable nested triggering of snippets
        yas-snippet-dir (expand-file-name "snippets" user-emacs-directory))
  (define-key yas-minor-mode-map (kbd "<tab>") nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  (define-key yas-minor-mode-map (kbd "<C-tab>") 'yas-expand)
  (add-hook 'snippet-mode-hook '(lambda () (setq-local require-final-newline nil)))
  (yas-global-mode))

(defun sk/force-yasnippet-off ()
  (yas-minor-mode -1)
  (setq yas-dont-activate t))
(add-hook 'term-mode-hook 'sk/force-yasnippet-off)
(add-hook 'shell-mode-hook 'sk/force-yasnippet-off)

(use-package wgrep
  :ensure t)

(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
                'smarter-move-beginning-of-line)

(defun my/untabify-buffer ()
  "Untabify the currently visited buffer."
  (interactive)
  (untabify (point-min) (point-max)))

(defun my/untabify-region-or-buffer ()
  "Untabify a region if selected, otherwise the whole buffer."
  (interactive)
  (unless (member major-mode indent-sensitive-modes)
    (save-excursion
      (if (region-active-p)
          (progn
            (untabify (region-beginning) (region-end))
            (message "Untabify selected region."))
        (progn
          (my/untabify-buffer)
          (message "Untabify buffer.")))
      )))

(defun my/indent-buffer ()
  "Indent the currently visited buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

(defun my/indent-region-or-buffer ()
  "Indent a region if selected, otherwise the whole buffer."
  (interactive)
  (unless (member major-mode indent-sensitive-modes)
    (save-excursion
      (if (region-active-p)
          (progn
            (indent-region (region-beginning) (region-end))
            (message "Indented selected region."))
        (progn
          (my/indent-buffer)
          (message "Indented buffer.")))
      (whitespace-cleanup))))

(global-set-key (kbd "C-C i") 'my/indent-region-or-buffer)

(defun my/cleanup-buffer ()
    "Perform a bunch of operations on the whitespace content of a buffer."
    (interactive)
    (my/indent-buffer)
    (my/untabify-buffer)
    (delete-trailing-whitespace))

(defun my/cleanup-region (beg end)
  "Remove tmux artifacts from region."
  (interactive "r")
  (dolist (re '("\\\\│\·*\n" "\W*│\·*"))
    (replace-regexp re "" nil beg end)))

(global-set-key (kbd "C-x M-t") 'my/cleanup-region)
(global-set-key (kbd "C-c n") 'my/cleanup-buffer)

(use-package org
  :ensure t)
(require 'find-lisp)
(setq org-directory org-root-directory)
(setq org-agenda-files (find-lisp-find-files org-todos-directory "\.org$"))
(setq org-enforce-todo-dependencies t)
(setq org-enforce-todo-checkbox-dependencies t)

(setq org-log-redeadline (quote time))
(setq org-log-reschedule (quote time))

;;open agenda in current window
(setq org-agenda-window-setup (quote current-window))
;;warn me of any deadlines in next 7 days
(setq org-deadline-warning-days 7)
;;show me tasks scheduled or due in next fortnight
(setq org-agenda-span (quote fortnight))
;;don't show tasks as scheduled if they are already shown as a deadline
(setq org-agenda-skip-scheduled-if-deadline-is-shown t)
;;don't give awarning colour to tasks with impending deadlines
;;if they are scheduled to be done
(setq org-agenda-skip-deadline-prewarning-if-scheduled (quote pre-scheduled))
;;don't show tasks that are scheduled or have deadlines in the
;;normal todo list
(setq org-agenda-todo-ignore-deadlines (quote all))
(setq org-agenda-todo-ignore-scheduled (quote all))
;;sort tasks in order of when they are due and then by priority
(setq org-agenda-sorting-strategy
  (quote
   ((agenda deadline-up priority-down)
    (todo priority-down category-keep)
    (tags priority-down category-keep)
    (search category-keep))))

(setq org-image-actual-width '(300))

(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))

;;; Change the ellipsis (default is ...)
(setq org-ellipsis " ↴")
;; Change the default bullets
(font-lock-add-keywords 'org-mode
                        '(("^ +\\([-*]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
;;; Use org-bullets
(use-package org-bullets
  :config
  (setq org-bullets-face-name (quote org-bullet-face))
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "PROGRESS(p)" "PAUSED" "BLOCKED" "REVIEW" "|" "DONE(d!)" "ARCHIVED")
              (sequence "REPORT(r!)" "BUG" "KNOWNCAUSE" "|" "FIXED(f!)")
              (sequence "|" "CANCELLED(c@)"))))

;; FIXME(vdemeester) rework the faces, it's ugly on current theme...
(setq org-todo-keyword-faces
      (quote (("TODO" . org-todo)
              ("PROGRESS" . "green")
              ("PAUSED" . "cyan")
              ("BLOCKED" . "red")
              ("REVIEW" . "yellow")
              ("DONE" . org-done)
              ("ARCHIVED" . org-done)
              ("CANCELLED" . "black")
              ("REPORT" . org-todo)
              ("BUG" . "red")
              ("KNOWNCAUSE" . "yellow")
              ("FIXED" . org-done))))

(setq org-todo-state-tags-triggers
      (quote (("CANCELLED" ("CANCELLED" . t)))))

(defun turn-on-auto-visual-line (expression)
  (if buffer-file-name
      (cond ((string-match expression buffer-file-name)
             (progn
               (auto-fill-mode -1)
               (visual-line-mode 1))
             ))))

(add-hook 'org-mode-hook
          '(lambda ()
             (org-defkey org-mode-map "\C-c[" 'undefined)
             (org-defkey org-mode-map "\C-c]" 'undefined)
             (org-defkey org-mode-map "\C-c;" 'undefined)
             (turn-on-auto-visual-line (concat org-notes-directory "/*")))
          'append)

(run-at-time "00:59" 3600 'org-save-all-org-buffers)

(setq
 org-cycle-separator-lines 0      ;; Don't show blank lines
 org-catch-invisible-edits 'error ;; don't edit invisible text
 org-refile-targets '((org-agenda-files . (:maxlevel . 6)))
 )

(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to PROGRESS otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "PROGRESS"))))

(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

(add-to-list 'org-structure-template-alist '("A" "#+DATE: ?"))
(add-to-list 'org-structure-template-alist '("C" "#+BEGIN_CENTER\n?\n#+END_CENTER\n"))
(add-to-list 'org-structure-template-alist '("D" "#+DESCRIPTION: ?"))
(add-to-list 'org-structure-template-alist '("E" "#+BEGIN_EXAMPLE\n?\n#+END_EXAMPLE\n"))
(add-to-list 'org-structure-template-alist '("L" "#+BEGIN_LaTeX\n?\n#+END_LaTeX"))
(add-to-list 'org-structure-template-alist '("N" "#+NAME: ?"))
(add-to-list 'org-structure-template-alist '("S" "#+SUBTITLE: ?"))
(add-to-list 'org-structure-template-alist '("T" ":DRILL_CARD_TYPE: twosided"))
(add-to-list 'org-structure-template-alist '("V" "#+BEGIN_VERSE\n?\n#+END_VERSE"))
(add-to-list 'org-structure-template-alist '("a" "#+AUTHOR: ?"))
(add-to-list 'org-structure-template-alist '("c" "#+CAPTION: ?"))
(add-to-list 'org-structure-template-alist '("e" "#+BEGIN_SRC emacs-lisp\n?\n#+END_SRC"))
(add-to-list 'org-structure-template-alist '("g" "#+BEGIN_SRC golang\n?\n#+END_SRC"))
(add-to-list 'org-structure-template-alist '("f" "#+TAGS: @?"))
(add-to-list 'org-structure-template-alist '("h" "#+BEGIN_HTML\n?\n#+END_HTML\n"))
(add-to-list 'org-structure-template-alist '("k" "#+KEYWORDS: ?"))
(add-to-list 'org-structure-template-alist '("l" "#+LABEL: ?"))
(add-to-list 'org-structure-template-alist '("n" "#+BEGIN_NOTES\n?\n#+END_NOTES"))
(add-to-list 'org-structure-template-alist '("o" "#+OPTIONS: ?"))
(add-to-list 'org-structure-template-alist '("p" "#+BEGIN_SRC python\n?\n#+END_SRC"))
(add-to-list 'org-structure-template-alist '("q" "#+BEGIN_QUOTE\n?\n#+END_QUOTE"))
(add-to-list 'org-structure-template-alist '("r" ":PROPERTIES:\n?\n:END:"))
(add-to-list 'org-structure-template-alist '("s" "#+BEGIN_SRC ?\n#+END_SRC\n"))
(add-to-list 'org-structure-template-alist '("t" "#+TITLE: ?"))

(setq org-use-speed-commands t)

(defun my/org-show-next-heading-tidily ()
  "Show next entry, keeping other entries closed."
  (if (save-excursion (end-of-line) (outline-invisible-p))
      (progn (org-show-entry) (show-children))
    (outline-next-heading)
    (unless (and (bolp) (org-on-heading-p))
      (org-up-heading-safe)
      (hide-subtree)
      (error "Boundary reached"))
    (org-overview)
    (org-reveal t)
    (org-show-entry)
    (show-children)))

(defun my/org-show-previous-heading-tidily ()
  "Show previous entry, keeping other entries closed."
  (let ((pos (point)))
    (outline-previous-heading)
    (unless (and (< (point) pos) (bolp) (org-on-heading-p))
      (goto-char pos)
      (hide-subtree)
      (error "Boundary reached"))
    (org-overview)
    (org-reveal t)
    (org-show-entry)
    (show-children)))

(setq org-speed-commands-user '(("n" . my/org-show-next-heading-tidily)
                                ("p" . my/org-show-previous-heading-tidily)
                                (":" . org-set-tags-command)
                                ("c" . org-toggle-checkbox)
                                ("d" . org-cut-special)
                                ("P" . org-set-property)
                                ("C" . org-clock-display)
                                ("z" . (lambda () (interactive)
                                         (org-tree-to-indirect-buffer)
                                         (other-window 1)
                                         (delete-other-windows)))))

(defvar oc-capture-prmt-history nil
  "History of prompt answers for org capture.")
(defun oc/prmt (prompt variable)
  "PROMPT for string, save it to VARIABLE and insert it."
  (make-local-variable variable)
  (set variable (read-string (concat prompt ": ") nil oc-capture-prmt-history)))
(defun oc/inc (what text &rest fmtvars)
  "Ask user to include WHAT.  If user agrees return TEXT."
  (when (y-or-n-p (concat "Include " what "?"))
    (apply 'format text fmtvars)))

(setq org-capture-templates
      '(;; other entries
        ("t" "Inbox list item" entry
         (file+headline (expand-file-name org-main-file org-todos-directory) "Inbox")
         "* TODO %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n")
        ("d" "Docker task" entry
         (file+headline (expand-file-name org-main-file org-todos-directory) "Tasks")
         "* TODO gh:docker/%(oc/prmt \"project\" 'd-prj)#%(oc/prmt \"issue/pr\" 'd-issue) %?%(oc/inc \"feature content\" \" [/]\n- [ ] Implementation\n- [ ] Tests\n- [ ] Docs\")")
        ("j" "Journal entry" entry
         (file+datetree+prompt (expand-file-name org-journal-file org-root-directory))
         "- %?\n%a\n%i\n")
        ;; other entries
        ))

(org-add-link-type
 "grep" 'my/follow-grep-link
 )
(defun my/follow-grep-link (regexp)
  "Run `rgrep' with REGEXP and FOLDER as argument,
like this : [[grep:REGEXP:FOLDER]]."
  (setq expressions (split-string regexp ":"))
  (setq exp (nth 0 expressions))
  (grep-compute-defaults)
  (if (= (length expressions) 1)
      (progn
        (rgrep exp "*" (expand-file-name "./")))
    (progn
      (setq folder (nth 1 expressions))
      (rgrep exp "*" (expand-file-name folder))))
  )

(use-package pt
  :load-path "~/.emacs.d/lisp/pt/")

;; pt-regexp (regexp directory &optional args)
(org-add-link-type
 "pt" 'my/follow-pt-link)
(defun my/follow-pt-link (regexp)
  "Run `pt-regexp` with REXEP and FOLDER as argument,
like this : [[pt:REGEXP:FOLDER]]"
  (setq expressions (split-string regexp ":"))
  (setq exp (nth 0 expressions))
  (if (= (length expressions) 1)
      (progn
        (pt-regexp exp (expand-file-name "./")))
    (progn
      (setq folder (nth 1 expressions))
      (pt-regexp exp (file-name-as-directory (expand-file-name folder)))))
  )

(defvar yt-iframe-format
  ;; You may want to change your width and height.
  (concat "<iframe width=\"440\""
          " height=\"335\""
          " src=\"https://www.youtube.com/embed/%s\""
          " frameborder=\"0\""
          " allowfullscreen>%s</iframe>"))

(org-add-link-type
 "youtube"
 (lambda (handle)
   (browse-url
    (concat "https://www.youtube.com/embed/"
            handle)))
 (lambda (path desc backend)
   (cl-case backend
     (html (format yt-iframe-format
                   path (or desc "")))
     (latex (format "\href{%s}{%s}"
                    path (or desc "video"))))))

(org-add-link-type
 "gh" 'my/follow-gh-link)
(defun my/follow-gh-link (issue)
  "Browse github issue/pr specified"
  (setq expressions (split-string issue "#"))
  (setq project (nth 0 expressions))
  (setq issue (nth 1 expressions))
  (browse-url
   (format "https://github.com/%s/issues/%s" project issue)))

(setq org-link-abbrev-alist
      '(("gmane" . "http://thread.gmane.org/%s")
        ("google" . "https://www.google.com/search?q=%s")
        ("github" . "http://github.com/%s")
        ))

;; from http://endlessparentheses.com/use-org-mode-links-for-absolutely-anything.html
(org-add-link-type
 "tag" 'endless/follow-tag-link)

(defun endless/follow-tag-link (tag)
  "Display a list of TODO headlines with tag TAG.
With prefix argument, also display headlines without a TODO keyword."
  (org-tags-view (null current-prefix-arg) tag))

(setq org-src-fontify-natively t)
(setq org-html-htmlize-output-type 'css)
(setq org-html-htmlize-font-prefix "org-")
(org-babel-do-load-languages
 'org-babel-load-languages
 '( (perl . t)
    (ruby . t)
    (sh . t)
    (python . t)
    (emacs-lisp . t)
    ;; (golang . t)
    (haskell . t)
    (ditaa . t)
    ))

(defun my/org-insert-src-block (src-code-type)
  "Insert a `SRC-CODE-TYPE' type source code block in org-mode."
  (interactive
   (let ((src-code-types
          '("emacs-lisp" "python" "C" "sh" "java" "js" "clojure" "C++" "css"
            "calc" "dot" "gnuplot" "ledger" "R" "sass" "screen" "sql" "awk"
            "ditaa" "haskell" "latex" "lisp" "matlab" "org" "perl" "ruby"
            "sqlite" "rust" "scala" "golang" "restclient")))
     (list (ido-completing-read "Source code type: " src-code-types))))
  (progn
    (newline-and-indent)
    (insert (format "#+BEGIN_SRC %s\n" src-code-type))
    (newline-and-indent)
    (insert "#+END_SRC\n")
    (previous-line 2)
    (org-edit-src-code)))

(defun my/org-insert-html-block ()
  "Insert a `HTML-BLOCK` type in org-mode."
  (interactive
   (progn
     (newline-and-indent)
     (insert "#+BEGIN_HTML\n")
     (newline-and-indent)
     (insert "#+END_HTML\n")
     (previous-line 2))))


(defun my/org-insert-blockquote-block ()
  "Insert a `BLOCKQUOTE-BLOCK` type in org-mode."
  (interactive
   (progn
     (newline-and-indent)
     (insert "#+BEGIN_BLOCKQUOTE\n")
     (newline-and-indent)
     (insert "#+END_BLOCKQUOTE\n")
     (previous-line 2))))



(add-hook 'org-mode-hook
          '(lambda ()
             (local-set-key (kbd "C-c s e") 'org-edit-src-code)
             (local-set-key (kbd "C-c s i") 'my/org-insert-src-block)
             (local-set-key (kbd "C-c s h") 'my/org-insert-html-block)
             (local-set-key (kbd "C-c s b") 'my/org-insert-blockquote-block))
          'append)

(require 'org-archive)
(setq org-archive-location (concat org-archive-directory org-archive-file-pattern))

(defvar org-my-archive-expiry-days 9
  "The number of days after which a completed task should be auto-archived.
This can be 0 for immediate, or a floating point value.")

(defun org-archive-done-tasks ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\* \\(DONE\\|CANCELED\\) " nil t)
   (if (save-restriction
            (save-excursion
      	(org-narrow-to-subtree)
      	(search-forward ":LOGBOOK:" nil t)))
          (forward-line)
        (org-archive-subtree)
        (goto-char (line-beginning-position))))))

(defalias 'archive-done-tasks 'org-archive-done-tasks)

(setq org-tags-column -90)

;; Wish I could use taggroup but it doesn't seem to work..
(setq org-tag-alist '(
                      ("important" . ?i)
                      ("urgent" . ?u)
                      ("ongoing" . ?o)         ;; ongoing "project", use to filter big project that are on the go
                      ("next" . ?n)            ;; next "project"/"task", use to filter next things to do
                      ("@home" . ?h)           ;; needs to be done at home
                      ("@work" . ?w)           ;; needs to be done at work
                      ("@client" . ?c)         ;; needs to be done at a client place (consulting..)
                      ("dev" . ?e)             ;; this is a development task
                      ("infra" . ?a)           ;; this is a sysadmin/infra task
                      ("document" . ?d)        ;; needs to produce a document (article, post, ..)
                      ("download" . ?D)        ;; needs to download something
                      ("media" . ?m)           ;; this is a media (something to watch, listen, record, ..)
                      ("mail" . ?M)            ;; mail-related (to write & send or to read)
                      ("triage" . ?t)          ;; need "triage", tag it to easily find them
                      ("task" . ?a)            ;; a simple task (no project), the name is kinda misleading
                      ;; docker tags
                      ("docker")
                      ("compose")
                      ("libcompose")
                      ("distribution")
                      ("docs")
                      ("rancher")
                      ;; sites tags
                      ("sites")
                      ("vdf")
                      ("znk")
                      ;; configs tags
                      ("configs")
                      ("emacs")
                      ("i3")
                      ("shell")
                      ;; services
                      ("services")
                      ;; zenika
                      ("znk")
                      ("formation")
                      ("event")
                      ("tribu")
                      ("devops")
                      ("craftmanship")
                      ("client")
                      ))

;; Sometimes I change tasks I'm clocking quickly
;; this removes clocked tasks with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks 1)
;; Clock out when moving task to a done state
(setq org-clock-out-when-done 1)

(defadvice org-clock-in (after sacha activate)
  "Set this task's status to 'PROGRESS'."
  (org-todo "PROGRESS"))

(setq org-agenda-custom-commands
   '(("d" "Daily agenda and all TODOs"
         ((tags "PRIORITY=\"A\""
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                 (org-agenda-overriding-header "High-priority unfinished tasks:")))
          (agenda "" ((org-agenda-ndays 1)))
          (alltodo ""
                   ((org-agenda-sorting-strategy '(priority-down))
                    (org-agenda-skip-function '(or (org-agenda-skip-entry-if 'todo 'progress)
                                                   (org-agenda-skip-entry-if 'todo 'review)
                                                   (org-agenda-skip-entry-if 'todo 'done)
                                                   (vde/org-skip-subtree-if-habit)
                                                   (vde/org-skip-subtree-if-priority ?A)
                                                   (org-agenda-skip-if nil '(scheduled deadline))))
                    (org-agenda-overriding-header "ALL normal priority tasks:"))))
         ((org-agenda-compact-blocks t)))
        ("t" todo "TODO"
         ((org-agenda-sorting-strategy '(priority-down))
          (org-agenda-prefix-format "  Mixed: ")))
        ("p" todo "PROGRESS"
         ((org-agenda-sorting-strategy '(priority-down))
          (org-agenda-prefix-format "  Mixed: ")))
        ("r" todo "REVIEW"
         ((org-agenda-sorting-strategy '(priority-down))
          (org-agenda-prefix-format "  Mixed: ")))
        ("u" todo "PAUSED"
         ((org-agenda-sorting-strategy '(priority-down))
          (org-agenda-prefix-format "  Mixed: ")))
        ("b" todo "BLOCKED"
         ((org-agenda-sorting-strategy '(priority-down))
          (org-agenda-prefix-format "  Mixed: ")))
        ("o" "Ongoing projects" tags-todo "ongoing"
         ((org-agenda-sorting-strategy '(priority-down))
          (org-tags-exclude-from-inheritance '("ongoing"))
          (org-agenda-prefix-format "  Mixed: ")))
        ("n" "Next tasks" tags-todo "next"
         ((org-agenda-sorting-strategy '(priority-down))
          (org-tags-exclude-from-inheritance '("next"))
          (org-agenda-prefix-format "  Mixed: ")))
        ("i" "Triage tasks — to look" tags-todo "triage"
         ((org-agenda-sorting-strategy '(priority-down))
          (org-agenda-prefix-format "  Mixed: ")))
        ;; Timelines
        ("d" "Timeline for today" ((agenda "" ))
         ((org-agenda-ndays 1)
          (org-agenda-show-log t)
          (org-agenda-log-mode-items '(clock closed))
          (org-agenda-clockreport-mode t)
          (org-agenda-entry-types '())))
        ("w" "Weekly review" agenda ""
         ((org-agenda-span 7)
          (org-agenda-log-mode 1)))
        ("W" "Weekly review sans DAILY" agenda ""
         ((org-agenda-span 7)
          (org-agenda-log-mode 1)
          (org-agenda-tag-filter-preset '("-DAILY"))))
        ;; ("2" "Bi-weekly review" agenda "" ((org-agenda-span 14) (org-agenda-log-mode 1)))
        ;; Panic tasks : urgent & important
        ;; Probably the most important to do, but try not have to much of them..
        ("P" . "Panic -emergency-")
        ("Pt" "TODOs" tags-todo "important&urgent/!TODO"
         ((org-agenda-sorting-strategy '(priority-down))
          (org-agenda-prefix-format "  Mixed: ")))
        ("Pb" "BLOCKEDs" tags-todo "important&urgent/!BLOCKED"
         ((org-agenda-sorting-strategy '(priority-down))
          (org-agenda-prefix-format "  Mixed: ")))
        ("Pr" "REVIEWs" tags-todo "important&urgent/!REVIEW"
         ((org-agenda-sorting-strategy '(priority-down))
          (org-agenda-prefix-format "  Mixed: ")))
        ;; Kaizen tasks : important but not urgent
        ("K" . "Kaizen -improvement-")
        ("Kt" "TODOs" tags-todo "important&-urgent/!TODO"
         ((org-agenda-sorting-strategy '(priority-down))
          (org-agenda-prefix-format "  Mixed: ")))
        ("Kb" "BLOCKEDs" tags-todo "important&-urgent/!BLOCKED"
         ((org-agenda-sorting-strategy '(priority-down))
          (org-agenda-prefix-format "  Mixed: ")))
        ("Kr" "REVIEWs" tags-todo "important&-urgent/!REVIEW"
         ((org-agenda-sorting-strategy '(priority-down))
          (org-agenda-prefix-format "  Mixed: ")))
        ;; Social investment : urgent
        ("S" . "Social -investment-")
        ("St" "TODOs" tags-todo "-important&urgent/!TODO"
         ((org-agenda-sorting-strategy '(priority-down))
          (org-agenda-prefix-format "  Mixed: ")))
        ("Sb" "BLOCKEDs" tags-todo "-important&urgent/!BLOCKED"
         ((org-agenda-sorting-strategy '(priority-down))
          (org-agenda-prefix-format "  Mixed: ")))
        ("Sr" "REVIEWs" tags-todo "-important&urgent/!REVIEW"
         ((org-agenda-sorting-strategy '(priority-down))
          (org-agenda-prefix-format "  Mixed: ")))
        ;; Organics
        ("O" . "Organics -inspiration-")
        ("Ot" "TODOs" tags-todo "-important&-urgent/!TODO"
         ((org-agenda-sorting-strategy '(priority-down))
          (org-agenda-prefix-format "  Mixed: ")))
        ("Ob" "BLOCKEDs" tags-todo "-important&-urgent/!BLOCKED"
         ((org-agenda-sorting-strategy '(priority-down))
          (org-agenda-prefix-format "  Mixed: ")))
        ("Or" "REVIEWs" tags-todo "-important&-urgent/!REVIEW"
         ((org-agenda-sorting-strategy '(priority-down))
          (org-agenda-prefix-format "  Mixed: ")))
        ("N" search ""
         ((org-agenda-files '("~org/notes.org"))
          (org-agenda-text-search-extra-files nil)))))

(defun vde/org-skip-subtree-if-priority (priority)
  "Skip an agenda subtree if it has a priority of PRIORITY.

PRIORITY may be one of the characters ?A, ?B, or ?C."
  (let ((subtree-end (save-excursion (org-end-of-subtree t)))
        (pri-value (* 1000 (- org-lowest-priority priority)))
        (pri-current (org-get-priority (thing-at-point 'line t))))
    (if (= pri-value pri-current)
        subtree-end
   nil)))

(defun vde/org-skip-subtree-if-habit ()
  "Skip an agenda entry if it has a STYLE property equal to \"habit\"."
  (let ((subtree-end (save-excursion (org-end-of-subtree t))))
    (if (string= (org-entry-get nil "STYLE") "habit")
        subtree-end
   nil)))

(use-package htmlize
  :ensure t
  :defer t)
;;      (setq org-html-head "<link rel=\"stylesheet\" type=\"text/css\" hrefl=\"css/stylesheet.css\" />")
(setq org-html-include-timestamps nil)
;; (setq org-html-htmlize-output-type 'css)
(setq org-html-head-include-default-style nil)

(use-package ox-publish)
;; (use-package ox-rss)

(setq org-html-html5-fancy t)

;; Define some variables to write less :D
(setq sbr-base-directory (expand-file-name "sbr" org-sites-directory)
      sbr-publishing-directory (expand-file-name "sbr" org-publish-folder)
      znk-base-directory (expand-file-name "zenika" org-sites-directory)
      znk-preview-publishing-directory (expand-file-name "zenika" org-publish-folder)
      znk-publishing-directory (expand-file-name "zenika-export" org-publish-folder)
      vdf-base-directory (expand-file-name "vdf" org-sites-directory)
      vdf-site-directory (expand-file-name "blog" sites-folder)
      vdf-publishing-directory (expand-file-name "posts" (expand-file-name "content" vdf-site-directory))
      vdf-static-directory (expand-file-name "static" vdf-site-directory)
      vdf-css-publishing-directory (expand-file-name "css" vdf-static-directory)
      vdf-assets-publishing-directory vdf-static-directory)

;; Project
(setq org-publish-project-alist
      `(("sbr-notes"
         :base-directory ,sbr-base-directory
         :base-extension "org"
         :publishing-directory ,sbr-publishing-directory
         :makeindex t
         :exclude "FIXME"
         :recursive t
         :htmlized-source t
         :publishing-function org-html-publish-to-html
         :headline-levels 4
         :auto-preamble t
         :html-head "<link rel=\"stylesheet\" type=\"text/css\" href=\"style/style.css\" />"
         :html-preamble "<div id=\"nav\">
<ul>
<li><a href=\"/\" class=\"home\">Home</a></li>
</ul>
</div>"
         :html-postamble "<div id=\"footer\">
%a %C %c
</div>")
        ("sbr-static"
         :base-directory ,sbr-base-directory
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg"
         :publishing-directory ,sbr-publishing-directory
         :recursive t
         :publishing-function org-publish-attachment
         )
        ("sbr" :components ("sbr-notes" "sbr-static"))
        ("vdf-notes"
         :base-directory ,vdf-base-directory
         :base-extension "org"
         :publishing-directory ,vdf-publishing-directory
         :exclude "FIXME"
         :section-numbers nil
         :with-toc nil
         :with-drawers t
         :htmlized-source t
         :org-html-htmlize-output-type 'css
         :html-html5-fancy t
         :publishing-function org-html-publish-to-html
         :headline-levels 4
         :body-only t)
        ("vdf-static-css"
         :base-directory ,vdf-base-directory
         :base-extension "css"
         :publishing-directory ,vdf-css-publishing-directory
         :recursive t
         :publishing-function org-publish-attachment
         )
        ("vdf-static-assets"
         :base-directory ,vdf-base-directory
         :base-extension "png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg"
         :publishing-directory ,vdf-assets-publishing-directory
         :recursive t
         :publishing-function org-publish-attachment
         )
        ("vdf" :components ("vdf-notes" "vdf-static-css" "vdf-static-assets"))
        ("znk-notes"
         :base-directory ,znk-base-directory
         :base-extension "org"
         :publishing-directory ,znk-publishing-directory
         :exclude "FIXME"
         :section-numbers nil
         :with-toc nil
         :with-drawers t
         :recursive t
         :htmlized-source t
         :publishing-function org-html-publish-to-html
         :headline-levels 4
         :body-only t)
        ("znk-notes-previews"
         :base-directory ,znk-base-directory
         :base-extension "org"
         :publishing-directory ,znk-preview-publishing-directory
         :makeindex t
         :exclude "FIXME"
         :recursive t
         :htmlized-source t
         :publishing-function org-html-publish-to-html
         :headline-levels 4
         :auto-preamble t
         :html-head "<link rel=\"stylesheet\" type=\"text/css\" href=\"style/style.css\" />"
         :html-preamble "<div id=\"nav\">
<ul>
<li><a href=\"/\" class=\"home\">Home</a></li>
</ul>
</div>"
         :html-postamble "<div id=\"footer\">
%a %C %c
</div>")
        ("znk-static"
         :base-directory ,znk-base-directory
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg"
         :publishing-directory ,znk-publishing-directory
         :recursive t
         :publishing-function org-publish-attachment
         )
        ("znk" :components ("znk-notes" "znk-notes-previews" "znk-static"))
        ))

(use-package ox-ioslide
  :ensure t)

(use-package org-capture)
(use-package org-protocol)
(setq org-protocol-default-template-key "l")
(push '("l" "Link" entry (function org-handle-link)
        "* TODO %(org-wash-link)\nAdded: %U\n%(org-link-hooks)\n%?")
      org-capture-templates)

(defun org-wash-link ()
  (let ((link (caar org-stored-links))
        (title (cadar org-stored-links)))
    (setq title (replace-regexp-in-string
                 " - Stack Overflow" "" title))
    (org-make-link-string link title)))

(defvar org-link-hook nil)

(defun org-link-hooks ()
  (prog1
      (mapconcat #'funcall
                 org-link-hook
                 "\n")
    (setq org-link-hook)))

(defun org-handle-link ()
  (let ((link (caar org-stored-links))
        file)
    (cond ((string-match "^https://www.youtube.com/" link)
           (org-handle-link-youtube link))
          ((string-match (regexp-quote
                          "http://stackoverflow.com/") link)
           (find-file ((expand-file-name org-stackoverflow-file org-notes-directory)))
           (goto-char (point-min))
           (re-search-forward "^\\*+ +Questions" nil t))
          (t
           (find-file ((expand-file-name org-web-article-file org-notes-directory)))
           (goto-char (point-min))
           (re-search-forward "^\\*+ +Articles" nil t)))))

(defun org-handle-link-youtube (link)
  (lexical-let*
      ((file-name (org-trim
                   (shell-command-to-string
                    (concat
                     "youtube-dl \""
                     link
                     "\""
                     " -o \"%(title)s.%(ext)s\" --get-filename"))))
       (dir videos-folder)
       (full-name
        (expand-file-name file-name dir)))
    (add-hook 'org-link-hook
              (lambda ()
                (concat
                 (org-make-link-string dir dir)
                 "\n"
                 (org-make-link-string full-name file-name))))
    (async-shell-command
     (format "youtube-dl \"%s\" -o \"%s\"" link full-name))
    (find-file (org-expand "ent.org"))
    (goto-char (point-min))
    (re-search-forward "^\\*+ +videos" nil t)))

(defun org-open-main-org-file ()
  "Open the main org-mode file (where lies all my things"
  (interactive)
  (find-file (expand-file-name org-main-file org-todos-directory)))
(defun org-open-notes-folder ()
  "Open the notes folder"
  (interactive)
  (find-file org-notes-directory))

(use-package org
  :ensure org
  :bind* (("M-m o a"   . org-agenda)
          ("M-m o c"   . org-capture)
          ("M-m o i"   . org-insert-link)
          ("M-m o s"   . org-store-link)
          ("M-m o S"   . org-list-make-subtree)
          ("M-m o A"   . org-archive-subtree)
          ("M-m o g"   . org-goto)
          ("M-m o L"   . org-toggle-link-display)
          ("M-m o I"   . org-toggle-inline-images)
          ("M-m o k"   . org-cut-subtree)
          ("M-m o R"   . org-refile)
          ("M-m o y"   . org-copy-subtree)
          ("M-m o h"   . org-toggle-heading)
          ("M-m o e"   . org-export-dispatch)
          ("M-m o u"   . org-update-dblock)
          ("M-m o U"   . org-update-all-dblocks)
          ("M-m o O"   . org-footnote)
          ("M-m o N"   . org-add-note)
          ("M-m o E"   . org-set-effort)
          ("M-m o B"   . org-table-blank-field)
          ("M-m o <"   . org-date-from-calendar)
          ("M-m o >"   . org-goto-calendar)
          ("M-m o m"   . org-mark-subtree)
          ("M-m o RET" . org-open-at-point)
          ("M-m o p"   . org-open-main-file)
          ("M-m o n"   . org-open-notes-folder)))
(which-key-add-key-based-replacements
  "M-m o" "org mode prefix")

(defhydra sk/hydra-org-organize (:color red
                                        :hint nil)
  "
       ^Meta^    ^Shift^   ^Shift-Meta^ ^Shift-Ctrl^  ^Move^        ^Item^
      ^^^^^^^^^^^^^--------------------------------------------------------------------
       ^ ^ _k_ ^ ^   ^ ^ _K_ ^ ^   ^ ^ _p_ ^ ^      ^ ^ _P_ ^ ^       _<_: promote  _u_: up     _q_: quit
       _h_ ^+^ _l_   _H_ ^+^ _L_   _b_ ^+^ _f_      _B_ ^+^ _F_       _>_: demote   _d_: down
       ^ ^ _j_ ^ ^   ^ ^ _J_ ^ ^   ^ ^ _n_ ^ ^      ^ ^ _N_ ^ ^
      "
  ("h" org-metaleft)
  ("l" org-metaright)
  ("j" org-metadown)
  ("k" org-metaup)
  ("H" org-shiftleft)
  ("L" org-shiftright)
  ("J" org-shiftdown)
  ("K" org-shiftup)
  ("b" org-shiftmetaleft)
  ("f" org-shiftmetaright)
  ("n" org-shiftmetadown)
  ("p" org-shiftmetaup)
  ("B" org-shiftcontrolleft)
  ("F" org-shiftcontrolright)
  ("P" org-shiftcontroldown)
  ("N" org-shiftcontrolup)
  ("<" org-promote)
  (">" org-demote)
  ("d" org-move-item-down)
  ("u" org-move-item-up)
  ("q" nil :color blue))
(bind-keys*
 ("M-m o o" . sk/hydra-org-organize/body))

(defhydra sk/hydra-org-todo (:color red
                                    :hint nil)
  "
 _d_: deadline    _o_: over    _s_: schedule   _c_: check   _q_: quit
"
  ("d" org-deadline :color blue)
  ("o" org-deadline-close :color blue)
  ("s" org-schedule :color blue)
  ("c" org-check-deadlines)
  ("q" nil :color blue))
(bind-keys*
 ("M-m o D" . sk/hydra-org-todo/body))

(defhydra sk/hydra-org-checkbox (:color pink
                                        :hint nil)
  "
 _t_: toggle   _s_: stats    _r_: reset    _c_: count    _q_: quit
"
  ("t" org-toggle-checkbox)
  ("c" org-update-checkbox-count-maybe)
  ("r" org-reset-checkbox-state-subtree)
  ("s" org-update-statistics-cookies)
  ("q" nil :color blue))
(bind-keys*
 ("M-m o b" . sk/hydra-org-checkbox/body))

(defhydra sk/hydra-org-property (:color red
                                        :hint nil)
  "
 _i_: insert  _p_: property   _s_: set    _d_: delete    _t_: toggle    _q_: quit
"
  ("i" org-insert-drawer)
  ("p" org-insert-property-drawer)
  ("s" org-set-property)
  ("d" org-delete-property)
  ("t" org-toggle-ordered-property)
  ("q" nil :color blue))

(bind-keys*
 ("M-m o P" . sk/hydra-org-property/body))

(defhydra sk/hydra-org-clock (:color red
                                     :hint nil)
  "
 ^Clock^                     ^Timer^     ^Stamp^
^^^^^^^^^^-------------------------------------------------
 _i_: in       _z_: resolve    _b_: begin  _t_: stamp       _q_: quit
 _o_: out      _l_: last       _e_: end    _u_: inactive
 _r_: report   _c_: cancel     _m_: timer
 _d_: display  _g_: goto       _s_: set
"
  ("i" org-clock-in)
  ("o" org-clock-out)
  ("r" org-clock-report)
  ("z" org-resolve-clocks)
  ("c" org-clock-cancel)
  ("d" org-clock-display)
  ("l" org-clock-in-last)
  ("g" org-clock-goto)
  ("m" org-timer)
  ("s" org-timer-set-timer)
  ("b" org-timer-start)
  ("e" org-timer-stop)
  ("t" org-time-stamp)
  ("u" org-time-stamp-inactive)
  ("q" nil :color blue))
(bind-keys*
 ("M-m o C" . sk/hydra-org-clock/body))

(defhydra sk/hydra-org-tables (:color red
                                      :hint nil)
  "
 ^Field^   ^Shift^   ^Insert^      ^Delete^         ^Field^     ^Table^      ^Formula^
^^^^^^^^^^^^------------------------------------------------------------------------------
 ^ ^ _k_ ^ ^   ^ ^ _K_ ^ ^   _r_: row      _dr_: del row    _e_: edit   _a_: align   _+_: sum    _q_: quit
 _h_ ^+^ _l_   _H_ ^+^ _L_   _c_: column   _dc_: del col    _b_: blank  _|_: create  _=_: eval
 ^ ^ _j_ ^ ^   ^ ^ _J_ ^ ^   _-_: hline                   _i_: info             _f_: edit
"
  ("a" org-table-align)
  ("l" org-table-next-field)
  ("h" org-table-previous-field)
  ("j" org-table-end-of-field)
  ("k" org-table-beginning-of-field)
  ("r" org-table-insert-row)
  ("c" org-table-insert-column)
  ("-" org-table-insert-hline)
  ("J" org-table-move-row-down)
  ("K" org-table-move-row-up)
  ("H" org-table-move-column-left)
  ("L" org-table-move-column-right)
  ("dr" org-table-kill-row)
  ("dc" org-table-delete-column)
  ("b" org-table-blank-field)
  ("e" org-table-edit-field)
  ("i" org-table-field-info)
  ("+" org-table-sum)
  ("=" org-table-eval-formula)
  ("f" org-table-edit-formulas)
  ("|" org-table-create-or-convert-from-region)
  ("q" nil :color blue))
(bind-keys*
 ("M-m o m" . sk/hydra-org-tables/body))

(defhydra sk/hydra-org-jump (:color pink
                                    :hint nil)
  "
 ^Outline^          ^Item^   ^Table^   ^Block^   ^Link^
 ^^^^^^^^^^^-------------------------------------------------------------------------------
 ^ ^ _k_ ^ ^   ^ ^ _K_ ^ ^   ^ ^ _u_ ^ ^   ^ ^ ^ ^ ^ ^   ^ ^ _p_ ^ ^   ^ ^ _P_ ^ ^    _q_ quit
 _h_ ^+^ _l_   ^ ^ ^+^ ^ ^   ^ ^ ^+^ ^ ^   _b_ ^+^ _f_   ^ ^ ^+^ ^ ^   ^ ^ ^+^ ^ ^
 ^ ^ _j_ ^ ^   ^ ^ _J_ ^ ^   ^ ^ _d_ ^ ^   ^ ^ ^ ^ ^ ^   ^ ^ _n_ ^ ^   ^ ^ _N_ ^ ^
"
  ("j" outline-next-visible-heading)
  ("k" outline-previous-visible-heading)
  ("l" org-down-element)
  ("h" org-up-element)
  ("J" org-forward-heading-same-level)
  ("K" org-backward-heading-same-level)
  ("u" org-next-item)
  ("d" org-previous-item)
  ("f" org-table-next-field)
  ("b" org-table-previous-field)
  ("n" org-next-block)
  ("p" org-previous-block)
  ("N" org-next-link)
  ("P" org-previous-link)
  ("q" nil :color blue))
(bind-keys*
 ("M-m o j" . sk/hydra-org-jump/body))

(defhydra sk/hydra-org-agenda-view (:color red
                                           :hint nil)
  "
 _d_: day        _g_: time grid    _a_: arch-trees    _L_: log closed clock
 _w_: week       _i_: inactive     _A_: arch-files    _c_: log clock check
 _t_: fortnight  _f_: follow       _r_: report        _l_: log mode toggle
 _m_: month      _e_: entry        _D_: diary         _q_: quit
 _y_: year       _!_: deadlines    _R_: reset
"
  ("R" org-agenda-reset-view)
  ("d" org-agenda-day-view)
  ("w" org-agenda-week-view)
  ("t" org-agenda-fortnight-view)
  ("m" org-agenda-month-view)
  ("y" org-agenda-year-view)
  ("l" org-agenda-log-mode)
  ("L" (org-agenda-log-mode '(4)))
  ("c" (org-agenda-log-mode 'clockcheck))
  ("f" org-agenda-follow-mode)
  ("a" org-agenda-archives-mode)
  ("A" (org-agenda-archives-mode 'files))
  ("r" org-agenda-clockreport-mode)
  ("e" org-agenda-entry-text-mode)
  ("g" org-agenda-toggle-time-grid)
  ("D" org-agenda-toggle-diary)
  ("!" org-agenda-toggle-deadlines)
  ("i"
   (let ((org-agenda-include-inactive-timestamps t))
     (org-agenda-check-type t 'timeline 'agenda)
     (org-agenda-redo)))
  ("q" nil :color blue))
(bind-keys*
 ("M-m o v" . sk/hydra-org-agenda-view/body))

(use-package magit
  :ensure t
  :commands magit-status
  :bind* (("C-c g" . magit-status)
          ("M-m s g" . magit-status))
  :config
  (setq magit-completing-read-function 'ivy-completing-read)
  (add-to-list 'magit-no-confirm 'stage-all-changes)
  (setq magit-push-always-verify nil)
  (setq magit-last-seen-setup-instructions "2.1.0"))

(use-package git-annex
  :ensure t)
(use-package magit-annex
  :bind* (("M-m s @" . magit-annex-sync))
  :ensure t)

(use-package yagist
  :ensure t
  :commands (yagist-region-or-buffer
             yagist-region-or-buffer-private)
  :bind* (("M-m g p" . yagist-region-or-buffer)
          ("M-m g P" . yagist-region-or-buffer-private))
  :init
  (setq yagist-encrypt-risky-config t))

(use-package browse-at-remote
  :ensure f
  :bind* (("M-m g i" . browse-at-remote)))

(use-package company
  :ensure t
  :commands (company-mode
             company-complete
             company-complete-common
             company-complete-common-or-cycle
             company-files
             company-dabbrev
             company-ispell
             company-c-headers
             company-elisp)
  :init
  (setq company-minimum-prefix-length 2)
  (setq company-tooltip-limit 20)                      ; bigger popup window
  (setq company-idle-delay .3)                         ; decrease delay before autocompletion popup shows
  (setq company-echo-delay 0)                          ; remove annoying blinking
  (setq company-begin-commands '(self-insert-command)) ; start autocompletion only after typing
  (add-hook 'after-init-hook 'global-company-mode)
  :bind (("M-t"   . company-complete)
         ("C-c f" . company-files)
         ("C-c a" . company-dabbrev)
         ("C-c d" . company-ispell)
         :map company-active-map
         ("C-n"    . company-select-next)
         ("C-p"    . company-select-previous)
         ([return] . company-complete-selection)
         ("C-w"    . backward-kill-word)
         ("C-c"    . company-abort)
         ("C-c"    . company-search-abort))
  :diminish (company-mode . "ς")
  :config
  (use-package company-quickhelp
    :ensure t
    :config
    (company-quickhelp-mode)))

(use-package eshell
  :commands (eshell)
  :bind* (("M-m SPC e" . sk/eshell-vertical)
          ("M-m SPC E" . sk/eshell-horizontal))
  :init
  (setq eshell-glob-case-insensitive t
        eshell-scroll-to-bottom-on-input 'this
        eshell-buffer-shorthand t
        eshell-history-size 1024
        eshell-cmpl-ignore-case t
        eshell-prompt-regexp " λ "
        eshell-aliases-file (concat user-emacs-directory ".eshell-aliases")
        eshell-last-dir-ring-size 512)
  (add-hook 'shell-mode-hook 'goto-address-mode))

;; Vertical split eshell
(defun sk/eshell-vertical ()
  "opens up a new shell in the directory associated with the current buffer's file."
  (interactive)
  (let* ((parent (if (buffer-file-name)
                     (file-name-directory (buffer-file-name))
                   default-directory))
         (name (car (last (split-string parent "/" t)))))
    (split-window-right)
    (other-window 1)
    (eshell "new")
    (rename-buffer (concat "*eshell: " name "*"))
    (eshell-send-input)))

;; Horizontal split eshell
(defun sk/eshell-horizontal ()
  "opens up a new shell in the directory associated with the current buffer's file."
  (interactive)
  (let* ((parent (if (buffer-file-name)
                     (file-name-directory (buffer-file-name))
                   default-directory))
         (name (car (last (split-string parent "/" t)))))
    (split-window-below)
    (other-window 1)
    (eshell "new")
    (rename-buffer (concat "*eshell: " name "*"))
    (eshell-send-input)))

(add-hook 'shell-mode-hook 'wcy-shell-mode-hook-func)
(defun wcy-shell-mode-hook-func  ()
  (set-process-sentinel (get-buffer-process (current-buffer))
                        #'shell-mode-kill-buffer-on-exit)
  )
(defun shell-mode-kill-buffer-on-exit (process state)
  (message "%s" state)
  (if (or
       (string-match "exited abnormally with code.*" state)
       (string-match "finished" state))
      (kill-buffer (current-buffer))))

;;
(eval-after-load "em-ls"
  '(progn
     (defun ted-eshell-ls-find-file-at-point (point)
       "RET on Eshell's `ls' output to open files."
       (interactive "d")
       (find-file (buffer-substring-no-properties
                   (previous-single-property-change point 'help-echo)
                   (next-single-property-change point 'help-echo))))

     (defun pat-eshell-ls-find-file-at-mouse-click (event)
       "Middle click on Eshell's `ls' output to open files.
 From Patrick Anderson via the wiki."
       (interactive "e")
       (ted-eshell-ls-find-file-at-point (posn-point (event-end event))))

     (let ((map (make-sparse-keymap)))
       (define-key map (kbd "RET")      'ted-eshell-ls-find-file-at-point)
       (define-key map (kbd "<return>") 'ted-eshell-ls-find-file-at-point)
       (define-key map (kbd "<mouse-2>") 'pat-eshell-ls-find-file-at-mouse-click)
       (defvar ted-eshell-ls-keymap map))

     (defadvice eshell-ls-decorated-name (after ted-electrify-ls activate)
       "Eshell's `ls' now lets you click or RET on file names to open them."
       (add-text-properties 0 (length ad-return-value)
                            (list 'help-echo "RET, mouse-2: visit this file"
                                  'mouse-face 'highlight
                                  'keymap ted-eshell-ls-keymap)
                            ad-return-value)
       ad-return-value)))

(setq compilation-scroll-output t)
;; I'm not scared of saving everything.
(setq compilation-ask-about-save nil)
;; Stop on the first error.
(setq compilation-scroll-output 'next-error)
;; Don't stop on info or warnings.
(setq compilation-skip-threshold 2)

(require 'ansi-color)
(defun my/colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'my/colorize-compilation-buffer)

(use-package quickrun
  :ensure t
  :commands (quickrun
             quickrun-region
             quickrun-with-arg
             quickrun-shell
             quickrun-compile-only
             quickrun-replace-region))

(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  :init (add-hook 'after-init-hook #'global-flycheck-mode)
  :config
  (progn
    (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
    (setq flycheck-indication-mode 'right-fringe)))

(use-package editorconfig
  :ensure t
  :demand t
  :config
  (editorconfig-mode 1))

(use-package markdown-mode
  :ensure t
  :mode ("\\.markdown\\'" "\\.mkd\\'" "\\.md\\'")
  :config
  (use-package pandoc-mode
    :ensure t
    :mode ("\\.markdown\\'" "\\.mkd\\'" "\\.md\\'")))

(use-package yaml-mode
  :ensure t
  :mode "\\.yml$")

(use-package toml-mode
  :ensure t)

(use-package paredit
  :ensure t)
(use-package rainbow-mode
  :ensure t)
(use-package rainbow-delimiters
  :ensure t)
(use-package highlight-parentheses
  :ensure t)

(defun my/lisps-mode-hook ()
  (paredit-mode t)
  (rainbow-delimiters-mode t)
  (highlight-parentheses-mode t)
  )

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (my/lisps-mode-hook)
            (eldoc-mode 1))
          )

(add-hook 'sql-interactive-mode-hook
          (lambda ()
            (toggle-truncate-lines t)))

(use-package dockerfile-mode
  :ensure t)

(use-package restclient
  :ensure t)
(use-package ob-restclient
  :ensure t
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((restclient . t))))

(use-package nix-mode
  :ensure t)

(use-package company-nixos-options
  :ensure t
  :config
  (add-to-list 'company-backends 'company-nixos-options))

(use-package nix-sandbox
  :ensure t)

(use-package direnv
  :ensure t)

;; The folder is by default $HOME/.emacs.d/provided
(setq user-emacs-provided-directory (concat user-emacs-directory "provided/"))
;; Regexp to find org files in the folder
(setq provided-configuration-file-regexp "\\`[^.].*\\.org\\'")
;; Define the function
(defun load-provided-configuration (dir)
  "Load org file from =use-emacs-provided-directory= as configuration with org-babel"
  (unless (file-directory-p dir) (error "Not a directory '%s'" dir))
  (dolist (file (directory-files dir nil provided-configuration-file-regexp nil) nil)
    (unless (member file '("." ".."))
      (let ((file (concat dir file)))
        (unless (file-directory-p file)
          (message "loading file %s" file)
          (org-babel-load-file file)
          )
        ))
    )
  )
;; Load it
(load-provided-configuration user-emacs-provided-directory)

(defun tangle-if-config ()
  "If the current buffer is a config '*.org' the code-blocks are
    tangled, and the tangled file is compiled."
  (when (member (file-name-nondirectory buffer-file-name) '("emacs.org" "provided/go-config.org"))
    (tangle-config buffer-file-name)))

(defun tangle-config-sync (file-name)
  (interactive)

  ;; Avoid running hooks when tangling.
  (let* ((prog-mode-hook nil)
         (src  file-name)
         ;; (dest (expand-file-name "emacs.el"  user-emacs-directory))
         (dest (format "%s.el" (file-name-sans-extension file-name))))
    (message (format "%s -> %s" src dest))
    (require 'ob-tangle)
    (org-babel-tangle-file src dest)
    (if (byte-compile-file dest)
        (byte-compile-dest-file dest)
   (with-current-buffer byte-compile-log-buffer
        (buffer-string)))))

(defun tangle-config-async (file-name)
  (async-start
   (lambda ()
     ;; make async emacs aware of packages (for byte-compilation)
     (package-initialize)
     (setq package-enable-at-startup nil)
     (require 'org)

     ;; Avoid running hooks when tangling.
     (let* ((prog-mode-hook nil)
            (src  file-name)
            (dest (format "%s.el" (file-name-sans-extension file-name))))
    (message (format "%s -> %s" src dest))
    (require 'ob-tangle)
    (org-babel-tangle-file src dest)
    (if (byte-compile-file dest)
           (byte-compile-dest-file dest)
         (with-current-buffer byte-compile-log-buffer
           (buffer-string))))
     )))

(defun tangle-config (file-name)
  "Tangle init.org asynchronously."

  (interactive)
  (message (format "Tangling %s" file-name))
  (tangle-config-async file-name))

(defun tangle-emacs-config ()
  (interactive)
  (message (format "Tangling emacs config"))
  (tangle-config-sync (substitute-env-in-file-name "$HOME/src/configs/emacs-config/.emacs.d/emacs.org")))
