
;;; -*- lexical-binding: t -*-
(setq-default lexical-binding t)

(let ((minver 24))
  (unless (>= emacs-major-version minver)
    (error "Your Emacs is too old -- this configuration requrise v%s or higher" minver)))

;; Add custom lisp files to load-path
(add-to-list 'load-path (concat user-emacs-directory "lisp"))
(add-to-list 'load-path (concat user-emacs-directory "config"))
(add-to-list 'load-path (concat user-emacs-directory "lisp/use-package"))

(setq
 desktop-directory (substitute-env-in-file-name "$HOME/desktop")
 downloads-directory (expand-file-name "downloads" desktop-directory)
 videos-directory (expand-file-name "videos" desktop-directory)
 music-directory (expand-file-name "music" desktop-directory)
 pictures-directory (expand-file-name "pictures" desktop-directory)
 github-general-folder (substitute-env-in-file-name "$HOME/src/github.com")
 github-username "vdemeester"
 github-personal-folder (expand-file-name github-username github-general-folder))

(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'blink-cursor-mode) (blink-cursor-mode -1))

(if (fboundp 'tooltip-mode) (tooltip-mode -1) (setq tooltip-use-echo-area t))'

(require 'package)
(package-initialize)

(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(setq use-package-always-pin "melpa-stable")

(when (not package-archive-contents)
  (package-refresh-contents))

(require 'use-package)

(setq load-prefer-newer t)

(use-package auto-compile
  :ensure t
  :config
  (progn
    (auto-compile-on-load-mode)
    (auto-compile-on-save-mode)))

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

;; (require 'evil-config)
(use-package org-config)

(use-package dashboard
  :ensure t
  :diminish dashboard-mode
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
