
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

(use-package diminish
  :ensure t)

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

(use-package exec-path-from-shell
  :ensure t
  :config
  (setq exec-path-from-shell-arguments (list "-l"))
  (setq exec-path-from-shell-check-startup-files nil)
  (add-to-list 'exec-path-from-shell-variables "SHELL")
  (add-to-list 'exec-path-from-shell-variables "GOPATH")
  (add-to-list 'exec-path-from-shell-variables "ENVIRONMENT_SETUP_DONE")
  (add-to-list 'exec-path-from-shell-variables "PYTHONPATH")
  (exec-path-from-shell-initialize))

(setq sentence-end-double-space nil)

(setq-default fill-column 80)

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

(setq x-stretch-cursor t)

(use-package visual-config)
(use-package org-config)
(use-package nix-config)
(use-package go-config)
(use-package vcs-config)
(use-package navigation-config)
