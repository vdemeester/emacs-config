;;; init.el --- emacs configuration entrypoint -*- lexical-binding: t; -*-
;; Time-stamp: <2018-10-01 12:20:01 vincent>
;; Author: Vincent Demeester

;;; Commentary:

;; Initialize the Emacs configuration

;;; Code:

(defconst emacs-start-time (current-time))
(defvar file-name-handler-alist-old file-name-handler-alist)

(setq package-enable-at-startup nil
      file-name-handler-alist nil
      message-log-max 16384
      gc-cons-threshold 402653184
      gc-cons-percentage 0.6
      auto-window-vscroll nil)

(add-hook 'after-init-hook
          `(lambda ()
             (setq file-name-handler-alist file-name-handler-alist-old
                   gc-cons-threshold 800000
                   gc-cons-percentage 0.1)
             (garbage-collect)) t)

(let ((minver 25))
  (unless (>= emacs-major-version minver)
    (error "Your Emacs is too old -- this configuration requrise v%s or higher" minver)))

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-language-environment 'utf-8)
(set-selection-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)

;;; package setup
(require 'package)

(setq package-archives
      '(("gnu-elpa" . "http://elpa.gnu.org/packages/")
	("melpa-stable" . "https://stable.melpa.org/packages/")
	("melpa" . "https://melpa.org/packages/")
	("org" . "https://orgmode.org/elpa/")))

(package-initialize)

(setq load-prefer-newer t)              ; Always load newer compiled files
(setq ad-redefinition-action 'accept)   ; Silence advice redefinition warnings

;; Bootstrap `use-package'
(setq use-package-always-pin "melpa-stable")
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Init `delight'
(unless (package-installed-p 'delight)
  (package-refresh-contents)
  (package-install 'delight))

(eval-when-compile (require 'use-package))

(use-package dash) ; A modern list library

(use-package use-package-chords :ensure t :pin melpa)
(use-package use-package-ensure-system-package :ensure t :pin melpa)

(require 'subr-x)
(require 'time-date)

;;; Initialization
(setq inhibit-default-init t)           ; Disable the site default settings

(use-package exec-path-from-shell       ; Set up environment variables
  :if (display-graphic-p)
  :unless (eq system-type 'windows-nt)
  :config
  (setq exec-path-from-shell-variables
        '("PATH"               ; Full path
          "INFOPATH"           ; Info directories
          "GOPATH"             ; Golang path
          ))

  (exec-path-from-shell-initialize))

;; Set separate custom file for the customize interface
(defconst vde-custom-file (locate-user-emacs-file "custom.el")
  "File used to store settings from Customization UI.")

(use-package cus-edit                   ; Set up custom.el
  :defer t
  :config
  (setq
   custom-file vde-custom-file
   custom-buffer-done-kill nil          ; Kill when existing
   custom-buffer-verbose-help nil       ; Remove redundant help text
   custom-unlispify-tag-names nil       ; Show me the real variable name
   custom-unlispify-menu-entries nil)
  :init (load vde-custom-file 'no-error 'no-message))

(use-package no-littering               ; Keep .emacs.d clean
  :config
  (require 'recentf)
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory)

  (setq
   create-lockfiles nil
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)

  (setq
   backup-directory-alist
   `((".*" . ,(no-littering-expand-var-file-name "backup/")))
   auto-save-file-name-transforms
   `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

(use-package server                     ; The server of `emacsclient'
  :config (or (server-running-p) (server-mode)))

(use-package pinentry
  :config
  (setenv "INSIDE_EMACS" (format "%s,comint" emacs-version))
  (pinentry-start))

;; Confirm before quitting Emacs
(setq confirm-kill-emacs #'y-or-n-p)

;;; Default rg arguments
;; https://github.com/BurntSushi/ripgrep
(defconst vde/rg-arguments
  `("--no-ignore-vcs"                   ;Ignore files/dirs ONLY from `.ignore'
    "--line-number"                     ;Line numbers
    "--smart-case"
    "--follow"                 ;Follow symlinks
    "--max-columns" "150"      ;Emacs doesn't handle long line lengths very well
    "--ignore-file" ,(expand-file-name ".ignore" (getenv "HOME")))
  "Default rg arguments used in the functions in `counsel' and `projectile'
packages.")

;;; Require files under ~/.emacs.d/lisp
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Enable `vde-mode' unless `disable-pkg-vde-mode' is set to `t' in
;; `setup-var-overrides.el'.
(when (not (bound-and-true-p disable-pkg-setup-mode))
  (use-package setup-vde-mode))

(use-package setup-style)
(use-package setup-keybindings)
(use-package setup-fold)
(use-package setup-ivy)
(use-package setup-vcs)
(use-package setup-dired)
(use-package setup-search)
(use-package setup-files)
(use-package setup-editing)
(use-package setup-multiple-cursors)
(use-package setup-navigating)
(use-package setup-windows)
(use-package setup-buffers)
(use-package setup-company)
(use-package setup-projectile)
(use-package setup-shells)
(use-package setup-compile)
(use-package setup-org)
(use-package setup-deft)
;; Programming languages
(use-package setup-nix)
(use-package setup-go)
(use-package setup-rust)
(use-package setup-web)
(use-package setup-docker)
(use-package setup-hydras)

(use-package command-log-mode
  :bind (("C-c e M" . command-log-mode)
         ("C-c e L" . clm/open-command-log-buffer)))

(eval-and-compile
  (define-inline emacs-path (path)
    (expand-file-name path user-emacs-directory)))

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; Finalization

(let ((elapsed (float-time (time-subtract (current-time)
                                          emacs-start-time))))
  (message "Loading %s...done (%.3fs)" load-file-name elapsed))

(add-hook 'after-init-hook
          `(lambda ()
             (let ((elapsed
                    (float-time
                     (time-subtract (current-time) emacs-start-time))))
               (message "Loading %s...done (%.3fs) [after-init]"
                        ,load-file-name elapsed))) t)

(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;;; init.el ends here
