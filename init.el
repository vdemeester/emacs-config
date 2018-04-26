;;; -*- lexical-binding: t; -*-
(require 'package)

(let ((minver 25))
  (unless (>= emacs-major-version minver)
(error "Your Emacs is too old -- this configuration requrise v%s or higher" minver)))

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
(setq message-log-max 10000) ; Debugging

;; Allow more than 800Kb cache during init
(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)

(defun vde-set-gc-threshold ()
  "Reset `gc-cons-threshold' and `gc-cons-percentage' to their default values."
  (setq gc-cons-threshold 16777216
gc-cons-percentage 0.1))

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

(use-package dash                       ; A modern list library
  :ensure t)

(require 'subr-x)
(require 'time-date)

;;; Initialization
(setq inhibit-default-init t)           ; Disable the site default settings

(use-package exec-path-from-shell       ; Set up environment variables
  :ensure t
  :if (display-graphic-p)
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
  :ensure t
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
(when (not (bound-and-true-p disable-pkg-vde-mode))
  (require 'vde-mode))

(use-package vde-style)
(use-package vde-keybindings)
(use-package vde-ivy)
(use-package vde-vcs)
(use-package vde-dired)
(use-package vde-search)
(use-package vde-files)
(use-package vde-editing)
(use-package vde-windows)
(use-package vde-buffers)
(use-package vde-company)
(use-package vde-projectile)
(use-package vde-shells)
(use-package vde-imenu)
(use-package vde-compile)
(use-package vde-org)
;; Programming languages
(use-package vde-nix)
(use-package vde-go)
(use-package vde-groovy)
(use-package vde-docker)
;; Fun stuff
(use-package vde-media)

;; Reset default values
(add-hook 'emacs-startup-hook #'vde-set-gc-threshold)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; init.el ends here
