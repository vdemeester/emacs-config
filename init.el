;;; -*- lexical-binding: t; -*-
(defconst emacs-start-time (current-time))

(when (< emacs-major-version 27)
  (setq package-enable-at-startup nil)

  (setq frame-inhibit-implied-resize t)

  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (horizontal-scroll-bar-mode -1)

  (setq gc-cons-threshold 402653184
        gc-cons-percentage 0.6)

  (add-hook 'after-init-hook
            `(lambda ()
               (setq gc-cons-threshold 16777216 ; 16mb
                     gc-cons-percentage 0.1)
               (garbage-collect)) t))

(defvar file-name-handler-alist-old file-name-handler-alist)

(setq file-name-handler-alist nil
      message-log-max 16384
      auto-window-vscroll nil)

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
      '(("melpa" . "http://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("org" . "https://orgmode.org/elpa/")
        ("gnu" . "https://elpa.gnu.org/packages/")))

(setq package-archive-priorities
      '(("melpa-stable" . 4)
        ("melpa" .  3)
        ("org" . 2)
        ("gnu" . 1)))

(require 'tls)

;; From https://github.com/hlissner/doom-emacs/blob/5dacbb7cb1c6ac246a9ccd15e6c4290def67757c/core/core-packages.el#L102
(setq gnutls-verify-error (not (getenv "INSECURE")) ; you shouldn't use this
      tls-checktrust gnutls-verify-error
      tls-program (list "gnutls-cli --x509cafile %t -p %p %h"
                        ;; compatibility fallbacks
                        "gnutls-cli -p %p %h"
                        "openssl s_client -connect %h:%p -no_ssl2 -no_ssl3 -ign_eof"))

;; Initialise the packages, avoiding a re-initialisation.
(unless (bound-and-true-p package--initialized)
  (setq package-enable-at-startup nil)
  (package-initialize))

(setq load-prefer-newer t)              ; Always load newer compiled files
(setq ad-redefinition-action 'accept)   ; Silence advice redefinition warnings

;; Init `delight'
(unless (package-installed-p 'delight)
  (package-refresh-contents)
  (package-install 'delight))

;; Configure `use-package' prior to loading it.
(eval-and-compile
  (setq use-package-always-ensure nil)
  (setq use-package-always-defer nil)
  (setq use-package-always-demand nil)
  (setq use-package-expand-minimally nil)
  (setq use-package-enable-imenu-support t))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(use-package dash) ; A modern list library

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
(defconst vde/custom-file (locate-user-emacs-file "custom.el")
  "File used to store settings from Customization UI.")

(use-package cus-edit                   ; Set up custom.el
  :defer t
  :config
  (setq
   custom-file vde/custom-file
   custom-buffer-done-kill nil          ; Kill when existing
   custom-buffer-verbose-help nil       ; Remove redundant help text
   custom-unlispify-tag-names nil       ; Show me the real variable name
   custom-unlispify-menu-entries nil)
  :init (load vde/custom-file 'no-error 'no-message))

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

(setenv "PAGER" "cat")
(setenv "TERM" "xterm-256color")
(setenv "NOTMUCH_CONFIG" (expand-file-name ".config/notmuch/notmuchrc" (getenv "HOME")))

(use-package server                     ; The server of `emacsclient'
  :config (or (server-running-p) (server-mode)))

(use-package pinentry
  :config
  (setenv "INSIDE_EMACS" (format "%s,comint" emacs-version))
  (pinentry-start))

;; Confirm before quitting Emacs
(setq confirm-kill-emacs #'y-or-n-p)

;;; Require files under ~/.emacs.d/lisp
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Enable `vde-mode' unless `disable-pkg-vde-mode' is set to `t' in
;; `setup-var-overrides.el'.
(when (not (bound-and-true-p disable-pkg-setup-mode))
  (use-package setup-vde-mode))

(use-package setup-style)
(use-package setup-keybindings)
(use-package setup-completion)
(use-package setup-vcs)
(use-package setup-dired)
(use-package setup-search)
(use-package setup-files)
(use-package setup-editing)
(use-package setup-multiple-cursors)
(use-package setup-navigating)
(use-package setup-windows)
(use-package setup-buffers)
(use-package setup-projectile)
(use-package setup-shells)
(use-package setup-compile)
(use-package setup-org)
;; Programming languages
(use-package setup-nix)
(use-package setup-go)
(use-package setup-web)
(use-package setup-docker)
(use-package setup-hydras)
(use-package setup-browser)
(use-package setup-notmuch)

;; C-up/down onn console
(when (not window-system)
  (define-key function-key-map "\eO1;5A"    [C-up])
  (define-key function-key-map "\eO1;5B"  [C-down])
  (define-key function-key-map "\eO1;5C" [C-right])
  (define-key function-key-map "\eO1;5D"  [C-left])
  )

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

(put 'magit-diff-edit-hunk-commit 'disabled nil)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; Finalization
;;; init.el ends here
