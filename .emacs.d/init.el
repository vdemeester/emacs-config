;; Personal information
(setq user-full-name "Vincent Demeester"
      user-mail-address "vincent@sbr.pm")

;; Check we are using Emacs 24
(when (/= emacs-major-version 24)
  (error "Only Emacs 24 is supported. You seem to use Emacs %d"
         emacs-major-version))

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

;; path to local config
(add-to-list 'load-path
             (concat
              (file-name-as-directory user-emacs-directory) "site-lisp/"))

;; Initialize el-get
;; (setq el-get-dir (expand-file-name "vendor" user-emacs-directory))

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")

;; el-get packages
(setq el-get-packages
      '(el-get
        dash
        s
        powerline                           ;; Powerline is cool :D
        naquadah-theme                      ;; Theme from Julien Danjou
        sublime-themes                      ;; Theme inspired by Sublime Text ones
        auto-complete                       ;; universal autocompletion
        auto-complete-css                   ;; CSS autocompletion
        magit                               ;; Git for Emacs
        git-modes                           ;; Various git-related modes
        git-commit-mode                     ;; Mode for "git commit"
        git-annex                           ;; Dired addon with git-annex support
        gist                                ;; Gist :-)
        expand-region
        autopair                            ;; Auto pairing for parentheses
        org-bullets                         ;; Org UTF-8 bullets
        flx
        ido-vertical-mode
        smex
        yasnippet
        smartparens
        deft
        ;; Languages and file support
        lua-mode
        go-mode
        haskell-mode
        mustache-mode
        markdown-mode
        pkgbuild-mode
        dockerfile-mode
        web-mode
        apache-mode
        scala-mode2
        ;; Clojure
        cider
        clojure-mode
        paredit
        paredit-extension
        keychain-environment
        ;; Web
        emacs-w3m
        ))

;; Conditionnal recipes
;; (unless (string-match "apple-darwin" system-configuration)
;; ...)

;; (when (ignore-errors
;;      (el-get-executable-find "svn")
;;      (loop for p in '(psvn                 ;; M-x svn-status
;;                       )
;;            do (add-to-list 'el-get-sources p))))

;; getelget -- bootstrap el-get if necessary and load the specified packages
(load-library "getelget.el")
;; install new packages and init already installed packages
(let ((emacs-lisp-mode-hook nil))
  (el-get-sync))

;; Load ${package}.conf.el after loading/require
(dolist (file (directory-files user-emacs-directory))
  (when (string-match (format "^\\(.+\\)\\.conf\\.el$") file)
    (eval-after-load (match-string-no-properties 1 file)
      `(load ,(concat user-emacs-directory file)))))

;; Appearance
(menu-bar-mode -1)                        ; No menu
(tool-bar-mode -1)                        ; No toolbar
(scroll-bar-mode -1)                        ; No scrollbar
(blink-cursor-mode -1)                        ; No blinking cursor
(show-paren-mode t)                        ; Display matching parenthesis ; C-M-n and C-M-p
(setq inhibit-splash-screen t)                ; No splash screen
(line-number-mode 1)                        ; show line number
(column-number-mode 1)                        ; show column number
(global-hl-line-mode 1)                        ; highlight current line
(mouse-avoidance-mode 'jump)                ; move the mouse away
(setq font-lock-maximum-decoration 2) ; faster font-lock-mode
(set-default 'indicate-buffer-boundaries '((up . nil) (down . nil) (t . left)))

;; Font
(set-default-font "Ubuntu Mono-14")
(set-frame-font "Ubuntu Mono-14")
(set-face-attribute 'default nil :family "Ubuntu Mono" :height 120)

;; Load the theme
(load-theme 'junio t)

;; Behaviour
(setq mouse-yank-at-point t                ; Yank where the point currently is
      x-select-enable-primary t ; Yank use the primary selection if available
      save-interprogram-paste-before-kill t ; Put clipboard/selection into kill ring
      mouse-1-click-follows-link nil)        ; Don't follow links with left click
(fset 'yes-or-no-p 'y-or-n-p) ; Always use y/n prompt
(setq use-dialog-box nil) ; No dialog box
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(setq-default indent-tabs-mode nil) ; don't use tabs
(setq next-screen-context-lines 5 ; Keep more lines when scrolling
      x-stretch-cursor t) ; stretch cursor to the width of the char

;; Save all tempfiles in $TMPDIR/emacs$UID/
(defconst emacs-tmp-dir (format "%s/%s%s/" temporary-file-directory "emacs" (user-uid)))
(setq backup-directory-alist
      `((".*" . ,emacs-tmp-dir))
      auto-save-file-name-transforms
      `((".*" ,emacs-tmp-dir t))
      auto-save-list-file-prefix emacs-tmp-dir)
;; All tempfiles are out of the way, we'll keep more of them :)
(setq delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

;; UTF-8 preferences
(prefer-coding-system 'utf-8)
(setq file-name-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
;; Use Emacs terminfo, not system terminfo (?)
(setq system-uses-terminfo nil)

;; Shell
(setq shell-file-name "bash")
(setq shell-command-switch "-ic")

(require 'auto-complete)
(require 'expand-region)

(require 'ido)
(require 'git-annex)
(require 'powerline)
(powerline-default-theme)

(require 'org)

;; TODO Load machine-specifc
;; TODO Load os-specific

;; Special ensime/scala
(add-to-list 'load-path "/usr/share/ensime/elisp")
(add-to-list 'exec-path "/usr/share/ensime")
(require 'ensime)
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

;; Clean & co
(defun untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer."
  (interactive)
  (indent-buffer)
  (untabify-buffer)
  (delete-trailing-whitespace))

(defun cleanup-region (beg end)
  "Remove tmux artifacts from region."
  (interactive "r")
  (dolist (re '("\\\\│\·*\n" "\W*│\·*"))
    (replace-regexp re "" nil beg end)))

(global-set-key (kbd "C-x M-t") 'cleanup-region)
(global-set-key (kbd "C-c n") 'cleanup-buffer)

;; flyspell
(setq flyspell-issue-welcome-flag nil)
(setq-default ispell-program-name "aspell")
(setq-default ispell-list-command "list")

;; other bindings
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

;; y and n in place of yes or no
(defalias 'yes-or-no-p 'y-or-n-p)

(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;; Server
(unless (string= (user-login-name) "root")
  (require 'server)
  (when (or (not server-process)
            (not (eq (process-status server-process)
                     'listen)))
    (unless (server-running-p server-name)
      (server-start))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("53c542b560d232436e14619d058f81434d6bbcdc42e00a4db53d2667d841702e" "1989847d22966b1403bab8c674354b4a2adf6e03e0ffebe097a6bd8a32be1e19" "ce79400f46bd76bebeba655465f9eadf60c477bd671cbcd091fe871d58002a88" "bf648fd77561aae6722f3d53965a9eb29b08658ed045207fe32ffed90433eb52" "0ebe0307942b6e159ab794f90a074935a18c3c688b526a2035d14db1214cf69c" "90b5269aefee2c5f4029a6a039fb53803725af6f5c96036dee5dc029ff4dff60" "33c5a452a4095f7e4f6746b66f322ef6da0e770b76c0ed98a438e76c497040bb" "9bcb8ee9ea34ec21272bb6a2044016902ad18646bd09fdd65abae1264d258d89" "c7359bd375132044fe993562dfa736ae79efc620f68bab36bd686430c980df1c" "e26780280b5248eb9b2d02a237d9941956fc94972443b0f7aeec12b5c15db9f3" "7d4d00a2c2a4bba551fcab9bfd9186abe5bfa986080947c2b99ef0b4081cb2a6" "a774c5551bc56d7a9c362dca4d73a374582caedb110c201a09b410c0ebbb5e70" "d6a00ef5e53adf9b6fe417d2b4404895f26210c52bb8716971be106550cea257" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "37d0cdc1e79ae56cd1ea262dd6b84939fcc15d7977e320e2c7249c140aafc032" default)))
 '(org-agenda-files (quote ("/home/vincent/desktop/org/todos/computers.org" "/home/vincent/desktop/org/todos/inbox.org" "/home/vincent/desktop/org/todos/personal.org" "/home/vincent/desktop/org/todos/xgbi.org"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
