;; Check we are using Emacs 24
(when (/= emacs-major-version 24)
  (error "Only Emacs 24 is supported. You seem to use Emacs %d"
	 emacs-major-version))

(add-to-list 'load-path "~/.emacs.d/vendor/el-get")

;; path to local config
(add-to-list 'load-path
             (concat
              (file-name-as-directory user-emacs-directory) "site-lisp/"))

;; Initialize el-get
(setq el-get-dir (expand-file-name "vendor" user-emacs-directory))

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
        ;; Clojure
        cider
        clojure-mode
	))

;; Conditionnal recipes
;; (unless (string-match "apple-darwin" system-configuration)
;; ...)

;; (when (ignore-errors
;;	(el-get-executable-find "svn")
;;	(loop for p in '(psvn                 ;; M-x svn-status
;;			 )
;;	      do (add-to-list 'el-get-sources p))))

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
(require 'naquadah-theme)

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
    `((".*" . ,emacs-tmp-dir)))
(setq auto-save-file-name-transforms
    `((".*" ,emacs-tmp-dir t)))
(setq auto-save-list-file-prefix
    emacs-tmp-dir)

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
 '(custom-safe-themes (quote ("d6a00ef5e53adf9b6fe417d2b4404895f26210c52bb8716971be106550cea257" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "37d0cdc1e79ae56cd1ea262dd6b84939fcc15d7977e320e2c7249c140aafc032" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
