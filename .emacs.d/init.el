;; Check we are using Emacs 24
(when (/= emacs-major-version 24)
  (error "Only Emacs 24 is supported. You seem to use Emacs %d"
         emacs-major-version))

(dolist (file (directory-files user-emacs-directory))
   (when (string-match (format "^\\(.+\\)\\.conf\\.el$") file)
     (eval-after-load (match-string-no-properties 1 file)
       `(load ,(concat user-emacs-directory file)))))

(add-to-list 'load-path "~/.emacs.d/vendor/el-get")

;; Initialize el-get
(setq el-get-dir (expand-file-name "vendor" user-emacs-directory))

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let (el-get-master-branch)
      (goto-char (point-max))
      (eval-print-last-sexp))))

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")

(el-get 'sync)

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

;; Shell
(setq shell-file-name "bash")
(setq shell-command-switch "-ic")

(require 'auto-complete)

(require 'ido)
(require 'git-annex)
(require 'powerline)
(powerline-default-theme)

;; Server
(unless (string= (user-login-name) "root")
  (require 'server)
  (when (or (not server-process)
            (not (eq (process-status server-process)
                     'listen)))
    (unless (server-running-p server-name)
      (server-start))))
