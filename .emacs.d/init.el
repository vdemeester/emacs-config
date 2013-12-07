;; Check we are using Emacs 24
(when (/= emacs-major-version 24)
  (error "Only Emacs 24 is supported. You seem to use Emacs %d"
         emacs-major-version))

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

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


