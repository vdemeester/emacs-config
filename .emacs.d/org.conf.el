(require 'dash)
(require 's)

(setq
 org-completion-use-ido t                  ;; use IDO for completion
 ;; org-startup-indented t                  ;; ident by default
 org-tags-column -90                       ;; align tags on the 90th columns
 org-hide-leading-stars t                  ;; don't show leading stars
 org-cycle-separator-lines 0               ;; don't show blank lines between collapsed trees
 org-src-fontify-natively t                ;; fontify code blocks
 org-edit-src-content-indentation 0        ;; don't indent source blocks
 org-catch-invisible-edits 'error          ;; don't edit invisible text
 org-agenda-include-diary t
 )

;; org files
(setq org-directory "~/desktop/org/")
(setq org-agenda-files '("~/desktop/org/todos/"))
(setq org-archive-location (concat org-directory "archive/%s_archive::"))

;; Todos keywords
(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "PROGRESS(p)" "|" "DONE(d)")
              (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING"))))

(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "#EE0000" :weight bold)
              ("NEXT" :foreground "#A197BF" :weight bold)
              ("PROGRESS" :foreground "#A197BF" :weight bold)
              ("DONE" :foreground "#8fbfdc" :weight bold)
              ("WAITING" :foreground "orange" :weight bold)
              ("HOLD" :foreground "magenta" :weight bold)
              ("CANCELLED" :foreground "forest green" :weight bold)
              ("MEETING" :foreground "forest green" :weight bold)
              ("PHONE" :foreground "forest green" :weight bold))))

(defun my-org-mode-hook ()
  "Personal org-mode-hook, that enable and disable stuff."
  (define-key org-mode-map (kbd "C-c [") 'nil)
  (define-key org-mode-map (kbd "C-c ]") 'nil)
  )
(add-hook 'org-mode-hook 'my-org-mode-hook)

;; Use bullets
(require 'org-bullets)
(add-hook 'org-mode-hook
          '(lambda ()
             (org-bullets-mode 1)))

(require 'org-protocol)
;; (require 'org-mine)

(require 'org-mobile)
;; mobileorg settings
(setq org-mobile-directory "/scpc:vincent@sif:desktop/org/")
(setq org-mobile-inbox-for-pull "~/desktop/org/todos/inbox.org")
(setq org-mobile-files '("~/desktop/org/todos/"))

;; org-babel
(require 'ob)
(setq org-src-fontify-natively t)
;; (setq org-confirm-babel-evaluate nil)

;; Automatic safe of all org-buffer each hours
(run-at-time "00:59" 3600 'org-save-all-org-buffers)
