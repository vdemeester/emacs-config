(require 'dash)
(require 's)

(setq
 org-completion-use-ido t                  ;; use IDO for completion
 ; org-startup-indented t                  ;; ident by default
 org-tags-column -90                       ;; align tags on the 90th columns
 org-hide-leading-stars t                  ;; don't show leading stars
 org-cycle-separator-lines 0               ;; don't show blank lines between collapsed trees
 org-src-fontify-natively t                ;; fontify code blocks
 org-edit-src-content-indentation 0        ;; don't indent source blocks
 org-catch-invisible-edits 'error          ;; don't edit invisible text
)

;; Todos keywords
(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "SOMEDAY(S!)" "PROGRESS(p)" "|" "DONE(d!)")
        (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)")))


(require 'org-mobile)
(setq org-directory "~/desktop/org")
;; mobileorg settings
(setq org-mobile-directory "/scpc:vincent@sif:desktop/org/")
(setq org-mobile-inbox-for-pull "~/desktop/org/inbox.org")
(setq org-mobile-files '("~/desktop/org"))

;; Use bullets
(require 'org-bullets)
(add-hook 'org-mode-hook
          '(lambda ()
             (org-bullets-mode 1)))

(require 'org-protocol)
;; (require 'org-mine)
