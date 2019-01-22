;;; setup-org.el --- setup org-mode -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package org
  :defer t
  :mode (("\\.org$" . org-mode))
  :commands (org-capture org-agenda)
  :ensure org-plus-contrib
  :hook (org-mode . vde/org-mode-hook)
  :bind (("C-c o c" . org-capture)
         ("C-c o l" . org-store-link)
         ("C-c o r r" . org-refile)
         ("C-c o r a" . org-agenda-refile)
         ("C-c o a" . org-agenda))
  :config
  (use-package find-lisp)
  (setq org-modules
        '(org-habit org-info org-docview org-protocol org-man org-git-link))
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)" "CANCELED(c)")
          (sequence "WAITING(w)" "SOMEDAY(s)" "|" "CANCELED(c)")
          (sequence "IDEA(i)" "|" "CANCELED(c)")))
  (setq org-blank-before-new-entry '((heading . t)
                                     (plain-list-item . nil)))
  (setq org-directory "~/desktop/org/")
  (setq org-agenda-files (find-lisp-find-files org-directory "\.org$"))
  (setq org-agenda-include-diary t)

  (setq org-refile-targets '((org-agenda-files :maxlevel . 3)))

  (setq org-use-speed-commands t)
  
  (setq org-log-done (quote time))
  (setq org-log-redeadline (quote time))
  (setq org-log-reschedule (quote time))
  (setq org-log-into-drawer t)

  (setq org-fontify-whole-heading-line t)

  (setq org-pretty-entities t)
  (setq org-insert-heading-respect-content t)
  (setq org-ellipsis " …")

  (setq org-agenda-window-setup (quote other-frame))

  (setcar (nthcdr 4 org-emphasis-regexp-components) 10)
  
  (setq org-tag-alist (quote ((:startgroup . nil)
                              ("@home" . ?m) ("@work" . ?w) ("@errand" . ?e) ("@health" . ?h)
                              (:endgroup . nil)
                              ("linux" . ?l) ("nixos" . ?n) ("emacs" . ?e)
                              ("openshift" . ?o) ("redhat" . ?r)
                              ("kubernetes" . ?k) ("knative" . ?t)
                              ("docker" . ?d)
                              )))
  (setq org-agenda-skip-scheduled-if-done t)
  (setq org-default-notes-file "~/desktop/org/inbox.org")
  (setq org-default-tasks-file "~/desktop/org/tasks.org")

  (setq org-protocol-default-template-key "l")
  (setq org-capture-templates '(("b" "Blog post" entry
                                 (file+headline "~/src/github.com/vdemeester/blog/content-org/posts.org" "Blog Ideas")
                                 "* %?\n:PROPERTIES:\n:END:\n")
                                ("bl" "Blog link post" entry
                                 (file+olp "~/src/github.com/vdemeester/blog/content-org/links.org" "Link")
                                 "* %a\n%?\n%i")
                                ("n" "Though or Note" entry
                                 (file org-default-notes-file))
                                ("j" "Journal entry" entry
                                 (file+datetree "~/desktop/org/journal.org")
                                 "* %^{title}\n%U\n%?\n%i\n")
                                ("w" "Worklog (journal) entry" entry
                                 (file+datetree "~/desktop/org/journal.org")
                                 "* worklog\n%U\n** Yesterday\n%?\n** Today\n** Next (later today, tomorrow)\n")
                                ("l" "Link" entry (file+olp org-default-notes-file "Links")
                                 "* %a\n%U\n%?\n%i")))
  
  ;; org-babel
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (shell . t)
     (python . t)
     (dot . t)))

  ;; org-links
  ;; from http://endlessparentheses.com/use-org-mode-links-for-absolutely-anything.html
  (org-link-set-parameters "tag"
                           :follow #'endless/follow-tag-link)
  (defun endless/follow-tag-link (tag)
    "Display a list of TODO headlines with tag TAG.
With prefix argument, also display headlines without a TODO keyword."
    (org-tags-view (null current-prefix-arg) tag))

  (org-link-set-parameters "grep"
                           :follow #'my/follow-grep-link
                           :face '(:foreground "DarkRed" :underline t))
  (defun my/follow-grep-link (regexp)
    "Run `rgrep' with REGEXP and FOLDER as argument,
like this : [[grep:REGEXP:FOLDER]]."
    (setq expressions (split-string regexp ":"))
    (setq exp (nth 0 expressions))
    (grep-compute-defaults)
    (if (= (length expressions) 1)
        (progn
          (rgrep exp "*" (expand-file-name "./")))
      (progn
        (setq folder (nth 1 expressions))
        (rgrep exp "*" (expand-file-name folder))))
    )

  (org-link-set-parameters "rg"
                           :follow #'my/follow-rg-link
                           :face '(:foreground "DarkGreen" :underline t))
  (defun my/follow-rg-link (regexp)
    "Run `ripgrep-regexp` with REXEP and FOLDER as argument,
like this : [[pt:REGEXP:FOLDER]]"
    (setq expressions (split-string regexp ":"))
    (setq exp (nth 0 expressions))
    (if (= (length expressions) 1)
        (progn
          (ripgrep-regexp exp (expand-file-name "./")))
      (progn
        (setq folder (nth 1 expressions))
        (ripgrep-regexp exp (file-name-as-directory (expand-file-name folder)))))
    )

  (org-link-set-parameters "gh"
                           :follow #'my/follow-gh-link
                           :face '(:foreground "DimGrey" :underline t))
  (defun my/follow-gh-link (issue)
    "Browse github issue/pr specified"
    (setq expressions (split-string issue "#"))
    (setq project (nth 0 expressions))
    (setq issue (nth 1 expressions))
    (browse-url
     (format "https://github.com/%s/issues/%s" project issue)))

  (org-link-set-parameters
   "org"
   :complete (lambda () (+org-link-read-file "org" org-directory))
   :follow   (lambda (link) (find-file (expand-file-name link org-directory)))
   :face     (lambda (link)
               (if (file-exists-p (expand-file-name link org-directory))
                   'org-link
                 'error)))
  (defun +org-link-read-file (key dir)
    (let ((file (read-file-name (format "%s: " (capitalize key)) dir)))
      (format "%s:%s"
              key
              (file-relative-name file dir))))
  )

(use-package org-projectile
  :defer 3
  :bind (("C-c n p" . org-projectile-project-todo-completing-read))
  :config
  (progn
    (setq org-projectile-projects-file
          "~/desktop/org/projects.org")
    (setq org-agenda-files (append org-agenda-files (org-projectile-todo-files)))
    (push (org-projectile-project-todo-entry) org-capture-templates)))

(defun vde/org-mode-hook ()
  "Org-mode hook"
  (setq show-trailing-whitespace t)
  (when (not (eq major-mode 'org-agenda-mode))
    (setq fill-column 90)
    (auto-fill-mode)
    (flyspell-mode)
    (org-indent-mode)
    (smartparens-mode)))

(use-package ob-async
  :after (org))

(use-package org-crypt
  :after (org)
  :config
  (org-crypt-use-before-save-magic)
  (setq org-tags-exclude-from-inheritance (quote ("crypt"))))

(use-package smartparens-org
  :after org-mode)

(use-package ox-hugo
  :after ox
  :commands (org-hugo-slug)
  :bind (:map vde-mode-map
              ("C-c G" . org-hugo-export-wim-to-md))
  :config
  (use-package ox-hugo-auto-export))

(use-package org-bullets
  :after (org)
  :hook (org-mode . org-bullets-mode)
  :config
  (setq org-bullets-bullet-list '("◉" "○" "⚫")))

;; Use `org-mode' instead of `lisp-interaction-mode' for scratch buffer
(setq
 inhibit-startup-message t            ; don't show the startup message
 inhibit-startup-screen t             ; … or screen
 initial-scratch-message nil          ; empty scratch buffer
 initial-major-mode 'org-mode  ; org-mode by default
 )

(use-package org-notify
  :after org
  :config
  (org-notify-start))

(use-package org-timer
  :after org)

(use-package org-todoist
  :after (org)
  :bind (("C-c o t s" . org-todoist-sync))
  :config
  (setq org-todoist-file "~/desktop/org/todoist.org"))

(use-package org-capture-pop-frame)

(provide 'setup-org)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
