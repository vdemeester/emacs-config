;;; -*- lexical-binding: t; -*-
(use-package org
  :defer t
  :mode (("\\.org$" . org-mode))
  :commands (org-capture org-agenda)
  :ensure org-plus-contrib
  :hook (org-mode . vde/org-mode-hook)
  :bind (("C-c c" . org-capture))  
  :config
  (require 'org-protocol)
  (setq org-modules
        '(org-habit org-info org-docview))
  (setq org-todo-keywords
        '((sequence "TODO" "WAITING(!)" "SOMEDAY(!)" "|" "DONE(!)" "CANCELED(!)")
          (sequence "IDEA")))
  (setq org-blank-before-new-entry '((heading . t)
                                     (plain-list-item . t)))
  (setq org-directory "~/desktop/org/")
  ;; you can override the document org-agenda-files by setting your
  ;; org-agenda-files in the variable org-user-agenda-files  
  (if (boundp 'org-user-agenda-files)
      (setq org-agenda-files org-user-agenda-files)
    (setq org-agenda-files (quote ("~/desktop/org")))) 
  
  (setq org-log-done (quote time))
  (setq org-log-redeadline (quote time))
  (setq org-log-reschedule (quote time))
  (setq org-log-into-drawer t)

  (setq org-pretty-entities t)
  (setq org-insert-heading-respect-content t)
  (setq org-ellipsis " …")
  
  (setq org-tag-alist (quote ((:startgroup . nil)
                              ("@home" . ?h) ("@work" . ?w) ("@errand" . ?e)
                              (:endgroup . nil)
                              ("laptop" . ?l) ("desktop" . ?d) ("server" . ?s)
                              ("openshift" . ?o) ("redhat" . ?r)
                              ("kubernetes" . ?k) ("knative" . ?n)
                              ("docker" . ?d) ("moby" . ?m)
                              )))
  (setq org-agenda-skip-scheduled-if-done t)
  (setq org-default-notes-file "~/desktop/org/notes.org")
  (setq org-default-tasks-file "~/desktop/org/tasks.org")

  (setq org-protocol-default-template-key "l")
  
  (setq org-capture-templates '(("b" "Blog post" entry
                                 (file+headline "~/src/github.com/vdemeester/blog/content-org/posts.org" "Blog Ideas")
                                 "* %?\n:PROPERTIES:\n:END:\n")
                                ("n" "Though or Note" entry
                                 (file org-default-notes-file))
                                ("j" "Journal entry" entry
                                 (file+datetree "~/desktop/org/journal.org")
                                 "* %^{title}\n%U\n%?\n"xo)
                                ("l" "Link" entry (file+olp org-default-notes-file "Links")
                                 "* %a\n %?\n %i")))

  ;; org-links
  ;; from http://endlessparentheses.com/use-org-mode-links-for-absolutely-anything.html
  (org-add-link-type
   "tag" 'endless/follow-tag-link)

  (defun endless/follow-tag-link (tag)
    "Display a list of TODO headlines with tag TAG.
With prefix argument, also display headlines without a TODO keyword."
    (org-tags-view (null current-prefix-arg) tag))
  
  (org-add-link-type
   "grep" 'my/follow-grep-link
   )
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
  
  (org-add-link-type
   "rg" 'my/follow-rg-link)
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
  
  (org-add-link-type
   "gh" 'my/follow-gh-link)
  (defun my/follow-gh-link (issue)
    "Browse github issue/pr specified"
    (setq expressions (split-string issue "#"))
    (setq project (nth 0 expressions))
    (setq issue (nth 1 expressions))
    (browse-url
     (format "https://github.com/%s/issues/%s" project issue))))

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

(use-package smartparens-org
  :after org-mode)

(use-package ox-hugo
  :after ox
  :commands (org-hugo-slug)
  :bind (:map vde-mode-map
              ("C-c G" . org-hugo-export-wim-to-md))
  :config
  (use-package ox-hugo-auto-export))

(provide 'vde-org)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
