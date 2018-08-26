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
  (setq org-directory "~/sync/org/")
  
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
                              ("laptop" . ?l) ("desktop" . ?d) ("server" . ?s))))
  (setq org-agenda-skip-scheduled-if-done t)
  (setq org-default-notes-file "~/sync/org/notes.org")
  (setq org-default-tasks-file "~/sync/org/tasks.org")

  (setq org-protocol-default-template-key "l")
  
  (setq org-capture-templates '(("b" "Blog post" entry
                                 (file+headline "~/src/github.com/vdemeester/blog/content-org/posts.org" "Blog Ideas")
                                 "* %?\n:PROPERTIES:\n:END:\n")
                                ("n" "Though or Note" entry
                                 (file org-default-notes-file))
                                ("l" "Link" entry (file+olp org-default-notes-file "Links")
                                 "* %a\n %?\n %i"))))

(use-package org-journal
  :init
  (setq org-journal-dir "~/sync/journal/")
  (setq org-journal-time-format ""))

(use-package org-projectile
  :defer 3
  :bind (("C-c n p" . org-projectile-project-todo-completing-read))
  :config
  (progn
    (setq org-projectile-projects-file
          "~/sync/org/projects.org")
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
              ("C-c G" . org-hugo-export-wim-to-md)))

(provide 'vde-org)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
