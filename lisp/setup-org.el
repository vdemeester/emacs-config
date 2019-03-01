;;; setup-org.el --- setup org-mode -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defvar org-directory "~/desktop/org/")
(defvar site-directory "~/desktop/sites/")

(defvar org-default-projects-dir (concat org-directory "projects") "Primary tasks directory.")
(defvar org-default-technical-dir (concat org-directory "technical") "Directory of shareable, technical notes.")
(defvar org-default-personal-dir (concat org-directory "personal") "Directory of un-shareable, personal notes.")
(defvar org-default-completed-dir (concat org-directory "projects/completed") "Directory of completed project files.")
(defvar org-default-inbox-file (concat org-directory "projects/inbox.org") "New stuff collected in this file.")
(defvar org-default-incubate-file (concat org-directory "projects/incubate.org") "Ideas simmering on back burner.")
(defvar org-default-notes-file (concat org-directory "personal/notes.org") "Non-actionable, personal notes.")
(defvar org-default-media-file (concat org-directory "projects/media.org") "Links to other things to check out.")
(defvar org-default-journal-file (concat org-directory "personal/journal.org") "Journaling stuff.")

(defvar org-default-publish-technical (concat site-directory "sbr.pm/technical"))

;; Use `org-mode' instead of `lisp-interaction-mode' for scratch buffer
(setq
 inhibit-startup-message t            ; don't show the startup message
 inhibit-startup-screen t             ; … or screen
 initial-scratch-message nil          ; empty scratch buffer
 initial-major-mode 'org-mode  ; org-mode by default
 )

(use-package s)

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
        '(org-habit org-info org-docview org-protocol org-man org-git-link org-notmuch))
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "STARTED(s)" "|" "DONE(d!)" "CANCELED(c)")
          (sequence "WAITING(w)" "SOMEDAY(s)" "|" "CANCELED(c)")
          (sequence "IDEA(i)" "|" "CANCELED(c)")))
  (setq org-blank-before-new-entry '((heading . t)
                                     (plain-list-item . nil)))

  (setq org-agenda-files (list org-default-projects-dir))
  (setq org-agenda-file-regexp "^[a-z0-9-_]+.org")

  (setq org-agenda-include-diary t)
  (setq org-use-property-inheritance t)

  (setq org-refile-use-outline-path 'file
        org-outline-path-complete-in-steps nil)

  (setq org-refile-targets (append '((org-default-media-file :level . 1)
                                     (org-default-notes-file :level . 0))
                                   (->>
                                    (directory-files org-default-projects-dir nil ".org")
                                    (-remove-item (file-name-base org-default-media-file))
                                    (--remove (s-starts-with? "." it))
                                    (--map (format "%s/%s" org-default-projects-dir it))
                                    (--map `(,it :level . 1)))))

  (setq org-use-speed-commands t)

  (setq org-log-done (quote time))
  (setq org-log-redeadline (quote time))
  (setq org-log-reschedule (quote time))
  (setq org-log-into-drawer t)

  (setq org-fontify-whole-heading-line t)
  (setq org-src-fontify-natively t)
  (setq org-src-tab-acts-natively t)

  (setq org-pretty-entities t)
  (setq org-insert-heading-respect-content t)
  (setq org-ellipsis " …")

  (setq org-agenda-window-setup (quote other-frame))

  (setcar (nthcdr 4 org-emphasis-regexp-components) 10)

  (setq org-tag-alist (quote (("linux") ("nixos") ("emacs") ("org")
                              ("openshift") ("redhat") ("kubernetes") ("knative" ) ("docker")
                              (:startgroup . nil)
                              ("@home" . ?h) ("@work" . ?w) ("@errand" . ?r) ("@health" . ?l)
                              (:endgroup . nil)
                              (:startgroup . nil)
                              ("@easy" . ?e) ("@medium" . ?m) ("@hard" . ?a)
                              (:endgroup . nil)
                              )))
  (setq org-agenda-skip-scheduled-if-done t)
  (setq org-default-notes-file "~/desktop/org/inbox.org")

  (defvar org-capture-templates (list))
  (setq org-protocol-default-template-key "l")

  ;; Tasks (-> inbox)
  (add-to-list 'org-capture-templates
               `("t" "Task Entry" entry
                 (file ,org-default-inbox-file)
                 "* %?\n:PROPERTIES:\n:CREATED:%U\n:END:\n\n%i\n\nFrom: %a"
                 :empty-lines 1))
  (add-to-list 'org-capture-templates
               `("r" "PR Review" entry
                 (file ,org-default-inbox-file)
                 "* TODO review gh:%^{issue} :review:\n:PROPERTIES:\n:CREATED:%U\n:END:\n\n%i\n%?\nFrom: %a"
                 :empty-lines 1))
  (add-to-list 'org-capture-templates
               `("l" "Link" entry
                 (file ,org-default-inbox-file)
                 "* %a\n%U\n%?\n%i"
                 :empty-lines 1))
  (add-to-list 'org-capture-templates
               '("n" "Thought or Note"  entry
                 (file org-default-notes-file)
                 "* %?\n\n  %i\n\n  See: %a" :empty-lines 1))

  ;; Journal
  (add-to-list 'org-capture-templates
               `("j" "Journal entry" entry
                 (file+datetree ,org-default-journal-file)
                 "* %^{title}\n%U\n%?\n%i\nFrom: %a"
                 :empty-lines 1))
  (add-to-list 'org-capture-templates
               `("w" "Worklog (journal) entry" entry
                 (file+datetree ,org-default-journal-file)
                 "* worklog\n%U\n** Yesterday\n%?\n** Today\n** Next (later today, tomorrow)\n"))
  (add-to-list 'org-capture-templates
               `("e" "Weekly review" entry
                 (file+datetree,org-default-journal-file)
                 "* weekly review\n%U

- [ ] review [[file:incubate.org][~incubate.org~]]
- [ ] review [[file:inbox.org][~inbox.org~]]
- [ ] [[https://todoist.com/app][Todoist]]
  - [ ] review [[https://todoist.com/app?lang=en#project%252F173761779][inbox]]
  - [ ] review [[https://todoist.com/app?lang=en#project%252F173761780%252Ffull][personal project]]
  - [ ] review [[https://todoist.com/app?lang=en#project%252F173761782%252Ffull][work project]]
- [ ] review ~org-mode~ workflow
  - what works, what doesn't ?
  - is there task / stuck projects ?
- [ ] export previous agenda (somewhere)"))

  ;; Olds, most likely to remove
  (add-to-list 'org-capture-templates
               `("b" "Blog post" entry
                 (file+headline "~/src/github.com/vdemeester/blog/content-org/posts.org" "Blog Ideas")
                 "* %?\n:PROPERTIES:\n:END:\n"))
  (add-to-list 'org-capture-templates
               `("bl" "Blog link post" entry
                 (file+olp "~/src/github.com/vdemeester/blog/content-org/links.org" "Link")
                 "* %a\n%?\n%i"))

  ;; org-babel
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((css . t)
     (dot . t)
     (emacs-lisp . t)
     (go . t)
     (http . t)
     (js . t)
     (python . t)
     (rust . t)
     (shell . t)
     (typescript . t)))

  ;; org-links
  ;; from http://endlessparentheses.com/use-org-mode-links-for-absolutely-anything.html
  (org-link-set-parameters "tag"
                           :follow #'endless/follow-tag-link)
  (defun endless/follow-tag-link (tag)
    "Display a list of TODO headlines with tag TAG.
With prefix argument, also display headlines without a TODO keyword."
    (org-tags-view (null current-prefix-arg) tag))

  (org-link-set-parameters "grep"
                           :follow #'vde/follow-grep-link
                           :face '(:foreground "DarkRed" :underline t))
  (defun vde/follow-grep-link (regexp)
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
                           :follow #'vde/follow-rg-link
                           :face '(:foreground "DarkGreen" :underline t))
  (defun vde/follow-rg-link (regexp)
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
                           :follow #'vde/follow-gh-link
                           :export #'vde/org-gh-export
                           :face '(:foreground "DimGrey" :underline t))
  (defun vde/org-gh-export (link description format)
    "Export a github page link from Org files."
    (let ((path (vde/gh-get-url link))
          (desc (or description link)))
      (cond
       ((eq format 'html) (format "<a hrefl=\"_blank\" href=\"%s\">%s</a>" path desc))
       ((eq format 'latex) (format "\\href{%s}{%s}" path desc))
       ((eq format 'texinfo) (format "@uref{%s,%s}" path desc))
       ((eq format 'ascii) (format "%s (%s)" desc path))
       (t path))))
  (defun vde/follow-gh-link (issue)
    "Browse github issue/pr specified"
    (browse-url (vde/gh-get-url issue)))

  (defun vde/gh-get-url (path)
    "Translate org-mode link `gh:foo/bar#1' to github url."
    (setq expressions (split-string path "#"))
    (setq project (nth 0 expressions))
    (setq issue (nth 1 expressions))
    (format "https://github.com/%s/issues/%s" project issue))

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

(defun vde/org-mode-hook ()
  "Org-mode hook"
  (setq show-trailing-whitespace t)
  (when (not (eq major-mode 'org-agenda-mode))
    (setq fill-column 90)
    (auto-fill-mode)
    (flyspell-mode)
    (org-indent-mode)
    (smartparens-mode)))

;; (use-package orca
;;   :after (org)
;;   :config
;;   (setq orca-handler-list
;;         '(;; (orca-handler-match-url "http://stackoverflow.com/" "~/Dropbox/org/wiki/stack.org" "Questions")
;;           ;; (orca-handler-match-url "https://www.reddit.com/" "~/Dropbox/org/wiki/emacs.org" "Reddit")
;;           ;; (orca-handler-match-url "https://emacs.stackexchange.com/" "~/Dropbox/org/wiki/emacs.org" "\\* Questions")
;;           ;; (orca-handler-current-buffer "\\* Tasks")
;;           ;; (orca-handler-file "~/Dropbox/org/ent.org" "\\* Articles")
;;           ;; (orfu-handle-link-youtube)
;;           (vde/handle-link-github))))

(defcustom orfu-github-project-name
  "https://github\\.com/\\([^/]+\\)"
  "Regex for Github repository projects."
  :type 'string)

(defun orfu-handle-link-github ()
  (let ((link (caar org-stored-links))
        (title (cl-cadar org-stored-links)))
    (when (string-match orfu-github-project-name link)
      (let ((project-name (match-string 1 link))
            (parts (split-string title "·")))
        (setf (cl-cadar org-stored-links)
              (concat (car parts)
                      (substring (cadr parts) 7)))
        (find-file (orfu-expand "wiki/github.org"))
        (goto-char (point-min))
        (re-search-forward (concat "^\\*+ +" project-name) nil t)))))

(use-package ob-go
  :after (org))
(use-package ob-rust
  :after (org))
(use-package ob-async
  :after (org))
(use-package ob-http
  :after (org))
(use-package ob-typescript
  :after (org))

(use-package org-crypt
  :after (org)
  :config
  (org-crypt-use-before-save-magic)
  (setq org-tags-exclude-from-inheritance (quote ("crypt"))))

(use-package smartparens-org
  :after org-mode)

(use-package ox-publish
  :config
  (setq org-html-coding-system 'utf-8-unix))
(use-package ox-slack
  :after ox)
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

(use-package org-notify
  :after org
  :config
  (org-notify-start))

(use-package org-todoist
  :after (org)
  :bind (("C-c o t s" . org-todoist-sync))
  :config
  (setq org-todoist-file (concat org-directory "projects/todoist.org")))

(use-package org-capture-pop-frame)

(provide 'setup-org)
;;; setup-org.el ends here

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
