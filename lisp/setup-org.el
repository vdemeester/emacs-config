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

(set-register ?i `(file . ,org-default-inbox-file))
(set-register ?I `(file . ,org-default-incubate-file))
(set-register ?j `(file . ,org-default-journal-file))
(set-register ?m `(file . ,org-default-media-file))

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
         ("C-c o a" . org-agenda)
         ("<f12>" . org-agenda)
         ("<f11>" . org-clock-goto))
  :config
  (use-package find-lisp)
  (setq org-modules '(org-crypt
                      org-docview
                      org-habit
                      org-id
                      org-info
                      org-irc
                      org-protocol
                      org-man
                      org-git-link
                      org-notmuch))
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "STARTED(s)" "|" "DONE(d!)" "CANCELED(c@/!)")
          (sequence "WAITING(w@/!)" "SOMEDAY(s)" "|" "CANCELED(c@/!)")
          (sequence "IDEA(i)" "|" "CANCELED(c@/!)")))
  (setq org-todo-state-tags-triggers '(
                                       ("CANCELLED" ("CANCELLED" . t))
                                       ("WAITING" ("WAITING" . t))
                                       (done ("WAITING"))
                                       ("TODO" ("WAITING") ("CANCELLED"))
                                       ("NEXT" ("WAITING") ("CANCELLED"))
                                       ("DONE" ("WAITING") ("CANCELLED"))))
  (setq org-blank-before-new-entry '((heading . t)
                                     (plain-list-item . nil)))

  (setq org-habit-show-habits-only-for-today nil)
  (setq org-habit-graph-column 80)
  (setq org-agenda-files (list org-default-projects-dir))
  (setq org-agenda-file-regexp "^[a-z0-9-_]+.org")

  (setq org-agenda-include-diary t)
  (setq org-use-property-inheritance t)

  (setq org-enforce-todo-dependencies t)

  (setq org-refile-use-outline-path 'file
        org-outline-path-complete-in-steps nil
        org-refile-allow-creating-parent-nodes 'confirm)

  (setq org-refile-targets (append '((org-default-media-file :level . 1)
                                     (org-default-inbox-file :level . 0))
                                   (->>
                                    (directory-files org-default-projects-dir nil ".org")
                                    (-remove-item (file-name-base org-default-media-file))
                                    (--remove (s-starts-with? "." it))
                                    (--map (format "%s/%s" org-default-projects-dir it))
                                    (--map `(,it :level . 1)))))

  (setq org-indirect-buffer-display 'dedicated-frame)
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

  (setq org-agenda-window-setup (quote current-window))
  (setq org-special-ctrl-a/e t)
  (setq org-special-ctrl-k t)
  (setq org-yank-adjusted-subtrees t)

  (setcar (nthcdr 4 org-emphasis-regexp-components) 10)

  (setq org-tag-alist (quote (("linux") ("nixos") ("emacs") ("org")
                              ("openshift") ("redhat") ("tektoncd") ("kubernetes") ("knative" ) ("docker")
                              ("docs") ("code") ("review")
                              (:startgroup . nil)
                              ("@home" . ?h) ("@work" . ?w) ("@errand" . ?e) ("@health" . ?l)
                              (:endgroup . nil)
                              (:startgroup . nil)
                              ("@link" . ?i) ("@read" . ?r) ("@project" . ?p)
                              (:endgroup . nil)
                              )))
  (setq org-agenda-skip-scheduled-if-done nil)

  (use-package org-super-agenda
    :config (org-super-agenda-mode))

  (setq org-agenda-span 'day
        org-agenda-compact-blocks t
        org-super-agenda-header-separator "")
  (setq org-agenda-sticky t)
  (setq org-agenda-custom-commands
        `(("n" "Personal agenda"
           ((agenda "")
            (tags-todo "+TODO=\"NEXT\""
                       ((org-agenda-overriding-header "Next items")))
            (tags-todo "@work-goals"
                       ((org-agenda-skip-function '(org-agenda-skip-if nil '(scheduled deadline)))
                        (org-agenda-overriding-header "Work")))
            (tags-todo "@home-goals"
                       ((org-agenda-skip-function '(org-agenda-skip-if nil '(scheduled deadline)))
                        (org-agenda-overriding-header "Home"))))
           ((org-super-agenda-groups
             '((:name "Important" :priority "A")
               (:habit t)
               (:name "Done" :log closed)
               (:name "Scheduled" :time-grid t)
               (:name "Work" :tag "@work")
               (:name "Perso" :tag "@home"))))
           (org-agenda-list))))

  (defun vde/is-project-p ()
    "Any task with a todo keyword subtask"
    (save-restriction
      (widen)
      (let ((has-subtask)
            (subtree-end (save-excursion (org-end-of-subtree t)))
            (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
        (save-excursion
          (forward-line 1)
          (while (and (not has-subtask)
                      (< (point) subtree-end)
                      (re-search-forward "^\*+ " subtree-end t))
            (when (member (org-get-todo-state) org-todo-keywords-1)
              (setq has-subtask t))))
        (and is-a-task has-subtask))))

  (defun vde/is-project-subtree-p ()
    "Any task with a todo keyword that is in a project subtree.
Callers of this function already widen the buffer view."
    (let ((task (save-excursion (org-back-to-heading 'invisible-ok)
                                (point))))
      (save-excursion
        (vde/find-project-task)
        (if (equal (point) task)
            nil
          t))))

  (defun vde/find-project-task ()
    "Move point to the parent (project) task if any"
    (save-restriction
      (widen)
      (let ((parent-task (save-excursion (org-back-to-heading 'invisible-ok) (point))))
        (while (org-up-heading-safe)
          (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
            (setq parent-task (point))))
        (goto-char parent-task)
        parent-task)))

  (defun vde/is-task-p ()
    "Any task with a todo keyword and no subtask"
    (save-restriction
      (widen)
      (let ((has-subtask)
            (subtree-end (save-excursion (org-end-of-subtree t)))
            (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
        (save-excursion
          (forward-line 1)
          (while (and (not has-subtask)
                      (< (point) subtree-end)
                      (re-search-forward "^\*+ " subtree-end t))
            (when (member (org-get-todo-state) org-todo-keywords-1)
              (setq has-subtask t))))
        (and is-a-task (not has-subtask)))))

  (defun vde/is-subproject-p ()
    "Any task which is a subtask of another project"
    (let ((is-subproject)
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (while (and (not is-subproject) (org-up-heading-safe))
          (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
            (setq is-subproject t))))
      (and is-a-task is-subproject)))

  ;; Set default column view headings: Task Effort Clock_Summary
  (setq org-columns-default-format "%80ITEM(Task) %TODO %3PRIORITY %10Effort(Effort){:} %10CLOCKSUM")

  (setq org-global-properties (quote (("Effort_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 0:00")
                                      ("STYLE_ALL" . "habit"))))

  (org-clock-persistence-insinuate)
  ;; Show lot of clocking history so it's easy to pick items off the C-F11 list
  (setq org-clock-history-length 23)
  ;; Change tasks to STARTED when clocking in
  (setq org-clock-in-switch-to-state 'vde/clock-in-to-started)
  ;; Clock out when moving task to a done state
  (setq org-clock-out-when-done t)
  ;; Save the running clock and all clock history when exiting Emacs, load it on startup
  (setq org-clock-persist t)

  (defun vde/clock-in-to-started (kw)
    "Switch a task from TODO to STARTED when clocking in.
Skips capture tasks, projects, and subprojects.
Switch projects and subprojects from STARTED back to TODO"
    (when (not (and (boundp 'org-capture-mode) org-capture-mode))
      (cond
       ((and (member (org-get-todo-state) (list "TODO"))
             (vde/is-task-p))
        "STARTED")
       ((and (member (org-get-todo-state) (list "STARTED"))
             (vde/is-project-p))
        "TODO"))))

  (defvar org-capture-templates (list))
  (setq org-protocol-default-template-key "l")

  ;; images
  (setq org-image-actual-width nil
        org-startup-with-inline-images t)

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
                 :empty-lines 1 :clock-in t :clock-resume t))
  (add-to-list 'org-capture-templates
               `("w" "Worklog (journal) entry" entry
                 (file+datetree ,org-default-journal-file)
                 "* worklog :@work:log:\n%U\n** Today\n%?\n** Next (later today, tomorrow)\n"))
  (add-to-list 'org-capture-templates
               `("e" "Weekly review" entry
                 (file+datetree,org-default-journal-file)
                 "* weekly review :weekly:review:\n%U

- [ ] review [[file:../projects/inbox.org][~inbox.org~]]
  Clean the file by either
  - refiling it to ~incubate.org~
  - removing it / archiving it
- [ ] review [[file:../projects/incubate.org][~incubate.org~]]
  - Is something worth becoming a project
  - Is something not worth thinking about anymore ?
- [ ] empty mail inbox (and create task if needed)
  - [ ] work
  - [ ] perso
- [ ] Review next week ~F12 n w f~
- [ ] review ~org-mode~ workflow
  - *what works, what doesn't ?*
  - *is there task / stuck projects ?*
  - *enhancement possible ?*
- [ ] export previous agenda (somewhere)"
                 :clock-in t :clock-resume t))

  ;; Olds, most likely to remove
  (add-to-list 'org-capture-templates
               `("b" "Blog post" entry
                 (file+headline "~/src/github.com/vdemeester/blog/content-org/posts.org" "Blog Ideas")
                 "* %?\n:PROPERTIES:\n:END:\n"))
  (add-to-list 'org-capture-templates
               `("bl" "Blog link post" entry
                 (file+olp "~/src/github.com/vdemeester/blog/content-org/links.org" "Link")
                 "* %a\n%?\n%i"))

  (setq org-ditaa-jar-path "/home/vincent/.nix-profile/lib/ditaa.jar") ;; FIXME(vdemeester) remove /home/vincent
  ;; org-babel
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((css . t)
     (dot . t)
     (ditaa . t)
     (emacs-lisp . t)
     (go . t)
     (gnuplot . t)
     (http . t)
     (js . t)
     ;;(ledger . t)
     (latex . t)
     (python . t)
     ;;(rust . t)
     (shell . t)
     ;;(typescript . t)
     ))

  (setq org-latex-listings t)

  (setq org-list-demote-modify-bullet
        '(("+" . "-") ("-" . "+")))

  (add-to-list 'ispell-skip-region-alist '(":\\(PROPERTIES\\|LOGBOOK\\):" ":END:"))
  (add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_SRC" "#\\+END_SRC"))
  (add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_EXAMPLE" "#\\+END_EXAMPLE"))

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
    (auto-revert-mode)
    (auto-fill-mode)
    (flyspell-mode)
    (org-indent-mode)
    (smartparens-mode)))

(use-package org-id
  :after org
  :custom
  (org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)
  :config
  (defun eos/org-custom-id-get (&optional pom create prefix)
    "Get the CUSTOM_ID property of the entry at point-or-marker POM.
   If POM is nil, refer to the entry at point. If the entry does
   not have an CUSTOM_ID, the function returns nil. However, when
   CREATE is non nil, create a CUSTOM_ID if none is present
   already. PREFIX will be passed through to `org-id-new'. In any
   case, the CUSTOM_ID of the entry is returned."
    (interactive)
    (org-with-point-at pom
      (let ((id (org-entry-get nil "CUSTOM_ID")))
        (cond
         ((and id (stringp id) (string-match "\\S-" id))
          id)
         (create
          (setq id (org-id-new (concat prefix "h")))
          (org-entry-put pom "CUSTOM_ID" id)
          (org-id-add-location id (buffer-file-name (buffer-base-buffer)))
          id)))))

  (defun eos/org-add-ids-to-headlines-in-file ()
    "Add CUSTOM_ID properties to all headlines in the
   current file which do not already have one."
    (interactive)
    (org-map-entries (lambda ()
                       (eos/org-custom-id-get (point) 'create)))))

(use-package ob-go
  :after (org))
(use-package ob-async
  :after (org))
(use-package ob-http
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

(use-package org-notify
  :after org
  :config
  (org-notify-start))

(use-package org-capture-pop-frame)

(use-package darkroom
  :custom
  (darkroom-text-scale-increase 2))
(use-package org-tree-slide
  :after (org darkroom)
  :custom
  (org-tree-slide-breadcrumbs nil)
  (org-tree-slide-header nil)
  (org-tree-slide-slide-in-effect nil)
  (org-tree-slide-heading-emphasis nil)
  (org-tree-slide-cursor-init t)
  (org-tree-slide-modeline-display nil)
  (org-tree-slide-skip-done nil)
  (org-tree-slide-skip-comments t)
  (org-tree-slide-fold-subtrees-skipped t)
  (org-tree-slide-skip-outline-level 8)
  (org-tree-slide-never-touch-face t)
  :config
  (defun prot/org-presentation ()
    "Specifies conditions that should apply locally upon
activation of `org-tree-slide-mode'."
    (if (eq darkroom-tentative-mode nil)
        (progn
          (darkroom-tentative-mode 1)
          (org-indent-mode 1)
          (set-frame-font "Hack-14" t t)
          (setq cursor-type '(bar . 1)))
      (darkroom-tentative-mode -1)
      (org-indent-mode -1)
      (prot/fonts-per-monitor)
      (setq cursor-type 'box)))
  :bind (("<f8>" . org-tree-slide-mode)
         :map org-tree-slide-mode-map
         ("<C-right>" . org-tree-slide-move-next-tree)
         ("<C-left>" . org-tree-slide-move-previous-tree))
  :hook (org-tree-slide-mode . prot/org-presentation))

(use-package orgit
  :after magit)

(provide 'setup-org)
;;; setup-org.el ends here

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
