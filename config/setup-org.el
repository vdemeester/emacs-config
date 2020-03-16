;;; setup-org.el --- -*- lexical-binding: t -*-

;; OrgConstants
(defconst org-directory "~/desktop/org/" "org-mode directory, where most of the org-mode file lives")
(defconst org-default-projects-dir (concat org-directory "projects") "Primary tasks directory.")
(defconst org-default-technical-dir (concat org-directory "technical") "Directory of shareable, technical notes.")
(defconst org-default-personal-dir (concat org-directory "personal") "Directory of un-shareable, personal notes.")
(defconst org-default-completed-dir (concat org-directory "archive/projects") "Directory of completed project files.")
(defconst org-default-inbox-file (concat org-directory "projects/inbox.org") "New stuff collected in this file.")
(defconst org-default-next-file (concat org-directory "projects/next.org") "Todo *next* collected in this file.")
(defconst org-default-incubate-file (concat org-directory "projects/incubate.org") "Ideas simmering on back burner.")
(defconst org-default-notes-file (concat org-directory "personal/notes.org") "Non-actionable, personal notes.")
(defconst org-default-journal-file (concat org-directory "personal/journal.org") "Journaling stuff.")
;; -OrgConstants

;; OrgRegisters
(set-register ?i `(file . ,org-default-inbox-file))
(set-register ?I `(file . ,org-default-incubate-file))
(set-register ?N `(file . ,org-default-next-file))
(set-register ?n `(file . ,org-default-notes-file))
(set-register ?j `(file . ,org-default-journal-file))
;; -OrgRegisters

;; OrgMain
(use-package s)
(use-package org
  :ensure org-plus-contrib ;; load from the package instead of internal
  :mode (("\\.org$" . org-mode))
  :config
  (setq org-agenda-files `(,org-default-projects-dir
                           ,user-emacs-directory
                           "~/.config/nixpkgs/"
                           "~/.emacs.d/")
        org-agenda-file-regexp "^[a-zA-Z0-9-_]+.org$"
        org-use-speed-commands t
        org-special-ctrl-a/e t
        org-special-ctrl-k t
        org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "STARTED(s)" "|" "DONE(d!)" "CANCELED(c@/!)")
                            (sequence "WAITING(w@/!)" "SOMEDAY(s)" "|" "CANCELED(c@/!)")
                            (sequence "IDEA(i)" "|" "CANCELED(c@/!)"))
        org-todo-state-tags-triggers '(("CANCELLED" ("CANCELLED" . t))
                                       ("WAITING" ("WAITING" . t))
                                       (done ("WAITING"))
                                       ("TODO" ("WAITING") ("CANCELLED"))
                                       ("NEXT" ("WAITING") ("CANCELLED"))
                                       ("DONE" ("WAITING") ("CANCELLED")))
        org-use-tag-inheritance t
        org-tag-alist '(("linux") ("nixos") ("emacs") ("org")
                        ("openshift") ("redhat") ("tektoncd") ("kubernetes") ("knative" ) ("docker")
                        ("docs") ("code") ("review")
                        (:startgroup . nil)
                        ("@home" . ?h) ("@work" . ?w) ("@errand" . ?e) ("@health" . ?l)
                        (:endgroup . nil)
                        (:startgroup . nil)
                        ("@link" . ?i) ("@read" . ?r) ("@project" . ?p)
                        (:endgroup . nil))
        org-log-done 'time
        org-log-redeadline 'time
        org-log-reschedule 'time
        org-log-into-drawer t
        org-enforce-todo-dependencies t
        org-refile-targets (append '((org-default-inbox-file :level . 0))
                                   (->>
                                    (directory-files org-default-projects-dir nil ".org")
                                    (--remove (s-starts-with? "." it))
                                    (--map (format "%s/%s" org-default-projects-dir it))
                                    (--map `(,it :level . 1))))
        org-refile-use-outline-path 'file
        org-refile-allow-creating-parent-nodes 'confirm
        org-outline-path-complete-in-steps nil
        org-columns-default-format "%80ITEM(Task) %TODO %3PRIORITY %10Effort(Effort){:} %10CLOCKSUM"
        org-fontify-whole-heading-line t
        org-pretty-entities t
        org-ellipsis " ⤵"
        org-archive-location (concat org-default-completed-dir "/%s::datetree/")
        org-use-property-inheritance t
        org-global-properties (quote (("EFFORT_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 0:00")
                                      ("STYLE_ALL" . "habit")))
        org-blank-before-new-entry '((heading . t)
                                     (plain-list-item . nil))
        org-insert-heading-respect-content t
        org-yank-adjusted-subtrees t
        org-image-actual-width nil
        org-startup-with-inline-images nil
        org-list-demote-modify-bullet '(("+" . "-") ("-" . "+")))
  (setcar (nthcdr 4 org-emphasis-regexp-components) 10)
  :bind (("C-c o l" . org-store-link)
         ("C-c o r r" . org-refile))
  :hook (org-mode . vde/org-mode-hook))
;; -OrgMain

;; OrgHook
(defun vde/org-mode-hook ()
  "Org-mode hook"
  (setq show-trailing-whitespace t)
  (when (not (eq major-mode 'org-agenda-mode))
    (setq fill-column 90)
    (auto-revert-mode)
    (auto-fill-mode)
    (org-indent-mode)
    (add-hook 'after-save-hook #'save-and-update-includes nil 'make-it-local)))
;; -OrgHook

;; OrgId
(use-package org-id
  :after (org)
  :config
  (setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)
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
;; -OrgId

;; OrgCrypt
(use-package org-crypt
  :after (org)
  :config
  (org-crypt-use-before-save-magic)
  (setq org-tags-exclude-from-inheritance '("crypt")))
;; -OrgCrypt

;; OrgAgenda
(use-package org-agenda
  :after (org)
  :commands (org-agenda)
  :config
  (use-package org-super-agenda
    :config (org-super-agenda-mode))
  (setq org-agenda-span 'day
        org-agenda-start-on-weekday 1
        org-agenda-include-diary t
        org-agenda-window-setup 'current-window
        org-agenda-skip-scheduled-if-done nil
        org-agenda-compact-blocks t
        org-agenda-sticky t
        org-super-agenda-header-separator ""
        org-agenda-custom-commands
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
               (:name "Done" :log closed)
               (:name "Scheduled" :time-grid t)
               (:name "Work" :tag "@work")
               (:name "Perso" :tag "@home")
               (:habit t))))
           (org-agenda-list))))
  :commands (org-agenda)
  :bind (("C-c o a" . org-agenda)
         ("<f12>" . org-agenda)
         ("C-c o r a" . org-agenda-refile)))
;; -OrgAgenda

;; OrgGcal
(use-package org-gcal
  :after (org)
  :commands (org-gcal-fetch)
  :config
  (require 'netrc)

  (defun get-authinfo (host port)
    (let* ((netrc (netrc-parse (expand-file-name "~/.authinfo.gpg")))
           (hostentry (netrc-machine netrc host port port)))
      (when hostentry (netrc-get hostentry "password"))))

  (setq org-gcal-client-id "959564825992-kvc7ofe9640cpc8ibgjqqgpi15e89nkn.apps.googleusercontent.com"
        org-gcal-client-secret (get-authinfo "gcal.api" "9999")
        org-gcal-file-alist '(("vdemeest@redhat.com" . "~/desktop/org/projects/schedule.org"))))
;; -OrgGcal

;; OrgHabit
(use-package org-habit
  :after (org)
  :config
  (setq org-habit-show-habits-only-for-today nil
        org-habit-graph-column 80))
;; -OrgHabit

;; OrgSrc
(use-package org-src
  :after (org)
  :config
  (setq org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-src-window-setup 'split-window-right
        org-edit-src-content-indentation 0))
;; -OrgSrc

;; OrgCaptureStart
(use-package org-capture
  :after org
  :commands (org-capture)
  :config
  ;; -OrgCaptureStart

  ;; OrgCaptureOldTemplate
  (add-to-list 'org-capture-templates
               `("l" "Link" entry
                 (file ,org-default-inbox-file)
                 "* %a\n%U\n%?\n%i"
                 :empty-lines 1))
  (add-to-list 'org-capture-templates
               '("n" "Thought or Note"  entry
                 (file org-default-notes-file)
                 "* %?\n\n  %i\n\n  See: %a" :empty-lines 1))
  ;; -OrgCaptureOldTemplate

  ;; OrgCaptureTask
  (add-to-list 'org-capture-templates
               `("t" "Tasks"))
  (add-to-list 'org-capture-templates
               `("tt" "New task" entry
                 (file ,org-default-inbox-file)
                 "* %?\n:PROPERTIES:\n:CREATED:%U\n:END:\n\n%i\n\nFrom: %a"
                 :empty-lines 1))
  (add-to-list 'org-capture-templates
               `("tr" "PR Review" entry
                 (file ,org-default-inbox-file)
                 "* TODO review gh:%^{issue} :review:\n:PROPERTIES:\n:CREATED:%U\n:END:\n\n%i\n%?\nFrom: %a"
                 :empty-lines 1))
  ;; -OrgCaptureTask

  ;; OrgCaptureJournalBase
  (add-to-list 'org-capture-templates
               `("j" "Journal"))
  ;; -OrgCaptureJournalBase

  ;; OrgCaptureJournalEntry
  (add-to-list 'org-capture-templates
               `("j" "Journal entry"))
  (add-to-list 'org-capture-templates
               `("jj" "Journal entry" entry
                 (file+datetree ,org-default-journal-file)
                 (file ,(concat user-emacs-directory "/etc/orgmode/journal.org"))
                 :empty-lines 1 :clock-in t :clock-resume t))
  ;; -OrgCaptureJournalEntry

  ;; OrgCaptureWorklog
  (add-to-list 'org-capture-templates
               `("jw" "Worklog (journal) entry" entry
                 (file+datetree ,org-default-journal-file)
                 (file ,(concat user-emacs-directory "/etc/orgmode/worklog.org"))
                 :unnarrowed t))
  ;; -OrgCaptureWorklog

  ;; OrgCaptureWeekly
  (add-to-list 'org-capture-templates
               `("je" "Weekly review" entry
                 (file+datetree,org-default-journal-file)
                 (file ,(concat user-emacs-directory "/etc/orgmode/weekly.org"))
                 :clock-in t :clock-resume t :unnarrowed t))
  ;; -OrgCaptureWeekly

  ;; OrgCaptureBlog
  (add-to-list 'org-capture-templates
               `("b" "Blog post"))
  (add-to-list 'org-capture-templates
               `("bp" "Blog post" entry
                 (file+headline "~/src/github.com/vdemeester/blog/content-org/posts.org" "Blog Ideas")
                 "* %?\n:PROPERTIES:\n:END:\n"))
  (add-to-list 'org-capture-templates
               `("bl" "Blog link post" entry
                 (file+olp "~/src/github.com/vdemeester/blog/content-org/links.org" "Link")
                 "* %a\n%?\n%i"))
  ;; -OrgCaptureBlog

  ;; OrgCaptureEnd
  :bind (("C-c o c" . org-capture)))
;; -OrgCaptureEnd

;; OrgProtocol
(use-package org-protocol
  :after org)
;; -OrgProtocol

;; OrgClock
(use-package org-clock
  :after org
  :commands (org-clock-in org-clock-out org-clock-goto)
  :config
  ;; Setup hooks for clock persistance
  (org-clock-persistence-insinuate)
  (setq org-clock-clocked-in-display nil
        ;; Show lot of clocking history so it's easy to pick items off the C-F11 list
        org-clock-history-length 23
        ;; Change tasks to STARTED when clocking in
        org-clock-in-switch-to-state 'vde/clock-in-to-started
        ;; Clock out when moving task to a done state
        org-clock-out-when-done t
        ;; Save the running clock and all clock history when exiting Emacs, load it on startup
        org-clock-persist t)
  (use-package find-lisp)
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
  :bind (("<f11>" . org-clock-goto)))
;; -OrgClock

;; OrgAttach
(use-package org-attach
  :config
  (setq org-link-abbrev-alist '(("att" . org-attach-expand-link))))
;; -OrgAttach

;; OrgLinks
;; my personal
(use-package ol-github
  :after (org))
(use-package ol-gitlab
  :after (org))
(use-package ol-ripgrep
  :after (org))
(use-package ol-grep
  :after (org))

;; built-in org-mode
(use-package ol-eshell
  :after (org))
(use-package ol-git-link
  :after (org))
(use-package ol-gnus
  :after (org))
(use-package ol-irc
  :after (org))
(use-package ol-info
  :after (org))
(use-package ol-man
  :after (org))
(use-package ol-notmuch
  :after (org))
;; -OrgLinks

;; OrgBabel
(use-package ob-async
  :after (org))

(use-package ob-go
  :after (org))

(use-package ob-http
  :after (org))
;; -OrgBabel

;; OrgExportConstants
(defconst site-directory "~/desktop/sites/" "Website folder that holds exported orgmode files and more.")
(defconst org-default-publish-technical (concat site-directory "sbr.pm/technical") "Publish directory for the technical orgmode files.")
;; -OrgExportConstants

;; OrgExportCfg
(use-package ox-publish
  :after (org ox)
  :config
  (setq org-html-coding-system 'utf-8-unix))
(use-package ox-slack
  :after ox)
(use-package ox-hugo
  :after ox
  :commands (org-hugo-slug)
  :config
  (use-package ox-hugo-auto-export))
;; -OrgExportCfg

(use-package org
  :defer t
  :ensure org-plus-contrib
  :config

  (defvar org-capture-templates (list))
  (setq org-protocol-default-template-key "l")

  ;; images
  (setq org-image-actual-width nil
        org-startup-with-inline-images nil)

  ;; Tasks (-> inbox)

  ;; Journal

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
     (shell . t)
     ))

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

(use-package smartparens-org
  :after org-mode)

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
