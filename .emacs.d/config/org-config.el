
(setq org-root-directory (expand-file-name "org" desktop-directory)
      org-sites-directory-name "sites"
      org-archive-directory-name "archive"
      org-archive-file-pattern "/%s_archive::"
      org-inbox-file "inbox.org"
      org-main-file "personal.org"
      org-todos-directory org-root-directory
      org-notes-directory org-root-directory
      org-sites-directory (expand-file-name org-sites-directory-name org-root-directory)
      org-archive-directory (expand-file-name org-archive-directory-name org-root-directory)
      org-docker-file "docker.org"
      org-journal-file "journal.org"
      org-publish-folder (substitute-env-in-file-name "$HOME/var/public_html")
      sites-folder (substitute-env-in-file-name "$HOME/src/sites/"))

(use-package org
  :ensure t)
(require 'find-lisp)
(setq org-directory org-root-directory)
(setq org-agenda-files (find-lisp-find-files org-todos-directory "\.org$"))
(setq org-enforce-todo-dependencies t)
(setq org-enforce-todo-checkbox-dependencies t)

(setq org-log-redeadline (quote time))
(setq org-log-reschedule (quote time))

;;warn me of any deadlines in next 7 days
(setq org-deadline-warning-days 7)
;;show me tasks scheduled or due in next fortnight
(setq org-agenda-span (quote fortnight))
;;don't show tasks as scheduled if they are already shown as a deadline
(setq org-agenda-skip-scheduled-if-deadline-is-shown t)
;;don't give awarning colour to tasks with impending deadlines
;;if they are scheduled to be done
(setq org-agenda-skip-deadline-prewarning-if-scheduled (quote pre-scheduled))
;;don't show tasks that are scheduled or have deadlines in the
;;normal todo list
(setq org-agenda-todo-ignore-deadlines (quote all))
(setq org-agenda-todo-ignore-scheduled (quote all))
;;sort tasks in order of when they are due and then by priority
(setq org-agenda-sorting-strategy
  (quote
   ((agenda deadline-up priority-down)
    (todo priority-down category-keep)
    (tags priority-down category-keep)
    (search category-keep))))

(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "PROGRESS(p)" "PAUSED" "BLOCKED" "REVIEW" "|" "DONE(d!)" "ARCHIVED")
              (sequence "REPORT(r!)" "BUG" "KNOWNCAUSE" "|" "FIXED(f!)")
              (sequence "|" "CANCELLED(c@)"))))
(setq org-todo-keyword-faces
      (quote (("TODO" . org-todo)
              ("PROGRESS" . "green")
              ("PAUSED" . "cyan")
              ("BLOCKED" . "red")
              ("REVIEW" . "yellow")
              ("DONE" . org-done)
              ("ARCHIVED" . org-done)
              ("CANCELLED" . "black")
              ("REPORT" . org-todo)
              ("BUG" . "red")
              ("KNOWNCAUSE" . "yellow")
              ("FIXED" . org-done))))

(setq org-todo-state-tags-triggers
      (quote (("CANCELLED" ("CANCELLED" . t)))))

(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to PROGRESS otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (if (not (string-blank-p (org-get-todo-state)))
        (org-todo (if (= n-not-done 0) "DONE" "PROGRESS")))
    ))

(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

(require 'org-archive)
(setq org-archive-location (concat org-archive-directory org-archive-file-pattern))

(defvar org-my-archive-expiry-days 9
  "The number of days after which a completed task should be auto-archived.
This can be 0 for immediate, or a floating point value.")

(defun org-archive-done-tasks ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\* \\(DONE\\|CANCELED\\) " nil t)
   (if (save-restriction
            (save-excursion
        (org-narrow-to-subtree)
        (search-forward ":LOGBOOK:" nil t)))
          (forward-line)
        (org-archive-subtree)
        (goto-char (line-beginning-position))))))

(defalias 'archive-done-tasks 'org-archive-done-tasks)

(defvar oc-capture-prmt-history nil
  "History of prompt answers for org capture.")
(defun oc/prmt (prompt variable)
  "PROMPT for string, save it to VARIABLE and insert it."
  (make-local-variable variable)
  (set variable (read-string (concat prompt ": ") nil oc-capture-prmt-history)))
(defun oc/inc (what text &rest fmtvars)
  "Ask user to include WHAT.  If user agrees return TEXT."
  (when (y-or-n-p (concat "Include " what "?"))
    (apply 'format text fmtvars)))

(setq org-capture-templates
   '(;; other entries
        ("t" "inbox"
      entry (file (expand-file-name org-inbox-file org-todos-directory))
         "* %?\n%i\n%a")
        ("d" "task"
      entry (file+headline (expand-file-name org-main-file org-todos-directory) "Tasks")
         "* TODO %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n")
        ("d" "docker task"
      entry (file+headline (expand-file-name org-docker-file org-todos-directory) "Tasks")
         "* TODO gh:docker/%(oc/prmt \"project\" 'd-prj)#%(oc/prmt \"issue/pr\" 'd-issue) %?%(oc/inc \"feature content\" \" [/]\n- [ ] Implementation\n- [ ] Tests\n- [ ] Docs\")")
        ("j" "Journal entry"
         entry (file+datetree+prompt (expand-file-name org-journal-file org-root-directory))
         "* %?\n%i\nFrom: %a\n%U" :empty-lines 1)
        ;; other entries
        ))

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

(use-package pt
  :load-path "~/.emacs.d/lisp/pt/")

;; pt-regexp (regexp directory &optional args)
(org-add-link-type
 "pt" 'my/follow-pt-link)
(defun my/follow-pt-link (regexp)
  "Run `pt-regexp` with REXEP and FOLDER as argument,
like this : [[pt:REGEXP:FOLDER]]"
  (setq expressions (split-string regexp ":"))
  (setq exp (nth 0 expressions))
  (if (= (length expressions) 1)
      (progn
        (pt-regexp exp (expand-file-name "./")))
    (progn
      (setq folder (nth 1 expressions))
      (pt-regexp exp (file-name-as-directory (expand-file-name folder)))))
  )

(defvar yt-iframe-format
  ;; You may want to change your width and height.
  (concat "<iframe width=\"440\""
          " height=\"335\""
          " src=\"https://www.youtube.com/embed/%s\""
          " frameborder=\"0\""
          " allowfullscreen>%s</iframe>"))

(org-add-link-type
 "youtube"
 (lambda (handle)
   (browse-url
    (concat "https://www.youtube.com/embed/"
            handle)))
 (lambda (path desc backend)
   (cl-case backend
     (html (format yt-iframe-format
                   path (or desc "")))
     (latex (format "\href{%s}{%s}"
                    path (or desc "video"))))))

(org-add-link-type
 "gh" 'my/follow-gh-link)
(defun my/follow-gh-link (issue)
  "Browse github issue/pr specified"
  (setq expressions (split-string issue "#"))
  (setq project (nth 0 expressions))
  (setq issue (nth 1 expressions))
  (browse-url
   (format "https://github.com/%s/issues/%s" project issue)))

(setq org-link-abbrev-alist
      '(("gmane" . "http://thread.gmane.org/%s")
        ("google" . "https://www.google.com/search?q=%s")
        ("github" . "http://github.com/%s")
        ))

;; from http://endlessparentheses.com/use-org-mode-links-for-absolutely-anything.html
(org-add-link-type
 "tag" 'endless/follow-tag-link)

(defun endless/follow-tag-link (tag)
  "Display a list of TODO headlines with tag TAG.
With prefix argument, also display headlines without a TODO keyword."
  (org-tags-view (null current-prefix-arg) tag))

(setq org-src-fontify-natively t)
(setq org-html-htmlize-output-type 'css)
(setq org-html-htmlize-font-prefix "org-")
(org-babel-do-load-languages
 'org-babel-load-languages
 '( (perl . t)
    (ruby . t)
    (sh . t)
    (python . t)
    (emacs-lisp . t)
    ;; (golang . t)
    (haskell . t)
    (ditaa . t)
    ))

(defun my/org-insert-src-block (src-code-type)
  "Insert a `SRC-CODE-TYPE' type source code block in org-mode."
  (interactive
   (let ((src-code-types
          '("emacs-lisp" "python" "C" "sh" "java" "js" "clojure" "C++" "css"
            "calc" "dot" "gnuplot" "ledger" "R" "sass" "screen" "sql" "awk"
            "ditaa" "haskell" "latex" "lisp" "matlab" "org" "perl" "ruby"
            "sqlite" "rust" "scala" "golang" "restclient")))
     (list (ido-completing-read "Source code type: " src-code-types))))
  (progn
    (newline-and-indent)
    (insert (format "#+BEGIN_SRC %s\n" src-code-type))
    (newline-and-indent)
    (insert "#+END_SRC\n")
    (previous-line 2)
    (org-edit-src-code)))

(defun my/org-insert-html-block ()
  "Insert a `HTML-BLOCK` type in org-mode."
  (interactive
   (progn
     (newline-and-indent)
     (insert "#+BEGIN_HTML\n")
     (newline-and-indent)
     (insert "#+END_HTML\n")
     (previous-line 2))))


(defun my/org-insert-blockquote-block ()
  "Insert a `BLOCKQUOTE-BLOCK` type in org-mode."
  (interactive
   (progn
     (newline-and-indent)
     (insert "#+BEGIN_BLOCKQUOTE\n")
     (newline-and-indent)
     (insert "#+END_BLOCKQUOTE\n")
     (previous-line 2))))



(add-hook 'org-mode-hook
          '(lambda ()
             (local-set-key (kbd "C-c s e") 'org-edit-src-code)
             (local-set-key (kbd "C-c s i") 'my/org-insert-src-block)
             (local-set-key (kbd "C-c s h") 'my/org-insert-html-block)
             (local-set-key (kbd "C-c s b") 'my/org-insert-blockquote-block))
          'append)

(require 'org-archive)
(setq org-archive-location (concat org-archive-directory org-archive-file-pattern))

(defvar org-my-archive-expiry-days 9
  "The number of days after which a completed task should be auto-archived.
This can be 0 for immediate, or a floating point value.")

(defun org-archive-done-tasks ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\* \\(DONE\\|CANCELED\\) " nil t)
   (if (save-restriction
            (save-excursion
        (org-narrow-to-subtree)
        (search-forward ":LOGBOOK:" nil t)))
          (forward-line)
        (org-archive-subtree)
        (goto-char (line-beginning-position))))))

(defalias 'archive-done-tasks 'org-archive-done-tasks)

;; Wish I could use taggroup but it doesn't seem to work..
(setq org-tag-alist '(
                   ("important" . ?i)
                   ("ongoing" . ?o)         ;; ongoing "project", use to filter big project that are on the go
                   ("next" . ?n)            ;; next "project"/"task", use to filter next things to do
                   ("@home" . ?h)           ;; needs to be done at home
                   ("@work" . ?w)           ;; needs to be done at work
                   ("dev" . ?e)             ;; this is a development task
                   ("infra" . ?a)           ;; this is a sysadmin/infra task
                   ("document" . ?d)        ;; needs to produce a document (article, post, ..)
                   ("download" . ?D)        ;; needs to download something
                   ("media" . ?m)           ;; this is a media (something to watch, listen, record, ..)
                   ("mail" . ?M)            ;; mail-related (to write & send or to read)
                   ("triage" . ?t)          ;; need "triage", tag it to easily find them
                   ("task" . ?a)            ;; a simple task (no project), the name is kinda misleading
                   ;; docker-related tags
                   ("docker")
                   ("kubernetes")
                   ("compose")
                   ("moby")
                   ("linuxkit")
                   ("docs")
                   ;; languages
                   ("golang")
                   ("python")
                   ("java")
                   ("clojure")
                   ("emacs-lisp")
                   ;; sites tags
                   ("sites")
                   ("vdf")
                   ;; configs tags
                   ("configs")
                   ("emacs")
                   ("i3")
                   ("shell")
                   ;; services
                   ("services")
                   ))

(defadvice org-clock-in (after sacha activate)
  "Set this task's status to 'PROGRESS'."
  (org-todo "PROGRESS"))

(setq org-agenda-custom-commands
      '(("d" "Daily agenda and all TODOs"
         ((tags "urgent+PRIORITY=\"A\""
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                 (org-agenda-overriding-header "High-priority unfinished tasks:")))
          (agenda "" ((org-agenda-ndays 1)))
          (tags "next"
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                 (org-agenda-overriding-header "Today's tasks")))
          (tags "PRIORITY=\"A\""
                ((org-agenda-skip-function '(or (org-agenda-skip-entry-if 'tag 'urgent)
                                                (org-agenda-skip-entry-if 'todo 'done)))
                 (org-agenda-overriding-header "Kaizen tasks -improvement-")))
          (alltodo ""
                   ((org-agenda-sorting-strategy '(priority-down))
                    (org-agenda-skip-function '(or (org-agenda-skip-entry-if 'todo 'progress)
                                                   (org-agenda-skip-entry-if 'todo 'review)
                                                   (org-agenda-skip-entry-if 'todo 'done)
                                                   (vde/org-skip-subtree-if-habit)
                                                   (vde/org-skip-subtree-if-priority ?A)
                                                   (org-agenda-skip-if nil '(scheduled deadline))))
                    (org-agenda-overriding-header "ALL normal priority tasks:"))))
         ((org-agenda-compact-blocks t)))
        ("t" todo "TODO"
         ((org-agenda-sorting-strategy '(priority-down))
          (org-agenda-prefix-format "  Mixed: ")))
        ("p" todo "PROGRESS"
         ((org-agenda-sorting-strategy '(priority-down))
          (org-agenda-prefix-format "  Mixed: ")))
        ("r" todo "REVIEW"
         ((org-agenda-sorting-strategy '(priority-down))
          (org-agenda-prefix-format "  Mixed: ")))
        ("u" todo "PAUSED"
         ((org-agenda-sorting-strategy '(priority-down))
          (org-agenda-prefix-format "  Mixed: ")))
        ("b" todo "BLOCKED"
         ((org-agenda-sorting-strategy '(priority-down))
          (org-agenda-prefix-format "  Mixed: ")))
        ("n" "Next tasks" tags-todo "next"
         ((org-agenda-sorting-strategy '(priority-down))
          (org-tags-exclude-from-inheritance '("next"))
          (org-agenda-prefix-format "  Mixed: ")))
        ("i" "Triage tasks — to look" tags-todo "triage"
         ((org-agenda-sorting-strategy '(priority-down))
          (org-agenda-prefix-format "  Mixed: ")))
        ;; Timelines
        ("d" "Timeline for today" ((agenda "" ))
         ((org-agenda-ndays 1)
          (org-agenda-show-log t)
          (org-agenda-log-mode-items '(clock closed))
          (org-agenda-clockreport-mode t)
          (org-agenda-entry-types '())))
        ("w" "Weekly review" agenda ""
         ((org-agenda-span 7)
          (org-agenda-log-mode 1)))
        ("W" "Weekly review sans DAILY" agenda ""
         ((org-agenda-span 7)
          (org-agenda-log-mode 1)
          (org-agenda-tag-filter-preset '("-DAILY"))))
        ;; Panic tasks : urgent & important
        ;; Probably the most important to do, but try not have to much of them..
        ("P" "Panic -emergency-" tags-todo "urgent+PRIORITY=\"A\""
         ((org-agenda-sorting-strategy '(priority-down))
          (org-agenda-prefix-format "  Mixed: ")))
        ;; Kaizen tasks : important but not urgent
        ("K" "Kaizen -improvement-" tags-todo "PRIORITY=\"A\"&-urgent"
         ((org-agenda-sorting-strategy '(priority-down))
          (org-agenda-prefix-format "  Mixed: ")))
        ;; Social investment : urgent
        ("S" "Social -investment-" tags-todo "-PRIORITY=\"A\"+urgent"
         ((org-agenda-sorting-strategy '(priority-down))
          (org-agenda-prefix-format "  Mixed: ")))
        ;; Organics
        ("O" "Organics -inspiration-" tags-todo "-PRIORITY=\"A\"&-urgent"
         ((org-agenda-sorting-strategy '(priority-down))
          (org-agenda-prefix-format "  Mixed: ")))
        ("N" search ""
         ((org-agenda-text-search-extra-files nil)))))

(defun vde/org-skip-subtree-if-priority (priority)
  "Skip an agenda subtree if it has a priority of PRIORITY.

PRIORITY may be one of the characters ?A, ?B, or ?C."
  (let ((subtree-end (save-excursion (org-end-of-subtree t)))
        (pri-value (* 1000 (- org-lowest-priority priority)))
        (pri-current (org-get-priority (thing-at-point 'line t))))
    (if (= pri-value pri-current)
        subtree-end
      nil)))

(defun vde/org-skip-subtree-if-habit ()
  "Skip an agenda entry if it has a STYLE property equal to \"habit\"."
  (let ((subtree-end (save-excursion (org-end-of-subtree t))))
    (if (string= (org-entry-get nil "STYLE") "habit")
        subtree-end
      nil)))

(use-package htmlize
  :ensure t
  :defer t)
;;      (setq org-html-head "<link rel=\"stylesheet\" type=\"text/css\" hrefl=\"css/stylesheet.css\" />")
(setq org-html-include-timestamps nil)
;; (setq org-html-htmlize-output-type 'css)
(setq org-html-head-include-default-style nil)

(use-package ox-publish)
;; (use-package ox-rss)

(setq org-html-html5-fancy t)

;; Define some variables to write less :D
(setq sbr-base-directory (expand-file-name "sbr" org-sites-directory)
      sbr-publishing-directory (expand-file-name "sbr" org-publish-folder)
      vdf-base-directory (expand-file-name "vdf" org-sites-directory)
      vdf-site-directory (expand-file-name "blog" github-personal-folder)
      vdf-publishing-directory (expand-file-name "posts" (expand-file-name "content" vdf-site-directory))
      vdf-static-directory (expand-file-name "static" vdf-site-directory)
      vdf-css-publishing-directory (expand-file-name "css" vdf-static-directory)
      vdf-assets-publishing-directory vdf-static-directory)

;; Project
(setq org-publish-project-alist
      `(("sbr-notes"
         :base-directory ,sbr-base-directory
         :base-extension "org"
         :publishing-directory ,sbr-publishing-directory
         :makeindex t
         :exclude "FIXME"
         :recursive t
         :htmlized-source t
         :publishing-function org-html-publish-to-html
         :headline-levels 4
         :auto-preamble t
         :html-head "<link rel=\"stylesheet\" type=\"text/css\" href=\"style/style.css\" />"
         :html-preamble "<div id=\"nav\">
<ul>
<li><a href=\"/\" class=\"home\">Home</a></li>
</ul>
</div>"
         :html-postamble "<div id=\"footer\">
%a %C %c
</div>")
        ("sbr-static"
         :base-directory ,sbr-base-directory
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg"
         :publishing-directory ,sbr-publishing-directory
         :recursive t
         :publishing-function org-publish-attachment
         )
        ("sbr" :components ("sbr-notes" "sbr-static"))
        ("vdf-notes"
         :base-directory ,vdf-base-directory
         :base-extension "org"
         :publishing-directory ,vdf-publishing-directory
         :exclude "FIXME"
         :section-numbers nil
         :with-toc nil
         :with-drawers t
         :htmlized-source t
         :org-html-htmlize-output-type 'css
         :html-html5-fancy t
         :publishing-function org-html-publish-to-html
         :headline-levels 4
         :body-only t)
        ("vdf-static-css"
         :base-directory ,vdf-base-directory
         :base-extension "css"
         :publishing-directory ,vdf-css-publishing-directory
         :recursive t
         :publishing-function org-publish-attachment
         )
        ("vdf-static-assets"
         :base-directory ,vdf-base-directory
         :base-extension "png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg"
         :publishing-directory ,vdf-assets-publishing-directory
         :recursive t
         :publishing-function org-publish-attachment
         )
        ("vdf" :components ("vdf-notes" "vdf-static-css" "vdf-static-assets"))
        ))

(provide 'org-config)
