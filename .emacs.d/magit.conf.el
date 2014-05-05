(defcustom magit-svn-externals-dir ".git_externals"
  "Directory from repository root that stores cloned SVN externals."
  :group 'magit
  :type 'string)

(defun magit-svn-fetch-externals()
  "Loops through all external repos found by `magit-svn-external-directories'
and runs git svn fetch, and git svn rebase on each of them."
  (interactive)
  (let ((externals (magit-svn-external-directories)))
    (if (not externals)
        (message "No SVN Externals found. Check magit-svn-externals-dir.")
      (dolist (external externals)
        (let ((default-directory (file-name-directory external)))
          (magit-run-git "svn" "fetch")
          (magit-run-git "svn" "rebase")))
      (magit-refresh))))

(defun magit-svn-external-directories()
  "Returns all .git directories within `magit-svn-externals-dir'"
  (require 'find-lisp)
  (find-lisp-find-files-internal (expand-file-name magit-svn-externals-dir)
                                 '(lambda(file dir)
                                    (string-equal file ".git"))
                                 'find-lisp-default-directory-predicate))

; (magit-key-mode-insert-action 'svn "x" "Fetch Externals" 'magit-svn-fetch-externals)

(add-hook 'magit-mode-hook (lambda()
                             (require 'magit-svn)
                             (if (magit-svn-get-ref-info)
                                 (magit-svn-mode))))
