;;; magit-annex-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "magit-annex" "magit-annex.el" (22792 62847
;;;;;;  481796 397000))
;;; Generated autoloads from magit-annex.el

(eval-after-load 'magit '(progn (define-key magit-mode-map "@" 'magit-annex-popup-or-init) (magit-define-popup-action 'magit-dispatch-popup 64 "Annex" 'magit-annex-popup-or-init 33)))

(autoload 'magit-annex-popup-or-init "magit-annex" "\
Call Magit-annex popup or offer to initialize non-annex repo.

\(fn)" t nil)

(autoload 'magit-annex-init "magit-annex" "\
Initialize git-annex repository.
\('git annex init [DESCRIPTION]')

\(fn &optional DESCRIPTION)" t nil)

(autoload 'magit-annex-unused "magit-annex" "\
Show unused data.
\('git annex unused [ARGS]')

\(fn &optional ARGS)" t nil)

(autoload 'magit-annex-unused-reflog "magit-annex" "\
Show unused data.
\('git annex unused --used-refspec=reflog [ARGS]')

\(fn &optional ARGS)" t nil)

(autoload 'magit-annex-list-files "magit-annex" "\
List annex files.
\('git annex list [ARGS]')

\(fn &optional ARGS)" t nil)

(autoload 'magit-annex-list-dir-files "magit-annex" "\
List annex files in DIRECTORY.
\('git annex list [ARGS] DIRECTORY')

\(fn DIRECTORY &optional ARGS)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; magit-annex-autoloads.el ends here
