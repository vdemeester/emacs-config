;;; direnv-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "direnv" "direnv.el" (22977 26779 767070 22000))
;;; Generated autoloads from direnv.el

(autoload 'direnv-update-environment "direnv" "\
Update the environment for FILE-NAME.

\(fn &optional FILE-NAME)" t nil)

(autoload 'direnv-edit "direnv" "\
Edit the .envrc associated with the current directory.

\(fn)" t nil)

(defvar direnv-mode nil "\
Non-nil if Direnv mode is enabled.
See the `direnv-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `direnv-mode'.")

(custom-autoload 'direnv-mode "direnv" nil)

(autoload 'direnv-mode "direnv" "\
Global minor mode to automatically update the environment using direnv.

When this mode is active, the environment inside Emacs will be
continuously updated to match the direnv environment for the currently
visited (local) file.

\(fn &optional ARG)" t nil)

(autoload 'direnv-envrc-mode "direnv" "\
Major mode for .envrc files as used by direnv.

Since .envrc files are shell scripts, this mode inherits from sh-mode.
\\{direnv-envrc-mode-map}

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.envrc\\'" . direnv-envrc-mode))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; direnv-autoloads.el ends here
