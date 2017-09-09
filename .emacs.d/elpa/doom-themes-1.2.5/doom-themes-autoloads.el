;;; doom-themes-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "doom-themes" "doom-themes.el" (22964 3868
;;;;;;  350661 333000))
;;; Generated autoloads from doom-themes.el

(autoload 'doom-color "doom-themes" "\
Retrieve a specific color named NAME (a symbol) from the current DOOM theme.

\(fn NAME)" nil nil)

(autoload 'doom-brighten-minibuffer "doom-themes" "\
Highlight the minibuffer whenever it is in use.

\(fn)" nil nil)

(autoload 'doom-buffer-mode "doom-themes" "\
Brighten source buffers by remapping common faces (like default, hl-line and
linum) to their doom-theme variants.

\(fn &optional ARG)" t nil)

(autoload 'doom-themes-neotree-config "doom-themes" "\
Install DOOM neotree configuration.

\(fn)" nil nil)

(autoload 'doom-themes-nlinum-config "doom-themes" "\
Install DOOM nlinum configuration.

\(fn)" nil nil)

(autoload 'doom-buffer-mode-maybe "doom-themes" "\
Enable `doom-buffer-mode' in the current buffer, if it isn't already and the
buffer represents a real file.

\(fn)" nil nil)

(when (and (boundp 'custom-theme-load-path) load-file-name) (add-to-list 'custom-theme-load-path (file-name-as-directory (file-name-directory load-file-name))))

;;;***

;;;### (autoloads nil nil ("doom-molokai-theme.el" "doom-neotree.el"
;;;;;;  "doom-nlinum.el" "doom-one-light-theme.el" "doom-one-theme.el"
;;;;;;  "doom-themes-common.el" "doom-themes-pkg.el") (22964 3868
;;;;;;  359661 337000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; doom-themes-autoloads.el ends here
