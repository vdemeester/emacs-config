;; 
;; 
;; Initial emacs configuration files. We'll keep it as small as possible
;; as the real configuration appends in an org file (litterate config.)

;; Support for Emacs 24 and higher only

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(let ((minver 24))
  (unless (>= emacs-major-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))

;; Keep track of loading time
(defconst emacs-start-time (current-time))

;; Add custom lisp files to the load-path
(add-to-list 'load-path "~/.emacs.d/lisp")
;; Add a specific version of use-package
(add-to-list 'load-path "~/.emacs.d/lisp/use-package")

(require 'vde-functions)
;; initialize all ELPA packages
(require 'setup-package)

(let ((elapsed (float-time (time-subtract (current-time)
					   emacs-start-time))))
  (message "Loaded packages in %.3fs" elapsed))

(setq org-root-directory (substitute-env-in-file-name "$HOME/desktop/org"))

;; keep customize settings in their own file
(setq custom-file
      (expand-file-name "custom.el"
			user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; Load the generated file from emacs.org
(load-file "~/.emacs.d/emacs.el")

(let ((elapsed (float-time (time-subtract (current-time)
					   emacs-start-time))))
  (message "Loaded settings...done in %.3fs" elapsed))


;; TODO(vdemeester) move t«his to the org-file
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
