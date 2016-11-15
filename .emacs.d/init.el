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

;; Make sure we have a decent and recent org-mode version
(unload-org-mode)
(require 'org)
(when (string-match "^[12345678]" (org-version))
  (progn
    (warn "Org-mode is out of date. We expect org 8 or higher, but instead we have %s" (org-version))
    (warn "Force the installation from org elpa.")
    (package-install 'org)
    (unload-org-mode)
    (require 'org)
    ))

(setq org-root-directory (substitute-env-in-file-name "$HOME/desktop/org"))

;; keep customize settings in their own file
(setq custom-file
      (expand-file-name "custom.el"
			user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; load the literate configuration
(require 'ob-tangle)

(org-babel-load-file "~/.emacs.d/emacs.org")

(let ((elapsed (float-time (time-subtract (current-time)
					   emacs-start-time))))
  (message "Loaded settings...done in %.3fs" elapsed))


;; TODO(vdemeester) move t«his to the org-file
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
