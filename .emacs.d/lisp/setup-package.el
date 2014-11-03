(require 'package)

;; add org to package repos
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))

;; add melpa and melpa-stable to package repos
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

;; If gpg cannot be found, signature checking will fail, so we
;; conditionnally enable it according wether gpg is availabel.
;; We re-run this check once $PATH has been configured
(defun sanityinc/package-maybe-enable-signatures ()
  (setq package-check-signature (when (executable-find "gpg") 'allow-unsigned)))

(sanityinc/package-maybe-enable-signatures)

;; On demand installation of packages
(defun require-package (package &optional min-version no-refresh)
  "Install given PACKAGE, optionally requiring MIN-VERSION.
if NO-REFRESH is non-nil, the available package lists will not be
re-downloaded in order to locate PACKAGE."
  (if (package-installed-p package min-version)
      t
    (if (or (assoc package package-archive-contents) no-refresh)
	(package-install package)
      (progn
	(when (not package-archive-contents)
	  (package-refresh-contents))
	(require-package package min-version t)))))

;; Fire up package.el
(package-initialize)

;; install fullframe for list-packages
(require-package 'fullframe)
(fullframe list-packages quit-window)

(provide 'setup-package)
