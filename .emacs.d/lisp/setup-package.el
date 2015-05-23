(require 'package)

;; add org to package repos
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))

;; add melpa and melpa-stable to package repos
(add-to-list 'package-archives '("mela-stable" . "http://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

;; elpy
(add-to-list 'package-archives
             '("elpy" . "http://jorgenschaefer.github.io/packages/"))

;; If gpg cannot be found, signature checking will fail, so we
;; conditionnally enable it according wether gpg is availabel.
;; We re-run this check once $PATH has been configured
(defun sanityinc/package-maybe-enable-signatures ()
  (setq package-check-signature (when (executable-find "gpg") 'allow-unsigned)))

(sanityinc/package-maybe-enable-signatures)

;; Fire up package.el
(package-initialize)

;; Load package contents if not present
(when (not package-archive-contents)
  (package-refresh-contents))

;; Load use-package
(require 'use-package)

(provide 'setup-package)
