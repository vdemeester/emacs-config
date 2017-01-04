(require 'package)
(require 'cl-lib)

;; add org to package repos
;; (add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/"))

;; add melpa and melpa-stable to package repos
(add-to-list 'package-archives '("mela-stable" . "https://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(setq use-package-always-pin "melpa")

;; elpy
(add-to-list 'package-archives
             '("elpy" . "https://jorgenschaefer.github.io/packages/"))

(setq tls-checktrust t)
(setq gnutls-verify-error t)

(let ((trustfile "/etc/ssl/cert.pem"))
  (setq tls-program
        `(,(format  "gnutls-cli --x509cafile %s -p %%p %%h" trustfile)
          ,(format "openssl s_client -connect %%h:%%p -CAfile %s -no_ssl2 -ign_eof" trustfile)))
  (setq gnutls-trustfiles (list trustfile)))

(let ((bad-hosts
       (cl-loop for bad
             in `("https://wrong.host.badssl.com/"
                  "https://self-signed.badssl.com/")
             if (condition-case e
                    (url-retrieve
                     bad (lambda (retrieved) t))
                  (error nil))
             collect bad)))
  (if bad-hosts
      (error (format "tls misconfigured; retrieved %s ok"
                     bad-hosts))
    (url-retrieve "https://badssl.com"
                  (lambda (retrieved) t))))

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
