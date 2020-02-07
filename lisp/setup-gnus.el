(use-package auth-source
  :config
  (setq auth-sources '("~/.authinfo.gpg" "~/.authinfo"))
  (setq user-full-name "Vincent Demeester")
  (setq user-mail-address "vincent@sbr.pm"))

(use-package epa-file
  :config
  (setq epa-file-cache-passphrase-for-symmetric-encryption t)
  :init
  (epa-file-enable))

(use-package gnus
  :hook
  (gnus-group-mode-hook . gnus-topic-mode)
  :config
  (setq nnml-directory "~/desktop/gnus/mail/")
  (setq nnfolder-directory "~/desktop/gnus/archive/")
  (setq nndraft-directory "~/desktop/gnus/drafts/")
  (setq nnmh-directory "~/desktop/gnus/drafts/")
  (setq gnus-select-method '(nnnil))
  (setq nntp-authinfo-file "~/.authinfo.gpg")
  (setq gnus-secondary-select-methods
        '((nntp "news.gwene.org")
          (nnimap "prv"
                  (nnimap-address "mail.gandi.net")
                  (nnimap-stream ssl)
                  (nnimap-authinfo-file "~/.authinfo.gpg"))
          (nnimap "redhat"
                  (nnimap-address "imap.gmail.com")
                  (nnimap-stream ssl)
                  (nnimap-authinfo-file "~/.authinfo.gpg"))
          (nnimap "vde"
                  (nnimap-address "imap.gmail.com")
                  (nnimap-stream ssl)
                  (nnimap-authinfo-file "~/.authinfo.gpg"))
          ))
  (setq gnus-parameters
        '(("prv"
           (posting-style
            (gcc "nnimap+prv:Sent")))
          ("redhat"
           (posting-style
            (gcc "nnimap+redhat:Sent")))
          ("nnimap redhat:INBOX"
           (display . all))
          ("vde"
           (posting-style
            (gcc "nnimap+vinc.demeester:Sent")))))
  (setq gnus-agent t)
  (setq mail-user-agent 'gnus-user-agent) ; also works with `sendmail-user-agent'
  (setq gnus-check-new-newsgroups 'ask-server)
  (setq gnus-read-active-file 'some)
  (setq gnus-use-dribble-file t)
  (setq gnus-always-read-dribble-file t)
  (setq gnus-extra-headers
        '(To Newsgroups X-GM-LABELS)))

(use-package gnus-agent
  :after gnus
  :config
  (setq gnus-agent-article-alist-save-format 1)  ; uncompressed
  (setq gnus-agent-cache t)
  (setq gnus-agent-confirmation-function 'y-or-n-p)
  (setq gnus-agent-consider-all-articles nil)
  (setq gnus-agent-directory "~/desktop/gnus/agent/")
  (setq gnus-agent-enable-expiration 'ENABLE)
  (setq gnus-agent-expire-all nil)
  (setq gnus-agent-expire-days 30)
  (setq gnus-agent-mark-unread-after-downloaded t)
  (setq gnus-agent-queue-mail t)        ; queue if unplugged
  (setq gnus-agent-synchronize-flags nil))

(setq smtpmail-smtp-server "mail.gandi.net"
      smtpmail-smtp-service 587
      gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")

;; (require 'nnrss)
;; (nnrss-opml-import "~/desktop/downloads/feedly.opml")

(provide 'setup-gnus)
