;;; -*- lexical-binding: t; -*-
(setq user-mail-address "vincent@demeester.fr"
      user-full-name "Vincent Demeester")

(setq gnus-select-method
      '(nnimap "vincent@demeester.fr"
               (nnimap-address "mail.gandi.net")  ; it could also be imap.googlemail.com if that's your server.
               (nnimap-server-port "imaps")
               (nnimap-stream ssl)))

(setq smtpmail-smtp-server "mail.gandi.net"
      smtpmail-smtp-service 587
      gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")

;; (require 'nnrss)
;; (nnrss-opml-import "~/desktop/downloads/feedly.opml")

(provide setup-gnus)
