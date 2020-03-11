;;; setup-mails.el --- -*- lexical-binding: t -*-

;; AuthSource
(use-package auth-source
  :config
  (setq auth-sources '("~/.authinfo.gpg" "~/.authinfo"))
  (setq user-full-name "Vincent Demeester")
  (setq user-mail-address "vincent@sbr.pm"))
;; -AuthSource

;; EPA
(use-package epa-file
  :config
  (setq epa-file-cache-passphrase-for-symmetric-encryption t)
  :init
  (epa-file-enable))
;; -EPA

;; GnusCfg
(use-package gnus
  :config
  (setq nnml-directory "~/desktop/gnus/mail")
  (setq nnfolder-directory "~/desktop/gnus/archive")
  (setq nndraft-directory "~/desktop/gnus/drafts")
  (setq nnmh-directory "~/desktop/gnus/drafts")
  (setq gnus-article-save-directory "~/desktop/gnus/news")
  (setq gnus-home-directory "~/desktop/gnus")
  (setq gnus-kill-files-directory "~/desktop/gnus/news")
  (setq gnus-cache-directory "~/desktop/gnus/news/cache")
  (setq gnus-startup-file "~/desktop/gnus/newsrc")
  (setq mail-source-directory "~/desktop/gnus/mail")
  (setq gnus-registry-cache-file "~/desktop/gnus/gnus.registry.eld")
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
            (address "vincent@demeester.fr")
            (signature-file "~/desktop/documents/.prv.signature")
            (gcc "nnimap+prv:Sent")))
          ("redhat"
           (posting-style
            (address "vdemeest@redhat.com")
            (signature-file "~/desktop/documents/.redhat.signature")))
          ("nnimap+redhat:INBOX"
           (display . all))
          ("vde"
           (posting-style
            (address "vinc.demeester.fr")
            (signature-file "~/desktop/documents/.vde.signature")))
          ("nnimap+vde:INBOX"
           (display . all))))
  (setq gnus-agent t)
  (setq mail-user-agent 'gnus-user-agent) ; also works with `sendmail-user-agent'
  (setq gnus-check-new-newsgroups 'ask-server)
  (setq gnus-read-active-file 'some)
  (setq gnus-use-dribble-file t)
  (setq gnus-always-read-dribble-file t)
  (setq gnus-novice-user nil)
  (setq gnus-extra-headers
        '(To Newsgroups X-GM-LABELS)))
;; -GnusCfg

;; GnusMmlSec
(use-package mml-sec
  :config
  (setq mml-secure-openpgp-signers
        '("8C4E8DDA04C18C6B503BD2DBB7E7CF1C634256FA")))
;; -GnusMmlSec

;; GnusAgent
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
;; -GnusAgent

;; GnusAsync
(use-package gnus-async
  :after gnus
  :config
  (setq gnus-asynchronous t)
  (setq gnus-use-article-prefetch 30))
;; -GnusAsync

;; GnusGroup
(use-package gnus-group
  :after gnus
  :config
  (setq gnus-level-subscribed 6)
  (setq gnus-level-unsubscribed 7)
  (setq gnus-level-zombie 8)
  (setq gnus-group-sort-function
        '((gnus-group-sort-by-unread)
          (gnus-group-sort-by-alphabet)
          (gnus-group-sort-by-rank)))
  (setq gnus-group-mode-line-format "Gnus: %%b")
  :hook
  (gnus-select-group-hook . gnus-group-set-timestamp)
  :bind (:map gnus-agent-group-mode-map
              ("M-n" . gnus-topic-goto-next-topic)
              ("M-p" . gnus-topic-goto-previous-topic)))
;; -GnusGroup

;; GnusTopic
(use-package gnus-topic
  :after (gnus gnus-group)
  :config
  (setq gnus-topic-display-empty-topics t)
  :hook
  (gnus-group-mode . gnus-topic-mode))
;; -GnusTopic

;; GnusSummary
(use-package gnus-sum
  :after (gnus gnus-group)
  :demand
  :config
  (setq gnus-auto-select-first nil)
  (setq gnus-summary-ignore-duplicates t)
  (setq gnus-suppress-duplicates t)
  (setq gnus-summary-goto-unread nil)
  (setq gnus-summary-make-false-root 'adopt)
  (setq gnus-summary-thread-gathering-function 'gnus-gather-threads-by-subject)
  (setq gnus-thread-sort-functions
        '((not gnus-thread-sort-by-number)
          (not gnus-thread-sort-by-date)))
  (setq gnus-subthread-sort-functions
        'gnus-thread-sort-by-date)
  (setq gnus-thread-hide-subtree nil)
  (setq gnus-thread-ignore-subject t)
  (setq gnus-user-date-format-alist
        '(((gnus-seconds-today) . "Today at %R")
          ((+ 86400 (gnus-seconds-today)) . "Yesterday, %R")
          (t . "%Y-%m-%d %R")))
  (setq gnus-summary-line-format "%U%R%z %-16,16&user-date;  %4L:%-30,30f  %B%S\n")
  (setq gnus-summary-mode-line-format "Gnus: %p (%U)")
  (setq gnus-sum-thread-tree-false-root "")
  (setq gnus-sum-thread-tree-indent " ")
  (setq gnus-sum-thread-tree-leaf-with-other "├─➤ ")
  (setq gnus-sum-thread-tree-root "")
  (setq gnus-sum-thread-tree-single-leaf "└─➤ ")
  (setq gnus-sum-thread-tree-vertical "│")
  :hook
  (gnus-summary-exit-hook . gnus-topic-sort-groups-by-alphabet)
  (gnus-summary-exit-hook . gnus-group-sort-groups-by-rank)
  :bind (:map gnus-agent-summary-mode-map
              ("<delete>" . gnus-summary-delete-article)
              ("n" . gnus-summary-next-article)
              ("p" . gnus-summary-prev-article)
              ("N" . gnus-summary-next-unread-article)
              ("P" . gnus-summary-prev-unread-article)
              ("M-n" . gnus-summary-next-thread)
              ("M-p" . gnus-summary-prev-thread)
              ("C-M-n" . gnus-summary-next-group)
              ("C-M-p" . gnus-summary-prev-group)
              ("C-M-^" . gnus-summary-refer-thread)))
;; -GnusSummary

;; GnusDired
(use-package gnus-dired
  :after (gnus dired)
  :hook (dired-mode . gnus-dired-mode))
;; -GnusDired


;; SendmailCfg
(use-package smtpmail
  :config
  (setq message-send-mail-function 'message-send-mail-with-sendmail)
  (setq sendmail-program "msmtp")
  (setq message-sendmail-f-is-evil 't)
  (setq message-sendmail-extra-arguments '("--read-envelope-from")))

(use-package sendmail
  :defer t
  :commands (mail-mode mail-text)
  :defines (send-mail-function)
  :config
  (setq send-mail-function 'sendmail-send-it
        sendmail-program "/home/vincent/bin/msmtp"))
;; -SendmailCfg

;; MessageCfg
(use-package message
  :commands (message-mode message-cite-original-without-signature)
  :config
  (setq mail-user-agent 'message-user-agent
        message-wide-reply-confirm-recipients t
        message-default-charset 'utf-8
        message-default-mail-headers "Cc: \nBcc: \n"
        message-kill-buffer-on-exit t
        message-generate-headers-first t)
  (add-to-list 'mm-body-charset-encoding-alist '(utf-8 . base64))
  (add-hook 'message-mode-hook 'turn-on-auto-fill))
;; -MessageCfg

;; Notmuch
(if *sys/full*
    (progn
      (setenv "NOTMUCH_CONFIG" (expand-file-name ".config/notmuch/notmuchrc" (getenv "HOME")))
      (use-package notmuch
        :defer t
        :bind ("<f6>" . notmuch)
        :config
        (setq notmuch-search-oldest-first nil
              mail-user-agent 'message-user-agent
              notmuch-tree-show-out t)
        (setq notmuch-saved-searches
              '((:key "i" :name "inbox" :query "tag:Inbox")
                (:key "r" :name "redhat inbox folder" :query "folder:redhat/Inbox")
                (:key "p" :name "perso inbox folder" :query "folder:perso/Inbox")
                (:key "u" :name "unread" :query "tag:unread")
                (:key "F" :name "flagged" :query "tag:flagged")
                (:key "S" :name "sent" :query "tag:Sent Mail"))))))
;; -Notmuch

(provide 'setup-mails)
;;; setup-mails ends here
