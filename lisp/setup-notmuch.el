;;; setup-notmuch.el --- setup notmuch client -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package notmuch
  :defer t
  :bind ("<f10>" . notmuch)
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
	  (:key "S" :name "sent" :query "tag:Sent Mail"))))

(use-package sendmail
  :defer t
  :commands (mail-mode mail-text)
  :defines (send-mail-function)
  :config
  (setq send-mail-function 'sendmail-send-it
	sendmail-program "/home/vincent/.nix-profile/bin/msmtp"
	mail-specify-envelope-from t))

(use-package message
  :commands (message-mode message-cite-original-without-signature)
  :config
  (add-hook 'message-mode-hook 'turn-on-auto-fill)
  (setq  message-default-mail-headers "Cc: \nBcc: \n"
	 message-kill-buffer-on-exit t
	 message-generate-headers-first t))

(provide 'setup-notmuch)
;;; setup-notmuch.el ends here

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
