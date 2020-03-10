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
