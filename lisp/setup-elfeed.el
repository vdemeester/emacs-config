;;; setup-elfeed.el --- setup elfeed client -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package elfeed
  :config
  (setq elfeed-db-directory "~/sync/elfeed/"))
(use-package elfeed-web)
(use-package elfeed-org
  :init
  (elfeed-org)
  (setq rmh-elfeed-org-files (list (concat org-default-personal-dir "/elfeed.org"))))

(provide 'setup-elfeed)
;;; setup-elfeed.el ends here

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
