;;; setup-browser.el --- setup eww and other browser -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package eww
  :defer t
  :init
  (setq browse-url-browser-function
        '((".*google.*maps.*" . browse-url-generic)
          ;; Github goes to firefox, but not gist
          ("http.*\/\/github.com" . browse-url-generic)
          ("http.*\/\/github.io" . browse-url-generic)
          ("groups.google.com" . browse-url-generic)
          ("docs.google.com" . browse-url-generic)
          ("melpa.org" . browse-url-generic)
          ("build.*\.elastic.co" . browse-url-generic)
          (".*-ci\.elastic.co" . browse-url-generic)
          ("internal-ci\.elastic\.co" . browse-url-generic)
          ("zendesk\.com" . browse-url-generic)
          ("salesforce\.com" . browse-url-generic)
          ("stackoverflow\.com" . browse-url-generic)
          ("apache\.org\/jira" . browse-url-generic)
          ("thepoachedegg\.net" . browse-url-generic)
          ("zoom.us" . browse-url-generic)
          ("t.co" . browse-url-generic)
          ("twitter.com" . browse-url-generic)
          ("\/\/a.co" . browse-url-generic)
          ("youtube.com" . browse-url-generic)
          ("amazon.com" . browse-url-generic)
          ("slideshare.net" . browse-url-generic)
          ("." . eww-browse-url)))
  (setq shr-external-browser 'browse-url-generic)
  (setq browse-url-generic-program (executable-find "firefox"))
  (add-hook 'eww-mode-hook #'toggle-word-wrap)
  (add-hook 'eww-mode-hook #'visual-line-mode)
  :config
  (define-key eww-mode-map "o" 'eww)
  (define-key eww-mode-map "O" 'eww-browse-with-external-browser))

(use-package ace-link
  :defer t
  :config
  (ace-link-setup-default))

(provide 'setup-browser)
;;; setup-browser.el ends here

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
