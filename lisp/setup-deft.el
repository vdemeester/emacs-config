;;; setup-deft.el -- setup deft
;;; Commentary:
;;; Code:
;;; -*- lexical-binding: t; -*-

(use-package deft
  :commands (deft)
  :bind (("<f9>" . deft)
         (:map deft-mode-map
               ("<f9>" . quit-window)
               ("C-g" . deft-filter-clear)
               ("C-c C-c" . deft-refresh)
               ("<M-return>" . deft-new-file)))
  :config
  (setq deft-extensions '("org" "md")
        deft-default-extension "org"
        deft-directory "~/desktop/org"
        deft-recursive t
        deft-recursive-ignore-dir-regexp (concat
                                          "\\(?:"
                                          "\\."
                                          "\\|\\.\\."
                                          "\\|archive"
                                          "\\|learn"
                                          "\\)")
        deft-ignore-file-regexp (concat
                                 "\\(?:"
                                 "inbox.org"
                                 "\\|todoist.org"
                                 "\\|journal.org"
                                 "\\)")
        deft-auto-save-interval 0)
  ;; display filter in mode-line instead of header
  (defun deft-print-header () (deft-set-mode-name))
  (defun deft-set-mode-name ()
    "Set the mode line text based on search mode and add the filter."
    (let* ((name (if deft-incremental-search "Deft" "Deft/R"))
           (filter (deft-whole-filter-regexp))
           (sep (unless (string= "" filter) "/")))
      (setq mode-name (concat name sep filter))))
  (setq deft-strip-summary-regexp
        (concat "\\("
                "[\n\t]" ;; blank
                "\\|^#\\+[[:upper:]_]+:.*$" ;; org-mode metadata
                "\\|^# .*$" ;; md titles
                "\\)")))

(provide 'setup-deft)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
