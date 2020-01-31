;;; -*- lexical-binding: t; -*-
;; Ignore directories during grep
(with-eval-after-load 'grep
  '(progn
     (add-to-list 'grep-find-ignored-directories "auto")
     (add-to-list 'grep-find-ignored-directories "elpa")))

;; Truncate lines during grep
(add-hook 'grep-mode-hook #'toggle-truncate-lines)
(use-package isearch
  :custom
  (search-whitespace-regexp ".*?")
  (isearch-lax-whitespace t)
  (isearch-regexp-lax-whitespace nil)
  :config
  (defun prot/isearch-mark-and-exit ()
    "Marks the current search string.  Can be used as a building
block for a more complex chain, such as to kill a region, or
place multiple cursors."
    (interactive)
    (push-mark isearch-other-end t 'activate)
    (setq deactivate-mark nil)
    (isearch-done))

  (defun stribb/isearch-region (&optional not-regexp no-recursive-edit)
    "If a region is active, make this the isearch default search
pattern."
    (interactive "P\np")
    (when (use-region-p)
      (let ((search (buffer-substring-no-properties
                     (region-beginning)
                     (region-end))))
        (message "stribb/ir: %s %d %d" search (region-beginning) (region-end))
        (setq deactivate-mark t)
        (isearch-yank-string search))))
  (advice-add 'isearch-forward-regexp :after 'stribb/isearch-region)
  (advice-add 'isearch-forward :after 'stribb/isearch-region)
  (advice-add 'isearch-backward-regexp :after 'stribb/isearch-region)
  (advice-add 'isearch-backward :after 'stribb/isearch-region)

  (defun contrib/isearchp-remove-failed-part-or-last-char ()
    "Remove failed part of search string, or last char if successful.
Do nothing if search string is empty to start with."
    (interactive)
    (if (equal isearch-string "")
        (isearch-update)
      (if isearch-success
          (isearch-delete-char)
        (while (isearch-fail-pos) (isearch-pop-state)))
      (isearch-update)))

  (defun contrib/isearch-done-opposite-end (&optional nopush edit)
    "End current search in the opposite side of the match.
Particularly useful when the match does not fall within the
confines of word boundaries (e.g. multiple words)."
    (interactive)
    (funcall #'isearch-done nopush edit)
    (when isearch-other-end (goto-char isearch-other-end)))
  :bind (("M-s M-o" . multi-occur)
         :map isearch-mode-map
         ("C-SPC" . prot/isearch-mark-and-exit)
         ("DEL" . contrib/isearchp-remove-failed-part-or-last-char)
         ("<C-return>" . contrib/isearch-done-opposite-end)))

(use-package anzu
  :ensure t
  :delight
  :custom
  (anzu-search-threshold 100)
  (anzu-replace-threshold nil)
  (anzu-deactivate-region nil)
  (anzu-replace-to-string-separator "")
  :config
  (global-anzu-mode 1)
  :bind (([remap isearch-query-replace] . anzu-isearch-query-replace)
         ([remap isearch-query-replace-regexp] . anzu-isearch-query-replace-regexp))
  ([remap query-replace] . anzu-query-replace)
  ([remap query-replace-regexp] . anzu-query-replace-regexp)
  ("M-s %" . anzu-query-replace-at-cursor))

(use-package swiper
  :after ivy
  :custom
  (swiper-action-recenter t)
  (swiper-goto-start-of-match t)
  (swiper-include-line-number-in-search t)
  :bind (("C-S-s" . swiper)
         ("M-s s" . swiper-multi)
         ("M-s w" . swiper-thing-at-point)
         :map swiper-map
         ("M-y" . yank)
         ("C-." . swiper-avy)))

(use-package wgrep                      ; Editable grep buffer
  :defer 2
  :custom
  (wgrep-auto-save-buffer t)
  (wgrep-change-readonly-file t))

(use-package visual-regexp              ; Regexp replace with in-buffer display
  :bind (("C-c s r" . vr/query-replace)
         ("C-c s R" . vr/replace)))

(provide 'setup-search)
