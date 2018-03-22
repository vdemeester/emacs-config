;; Ignore directories during grep
(with-eval-after-load 'grep
  '(progn
     (add-to-list 'grep-find-ignored-directories "auto")
     (add-to-list 'grep-find-ignored-directories "elpa")))

;; Truncate lines during grep
(add-hook 'grep-mode-hook #'toggle-truncate-lines)

(use-package wgrep                      ; Editable grep buffer
  :ensure t
  :defer t
  :config)

(use-package visual-regexp              ; Regexp replace with in-buffer display
  :ensure t
  :bind (("C-c s r" . vr/query-replace)
         ("C-c s R" . vr/replace)))

(use-package ez-query-replace           ; Better query replace
  :ensure t
  :pin melpa
  :bind (([remap query-replace] . ez-query-replace)
         ("C-c M-%" . ez-query-replace-repeat)))

(provide 'vde-search)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End: