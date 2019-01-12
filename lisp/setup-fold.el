;;; vde-fold.el --- setup folding 👼 -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package fold-this
  :config
  (bind-keys
   :map fold-this-keymap
   ("<mouse-1>" . fold-this-unfold-at-point))
  (bind-keys
   :map region-bindings-mode-map
   ("&" . fold-this)))

(provide 'setup-fold)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
