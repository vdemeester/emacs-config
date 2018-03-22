(use-package which-key
  :ensure t
  :init (which-key-mode)
  :config
  (setq
   which-key-idle-delay 0.4
   which-key-sort-order 'which-key-prefix-then-key-order))

;; Disable C-x C-n to avoid the disabled command buffer
(unbind-key "C-x C-n" global-map)

(provide 'vde-keybindings)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End: