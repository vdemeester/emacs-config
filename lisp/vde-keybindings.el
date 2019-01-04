;;; -*- lexical-binding: t; -*-
(use-package key-chord
  :ensure t
  :chords
  (("''" . "’")
   (",w" . whitespace-mode)
   (",l" . linum-mode))
  :custom
  (key-chord-two-keys-delay 0.05)
  :config
  (key-chord-mode 1))

(use-package which-key
  :ensure t
  :init (which-key-mode)
  :config
  (setq
   which-key-idle-delay 0.4
   which-key-sort-order 'which-key-prefix-then-key-order)
  (add-to-list 'which-key-replacement-alist '(("TAB" . nil) . ("↹" . nil)))
  (add-to-list 'which-key-replacement-alist '(("RET" . nil) . ("⏎" . nil)))
  (add-to-list 'which-key-replacement-alist '(("DEL" . nil) . ("⇤" . nil)))
  (add-to-list 'which-key-replacement-alist '(("SPC" . nil) . ("␣" . nil))))

;; Disable C-x C-n to avoid the disabled command buffer
(unbind-key "C-x C-n" global-map)

(provide 'vde-keybindings)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
