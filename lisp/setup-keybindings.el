;;; setup-keybindings.el --- setup keybindings 👼 -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package key-chord
  :chords
  (("''" . "’")
   (",w" . whitespace-mode)
   (",l" . display-line-numbers-mode))
  :custom
  (key-chord-two-keys-delay 0.05)
  :config
  (key-chord-mode 1))

(use-package which-key
  :init (which-key-mode)
  :config
  (setq
   which-key-idle-delay 0.4
   which-key-sort-order 'which-key-prefix-then-key-order)
  (add-to-list 'which-key-replacement-alist '(("TAB" . nil) . ("↹" . nil)))
  (add-to-list 'which-key-replacement-alist '(("RET" . nil) . ("⏎" . nil)))
  (add-to-list 'which-key-replacement-alist '(("DEL" . nil) . ("⇤" . nil)))
  (add-to-list 'which-key-replacement-alist '(("SPC" . nil) . ("␣" . nil))))

(use-package region-bindings-mode
  :config
  ;; Do not activate `region-bindings-mode' in Special modes like `dired' and
  ;; `ibuffer'. Single-key bindings like 'm' are useful in those modes even
  ;; when a region is selected.
  (setq region-bindings-mode-disabled-modes '(dired-mode ibuffer-mode))

  (region-bindings-mode-enable)

  (defun vde/disable-rbm-deactivate-mark ()
    "Disable `region-bindings-mode' and deactivate mark."
    (interactive)
    (region-bindings-mode -1)
    (deactivate-mark)
    (message "Mark deactivated"))

  (bind-keys
   :map region-bindings-mode-map
   ("<C-SPC>" . vde/disable-rbm-deactivate-mark)))

;; Disable C-x C-n to avoid the disabled command buffer
(unbind-key "C-x C-n" global-map)

(provide 'setup-keybindings)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
