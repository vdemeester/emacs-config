(use-package which-key
  :init (which-key-mode)
  :custom
  (which-key-idle-delay 2)
  (which-key-idle-secondary-delay 0.05)
  (which-key-show-early-on-C-h t)
  (which-key-sort-order 'which-key-prefix-then-key-order)
  (which-key-popup-type 'side-window)
  (which-key-show-prefix 'echo)
  (which-key-max-display-columns 6)
  (which-key-separator " → ")
  :config
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
