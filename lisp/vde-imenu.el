(use-package imenu-list
  :commands (vde/imenu-list-display-toggle)
  :bind (("C-'" . vde/imenu-list-display-toggle))
  :config
    (progn
    (setq imenu-list-size     0.2)
    (setq imenu-list-position 'right)

    (defun vde/imenu-list-hide ()
      (interactive)
      (switch-to-buffer-other-window imenu-list-buffer-name)
      (quit-window))

    (defun vde/imenu-list-visible-p ()
      "Returns `t' if the `imenu-list' buffer is visible."
      (catch 'break
        (dolist (win (window-list))
          (when (string= imenu-list-buffer-name (buffer-name (window-buffer win)))
            (throw 'break t)))))

    (defun vde/imenu-list-display-toggle (noselect)
      "Toggle the display of Imenu-list buffer.

If NOSELECT is non-nil, do not select the imenu-list buffer."
      (interactive "P")
      (if (vde/imenu-list-visible-p)
          (vde/imenu-list-hide)
        (if noselect
            (imenu-list-noselect)
          (imenu-list))))

    (defun vde/imenu-list-goto-entry-and-hide ()
      "Execute `imenu-list-goto-entry' and hide the imenu-list buffer."
      (interactive)
      (imenu-list-goto-entry)
      (vde/imenu-list-hide))
    (bind-key "C-<return>"
              #'vde/imenu-list-goto-entry-and-hide
              imenu-list-major-mode-map)

    (defun vde/imenu-auto-update (orig-fun &rest args)
      "Auto update the *Ilist* buffer if visible."
      (prog1 ; Return value of the advising fn needs to be the same as ORIG-FUN
          (apply orig-fun args)
        (when (vde/imenu-list-visible-p)
          (imenu-list-update-safe)))) ; update `imenu-list' buffer
    (advice-add 'switch-to-buffer :around #'vde/imenu-auto-update)
    (advice-add 'revert-buffer    :around #'vde/imenu-auto-update)))

(provide 'vde-imenu)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
