;;; -*- lexical-binding: t; -*-
(use-package compile
  :defer 2
  :config
  (progn
    ;; http://stackoverflow.com/a/13408008/1219634
    (setq
     compilation-scroll-output t
     ;; I'm not scared of saving everything.
     compilation-ask-about-save nil
     ;; Automatically scroll and jump to the first error
     compilation-scroll-output 'next-error
     ;; compilation-scroll-output 'first-error
     ;; compilation-auto-jump-to-first-error t
     ;; Skip over warnings and info messages in compilation
     compilation-skip-threshold 2
     ;; Don't freeze when process reads from stdin
     compilation-disable-input t
     ;; Show three lines of context around the current message
     compilation-context-lines 3)
    (require 'ansi-color)
    (defun vde/colorize-compilation-buffer ()
      (unless (or (derived-mode-p 'grep-mode) ;Don't mess up colors in Grep/Ag results buffers
                  (derived-mode-p 'ag-mode))
        (ansi-color-apply-on-region compilation-filter-start (point))))
    (add-hook 'compilation-filter-hook #'vde/colorize-compilation-buffer)

    (defun vde/mark-compilation-window-as-dedicated ()
      "Setup the *compilation* window with custom settings."
      (when (string-prefix-p "*compilation: " (buffer-name))
        (save-selected-window
          (save-excursion
            (let* ((w (get-buffer-window (buffer-name))))
              (when w
                (select-window w)
                (switch-to-buffer (buffer-name))
                (set-window-dedicated-p w t)))))))
    (add-hook 'compilation-mode-hook 'vde/mark-compilation-window-as-dedicated)))

(use-package flycheck
  :if (not (eq system-type 'windows-nt))
  :defer 4
  :commands (flycheck-mode
             flycheck-next-error
             flycheck-previous-error)
  :init
  (dolist (where '((emacs-lisp-mode-hook . emacs-lisp-mode-map)
                   (haskell-mode-hook    . haskell-mode-map)
                   (js2-mode-hook        . js2-mode-map)
                   (go-mode-hook         . go-mode-map)
                   (c-mode-common-hook   . c-mode-base-map)))
    (add-hook (car where)
              `(lambda ()
                 (bind-key "M-n" #'flycheck-next-error ,(cdr where))
                 (bind-key "M-p" #'flycheck-previous-error ,(cdr where)))
              t))
  :config
  (add-hook 'prog-mode-hook 'flycheck-mode)
  (defalias 'show-error-at-point-soon
    'flycheck-show-error-at-point)
  (setq flycheck-idle-change-delay 1.2))

(provide 'setup-compile)
