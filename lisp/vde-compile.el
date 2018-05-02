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
    (add-hook 'compilation-filter-hook #'vde/colorize-compilation-buffer)))

(provide 'vde-compile)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
