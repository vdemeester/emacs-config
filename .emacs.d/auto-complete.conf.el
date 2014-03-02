(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(setq ac-comphist-file (expand-file-name "run/ac-comphist.dat"
                                         user-emacs-directory))

(require 'auto-complete-config)
(ac-config-default)

(setq ac-use-quick-help nil)            ;; It's slow

;; Don't use up/down arrow (use M-n, M-p only) to browse list
(define-key ac-completing-map [down] nil)
(define-key ac-completing-map [up] nil)
