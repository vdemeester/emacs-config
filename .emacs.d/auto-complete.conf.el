(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(setq ac-comphist-file (expand-file-name "run/ac-comphist.dat"
                                         user-emacs-directory))

(require 'auto-complete-config)
(ac-config-default)

(setq ac-use-quick-help nil)            ;; It's slow

(defun ielm-auto-complete ()
  "Enables `auto-complete' support in \\[ielm]."
  (setq ac-sources '(ac-source-functions
                     ac-source-variables
                     ac-source-features
                     ac-source-symbols
                     ac-source-words-in-same-mode-buffers))
  (add-to-list 'ac-modes 'inferior-emacs-lisp-mode)
  (auto-complete-mode 1))
(add-hook 'ielm-mode-hook 'ielm-auto-complete)

;; Don't use up/down arrow (use M-n, M-p only) to browse list
(define-key ac-completing-map [down] nil)
(define-key ac-completing-map [up] nil)
