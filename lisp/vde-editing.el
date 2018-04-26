(use-package undo-tree                  ; Show buffer changes as a tree
  :ensure t
  :pin melpa
  :init (global-undo-tree-mode)
  :config (setq undo-tree-visualizer-timestamps t))

(use-package smartparens
  :ensure t
  :pin melpa
  :init
  (progn
    (use-package smartparens-config)
    (show-smartparens-global-mode 1))
  :config
  (progn
    (require 'smartparens-config)
    (add-hook 'prog-mode-hook 'turn-on-smartparens-strict-mode)
    (add-hook 'markdown-mode-hook 'turn-on-smartparens-strict-mode)
    
    (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)
    (sp-local-pair 'web-mode "{%" "%}")
    (sp-with-modes 'emacs-lisp-mode
      ;; disable ', it's the quote character!
      (sp-local-pair "'" nil :actions nil)
      ;; also only use the pseudo-quote inside strings where it
      ;; serves as hyperlink.
      (sp-local-pair "`" "'" :when '(sp-in-string-p sp-in-comment-p)))))

(provide 'vde-editing)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
