(use-package aggressive-indent          ; Automatically indent code
  :bind ("C-c t i" . aggressive-indent-mode)
  :hook ((lisp-mode       . aggressive-indent-mode)
         (emacs-lisp-mode . aggressive-indent-mode)
         (clojure-mode    . aggressive-indent-mode))
  :config
  ;; Free C-c C-q, used in Org and in CIDER
  (unbind-key "C-c C-q" aggressive-indent-mode-map)
  ;; (add-to-list 'aggressive-indent-excluded-modes 'cider-repl-mode)
  )

(use-package undo-tree                  ; Show buffer changes as a tree
  :defer 1 
  :init (global-undo-tree-mode)
  :config (setq undo-tree-visualizer-timestamps t))

(use-package smartparens
  :defer 1
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

(use-package super-save                 ; Autosave buffers when they lose focus
  :init (super-save-mode)
  :config (setq super-save-auto-save-when-idle t))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package iedit
  :defines hydra-iedit/body
  :bind* (:map global-map
	       ("C-*" . iedit-mode)
	       :map iedit-mode-keymap
	       ("M-n" . iedit-next-occurence)
	       ("M-p" . iedit-prev-occurence))
  :config
  (defhydra hydra-iedit (:color pink :columns 1)
    "IEDIT"
    ("C-*" iedit-mode "toggle")
    ("C-p" iedit-prev-occurrence "prev")
    ("C-n" iedit-next-occurrence "next")
    ("C-g" iedit-quit "toggle" :color blue)))

(use-package shift-number
  :bind (("C-c +" . shift-number-up)
         ("C-c -" . shift-number-down)))

(defvar indent-sensitive-modes '(coffee-mode slim-mode yaml-mode))
(use-package smart-newline
  :defer 2
  :hook ((prog-mode . maybe-enable-smart-newline-mode))
  :commands smart-newline-mode
  :init
  (defun maybe-enable-smart-newline-mode ()
    (when (not (member major-mode indent-sensitive-modes))
      (smart-newline-mode))))

(use-package string-edit
  :bind ("C-c C-'" . string-edit-at-point))

(use-package string-inflection
  :bind ("C-c `" . string-inflection-all-cycle))

(use-package visual-regexp
  :bind (("C-c r"   . vr/replace)
         ("C-c %"   . vr/query-replace)
         ("C-c m" . vr/mc-mark)))

(provide 'vde-editing)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
