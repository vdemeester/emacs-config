;;; setup-editing.el --- setup editing related mode(s)
;;; Commentary:
;;; Code:
;;; -*- lexical-binding: t; -*-

(use-package aggressive-indent          ; Automatically indent code
  :bind ("C-c e i" . aggressive-indent-mode)
  :hook ((lisp-mode       . aggressive-indent-mode)
         (emacs-lisp-mode . aggressive-indent-mode)
         (clojure-mode    . aggressive-indent-mode))
  :config
  ;; Free C-c C-q, used in Org and in CIDER
  (unbind-key "C-c C-q" aggressive-indent-mode-map))

(use-package undo-tree                  ; Show buffer changes as a tree
  :defer 1
  :init (global-undo-tree-mode)
  :config (setq undo-tree-visualizer-timestamps t))

(use-package whitespace
  :defer 1
  :config
  (setq whitespace-style '(face tabs spaces trailing space-before-tab newline indentation empty space-after-tab space-mark tab-mark newline-mark)))

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

(use-package string-edit
  :bind ("C-c C-'" . string-edit-at-point))

(use-package visual-regexp
  :bind (("C-c r"   . vr/replace)
         ("C-c %"   . vr/query-replace)
         ("C-c m" . vr/mc-mark)))

(use-package yasnippet
  :after (company prog-mode)
  :defer 5
  :bind (("C-c y d" . yas-load-directory)
         ("C-c y i" . yas-insert-snippet)
         ("C-c y f" . yas-visit-snippet-file)
         ("C-c y n" . yas-new-snippet)
         ("C-c y t" . yas-tryout-snippet)
         ("C-c y l" . yas-describe-tables)
         ("C-c y g" . yas-global-mode)
         ("C-c y m" . yas-minor-mode)
         ("C-c y a" . yas-reload-all)
         ("C-c y x" . yas-expand)) 
  :bind (:map yas-keymap
              ("C-i" . yas-next-field-or-maybe-expand))
  :mode ("/\\.emacs\\.d/etc/yasnippet/snippets/" . snippet-mode)
  :config
  (yas-load-directory (emacs-path "etc/yasnippet/snippets"))
  (yas-global-mode 1)
  :init
  (add-hook 'term-mode-hook (lambda ()
                              (yas-minor-mode -1))))

(use-package hs-minor-mode
  :hook ((prog-mode . hs-minor-mode)))

(use-package easy-kill
  :config
  (global-set-key [remap kill-ring-save] 'easy-kill)
  (global-set-key [remap mark-sexp] 'easy-mark))

(use-package scratch
  :commands (scratch)
  :bind (("C-c t s" . scratch)))

(use-package define-word)

(setq display-line-numbers-type 'relative)
(add-hook 'prog-mode-hook
          'display-line-numbers-mode)
(add-hook 'prog-mode-hook 'toggle-truncate-lines)

(provide 'setup-editing)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
