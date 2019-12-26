;;; setup-editing.el --- setup editing related mode(s)
;;; Commentary:
;;; Code:
;;; -*- lexical-binding: t; -*-

(setq enable-remote-dir-locals t)
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
  :hook ((prog-mode . whitespace-mode))
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
  :bind (("C-=" . er/expand-region)
         ("C--". er/contract-region)))

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
  :hook (go-mode . yas-minor-mode)
  :config
  (yas-load-directory (concat user-emacs-directory "etc/yasnippet/snippets"))
  (yas-global-mode 1)
  :init
  (add-hook 'term-mode-hook (lambda () (yas-minor-mode -1))))

(use-package hs-minor-mode
  :hook ((prog-mode . hs-minor-mode)))

(use-package easy-kill
  :config
  (global-set-key [remap kill-ring-save] 'easy-kill)
  (global-set-key [remap mark-sexp] 'easy-mark))

(use-package define-word)

(setq display-line-numbers-type 'relative)
(add-hook 'prog-mode-hook
          'display-line-numbers-mode)
(add-hook 'prog-mode-hook 'toggle-truncate-lines)

(use-package newcomment
  :custom
  (comment-empty-lines t)
  (comment-fill-column nil)
  (comment-multi-line t)
  (comment-style 'multi-line)
  :config
  (defun prot/comment-dwim (&optional arg)
    "Alternative to `comment-dwim': offers a simple wrapper
around `comment-line' and `comment-dwim'.

If the region is active, then toggle the comment status of the
region or, if the major mode defines as much, of all the lines
implied by the region boundaries.

Else toggle the comment status of the line at point."
    (interactive "*P")
    (if (use-region-p)
        (comment-dwim arg)
      (save-excursion
        (comment-line arg))))

  :bind (("C-;" . prot/comment-dwim)
         ("C-:" . comment-kill)
         ("M-;" . comment-indent)
         ("C-x C-;" . comment-box)))

(use-package flyspell
  :init
  (setq flyspell-issue-message-flag nil)
  (setq flyspell-issue-welcome-flag nil)
  (setq ispell-program-name "hunspell")
  (setq ispell-local-dictionary "en_GB")
  (setq ispell-local-dictionary-alist
        '(("en_GB"
           "[[:alpha:]]"
           "[^[:alpha:]]"
           "[']"
           nil
           ("-d" "en_GB,fr_FR")
           nil
           utf-8)))
  :config
  (define-key flyspell-mode-map (kbd "C-;") nil)
  :hook
  (text-mode . turn-on-flyspell)
  (prog-mode . turn-off-flyspell))

(use-package flyspell-correct-ivy
  :after flyspell
  :bind (:map flyspell-mode-map
              ([remap flyspell-correct-word-before-point] . flyspell-correct-previous-word-generic)))

(use-package electric
  :custom
  (electric-pair-inhibit-predicate 'electric-pair-default-inhibit)
  (electric-pair-pairs '((8216 . 8217)
                         (8220 . 8221)
                         (171 . 187)))
  (electric-pair-skip-self 'electric-pair-default-skip-self)
  (electric-quote-context-sensitive t)
  (electric-quote-paragraph t)
  (electric-quote-string nil)
  :config
  (electric-indent-mode 1)
  (electric-pair-mode 1)
  (electric-quote-mode -1))

(use-package emacs
  :init
  (setq-default tab-always-indent 'complete)
  (setq-default tab-width 4)
  (setq-default indent-tabs-mode nil))

(use-package emacs
  :hook (before-save . delete-trailing-whitespace))

(use-package delsel
  :config
  (delete-selection-mode 1))

(use-package emacs
  :custom
  (repeat-on-final-keystroke t)
  (set-mark-command-repeat-pop t)
  :bind ("M-z" . zap-up-to-char))

(provide 'setup-editing)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
