;;; setup-company.el -- setup company (completions)
;;; Commentary:
;;; Code:
;;; -*- lexical-binding: t; -*-
(use-package company
  :commands global-company-mode
  :init
  (add-hook 'after-init-hook #'global-company-mode)
  (setq
   company-idle-delay 0.2
   company-selection-wrap-around t
   company-minimum-prefix-length 2
   company-require-match nil
   company-dabbrev-ignore-case nil
   company-dabbrev-downcase nil
   company-show-numbers t
   company-tooltip-align-annotations t)
  :config
  (bind-keys :map company-active-map
             ("C-d" . company-show-doc-buffer)
             ("C-l" . company-show-location)
             ("C-n" . company-select-next)
             ("C-p" . company-select-previous)
             ("C-t" . company-select-next)
             ("C-s" . company-select-previous)
             ("TAB" . company-complete))
  (setq company-backends
        '(company-css
          company-clang
          company-capf
          company-semantic
          company-xcode
          company-cmake
          company-files
          company-gtags
          company-etags
          company-keywords)))

(use-package company-emoji
  :ensure company
  :config
  (add-to-list 'company-backends 'company-emoji))

(use-package lsp-mode
  :config (require 'lsp-clients)
  ;; (with-eval-after-load "flycheck"
  ;;   (require 'lsp-flycheck)
  ;;   (add-to-list 'flycheck-checkers 'lsp))
  )

(with-eval-after-load "company"
  (use-package company-lsp
    :after lsp-mode
    :config
    (push 'company-lsp company-backends)))

(with-eval-after-load "projectile"
  (defun my-set-projectile-root ()
    (when lsp--cur-workspace
      (setq projectile-project-root (lsp--workspace-root lsp--cur-workspace))))
  (add-hook 'lsp-before-open-hook #'my-set-projectile-root))

;; lsp-ui: This contains all the higher level UI modules of lsp-mode, like flycheck support and code lenses.
;; https://github.com/emacs-lsp/lsp-ui
(use-package lsp-ui
  :after lsp-mode
  :hook ((lsp-mode . lsp-ui-mode) 
         (lsp-ui-mode . lsp-ui-peek-mode))
  :config
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
  (setq lsp-ui-sideline-enable nil
        lsp-ui-doc-enable nil
        lsp-ui-flycheck-enable t
        lsp-ui-imenu-enable t
        lsp-ui-sideline-ignore-duplicate t))

(provide 'setup-company)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
