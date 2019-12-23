;;; setup-go.el --- setup go mode(s)
;;; Commentary:
;;; Code:
;;; -*- lexical-binding: t; -*-

(use-package go-mode
  :mode "\\.go$"
  :interpreter "go"
  :config
  (use-package company-go
    :config
    (setq company-go-show-annotation t)
    (push 'company-go company-backends))
  (setq gofmt-command "goimports")
  (if (not (executable-find "goimports"))
      (warn "go-mode: couldn't find goimports; no code formatting/fixed imports on save")
    (add-hook 'before-save-hook 'gofmt-before-save))
  (if (not (string-match "go" compile-command))   ; set compile command default
      (set (make-local-variable 'compile-command)
           "go build -v && go test -v && go vet")))

(use-package flycheck-golangci-lint
  :hook (go-mode . flycheck-golangci-lint-setup)
  :config (setq flycheck-golangci-lint-tests t))

;; (use-package lsp-go
;;   :defer 2
;;   :after lsp-mode
;;   :hook ((go-mode . lsp-go-enable)))

(provide 'setup-go)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
