;;(exec-path-from-shell-copy-env "GOPATH")
;;(exec-path-from-shell-copy-env "GO15VENDOREXPERIMENT")
(setenv "GOPATH" "/home/vincent/go")
(use-package go-mode
  :ensure t
  :config
  (bind-key "C-h f" 'godoc-at-point go-mode-map)
  (bind-key "C-c C-u" 'go-remove-unused-imports go-mode-map)
  (bind-key "C-c C-i" 'go-godoc-history go-mode-map))

(use-package company-go
  :ensure t
  :config (add-to-list 'company-backends 'company-go))
(use-package go-eldoc
  :ensure t
  :config (add-hook 'go-mode-hook 'go-eldoc-setup))
(use-package gotest
  :ensure t
  :init
  (bind-key "C-c r" 'go-run go-mode-map)
  (bind-key "C-c t C-g a" 'go-test-current-project go-mode-map)
  (bind-key "C-c t m" 'go-test-current-file go-mode-map)
  (bind-key "C-c t ." 'go-test-current-test go-mode-map)
  (bind-key "C-c t c" 'go-test-current-coverage go-mode-map)
  (bind-key "C-c t b" 'go-test-current-benchmark go-mode-map)
  (bind-key "C-c t C-g b" 'go-test-current-project-benchmarks go-mode-map))
(use-package golint
  :ensure t)
(use-package go-guru
  :load-path "~/lib/go/src/golang.org/x/tools/cmd/guru"
  :config
  (add-hook 'go-mode-hook #'go-guru-hl-identifier-mode))
(use-package go-rename
  :bind ("C-c r" . go-rename)
  :load-path "~/lib/go/src/golang.org/x/tools/refactor/rename")

(defun my-go-mode-hook ()
  (setq gofmt-command "gofmts")
  (add-hook 'before-save-hook 'gofmt-before-save)
  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
           "go build -v && go test -v && go vet")))
(add-hook 'go-mode-hook 'my-go-mode-hook)

(defun vde/go-test-after-save-hook ()
  "A file save hook that will run go test on the current package
whenever a file is saved. Use it with local variables and go-mode
for example."
  (unless (ignore-errors (go-test-current-test))
    (unless (ignore-errors (go-test-current-file))
      (go-test-current-project))))
