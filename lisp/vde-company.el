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
   company-show-numbers t)
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
        '((company-css
           company-clang
           company-capf
           company-semantic
           company-xcode
           company-cmake
           company-files
           company-gtags
           company-etags
           company-keywords))))

(use-package company-emoji
  :ensure company
  :config
  (add-to-list 'company-backends 'company-emoji))

(use-package lsp-mode
    :init
    (add-hook 'prog-mode-hook 'lsp-mode)
    :config
    (use-package lsp-flycheck
        :ensure f ; comes with lsp-mode
        :after flycheck))

;; `company' backend for `lsp-mode'
(use-package company-lsp
  :after company lsp-mode
  :init
  (push 'company-lsp company-backends))

(provide 'vde-company)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
