(use-package web-mode
  :defer t
  :mode
  ("\\.html\\'" . web-mode)
  ("\\.phtml\\'" . web-mode)
  ("\\.[agj]sp\\'" . web-mode)
  ("\\.as[cp]x\\'" . web-mode)
  ("\\.erb\\'" . web-mode)
  ("\\.mustache\\'" . web-mode)
  ("\\.djhtml\\'" . web-mode)
  ("\\.jsp\\'" . web-mode)
  ("\\.eex\\'" . web-mode)
  ("\\.tsx\\'" . web-mode)
  :config
  (setq web-mode-attr-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-indent-style 2)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-sql-indent-offset 2)
  (eval-after-load 'smartparens
    (lambda ()
      (setq web-mode-enable-auto-pairing nil)
      (sp-with-modes '(web-mode)
        (sp-local-pair "%" "%"
                       :unless '(sp-in-string-p)
                       :post-handlers '(((lambda (&rest _ignored)
                                           (just-one-space)
                                           (save-excursion (insert " ")))
                                         "SPC" "=" "#")))
        (sp-local-tag "%" "<% "  " %>")
        (sp-local-tag "=" "<%= " " %>")
        (sp-local-tag "#" "<%# " " %>")))))

(use-package js2-mode
  :defer t
  :mode
  ("\\.js\\'" . js2-mode)
  :interpreter
  ("node" . js2-mode))

(use-package js2-refactor
  :defer t
  :diminish js2-refactor-mode
  :commands
  (js2-refactor-mode)
  :init
  (add-hook 'js2-mode-hook #'js2-refactor-mode))

(use-package typescript-mode
  ;; npm install -g typescript
  :defer t
  :mode
  ("\\.ts\\'" . typescript-mode)
  ("\\.ts$\\'" . typescript-mode)
  :config
  (setq typescript-enabled-frameworks '(typescript)))

(use-package js-import
  :defer t
  :commands (js-import js-import-dev))

(use-package rjsx-mode
  :defer t
  :mode
  ("\\.jsx?\\'" . rjsx-mode)
  :interpreter
  ("node" . rjsx-mode)
  :config
  ;; Inspired by http://blog.binchen.org/posts/indent-jsx-in-emacs.html
  (defun +js-jsx-indent-line-align-closing-bracket ()
    "Workaround sgml-mode and align closing bracket with opening bracket"
    (save-excursion
      (beginning-of-line)
      (when (looking-at-p "^ +\/?> *$")
        (delete-char sgml-basic-offset))))

  (advice-add #'js-jsx-indent-line
              :after
              #'+js-jsx-indent-line-align-closing-bracket)

  (with-eval-after-load 'rjsx
    (define-key rjsx-mode-map "<" nil)))

(provide 'vde-web)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
