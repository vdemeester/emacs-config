(setq nxml-sexp-element-flag t           ; treat <e><p>...</p></e> like (e (p ...)) for C-M-f/b/k/d/u
      nxml-slash-auto-complete-flag t)   ; complete the element on </
(add-hook 'nxml-mode-hook (lambda () (define-key nxml-mode-map (kbd "M-D") 'nxml-backward-down-element)))
