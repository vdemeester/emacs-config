(use-package avy                   ; Jump to characters in buffers
  :bind (("C-c j"   . avy-goto-word-1)
         ("C-c n b" . avy-pop-mark)
         ("C-c n j" . avy-goto-char-2)
         ("C-c n t" . avy-goto-char-timer)
         ("C-c n w" . avy-goto-word-1)))

(use-package helpful
  :unless noninteractive
  :bind (("C-c h F" . helpful-function)
         ("C-c h C" . helpful-command)
         ("C-c h M" . helpful-macro)
         ("C-c h L" . helpful-callable)
         ("C-c h S" . helpful-at-point)
         ("C-c h V" . helpful-variable)))
(use-package winner
  :unless noninteractive
  :defer 5
  :config
  (winner-mode 1))

(use-package hideshow
  :defer 5
  :bind (("C-c @ a" . hs-show-all)
         ("C-c @ c" . hs-toggle-hiding)
         ("C-c @ t" . hs-hide-all)
         ("C-c @ d" . hs-hide-block)
         ("C-c @ l" . hs-hide-level)))

(use-package mwim
  :bind (:map prog-mode-map
              ("C-a" . mwim-beginning-of-code-or-line)
              ("C-e" . mwim-end-of-code-or-line)))

(provide 'setup-navigating)
