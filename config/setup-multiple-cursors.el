(use-package multiple-cursor
  :bind (:map region-bindings-mode-map
              ("a" . mc/mark-all-like-this)
              ("p" . mc/mark-previous-like-this)
              ("n" . mc/mark-next-like-this)
              ("P" . mc/unmark-previous-like-this)
              ("N" . mc/unmark-next-like-this)
              ("[" . mc/cycle-backward)
              ("]" . mc/cycle-forward)
              ("m" . mc/mark-more-like-this-extended)
              ("h" . mc-hide-unmatched-lines-mode)
              ("\\" . mc/vertical-align-with-space)
              ("#" . mc/insert-numbers) ; use num prefix to set the starting number
              ("^" . mc/edit-beginnings-of-lines)
              ("$" . mc/edit-ends-of-lines)))

(provide 'setup-multiple-cursors)
