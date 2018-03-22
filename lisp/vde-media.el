(use-package mpdel
  :ensure t
  :pin melpa
  :config (mpdel-mode))

(use-package ivy-mpdel
  :ensure t
  :pin melpa
  :after mpdel)

(provide 'vde-media)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
