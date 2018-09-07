;;; -*- lexical-binding: t; -*-
(use-package pulseaudio-control
  :defer 5
  :init
  (pulseaudio-control-default-keybindings)
  :config
  (setq pulseaudio-control-pactl-path (executable-find "pactl")))

(provide 'vde-media)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
