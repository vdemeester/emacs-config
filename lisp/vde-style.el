
(setq echo-keystrokes 0.1)     ; Faster echo keystrokes

;; Avoid showing ?? in the mode line when we have long lines.
(setq line-number-display-limit-width 10000)

;; Turn off mouse interface early in startup to avoid momentary display
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'horizontal-scroll-bar-mode) (horizontal-scroll-bar-mode -1))

(provide 'vde-style)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
