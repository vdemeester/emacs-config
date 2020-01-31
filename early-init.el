(setq package-enable-at-startup nil)

(setq frame-inhibit-implied-resize)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)

(add-hook 'after-init-hook
          `(lambda ()
             (setq gc-cons-threshold 16777216 ; 16mb
                   gc-cons-percentage 0.1)
             (garbage-collect)) t)

(provide 'early-init)
