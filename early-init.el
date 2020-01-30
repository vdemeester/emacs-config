;;; -*- lexical-binding: t; -*-
;; Do not initialise the package manager.  This is done in `init.el'.
(setq package-enable-at-startup nil)

;; Do not resize the frame at this early stage.
(setq frame-inhibit-implied-resize)

(provide 'early-init)
