;;; early-init.el --- -*- lexical-binding: t -*-
;; PkgStartup
(setq package-enable-at-startup nil)
;; -PkgStartup

;; FrameResize
(setq frame-inhibit-implied-resize t)
;; -FrameResize

;; DisableUI
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(horizontal-scroll-bar-mode -1)
;; -DisableUI

;; GarbageCollection
(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)
;; -GarbageCollection

;; FileNameHandler
(defvar file-name-handler-alist-original file-name-handler-alist)
(setq file-name-handler-alist nil)
;; -FileNameHandler

;; AfterInitHook
(add-hook 'after-init-hook
          `(lambda ()
             (setq gc-cons-threshold 67108864 ; 64mb
                   gc-cons-percentage 0.1
                   file-name-handler-alist file-name-handler-alist-original)
             (garbage-collect)) t)
;; -AfterInitHook
