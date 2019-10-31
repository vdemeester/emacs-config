;;; setup-style.el --- setup emacs style 😎
;;; Commentary:
;;; Code:
;;; -*- lexical-binding: t; -*-

;;; ¯\_(ツ)_/¯

;;; Fonts used:
;;; - Iosevka (https://github.com/be5invis/Iosevka)
;;; - Fira Sans (https://github.com/mozilla/Fira/)
(setq font-height 110)
(cond
 ((string= (system-name) "hokkaido")
  (setq font-height 100)))

;; Middle/Near East: שלום, السّلام عليكم
(when (member "Noto Sans Arabic" (font-family-list))
  (set-fontset-font t 'arabic "Noto Sans Arabic"))
(when (member "Noto Sans Hebrew" (font-family-list))
  (set-fontset-font t 'arabic "Noto Sans Hebrew"))

;; Africa: ሠላም
(when (member "Noto Sans Ethiopic" (font-family-list))
  (set-fontset-font t 'ethiopic "Noto Sans Ethiopic"))

(set-face-attribute 'default nil
		    :family "Fira Code" ; "Overpass Mono" to try someday
                    :height font-height)
(set-face-attribute 'variable-pitch nil
                    :family "Fira Sans"
                    :height font-height
                    :weight 'regular)

;;; Utilities and key bindings
(defun mu-reset-fonts ()
  "Reset fonts to my preferences."
  (interactive)
  (set-face-attribute 'default nil
		      :family "Fira Code"
                      :height font-height)
  (set-face-attribute 'variable-pitch nil
                      :family "Fira Sans"
                      :height font-height
                      :weight 'regular))

(bind-key "C-c t f" #'mu-reset-fonts)

;;; Interface
(use-package frame                      ; Frames
  :bind ("C-c w f" . toggle-frame-fullscreen)
  :init
  ;; Kill `suspend-frame'
  (unbind-key "C-x C-z")
  :config (add-to-list 'initial-frame-alist '(fullscreen . maximized)))

(setq echo-keystrokes 0.1)     ; Faster echo keystrokes

;; Avoid showing ?? in the mode line when we have long lines.
(setq line-number-display-limit-width 10000)

;; Turn off mouse interface early in startup to avoid momentary display
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'horizontal-scroll-bar-mode) (horizontal-scroll-bar-mode -1))

;;; Theme
(setq custom-safe-themes t)    ; Treat themes as safe

(use-package doom-themes
  :config
  (load-theme 'doom-one t)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

(use-package solaire-mode
  :config
  (setq solaire-mode-remap-modeline nil)
  (add-hook 'after-change-major-mode-hook #'turn-on-solaire-mode)
  (add-hook 'after-revert-hook #'turn-on-solaire-mode)
  (add-hook 'minibuffer-setup-hook #'solaire-mode-in-minibuffer)
  (add-hook 'ediff-prepare-buffer-hook #'solaire-mode)
  (advice-add #'persp-load-state-from-file :after #'solaire-mode-restore-persp-mode-buffers))

(line-number-mode)
(column-number-mode)

;; Show buffer position percentage starting from top
(setq mode-line-percent-position '(-3 "%o"))
(defvar mu-eyebrowse-mode-line
  '(:propertize
    (:eval
     (when (bound-and-true-p eyebrowse-mode)
       (let* ((num (eyebrowse--get 'current-slot))
              (tag (when num
                     (nth 2 (assoc num (eyebrowse--get 'window-configs)))))
              (str (concat
                    " "
                    (if (and tag (< 0 (length tag)))
                        tag
                      (when num (int-to-string num)))
                    " ")))
         str)))
    face (:background "#81a2be" :foreground "#373b41"))
  "Mode line format for Eyebrowse.")

(put 'mu-eyebrowse-mode-line 'risky-local-variable t)

(setq-default mode-line-format
              '("%e"
                mu-eyebrowse-mode-line
                mode-line-front-space
                mode-line-mule-info
                mode-line-client
                mode-line-modified
                mode-line-remote
                mode-line-frame-identification
                mode-line-buffer-identification " " mode-line-position
                (vc-mode vc-mode)
                (multiple-cursors-mode mc/mode-line)
                " " mode-line-modes
                (org-mode-line-string org-mode-line-string)
                mode-line-end-spaces))

(defmacro rename-modeline (package-name mode new-name)
  "Rename PACKAGE-NAME with MODE into NEW-NAME in the mode line."
  `(eval-after-load ,package-name
     '(defadvice ,mode (after rename-modeline activate)
        (setq mode-name ,new-name))))

(rename-modeline "js2-mode" js2-mode "JS2")

(line-number-mode 1)
(column-number-mode 1)

(global-hl-line-mode 1)

(defun generic-term-init ()
  (visual-line-mode -1)
  (setq-local global-hl-line-mode nil)
  (setq-local scroll-margin 0))

(add-hook 'term-mode-hook #'generic-term-init)
(add-hook 'shell-mode-hook #'generic-term-init)
(add-hook 'eshell-mode-hook #'generic-term-init)

(use-package moody
  :config
  (setq x-underline-at-descent-line t)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode)
  (moody-replace-org-clock-mode))

;;;; org-mode

(defvar moody-org-clock-mode
  ;;'(:eval (moody-ribbon (substring vc-mode 1) nil 'up))
  '(:eval (moody-tab (substring org-mode-line-string 1) nil 'up)))
(put 'moody-org-clock-mode 'risky-local-variable t)
(make-variable-buffer-local 'moody-org-clock-mode)

(defun moody-replace-org-clock-mode (&optional reverse)
  (interactive "P")
  (moody-replace-element '(org-mode-line-string org-mode-line-string)
                         '(org-mode-line-string moody-org-clock-mode)
                         reverse))

(use-package minions                    ; A minor-mode menu for the mode line
  :init (minions-mode)
  :config
  (setq
   minions-mode-line-lighter "λ="
   minions-direct '(flycheck-mode
                    cider-mode)))

(setq-default indicate-buffer-boundaries 'left)
(setq-default indicate-empty-lines +1)

(use-package highlight
  :ensure t
  :pin melpa)

(use-package highlight-numbers
  :hook (prog-mode . highlight-numbers-mode))

(use-package symbol-overlay
  :defer 4
  :bind
  ("M-s h ." . symbol-overlay-put)
  ("M-s h n" . symbol-overlay-jump-next)
  ("M-s h p" . symbol-overlay-jump-prev)
  :hook (prog-mode . symbol-overlay-mode)
  :config
  (setq symbol-overlay-idle-time 0.2))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package rainbow-mode
  :commands rainbow-mode
  :hook (prog-mode . rainbow-mode))

(use-package visual-fill-column
  :commands visual-fill-column-mode)

(use-package hide-mode-line-mode
  :commands hide-mode-line-mode)

(provide 'setup-style)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
