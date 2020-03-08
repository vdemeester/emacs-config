;;; -*- lexical-binding: t; -*-
;;; ¯\_(ツ)_/¯
;;; - Iosevka (https://github.com/be5invis/Iosevka)
;;; - Fira Sans (https://github.com/mozilla/Fira/)
(defconst font-height 130)
;; Middle/Near East: שלום, السّلام عليكم
(when (member "Noto Sans Arabic" (font-family-list))
  (set-fontset-font t 'arabic "Noto Sans Arabic"))
(when (member "Noto Sans Hebrew" (font-family-list))
  (set-fontset-font t 'arabic "Noto Sans Hebrew"))

;; Africa: ሠላም
(when (member "Noto Sans Ethiopic" (font-family-list))
  (set-fontset-font t 'ethiopic "Noto Sans Ethiopic"))

(set-face-attribute 'default nil
                    :family "Ubuntu Mono"
                    :height font-height)
(set-face-attribute 'variable-pitch nil
                    :family "Ubuntu Sans"
                    :height font-height
                    :weight 'regular)

;;; Utilities and key bindings
(defun mu-reset-fonts ()
  "Reset fonts to my preferences."
  (interactive)
  (set-face-attribute 'default nil
                      :family "Ubuntu Mono"
                      :height font-height)
  (set-face-attribute 'variable-pitch nil
                      :family "Ubuntu Sans"
                      :height font-height
                      :weight 'regular))

(bind-key "C-c f r" #'mu-reset-fonts)

;;; Interface
(use-package frame                      ; Frames
  :bind ("C-c w f" . toggle-frame-fullscreen)
  :init
  ;; Kill `suspend-frame'
  (unbind-key "C-x C-z")
  :config (add-to-list 'initial-frame-alist '(fullscreen . maximized)))

(use-package emacs
  :custom
  (use-file-dialog nil)
  (use-dialog-box nil)
  (echo-keystrokes 0.1) ; Faster echo keystrokes
  (line-number-display-limit-width 10000) ;; Avoid showing ?? in the mode line when we have long lines.
  (display-time-world-list '(("Europe/London" "London")
                             ("Europe/Paris" "Paris")
                             ("America/New_York" "Boston")
                             ("America/Los_Angeles" "San-Francisco")
                             ("Asia/Calcutta" "Bangalore")
                             ("Australia/Brisbane" "Brisbane")))
  :config
  (line-number-mode 1)
  (column-number-mode 1)
  (global-hl-line-mode 1)
  (global-unset-key (kbd "C-z"))
  (global-unset-key (kbd "C-x C-z"))
  (global-unset-key (kbd "C-h h")))

;;; Theme
(setq custom-safe-themes t)    ; Treat themes as safe

(use-package shortbrain-light-theme
  :config
  (load-theme 'shortbrain-light))

(use-package solaire-mode
  :config
  (setq solaire-mode-remap-modeline nil)
  (add-hook 'after-change-major-mode-hook #'turn-on-solaire-mode)
  (add-hook 'after-revert-hook #'turn-on-solaire-mode)
  (add-hook 'minibuffer-setup-hook #'solaire-mode-in-minibuffer)
  (add-hook 'ediff-prepare-buffer-hook #'solaire-mode)
  (advice-add #'persp-load-state-from-file :after #'solaire-mode-restore-persp-mode-buffers))

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
                mode-line-end-spaces))

(defmacro rename-modeline (package-name mode new-name)
  "Rename PACKAGE-NAME with MODE into NEW-NAME in the mode line."
  `(eval-after-load ,package-name
     '(defadvice ,mode (after rename-modeline activate)
        (setq mode-name ,new-name))))

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
  (moody-replace-vc-mode))

(use-package minions                    ; A minor-mode menu for the mode line
  :init (minions-mode)
  :config
  (setq
   minions-mode-line-lighter "λ="
   minions-direct '(flycheck-mode)))

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

(defun set-light-theme ()
  "Set the light theme with some customization if needed."
  (interactive)
  (use-package shortbrain-light-theme
    :config
    (load-theme 'shortbrain-light t)))

(defun set-dark-theme ()
  "Set the dark theme with some customization if needed."
  (interactive)
  (use-package shortbrain-theme
    :config
    (load-theme 'shortbrain t)))

(defun theme-switcher ()
  (interactive)
  (let ((current-hour (string-to-number (format-time-string "%H"))))
    (if (and (> current-hour 6) (< current-hour 20))
        (set-light-theme)
      (set-dark-theme))))

;; Run at every 3600 seconds, after 0s delay
;; (run-with-timer 0 3600 'theme-switcher)

(provide 'setup-style)
