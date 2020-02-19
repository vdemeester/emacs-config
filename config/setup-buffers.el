;;; -*- lexical-binding: t; -*-
;; Don't let the cursor go into minibuffer prompt
(let ((default (eval (car (get 'minibuffer-prompt-properties 'standard-value))))
      (dont-touch-prompt-prop '(cursor-intangible t)))
  (setq minibuffer-prompt-properties
        (append default dont-touch-prompt-prop))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode))

;; Allow to read from minibuffer while in minibuffer.
(setq enable-recursive-minibuffers t)

;; Show the minibuffer depth (when larger than 1)
(minibuffer-depth-indicate-mode 1)

(use-package savehist                   ; Save minibuffer history
  :init (savehist-mode t)
  :custom
  (history-length 1000)
  (savehist-save-minibuffer-history t)
  (savehist-autosave-interval 180)
  :config
  (savehist-mode 1))

(use-package emacs
  :init
  ;; Configure `display-buffer' behaviour for some special buffers
  (setq display-buffer-alist
        '(;; bottom side window
          ("\\*e?shell.*"
           (display-buffer-in-side-window)
           (window-height . 0.25)
           (side . bottom)
           (slot . -1))
          ("\\*v?term.*"
           (display-buffer-in-side-window)
           (window-height . 0.25)
           (side . bottom)
           (slot . -1))
          ("\\*\\(Backtrace\\|Warnings\\|Compile-Log\\|[Hh]elp\\|Messages\\)\\*"
           (display-buffer-in-side-window)
           (window-height . 0.25)
           (side . bottom)
           (slot . 0))
          ("\\*\\(helpful\\).*"
           (display-buffer-in-side-window)
           (window-height . 0.25)
           (side . bottom)
           (slot . 0))
          ("\\*\\(compilation\\|go test\\).*"
           (display-buffer-in-side-window)
           (window-height . 0.25)
           (side . bottom)
           (slot . 0))
          ("\\*\\(ielm\\).*"
           (display-buffer-in-side-window)
           (window-height . 0.25)
           (side . bottom)
           (slot . 1))
          ;; right side window
          ("\\*wclock*"
           (display-buffer-in-side-window)
           (window-width . 0.20)
           (side . right)
           (slot . -1))
          ("\\*undo-tree*"
           (display-buffer-in-side-window)
           (window-width . 0.20)
           (side . right)
           (slot . -1))
          ("\\*\\(Flycheck\\|Package-Lint\\).*"
           (display-buffer-in-side-window)
           (window-width . 0.20)
           (side . right)
           (slot . 0)
           (window-parameters . ((no-other-window . t)
                                 (mode-line-format . (" "
                                                      mode-line-buffer-identification)))))
          ("\\*Faces\\*"
           (display-buffer-in-side-window)
           (window-width . 0.20)
           (side . right)
           (slot . 1)
           (window-parameters . ((no-other-window . t)
                                 (mode-line-format . (" "
                                                      mode-line-buffer-identification)))))
          ("\\*Custom.*"
           (display-buffer-in-side-window)
           (window-width . 0.20)
           (side . right)
           (slot . 2))))
  (setq window-sides-vertical nil)
  (setq window-combination-resize t) ; Size new windows proportionally
  :bind (("C-x +" . balance-windows-area)
         ("<f7>" . window-toggle-side-windows)))

(use-package uniquify                   ; Unique buffer names
  :custom
  (uniquify-buffer-name-style 'post-forward)
  (uniquify-separator ":")
  (uniquify-ignore-buffers-re "^\\*")
  (uniquify-after-kill-buffer-p t))

(use-package ibuf-ext                   ; Extensions for Ibuffer
  :config
  ;; Do not show empty groups
  (setq ibuffer-show-empty-filter-groups nil))

(use-package ibuffer                    ; Buffer management
  :custom
  (ibuffer-expert t)
  (ibuffer-filter-group-name-face 'font-lock-doc-face)
  (ibuffer-default-sorting-mode 'filename/process)
  (ibuffer-use-header-line t)
  :bind (("C-x C-b" . ibuffer)
         ([remap list-buffers] . ibuffer))
  :config
  ;; Use human readable Size column instead of original one
  (define-ibuffer-column size-h
    (:name "Size" :inline t)
    (cond
     ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
     ((> (buffer-size) 1000) (format "%7.1fk" (/ (buffer-size) 1000.0)))
     (t (format "%8d" (buffer-size)))))

  (setq ibuffer-formats
        '((mark modified read-only " "
                (name 18 18 :left :elide)
                " "
                (size-h 9 -1 :right)
                " "
                (mode 16 16 :left :elide)
                " "
                filename-and-process)
          (mark modified read-only " "
                (name 18 18 :left :elide)
                " "
                (size 9 -1 :right)
                " "
                (mode 16 16 :left :elide)
                " "
                (vc-status 16 16 :left)
                " "
                filename-and-process))))

(use-package ibuffer-vc                 ; Group buffers by VC project and status
  :defer 2
  :init (add-hook 'ibuffer-hook
                  (lambda ()
                    (ibuffer-vc-set-filter-groups-by-vc-root)
                    (unless (eq ibuffer-sorting-mode 'filename/process)
                      (ibuffer-do-sort-by-filename/process)))))

(provide 'setup-buffers)
