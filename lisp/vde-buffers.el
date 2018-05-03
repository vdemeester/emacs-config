
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

(setq
 ;; Never use dialogs for minibuffer input
 use-dialog-box nil
 ;; Store more history
 history-length 1000)

(use-package savehist                   ; Save minibuffer history
  :init (savehist-mode t)
  :config
  (setq
   savehist-save-minibuffer-history t
   savehist-autosave-interval 180))

;; Configure `display-buffer' behaviour for some special buffers
(setq
 display-buffer-alist
 `(
   ;; Messages, errors, processes, Calendar and REPLs in the bottom side window
   (,(rx bos (or "*Apropos"             ; Apropos buffers
                 "*Man"                 ; Man buffers
                 "*Help"                ; Help buffers
                 "*Warnings*"           ; Emacs warnings
                 "*Process List*"       ; Processes
                 "*Proced"              ; Proced processes list
                 "*Compile-Log*"        ; Emacs byte compiler log
                 "*compilation"         ; Compilation buffers
                 "*Flycheck errors*"    ; Flycheck error list
                 "*Calendar"            ; Calendar window
                 "*SQL"                 ; SQL REPL
		 ))
    (display-buffer-reuse-window display-buffer-in-side-window)
    (side . bottom)
    (reusable-frames . visible)
    (window-height . 0.45))
   ;; Open shell in a single window
   (,(rx bos "*shell")
    (display-buffer-same-window)
    (reusable-frames . nil))
   ;; Open PDFs in the right side window
   (,(rx bos "*pdf")
    (display-buffer-reuse-window display-buffer-in-side-window)
    (side . right)
    (reusable-frames . visible)
    (window-width . 0.5))
   ;; Let `display-buffer' reuse visible frames for all buffers.  This must
   ;; be the last entry in `display-buffer-alist', because it overrides any
   ;; previous entry with more specific actions.
   ("." nil (reusable-frames . visible))))

(use-package uniquify                   ; Unique buffer names
  :config
  (setq
   uniquify-buffer-name-style 'post-forward
   uniquify-separator ":"
   ;; Ignore special buffers
   uniquify-ignore-buffers-re "^\\*"))

(use-package ibuf-ext                   ; Extensions for Ibuffer
  :config
  ;; Do not show empty groups
  (setq ibuffer-show-empty-filter-groups nil))

(use-package ibuffer                    ; Buffer management
  :bind (("C-x C-b" . vde-ibuffer-open)
         ([remap list-buffers] . ibuffer)
         :map ibuffer-mode-map
         ("q" . vde-pop-window-configuration))
  :config
  (setq
   ibuffer-expert t              ; Do not prompt when on kill buffers operations
   ibuffer-filter-group-name-face 'font-lock-doc-face)

  (defun vde-ibuffer-open ()
    "Save window configuration and call `ibuffer'."
    (interactive)
    (vde-save-wins-then-call 'ibuffer))

  ;; Use a single full frame for ibuffer
  (with-eval-after-load 'ibuffer
    (fullframe ibuffer vde-pop-window-configuration))
  
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

;; Use `emacs-lisp-mode' instead of `lisp-interaction-mode' for scratch buffer
(setq initial-major-mode 'emacs-lisp-mode)

(provide 'vde-buffers)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
