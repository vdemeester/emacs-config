;; This file stores my configuration for Ivy and related packages.
(use-package ivy
  :ensure t
  :bind (:map vde-mode-map
         ("M-u" . ivy-resume)    ;Override the default binding for `upcase-word'
         ("C-c w" . ivy-push-view) ;Push window configuration to `ivy-views'
         ("C-c W" . ivy-pop-view)) ;Remove window configuration from `ivy-views'
  :init
  (progn
    (bind-to-vde-map "v" #'counsel-set-variable))
  :config
  (progn
    ;; Disable ido
    (with-eval-after-load 'ido
      (ido-mode -1)
      ;; Enable ivy
      (ivy-mode 1))

    ;; Show recently killed buffers when calling `ivy-switch-buffer'
    (setq ivy-use-virtual-buffers t)
    (setq ivy-virtual-abbreviate 'full) ;Show the full virtual file paths

    ;; Jump back to first candidate when on the last one
    ivy-wrap t
    (setq ivy-count-format "%d/%d ")
    (setq ivy-re-builders-alist '((t . ivy--regex-plus))) ;Default
    ;; (setq ivy-re-builders-alist '((t . ivy--regex-fuzzy)))

    ;; Do not show "./" and "../" in the `counsel-find-file' completion list
    (setq ivy-extra-directories nil)    ;Default value: ("../" "./")
    ))

(use-package ivy-hydra                  ; Additional bindings for Ivy
  :ensure t
  :after ivy)

(use-package ivy-historian              ; Store minibuffer candidates
  :ensure t
  :pin melpa
  :init (ivy-historian-mode +1))

(use-package counsel
  :ensure t
  :bind (:map vde-mode-map
	      ("M-i" . counsel-grep-or-swiper)
	      ("C-M-y" . counsel-yank-pop)
	      ("C-h F" . counsel-faces)       ;Overrides `Info-goto-emacs-command-node'
	      ("C-h S" . counsel-info-lookup-symbol)
	      ("C-c u" . counsel-unicode-char)
	      ("C-c C" . counsel-colors-emacs) ;Alternative to `list-colors-display'
	      ([remap execute-extended-command] . counsel-M-x)
	      ([remap bookmark-jump] . counsel-bookmark) ;Jump to book or set it if it doesn't exist, C-x r b
	      ([remap bookmark-set] . counsel-bookmark)  ;C-x r m
	      ([remap find-file]  . counsel-find-file)
	      ([remap describe-bindings] . counsel-descbinds)
	      ([remap finder-by-keyword] . counsel-package) ;C-h p
	      ([remap describe-variable] . counsel-describe-variable)
	      ([remap describe-function] . counsel-describe-function))
  :init
  (progn
    (bind-to-vde-map "s" #'counsel-rg))
  :config
  (progn
    ;; counsel-find-file
    (setq counsel-find-file-at-point t)
    (setq counsel-find-file-ignore-regexp
          (concat
           ;; file names beginning with # or .
           "\\(?:\\`[#.]\\)"
           ;; file names ending with # or ~
           "\\|\\(?:[#~]\\'\\)"))
    ;; Note that `ivy-extra-directories' should also not contain the "../" and
    ;; "./" elements if you don't want to see those in the `counsel-find-file'
    ;; completion list.
    (ivy-set-actions
     'counsel-find-file
     `(("x"
        (lambda (x) (delete-file (expand-file-name x ivy--directory)))
        ,(propertize "delete" 'face 'font-lock-warning-face))))

    ;; Show parent directory in the prompt
    (ivy-set-prompt 'counsel-ag #'counsel-prompt-function-dir)

    ;; counsel-rg
    ;; Redefine `counsel-rg-base-command' with my required options, especially
    ;; the `--follow' option to allow search through symbolic links (part of
    ;; `modi/rg-arguments').
    (setq counsel-rg-base-command
          (concat (mapconcat #'shell-quote-argument
                             (append '("rg")
                                     vde/rg-arguments
                                     '("--no-heading" ;No file names above matching content
                                       ))
                             " ")
                  " %s"            ;This MUST be %s, not %S
                                        ;https://github.com/abo-abo/swiper/issues/427
                  ))))

(provide 'vde-ivy)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
