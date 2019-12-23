;;; setup-ivy.el --- setup ivy and related packages (counsel)
;;; Commentary:
;;; Code:
;;; -*- lexical-binding: t; -*-

(use-package swiper
  :after ivy
  :bind (:map swiper-map
              ("M-y" . yank)
              ("C-." . swiper-avy)
              ("M-c" . swiper-mc)))

(use-package ivy
  :bind (:map vde-mode-map
              ("C-x b" . vde/switch-buffer)
              ("C-x B" . ivy-switch-buffer)
              ("M-u" . ivy-resume)    ;Override the default binding for `upcase-word'
              ("C-c C-w p" . ivy-push-view) ;Push window configuration to `ivy-views'
              ("C-c C-w P" . ivy-pop-view)  ;Remove window configuration from `ivy-views'
              ("C-c C-w s" . ivy-switch-view)) ; Switch window configuration to `ivy-views'
  :init
  (progn
    (bind-to-vde-map "v" #'counsel-set-variable))
  (ivy-mode 1)
  :config
  (progn
    (defun vde/switch-buffer (arg)
      "Custom switch to buffer.
With universal argument ARG or when not in project, rely on
`ivy-switch-buffer'.
Otherwise, use `counsel-projectile-switch-project'."
      (interactive "P")
      (if (or arg
              (not (projectile-project-p)))
          (ivy-switch-buffer)
        (counsel-projectile-switch-to-buffer)))
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
  :after ivy)

(use-package ivy-rich
  :after ivy
  :custom
  (ivy-virtual-abbreviate 'full
                          ivy-rich-switch-buffer-align-virtual-buffer t
                          ivy-rich-path-style 'abbrev)
  :config (ivy-rich-mode 1))
(use-package counsel
  :bind (:map vde-mode-map
              ("M-i" . counsel-semantic-or-imenu)
              ;;("M-i" . counsel-grep-or-swiper)
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
    (setq
     counsel-describe-function-function 'helpful-function
     counsel-describe-variable-function 'helpful-variable)
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

    (push '(counsel-rg . "--glob '**' -- ") ivy-initial-inputs-alist)
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

(provide 'setup-ivy)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

