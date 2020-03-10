;;; -*- lexical-binding: t; -*-
(use-package ivy
  :delight
  :custom
  (ivy-count-format "%d/%d ")
  (ivy-height-alist '((t lambda (_caller) (/ (window-height) 4))))
  (ivy-use-virtual-buffers t)
  (ivy-virtual-abbreviate 'full) ;Show the full virtual file paths
  (ivy-wrap nil)
  (ivy-re-builders-alist
   '((counsel-M-x . ivy--regex-fuzzy)
     (t . ivy--regex-plus)))
  (ivy-display-style 'fancy)
  (ivy-use-selectable-prompt t)
  (ivy-fixed-height-minibuffer nil)
  (ivy-extra-directories nil) ; Default value: ("../" "./")
  :bind (:map vde-mode-map
              ("C-x b" . vde/switch-buffer)
              ("C-x B" . ivy-switch-buffer)
              ("M-u" . ivy-resume)    ;Override the default binding for `upcase-word'
              ("C-c C-w p" . ivy-push-view) ;Push window configuration to `ivy-views'
              ("C-c C-w P" . ivy-pop-view)  ;Remove window configuration from `ivy-views'
              ("C-c C-w s" . ivy-switch-view) ; Switch window configuration to `ivy-views'
              :map ivy-occur-mode-map
              ("f" . forward-char)
              ("b" . backward-char)
              ("n" . ivy-occur-next-line)
              ("p" . ivy-occur-previous-line)
              ("<C-return>" . ivy-occur-press))
  :init
  (progn
    (bind-to-vde-map "v" #'counsel-set-variable))
  :hook
  (ivy-occur-mode . hl-line-mode)
  :config
  (ivy-set-occur 'ivy-switch-buffer 'ivy-switch-buffer-occur)
  (ivy-set-occur 'swiper 'swiper-occur)
  (ivy-set-occur 'swiper-isearch 'swiper-occur)
  (ivy-mode 1)
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

(use-package prescient
  :custom
  (prescient-history-length 50)
  ;; (prescient-save-file "~/.emacs.d/prescient-items")
  (prescient-filter-method '(fuzzy initialism regexp))
  :config
  (prescient-persist-mode 1))

;;; Default rg arguments
;; https://github.com/BurntSushi/ripgrep
(defconst vde/rg-arguments
  `("--no-ignore-vcs"                   ;Ignore files/dirs ONLY from `.ignore'
    "--line-number"                     ;Line numbers
    "--smart-case"
    "--max-columns" "150"      ;Emacs doesn't handle long line lengths very well
    "--ignore-file" ,(expand-file-name ".ignore" (getenv "HOME")))
  "Default rg arguments used in the functions in `counsel' and `projectile' packages.")

(use-package ivy-prescient
  :after (prescient ivy)
  :custom
  (ivy-prescient-sort-commands
   '(:not swiper ivy-switch-buffer counsel-switch-buffer))
  (ivy-prescient-retain-classic-highlighting t)
  (ivy-prescient-enable-filtering t)
  (ivy-prescient-enable-sorting t)
  :config
  (defun prot/ivy-prescient-filters (str)
    "Specify an exception for `prescient-filter-method'.

This new rule can be used to tailor the results of individual
Ivy-powered commands, using `ivy-prescient-re-builder'."
    (let ((prescient-filter-method '(literal regexp)))
      (ivy-prescient-re-builder str)))

  (setq ivy-re-builders-alist
        '((counsel-rg . prot/ivy-prescient-filters)
          (counsel-grep . prot/ivy-prescient-filters)
          (counsel-yank-pop . prot/ivy-prescient-filters)
          (swiper . prot/ivy-prescient-filters)
          (swiper-isearch . prot/ivy-prescient-filters)
          (swiper-all . prot/ivy-prescient-filters)
          (t . ivy-prescient-re-builder)))
  (ivy-prescient-mode 1))

(use-package counsel
  :after ivy
  :custom
  (counsel-yank-pop-preselect-last t)
  (counsel-yank-pop-separator "\n—————————\n")
  (counsel-describe-function-function 'helpful-function)
  (counsel-describe-variable-function 'helpful-variable)
  (counsel-find-file-at-point t)
  (counsel-find-file-ignore-regexp
   ;; Note that `ivy-extra-directories' should also not contain the "../" and
   ;; "./" elements if you don't want to see those in the `counsel-find-file'
   ;; completion list.
   (concat
    ;; file names beginning with # or .
    "\\(?:\\`[#.]\\)"
    ;; file names ending with # or ~
    "\\|\\(?:[#~]\\'\\)"))
  :bind (:map vde-mode-map
              ("M-i" . counsel-semantic-or-imenu)
              ;;("M-i" . counsel-grep-or-swiper)
              ("C-x C-r" . counsel-recentf)
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
              ([remap describe-function] . counsel-describe-function)
              ("M-s r" . counsel-rg)
              ("M-s g" . counsel-git-grep)
              ("M-s z" . prot/counsel-fzf-rg-files)
              :map ivy-minibuffer-map
              ("C-r" . counsel-minibuffer-history)
              ("C-SPC" . ivy-restrict-to-matches))
  :init
  (progn
    (bind-to-vde-map "s" #'counsel-rg))
  :config
  (progn
    (defun prot/counsel-fzf-rg-files (&optional input dir)
      "Run `fzf' in tandem with `ripgrep' to find files in the
present directory.  If invoked from inside a version-controlled
repository, then the corresponding root is used instead."
      (interactive)
      (let* ((process-environment
              (cons (concat "FZF_DEFAULT_COMMAND=rg -Sn --color never --files --no-follow --hidden")
                    process-environment))
             (vc (vc-root-dir)))
        (if dir
            (counsel-fzf input dir)
          (if (eq vc nil)
              (counsel-fzf input default-directory)
            (counsel-fzf input vc)))))

    (defun prot/counsel-fzf-dir (arg)
      "Specify root directory for `counsel-fzf'."
      (prot/counsel-fzf-rg-files ivy-text
                                 (read-directory-name
                                  (concat (car (split-string counsel-fzf-cmd))
                                          " in directory: "))))

    (defun prot/counsel-rg-dir (arg)
      "Specify root directory for `counsel-rg'."
      (let ((current-prefix-arg '(4)))
        (counsel-rg ivy-text nil "")))

    ;; TODO generalise for all relevant file/buffer counsel-*?
    (defun prot/counsel-fzf-ace-window (arg)
      "Use `ace-window' on `prot/counsel-fzf-rg-files' candidate."
      (ace-window t)
      (let ((default-directory (if (eq (vc-root-dir) nil)
                                   counsel--fzf-dir
                                 (vc-root-dir))))
        (if (> (length (aw-window-list)) 1)
            (progn
              (find-file arg))
          (find-file-other-window arg))
        (balance-windows)))

    ;; Pass functions as appropriate Ivy actions (accessed via M-o)
    (ivy-add-actions
     'counsel-fzf
     '(("r" prot/counsel-fzf-dir "change root directory")
       ("g" prot/counsel-rg-dir "use ripgrep in root directory")
       ("a" prot/counsel-fzf-ace-window "ace-window switch")))

    (ivy-add-actions
     'counsel-rg
     '(("r" prot/counsel-rg-dir "change root directory")
       ("z" prot/counsel-fzf-dir "find file with fzf in root directory")))

    (ivy-add-actions
     'counsel-find-file
     '(("g" prot/counsel-rg-dir "use ripgrep in root directory")
       ("z" prot/counsel-fzf-dir "find file with fzf in root directory")))

    (ivy-set-actions
     'counsel-find-file
     `(("x"
        (lambda (x) (delete-file (expand-file-name x ivy--directory)))
        ,(propertize "delete" 'face 'font-lock-warning-face))))

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

(use-package company
  :commands global-company-mode
  :init
  (add-hook 'after-init-hook #'global-company-mode)
  (setq
   company-idle-delay 0.2
   company-selection-wrap-around t
   company-minimum-prefix-length 2
   company-require-match nil
   company-dabbrev-ignore-case nil
   company-dabbrev-downcase nil
   company-show-numbers t
   company-tooltip-align-annotations t)
  :config
  (bind-keys :map company-active-map
             ("C-d" . company-show-doc-buffer)
             ("C-l" . company-show-location)
             ("C-n" . company-select-next)
             ("C-p" . company-select-previous)
             ("C-t" . company-select-next)
             ("C-s" . company-select-previous)
             ("TAB" . company-complete))
  (setq company-backends
        '(company-css
          company-clang
          company-capf
          company-semantic
          company-xcode
          company-cmake
          company-files
          company-gtags
          company-etags
          company-keywords)))

(use-package company-prescient
  :ensure company
  :after (company prescient)
  :config
  (company-prescient-mode 1))

(use-package company-emoji
  :ensure company
  :config
  (add-to-list 'company-backends 'company-emoji))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :custom
  (lsp-enable-file-watchers nil)
  (lsp-gopls-staticcheck t)
  (lsp-gopls-complete-unimported t)
  (lsp-eldoc-render-all nil)
  (lsp-enable-snippet nil)
  (lsp-enable-links nil)
  (lsp-enable-folding nil)
  (lsp-enable-completion-at-point nil) ;; company-lsp takes care of it ?
  (lsp-diagnostic-package :none)
  (lsp-restart 'auto-restart)
  (lsp-ui-sideline-enable nil)
  (lsp-ui-sideline-show-hover nil)
  (lsp-ui-sideline-delay 2.0)
  (lsp-ui-doc-enable nil)
  (lsp-ui-doc-max-width 30)
  (lsp-ui-doc-max-height 15)
  (lsp-document-highlight-delay 2.0)
  (lsp-auto-guess-root t)
  (lsp-ui-flycheck-enable nil)
  ;; @see https://github.com/emacs-lsp/lsp-mode/pull/1498
  ;; and read code related to auto configure
  ;; require clients could be slow and that's only thing auto configure
  ;; could do for me. Manual loading of client is faster.
  (lsp-auto-configure nil)
  (lsp-prefer-flymake nil) ; Use flycheck instead of flymake
  :config
  ;; don't ping LSP lanaguage server too frequently
  (defvar lsp-on-touch-time 0)
  (defadvice lsp-on-change (around lsp-on-change-hack activate)
    ;; don't run `lsp-on-change' too frequently
    (when (> (- (float-time (current-time))
                lsp-on-touch-time) 30) ;; 30 seconds
      (setq lsp-on-touch-time (float-time (current-time)))
      ad-do-it))
  :hook ((go-mode . lsp-deferred)
         (python-mode . lsp-deferred)))

;; lsp-ui: This contains all the higher level UI modules of lsp-mode, like flycheck support and code lenses.
;; https://github.com/emacs-lsp/lsp-ui
(use-package lsp-ui
  :after lsp-mode
  ;;:hook ((lsp-mode . lsp-ui-mode)
  ;;       (lsp-ui-mode . lsp-ui-peek-mode))
  :config
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references))

;;Set up before-save hooks to format buffer and add/delete imports.
;;Make sure you don't have other gofmt/goimports hooks enabled.
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

(with-eval-after-load "company"
  (use-package company-lsp
    :after lsp-mode
    :config
    (push 'company-lsp company-backends)))

(with-eval-after-load "projectile"
  (defun my-set-projectile-root ()
    (when lsp--cur-workspace
      (setq projectile-project-root (lsp--workspace-root lsp--cur-workspace))))
  (add-hook 'lsp-before-open-hook #'my-set-projectile-root))

(use-package dap-mode
  :after lsp-mode
  :bind (:map dap-mode-map
              ([f9] . dap-debug)
              ;; ([f9] . dap-continue)
              ;; ([S-f9] . dap-disconnect)
              ;; ([f10] . dap-next)
              ;; ([f11] . dap-step-in)
              ;; ([S-f11] . dap-step-out)
              ([C-f9] . dap-hide/show-ui))
  :hook (dap-stopped-hook . (lambda (arg) (call-interactively #'dap-hydra)))
  :config
  ;; FIXME: Create nice solution instead of a hack
  (defvar dap-hide/show-ui-hidden? t)
  (defun dap-hide/show-ui ()
    "Hide/show dap ui. FIXME"
    (interactive)
    (if dap-hide/show-ui-hidden?
        (progn
          (setq dap-hide/show-ui-hidden? nil)
          (dap-ui-locals)
          (dap-ui-repl))
      (dolist (buf '("*dap-ui-inspect*" "*dap-ui-locals*" "*dap-ui-repl*" "*dap-ui-sessions*"))
        (when (get-buffer buf)
          (kill-buffer buf)))
      (setq dap-hide/show-ui-hidden? t)))

  (dap-mode)
  (dap-ui-mode)
  (dap-tooltip-mode))

(provide 'setup-completion)
