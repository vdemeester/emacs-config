;;; setup-files.el --- setup files related modes
;;; Commentary:
;;; Code:
;;; -*- lexical-binding: t; -*-

(use-package files                      ; Core commands for files
  :bind (("<f5>" . revert-buffer)))

(use-package ripgrep
  :defer 2)

(use-package recentf
  :custom
  (recentf-max-menu-items 15)
  (recentf-max-saved-items 200)
  (recentf-show-file-shortcuts-flag nil)
  :config
  (recentf-mode 1)
  (add-to-list 'recentf-exclude "^/\\(?:ssh\\|su\\|sudo\\)?:")
  ;; Magic advice to rename entries in recentf when moving files in
  ;; dired.
  (defun rjs/recentf-rename-notify (oldname newname &rest args)
    (if (file-directory-p newname)
        (rjs/recentf-rename-directory oldname newname)
      (rjs/recentf-rename-file oldname newname)))

  (defun rjs/recentf-rename-file (oldname newname)
    (setq recentf-list
          (mapcar (lambda (name)
                    (if (string-equal name oldname)
                        newname
                      oldname))
                  recentf-list))
    recentf-cleanup)

  (defun rjs/recentf-rename-directory (oldname newname)
    ;; oldname, newname and all entries of recentf-list should already
    ;; be absolute and normalised so I think this can just test whether
    ;; oldname is a prefix of the element.
    (setq recentf-list
          (mapcar (lambda (name)
                    (if (string-prefix-p oldname name)
                        (concat newname (substring name (length oldname)))
                      name))
                  recentf-list))
    recentf-cleanup)

  (advice-add 'dired-rename-file :after #'rjs/recentf-rename-notify)

  (defun contrib/recentf-add-dired-directory ()
    "Include Dired buffers in the list.  Particularly useful when
combined with a completion framework's ability to display virtual
buffers."
    (when (and (stringp dired-directory)
               (equal "" (file-name-nondirectory dired-directory)))
      (recentf-add-file dired-directory))))

(setq view-read-only t)                 ; View read-only

(use-package direnv
  :config
  (direnv-mode))

(use-package hardhat                    ; Protect user-writable files
  :init (global-hardhat-mode))

(use-package image-file                 ; Visit images as images
  :init (auto-image-file-mode))

(use-package markdown-mode              ; Edit markdown files
  :mode ("\\.md\\'" . markdown-mode)
  :config
  (setq markdown-fontify-code-blocks-natively t)

  ;; Don't change font in code blocks
  (set-face-attribute 'markdown-code-face nil
                      :inherit nil)

  ;; Process Markdown with Pandoc, using a custom stylesheet for nice output
  (let ((stylesheet (expand-file-name
                     (locate-user-emacs-file "etc/pandoc.css"))))
    (setq markdown-command
          (mapconcat #'shell-quote-argument
                     `("pandoc" "--toc" "--section-divs"
                       "--css" ,(concat "file://" stylesheet)
                       "--standalone" "-f" "markdown" "-t" "html5")
                     " ")))
  (add-hook 'markdown-mode-hook #'auto-fill-mode))

(use-package highlight-indentation
  :config
  (set-face-background 'highlight-indentation-face "#e3e3d3")
  (set-face-background 'highlight-indentation-current-column-face "#c3b3b3"))

(use-package yaml-mode
  :mode "\\.ya?ml\\'"
  :hook ((yaml-mode . highlight-indentation-mode)
         (yaml-mode . highlight-indentation-current-column-mode)))

(use-package toml-mode
  :mode "\\.to?ml\\'")

;;;###autoload
(defun vde/delete-this-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (or (buffer-file-name) (error "No file is currently being edited"))
  (when (yes-or-no-p (format "Really delete '%s'?"
                             (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))

;;;###autoload
(defun vde/rename-this-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file!" name))
    (if (get-buffer new-name)
        (message "A buffer named '%s' already exists!" new-name)
      (progn
        (when (file-exists-p filename)
          (rename-file filename new-name 1))
        (rename-buffer new-name)
        (set-visited-file-name new-name)))))

(bind-key "C-c f D" #'vde/delete-this-file)
(bind-key "C-c f R" #'vde/rename-this-file-and-buffer)

;; Additional bindings for built-ins
(bind-key "C-c f v d" #'add-dir-local-variable)
(bind-key "C-c f v l" #'add-file-local-variable)
(bind-key "C-c f v p" #'add-file-local-variable-prop-line)

(defun vde/reload-dir-locals-for-current-buffer ()
  "Reload dir locals for the current buffer."
  (interactive)
  (let ((enable-local-variables :all))
    (hack-dir-local-variables-non-file-buffer)))

(defun vde/reload-dir-locals-for-all-buffers-in-this-directory ()
  "Reload dir-locals for all buffers in current buffer's `default-directory'."
  (interactive)
  (let ((dir default-directory))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (equal default-directory dir))
        (vde/reload-dir-locals-for-current-buffer)))))

(bind-key "C-c f v r" #'vde/reload-dir-locals-for-current-buffer)
(bind-key "C-c f v r" #'vde/reload-dir-locals-for-all-buffers-in-this-directory)

(provide 'setup-files)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
