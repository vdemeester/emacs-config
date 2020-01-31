;;; -*- lexical-binding: t; -*-
(use-package dired
  :defer t
  :custom
  (dired-auto-revert-buffer t)
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always)
  (dired-isearch-filenames 'dwim)
  (delete-by-moving-to-trash t)
  (dired-listing-switches "-lFaGh1v --group-directories-first")
  (dired-ls-F-marks-symlinks t)
  (dired-dwim-target t)
  :bind (("<C-return>" . vde/open-in-external-app)
         ("C-c f g"    . vde/dired-get-size)
         ("C-c f f"    . find-name-dired)
         (:map dired-mode-map
               ("M-p"         . vde/dired-up)
               ("^"           . vde/dired-up)
               ("<backspace>" . vde/dired-up)
               ("M-n"         . vde/dired-down)
               ("RET"         . find-file-reuse-dir-buffer)
               ("!"           . vde/sudired)
               ("<prior>"     . beginend-dired-mode-goto-beginning)
               ("<next>"      . beginend-dired-mode-goto-end)))
  :config
  (when (string= system-type "darwin")
    (setq dired-use-ls-dired t
          insert-directory-program "/usr/local/bin/gls"))

  ;; Enable dired-find-alternate-file
  (put 'dired-find-alternate-file 'disabled nil)

  ;; Handle long file names
  (add-hook 'dired-mode-hook #'toggle-truncate-lines)

  (defun vde/dired-up ()
    "Go to previous directory."
    (interactive)
    (find-alternate-file ".."))

  (defun vde/dired-down ()
    "Enter directory."
    (interactive)
    (dired-find-alternate-file))

  (defun vde/open-in-external-app ()
    "Open the file(s) at point with an external application."
    (interactive)
    (let* ((file-list
            (dired-get-marked-files)))
      (mapc
       (lambda (file-path)
         (let ((process-connection-type nil))
           (start-process "" nil "xdg-open" file-path))) file-list)))

  (defun find-file-reuse-dir-buffer ()
    "Like `dired-find-file', but reuse Dired buffers."
    (interactive)
    (set-buffer-modified-p nil)
    (let ((file (dired-get-file-for-visit)))
      (if (file-directory-p file)
          (find-alternate-file file)
        (find-file file))))

  (defun vde/sudired ()
    "Open directory with sudo in Dired."
    (interactive)
    (require 'tramp)
    (let ((dir (expand-file-name default-directory)))
      (if (string-match "^/sudo:" dir)
          (user-error "Already in sudo")
        (dired (concat "/sudo::" dir)))))

  (defun vde/dired-get-size ()
    "Quick and easy way to get file size in Dired."
    (interactive)
    (let ((files (dired-get-marked-files)))
      (with-temp-buffer
        (apply 'call-process "du" nil t nil "-sch" files)
        (message
         "Size of all marked files: %s"
         (progn
           (re-search-backward "\\(^[0-9.,]+[A-Za-z]+\\).*total$")
           (match-string 1)))))))

(use-package find-dired
  :after dired
  :custom
  (find-ls-option ;; applies to `find-name-dired'
   '("-ls" . "-AFhlv --group-directories-first"))
  (find-name-arg "-iname"))

(use-package dired-x                    ; Enable some nice Dired features
  :bind ("C-x C-j" . dired-jump)
  :custom
  (dired-omit-verbose nil)
  (dired-clean-confirm-killing-deleted-buffers nil)
  :hook
  (dired-mode . dired-omit-mode)
  :config
  (setq dired-omit-files (concat dired-omit-files "\\|^\\.+$\\|^\\..+$")))

(use-package dired-aux                  ; Other Dired customizations
  :after dired
  :config
  (setq
   ;; Ask for creation of missing directories when copying/moving
   dired-create-destination-dirs 'ask
   ;; Search only file names when point is on a file name
   dired-isearch-filenames'dwim))

(use-package dired-collapse
  :defer 1
  :commands (dired-collapse-mode)
  :init
  (add-hook 'dired-mode-hook #'dired-collapse-mode))

(use-package dired-quick-sort
  :defer 1
  :after dired
  :config
  (dired-quick-sort-setup))

(use-package async)

(use-package dired-async
  :after (dired async)
  :config
  (dired-async-mode 1))

(use-package dired-narrow
  :after dired
  :custom
  (dired-narrow-exit-when-one-left t)
  (dired-narrow-enable-blinking t)
  (dired-narrow-blink-time 0.3)
  :bind (:map dired-mode-map
              ("M-s n" . dired-narrow)))

(use-package wdired
  :after dired
  :commands (wdired-mode
             wdired-change-to-wdired-mode)
  :custom
  (wdired-allow-to-change-permissions t)
  (wdired-create-parent-directories t))

(use-package dired-rsync
  :ensure t
  :bind (:map dired-mode-map
              ("r" . dired-rsync)))

(provide 'setup-dired)
