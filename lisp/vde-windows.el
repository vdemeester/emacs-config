(setq window-combination-resize t) ; Size new windows proportionally

(use-package fullframe                 ; Generalized execution in a single frame
  :ensure t
  :defer t)

;; Better shrink/enlarge windows
(bind-keys*
 ("M-S-<up>"    . enlarge-window)
 ("M-S-<down>"  . shrink-window)
 ("M-S-<left>"  . shrink-window-horizontally)
 ("M-S-<right>" . enlarge-window-horizontally))

;;;###autoload
(defun vde-window-split-toggle ()
  "Toggle between horizontal and vertical split with two windows."
  (interactive)
  (if (> (length (window-list)) 2)
      (error "Can't toggle with more than 2 windows!")
    (let ((func (if (window-full-height-p)
                    #'split-window-vertically
                  #'split-window-horizontally)))
      (delete-other-windows)
      (funcall func)
      (save-selected-window
        (other-window 1)
        (switch-to-buffer (other-buffer))))))

(bind-key "C-c w t" #'vde-window-split-toggle)

(defvar vde-saved-window-configuration nil)

(defun vde-save-wins-then-call (func &optional args)
  "Save current window configuration, then call FUNC optionally with ARGS."
  (interactive)
  (push (current-window-configuration) vde-saved-window-configuration)
  (cond
   ;; We have arguments for the function
   ((bound-and-true-p args) (funcall func args))
   ;; The function expects exactly one argument, and we want it to be nil
   ((equal args "nil") (funcall func nil))
   ;; The function does not expect arguments
   (t (funcall func))))

(defun vde-restore-window-configuration (config)
  "Kill current buffer and restore the window configuration in CONFIG."
  (interactive)
  (kill-this-buffer)
  (set-window-configuration config))

(defun vde-pop-window-configuration ()
  "Restore the previous window configuration and clear current window."
  (interactive)
  (let ((config (pop vde-saved-window-configuration)))
    (if config
        (vde-restore-window-configuration config)
      (if (> (length (window-list)) 1)
          (delete-window)
        (bury-buffer)))))

(use-package eyebrowse                  ; Easy workspaces creation and switching
  :ensure t
  :init (eyebrowse-mode t)
  :config
  (setq
   eyebrowse-mode-line-separator " "
   eyebrowse-mode-line-style 'always
   eyebrowse-new-workspace t
   eyebrowse-wrap-around t))

(use-package ace-window                 ; Better movements between windows
  :ensure t
  :bind (("C-x o"   . ace-window)
         ("C-c w w" . ace-window)
         ("C-c w s" . ace-swap-window))
  :config (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package shackle
  :ensure t
  :config
  (progn
    (setq shackle-select-reused-windows nil)
    (setq shackle-default-alignment 'below)
    (setq shackle-default-size 0.25)

    (setq shackle-rules
          '((compilation-mode              :select nil :popup t               :size 0.20 :align below)
            ("*undo-tree*"                                                    :size 0.25 :align right)
            ("*eshell*"                    :select t                          :other t               )
            ("*Shell Command Output*"      :select nil                                               )
            ("\\*Async Shell.*\\*" :regexp t :ignore t                                                 )
            (occur-mode                    :select nil                                   :align t    )
            ("*Help*"                      :select t   :inhibit-window-quit t :other t               )
            ("*Completions*"                                                  :size 0.3  :align t    )
            ("*Messages*"                  :select nil :inhibit-window-quit t :other t               )
            ("\\*[Wo]*Man.*\\*"    :regexp t :select t   :inhibit-window-quit t :other t               )
            ("\\*poporg.*\\*"      :regexp t :select t                          :other t               )
            ("\\`\\*helm.*?\\*\\'"   :regexp t                                    :size 0.3  :align t    )
            ("*Calendar*"                  :select t                          :size 0.3  :align below)
            ("*info*"                      :select t   :inhibit-window-quit t                         :same t)
            (magit-status-mode             :select t   :inhibit-window-quit t                         :same t)
            (magit-log-mode                :select t   :inhibit-window-quit t                         :same t)
            ))
    (shackle-mode 1)))

(provide 'vde-windows)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End
