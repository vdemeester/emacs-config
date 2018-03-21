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

(provide 'vde-windows)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
