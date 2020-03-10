;;; -*- lexical-binding: t; -*-

;;;###autoload
(defun vde/window-split-toggle ()
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

(bind-key "C-c w t" #'vde/window-split-toggle)

(defvar vde/saved-window-configuration nil)

(defun vde/save-wins-then-call (func &optional args)
  "Save current window configuration, then call FUNC optionally with ARGS."
  (interactive)
  (push (current-window-configuration) vde/saved-window-configuration)
  (cond
   ;; We have arguments for the function
   ((bound-and-true-p args) (funcall func args))
   ;; The function expects exactly one argument, and we want it to be nil
   ((equal args "nil") (funcall func nil))
   ;; The function does not expect arguments
   (t (funcall func))))

(use-package eyebrowse                  ; Easy workspaces creation and switching
  :init (eyebrowse-mode t)
  :config
  (setq
   eyebrowse-mode-line-separator " "
   eyebrowse-mode-line-style 'always
   eyebrowse-new-workspace t
   eyebrowse-wrap-around t))

(use-package ace-window                 ; Better movements between windows
  :custom
  (aw-keys '(?a ?u ?i ?e ?, ?c ?t ?r ?m))
  (aw-scope 'frame)
  (aw-dispatch-always t)
  (aw-dispatch-alist
   '((?s aw-swap-window "Swap Windows")
     (?2 aw-split-window-vert "Split Window Vertically")
     (?3 aw-split-window-horz "Split Window Horizontally")
     (?? aw-show-dispatch-help)))
  (aw-minibuffer-flag t)
  (aw-ignore-current nil)
  (aw-display-mode-overlay t)
  (aw-background t)
  :bind (("C-x o"   . ace-window)
         ("C-c w w" . ace-window)
         ("C-c w s" . ace-swap-window)))

(use-package windmove
  :bind (("M-<left>" . windmove-left)
         ("M-<down>" . windmove-down)
         ("M-<up>" . windmove-up)
         ("M-<right>" . windmove-right)))

(provide 'setup-windows)
