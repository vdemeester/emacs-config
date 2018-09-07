;;; -*- lexical-binding: t; -*-
;; My minor mode
;; Main use is to have my key bindings have the highest priority

(defvar vde-special-keymap-prefix (kbd "C-x m")
  "`vde-mode' keymap prefix.
Overrides the default binding for `compose-mail'.")

(defvar vde-mode-special-map (make-sparse-keymap)
  "Special keymap for `vde-mode' whose bindings begin with
`vde-special-keymap-prefix'.")
(fset 'vde-mode-special-map vde-mode-special-map)

(defvar vde-mode-map (let ((map (make-sparse-keymap)))
                        (define-key map vde-special-keymap-prefix 'vde-mode-special-map)
                        map)
  "Keymap for `vde-mode'.")

;;;###autoload
(define-minor-mode vde-mode
  "A minor mode so that my key settings override annoying major modes."
  ;; If init-value is not set to t, this mode does not get enabled in
  ;; `fundamental-mode' buffers even after doing \"(global-vde-mode 1)\".
  ;; More info: http://emacs.stackexchange.com/q/16693/115
  :init-value t
  :lighter    " μ"
  :keymap     vde-mode-map)

;;;###autoload
(define-globalized-minor-mode global-vde-mode vde-mode vde-mode)

;; https://github.com/jwiegley/use-package/blob/master/bind-key.el
;; The keymaps in `emulation-mode-map-alists' take precedence over
;; `minor-mode-map-alist'
(add-to-list 'emulation-mode-map-alists `((vde-mode . ,vde-mode-map)))

;; Turn off the minor mode in the minibuffer
(defun turn-off-vde-mode ()
  "Turn off vde-mode."
  (vde-mode -1))
(add-hook 'minibuffer-setup-hook #'turn-off-vde-mode)

(defmacro bind-to-vde-map (key fn)
  "Bind a function to the `vde-mode-special-map'.
USAGE: (bind-to-vde-map \"f\" #'full-screen-center)."
  `(define-key vde-mode-special-map (kbd ,key) ,fn))

;; http://emacs.stackexchange.com/a/12906/115
(defun unbind-from-vde-map (key)
  "Unbind a function from the `vde-mode-map'
USAGE: (unbind-from-vde-map \"C-x m f\")
"
  (interactive "kUnset key from vde-mode-map: ")
  (define-key vde-mode-map (kbd (key-description key)) nil)
  (message "%s" (format "Unbound %s key from the %s."
                        (propertize (key-description key)
                                    'face 'font-lock-function-name-face)
                        (propertize "vde-mode-map"
                                    'face 'font-lock-function-name-face))))


(provide 'vde-mode)

;; Minor mode tutorial: http://nullprogram.com/blog/2013/02/06/
