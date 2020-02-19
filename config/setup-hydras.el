;;; -*- lexical-binding: t; -*-
(defhydra hydra-goto-line (goto-map "")
  "goto-line"
  ("g" goto-line "go")
  ("m" set-mark-command "mark" :bind nil)
  ("q" nil "quit"))

(defhydra hydra-yank-pop ()
  "yank"
  ("C-y" yank nil)
  ("M-y" yank-pop nil)
  ("y" (yank-pop 1) "next")
  ("Y" (yank-pop -1) "prev"))   ; or browse-kill-ring

(defhydra hydra-zoom (global-map "<f2>")
  "zoom"
  ("g" text-scale-increase "in")
  ("l" text-scale-decrease "out")
  ("r" (text-scale-set 0) "reset")
  ("0" (text-scale-set 0) :bind nil :exit t)
  ("1" (text-scale-set 0) nil :bind nil :exit t))

;; Better shrink/enlarge windows
(defhydra hydra-resize (global-map "<f2>")
  "resize windows"
  ("<up>" enlarge-window "enlarge")
  ("<down>" shrink-window "shrink")
  ("<left>" shrink-window-horizontally "shrink horizontaly")
  ("<right>" enlarge-window-horizontally "enlarge horizontaly"))

(defvar hide-mode-line-mode nil)
(defvar whitespace-mode nil)
(defvar subword-mode nil)
(defhydra hydra-toggle (:color pink :hint nil)
  "
_a_ abbrev-mode:          %`abbrev-mode
_b_ subword-mode:         %`subword-mode
_d_ debug-on-error:       %`debug-on-error
_h_ hide-mode-line-mode   %`hide-mode-line-mode
_f_ auto-fill-mode:       %`auto-fill-function
_r_ readonly-mode:        %`buffer-read-only
_t_ truncate-lines        %`truncate-lines
_v_ visual-line-mode:     %`visual-line-mode
_w_ whitespace-mode:      %`whitespace-mode
_s_ smartparens-strict:   %`smartparens-strict-mode
_V_ visible-mode:         %`visible-mode
"
  ("a" abbrev-mode             nil)
  ("b" subword-mode            nil)
  ("d" toggle-debug-on-error   nil)
  ("f" auto-fill-mode          nil)
  ("h" hide-mode-line-mode     nil)
  ("r" dired-toggle-read-only  nil)
  ("t" toggle-truncate-lines   nil)
  ("v" visual-line-mode        nil)
  ("V" visible-mode            nil)
  ("w" whitespace-mode         nil)
  ("s" smartparens-strict-mode nil)
  ("q" nil "quit"))

(global-set-key (kbd "C-c C-v") 'hydra-toggle/body)

(defhydra hydra-marked-items (dired-mode-map "")
  "
Number of marked items: %(length (dired-get-marked-files))
"
  ("m" dired-mark "mark"))

(bind-key "M-y" #'hydra-yank-pop/yank-pop)
(bind-key "C-y" #'hydra-yank-pop/yank)

(provide 'setup-hydras)
