;;; setup-navigating.el --- setup navigating related modes
;;; Commentary:
;;; Code:
;;; -*- lexical-binding: t; -*-

(use-package avy                   ; Jump to characters in buffers
  :bind (("C-c j"   . avy-goto-word-1)
         ("C-c n b" . avy-pop-mark)
         ("C-c n j" . avy-goto-char-2)
         ("C-c n t" . avy-goto-char-timer)
         ("C-c n w" . avy-goto-word-1)) 
  :chords (("jj" . avy-goto-char-timer)
           ("jk" . avy-goto-word-or-subword-1)
           ("jl" . avy-goto-line)))

(use-package bm
  :bind (("C-c b b" . bm-toggle)
         ("C-c b n" . bm-next)
         ("C-c b p" . bm-previous))
  :commands (bm-repository-load
             bm-buffer-save
             bm-buffer-save-all
             bm-buffer-restore)
  :init
  (add-hook 'after-init-hook 'bm-repository-load)
  (add-hook 'find-file-hooks 'bm-buffer-restore)
  (add-hook 'after-revert-hook #'bm-buffer-restore)
  (add-hook 'kill-buffer-hook #'bm-buffer-save)
  (add-hook 'after-save-hook #'bm-buffer-save)
  (add-hook 'vc-before-checkin-hook #'bm-buffer-save)
  (add-hook 'kill-emacs-hook #'(lambda nil
                                 (bm-buffer-save-all)
                                 (bm-repository-save))))

(use-package helpful
  :bind (("C-c h F" . helpful-function)
         ("C-c h C" . helpful-command)
         ("C-c h M" . helpful-macro)
         ("C-c h L" . helpful-callable)
         ("C-c h S" . helpful-at-point)
         ("C-c h V" . helpful-variable)))

(use-package smart-jump
  :bind ("M-." . smart-jump-go)
  :config
  (smart-jump-setup-default-registers))

(use-package winner
  :unless noninteractive
  :defer 5
  :config
  (winner-mode 1))

(provide 'setup-navigating)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
