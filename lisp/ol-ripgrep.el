;;; ol-ripgrep.el --- Links to Ripgrep -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Vincent Demeester

;; Author: Vincent Demeester <vincent@sbr.pm>
;; Keywords: org link ripgrep
;;
;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3.0, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:

;; This file implements links to Ripgrep from within Org mode.
;; rg:orgmode         : run ripgrep on current working dir with orgmode expression
;; rg:orgmode:config/ : run ripgrep on config/ dir with orgmode expression

;;; Code:

(require 'ol)

;; Install the link type
(org-link-set-parameters "rg"
                         :follow #'org-ripgrep-follow-link
                         :face '(:foreground "DarkGreen" :underline t))

(defun org-ripgrep-follow-link (issue)
  "Run `ripgrep-regexp` with REXEP and FOLDER as argument,
like this : [[pt:REGEXP:FOLDER]]"
  (setq expressions (split-string regexp ":"))
  (setq exp (nth 0 expressions))
  (if (= (length expressions) 1)
      (progn
        (ripgrep-regexp exp (expand-file-name "./")))
    (progn
      (setq folder (nth 1 expressions))
      (ripgrep-regexp exp (file-name-as-directory (expand-file-name folder))))))

(provide 'ol-ripgrep)
;;; ol-ripgrep.el ends here
