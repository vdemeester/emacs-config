;;; shortbrain-theme.el --- Shortbrain theme

;; Copyright (C) 2013-2016 Marian Schubert
;; Copyright (C) 2018 Gergely Nagy
;; Copyright (C) 2018 Vincent Demeester

;; Author: Vincent Demeester
;; Version: 0.1
;; URL: https://git.madhouse-project.org/algernon/brutalist-theme.el
;; FIXME(vdemeester): change the URL

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Low distraction, minimalistic color theme, with minimal colors, preferring
;; other styles of markup (italic, bold).
;; TODO(vdemeester) handle guide-key faces

;;; Credits:

;; Based on: 
;; https://git.madhouse-project.org/algernon/brutalist-theme.el

;; Which in turn was inspired by:
;;
;; http://github.com/maio/eink-emacs
;; https://bitbucket.org/kisom/eink.vim
;; https://github.com/dmand/eink.el
;; http://www.daveliepmann.stfi.re/tufte-css/?sf=wklwy

;;; Code:

(deftheme shortbrain
  "Minimal, low-color, low-distraction theme.")

(let ((fg "#111111")
      (fg-table "#222291")
      (fg-dim "dim gray")
      (fg-slight-dim "grey70")
      (bg "#fffff8")
      (bg-light "#ddddd8")
      (fg-light "#ddddd8")
      (bg-highlight "#FFF1AA")
      (bg-highlight-2 "LightCyan")
      (bg-highlight-3 "LightGreen")
      (bg-highlight-dim "#eeeee8")

      (diff-added "#e9ffe9")
      (diff-added-highlight "#a4f4a3")
      (diff-removed "#ffecec")
      (diff-removed-highlight "#f9cbca")

      (powerline1 "grey22")
      (powerline2 "grey40")

      (string "blue")
      (cursor "white smoke")

      (paren-match "blue")
      (paren-mismatch "red"))

  (custom-theme-set-faces
   'shortbrain

   ;; generic stuff
   `(default ((t (:background ,bg :foreground ,fg))))
   `(button ((t (:foreground ,fg :underline t))))
   `(cursor ((t (:background ,fg :foreground ,cursor))))
   `(custom-variable-tag ((t (:foreground ,fg :weight bold))))
   `(default-italic ((t (:italic t))))
   `(font-latex-bold-face ((t (:foreground ,fg))))
   `(font-latex-italic-face ((t (:foreground ,fg :slant italic))))
   `(font-latex-match-reference-keywords ((t (:foreground ,fg))))
   `(font-latex-match-variable-keywords ((t (:foreground ,fg))))
   `(font-latex-string-face ((t (:foreground ,string))))
   `(font-lock-builtin-face ((t (:foreground ,fg))))
   `(font-lock-comment-face ((t (:foreground ,fg-dim))))
   `(font-lock-comment-delimiter-face ((t (:inherit font-lock-comment-face))))
   `(font-lock-constant-face ((t (:foreground ,fg))))
   `(font-lock-doc-face ((t (:foreground ,fg-dim :slant italic))))
   `(font-lock-function-name-face ((t (:foreground ,fg))))
   `(font-lock-keyword-face ((t (:foreground ,fg :weight bold))))
   `(font-lock-preprocessor-face ((t (:foreground ,fg :slant italic))))
   `(font-lock-reference-face ((t (:foreground ,fg))))
   `(font-lock-string-face ((t (:foreground ,string))))
   `(font-lock-type-face ((t (:foreground ,fg :underline t))))
   `(font-lock-variable-name-face ((t (:foreground ,fg :underline nil))))
   `(font-lock-warning-face ((t (:foreground ,fg :weight bold))))
   `(fringe ((t (:background ,bg :foreground ,fg))))
   `(highlight ((t nil)))
   `(lazy-highlight ((t (:background ,bg-highlight-3 :slant italic))))
   `(hl-line ((t (:inverse-video t))))
   `(ido-first-match ((t (:foreground ,fg))))
   `(ido-only-match ((t (:foreground ,fg))))
   `(ido-subdir ((t (:foreground ,fg))))
   `(isearch ((t (:background ,bg-highlight-dim :foreground ,fg))))
   `(link ((t (:foreground ,string :underline t))))
   `(minibuffer-prompt ((t (:foreground ,fg :weight bold))))
   `(mode-line ((t (:background ,bg-light :foreground ,fg :height 0.8))))
   `(mode-line-buffer ((t (:foreground ,fg :weight bold))))
   `(mode-line-inactive ((t (:background ,bg-light :foreground ,bg-light :height 0.8))))
   `(mode-line-minor-mode ((t (:weight ultra-light))))
   `(modeline ((t (:background ,bg :foreground ,fg :height 0.8))))
   `(region ((t (:background ,bg-highlight :foreground ,fg :inverse-video nil))))
   `(slime-repl-inputed-output-face ((t (:foreground ,fg))))
   `(whitespace-line ((t (:background ,bg-highlight-2 :foreground ,fg))))

   ;; org
   `(org-agenda-date ((t (:foreground ,fg :height 1.2))))
   `(org-agenda-date-today ((t (:foreground ,fg :weight bold :height 1.4))))
   `(org-agenda-date-weekend ((t (:foreground ,fg :weight normal))))
   `(org-agenda-structure ((t (:foreground ,fg :weight bold))))
   `(org-block ((t (:foreground ,fg))))
   `(org-block-begin-line ((t (:foreground ,fg-light :height 0.8))))
   `(org-block-end-line ((t (:foreground ,fg-light :height 0.8))))
   `(org-verbatim ((t (:foreground ,fg :weight semi-bold))))
   `(org-date ((t (:foreground ,fg) :underline)))
   `(org-done ((t (:foreground ,fg-light))))
   `(org-hide ((t (:foreground ,bg))))
   ;; use :overline to give headings more top margin
   `(org-level-1 ((t (:foreground ,fg :weight semi-bold :height 1.3))))
   `(org-level-2 ((t (:foreground ,fg :weight semi-bold :height 1.1 :overline ,bg))))
   `(org-level-3 ((t (:foreground ,fg :weight semi-bold :height 1.1 :overline ,bg))))
   `(org-level-4 ((t (:foreground ,fg :weight semi-bold :height 1.1 :overline ,bg))))
   `(org-level-5 ((t (:foreground ,fg :weight semi-bold :height 1.1 :overline ,bg))))
   `(org-level-6 ((t (:foreground ,fg :weight semi-bold :height 1.1 :overline ,bg))))
   `(org-link ((t (:foreground ,fg :underline t))))
   `(org-quote ((t (:foreground ,fg :slant italic :inherit org-block))))
   `(org-scheduled ((t (:foreground ,fg))))
   `(org-sexp-date ((t (:foreground ,fg))))
   `(org-special-keyword ((t (:foreground ,fg))))
   `(org-todo ((t (:foreground ,fg))))
   `(org-verse ((t (:inherit org-block :slant italic))))
   `(org-table ((t (:foreground ,fg-table))))

   ;; powerline
   `(powerline-active1 ((t (:background ,powerline1 :foreground ,bg :inherit mode-line))))
   `(powerline-active2 ((t (:background ,powerline2 :foreground ,bg :inherit mode-line))))

   ;; message-mode, notmuch and gnus
   `(gnus-header-content ((t (:foreground ,fg))))
   `(gnus-header-from ((t (:foreground ,fg))))
   `(gnus-header-name ((t (:foreground ,fg))))
   `(gnus-header-subject ((t (:foreground ,fg))))
   `(message-header-name ((t (:foreground ,fg :weight semi-bold))))
   `(message-header-other ((t (:foreground ,fg))))
   `(message-header-to ((t (:inherit message-header-other))))
   `(message-header-cc ((t (:inherit message-header-other))))
   `(message-header-subject ((t (:inherit message-header-other :foreground ,fg-table))))
   `(message-mml ((t (:box (:line-width 1 :color ,fg-slight-dim)))))
   `(message-cited-text ((t (:foreground ,fg-dim))))
   `(notmuch-message-summary-face ((t (:foreground ,fg))))
   `(notmuch-tree-match-author-face ((t (:foreground ,fg))))
   `(notmuch-tree-match-tag-face ((t (:foreground ,fg :weight normal))))
   `(notmuch-wash-toggle-button ((t (:inherit message-mml))))
   `(notmuch-tag-flagged ((t (:inherit default))))

   ;; magit
   `(magit-header ((t (:weight semi-bold))))
   `(magit-item-mark ((t (:background ,bg-highlight))))
   `(magit-item-highlight ((t (:weight normal))))
   `(magit-diff-context-highlight ((t (:foreground ,fg))))
   `(magit-branch-local ((t (:inherit magit-head))))
   `(magit-branch-remote ((t (:inherit magit-head))))
   `(magit-branch-current ((t (:inherit magit-head))))
   `(magit-branch-remote-head ((t (:inherit magit-head))))
   `(magit-tag ((t (:inherit magit-branch-local :underline nil))))
   `(magit-hash ((t (:inherit default :weight semi-bold))))
   `(magit-head ((t (:foreground ,fg-table :underline t :weight bold))))
   `(magit-log-author ((t (:foreground ,fg-dim))))
   `(magit-section-heading ((t (:weight semi-bold
					:underline t
					:height 1.2))))
   `(magit-section-highlight ((t (:fg ,fg))))

   `(git-commit-comment-branch ((t (:inherit magit-head))))
   `(git-commit-known-pseudo-header ((t (:inherit font-lock-keyword-face
                                                  :box (:line-width 1 :color ,fg-slight-dim)))))
   `(git-commit-summary ((t (:inherit font-lock-type-face :underline nil :weight bold))))

   ;; diff
   `(diff-added ((t (:background ,diff-added))))
   `(diff-removed ((t (:background ,diff-removed))))
   `(diff-refine-added ((t (:background ,diff-added-highlight))))
   `(diff-refine-removed ((t (:background ,diff-removed-highlight))))
   `(magit-diff-added-highlight ((t (:weight demibold :background ,diff-added))))
   `(magit-diff-added ((t (:background ,diff-added))))
   `(magit-diff-removed-highlight ((t (:weight demibold :background ,diff-removed))))
   `(magit-diff-removed ((t (:background ,diff-removed))))

   ;; git-timemachine
   `(git-timemachine-minibuffer-author-face ((t (:inherit default))))
   `(git-timemachine-minibuffer-detail-face ((t (:weight bold))))

   ;; compile
   `(compilation-error ((t (:inherit error))))

   ;; flycheck
   `(flycheck-error ((t (:inherit error))))
   `(flycheck-warning ((t (:inherit warning))))

   ;; dired
   `(dired-directory ((t (:weight bold))))
   `(dired-subtree-depth-1-face ((t (:inherit default))))
   `(dired-subtree-depth-2-face ((t (:inherit default))))
   `(dired-subtree-depth-3-face ((t (:inherit default))))
   `(dired-subtree-depth-4-face ((t (:inherit default))))

   ;; helm
   `(helm-source-header ((t (:foreground ,fg :background ,bg-light :weight bold))))
   `(helm-header ((t (:foreground ,fg))))
   `(helm-selection-line ((t (:inherit region :weight bold))))
   `(helm-selection ((t (:inherit hl-line))))
   `(helm-ff-directory ((t (:foreground ,fg :weight bold))))
   `(helm-ff-dotted-directory ((t (:foreground ,fg :weight bold))))
   `(helm-ff-symlink ((t (:foreground ,fg :slant italic))))
   `(helm-ff-executable ((t (:foreground ,fg))))

   ;; iedit
   `(iedit-occurrence ((t (:background ,bg-highlight-3 :foreground ,fg))))

   ;; company
   `(company-echo-common ((t (:foreground ,fg))))
   `(company-tooltip-selection ((t (:background ,bg-highlight))))

   ;; parens - parenface
   '(parenface-paren-face ((t (:foreground ,fg-slight-dim))))
   '(parenface-curly-face ((t (:foreground ,fg-slight-dim))))
   '(parenface-bracket-face ((t (:foreground ,fg-slight-dim))))

   ;; parens - paren-face
   '(parenthesis ((t (:foreground ,fg-slight-dim))))

   ;; parens - other
   `(sp-show-pair-match-face ((t (:foreground ,paren-match :weight bold))))
   `(sp-show-pair-mismatch-face ((t (:background ,paren-mismatch :foreground ,paren-match :weight bold))))
   `(show-paren-match ((t (:foreground ,paren-match :weight bold))))
   `(show-paren-mismatch ((t (:background ,paren-mismatch :foreground ,paren-match :weight bold))))

   ;; sh
   `(sh-heredoc ((t (:foreground ,fg :slant italic))))
   `(sh-quoted-exec ((t (:inherit font-lock-string-face :weight semi-bold))))

   ;; js2
   `(js2-function-param ((t (:foreground ,fg))))
   `(js2-external-variable ((t (:foreground ,fg))))
   `(js2-jsdoc-type ((t (:inherit font-lock-type-face))))
   `(js2-jsdoc-value ((t (:inherit font-lock-variable-name-face))))
   `(js2-jsdoc-html-tag-name ((t (:inherit web-mode-html-tag-face))))

   ;; perl
   `(cperl-hash-face ((t (:foreground ,fg))))
   `(cperl-array-face ((t (:foreground ,fg))))
   `(cperl-nonoverridable-face ((t (:foreground ,fg))))

   ;; linum / nlinum-relative
   `(nlinum-relative-current-face ((t (:inherit normal :weight bold))))
   `(linum ((t (:inherit normal :weight bold))))

   ;; web-mode
   `(web-mode-current-element-highlight-face ((t (:inherit normal :weight bold :foreground ,fg))))

   ;; markdown-mode
   `(markdown-blockquote-face ((t (:foreground ,fg-table :slant normal))))
   `(markdown-inline-code-face ((t (:slant italic))))
   `(markdown-pre-face ((t (:inherit markdown-inline-code-face))))
   `(markdown-reference-face ((t (:inherit markdown-link-face))))

   ;; misc
   `(idle-highlight ((t (:background ,bg-highlight))))
   `(yas-field-highlight-face ((t (:background ,bg-highlight-dim :foreground ,fg))))
   `(eshell-prompt ((t (:foreground ,fg :weight bold))))
   `(cider-result-overlay-face ((t (:weight bold))))

   ;; evil-quickscope
   `(evil-quickscope-first-face ((t (:foreground ,fg :background ,bg-highlight-dim))))
   `(evil-quickscope-second-face ((t (:foreground ,fg :background ,bg-highlight-3))))

   ;; evil-snipe
   `(evil-snipe-first-match-face ((t (:foreground ,fg :background ,bg-highlight-dim))))
   `(evil-snipe-matches-face ((t (:foreground ,fg :background ,bg-highlight-3))))

   ;; evil-goggles
   `(evil-goggles-delete-face ((t (:inherit diff-removed))))
   `(evil-goggles-paste-face ((t (:inherit diff-added))))
   `(evil-goggles-undo-redo-add-face ((t (:inherit diff-added))))
   `(evil-goggles-undo-redo-change-face ((t (:inherit diff-changed))))
   `(evil-goggles-undo-redo-remove-face ((t (:inherit diff-removed))))
   `(evil-goggles-yank-face ((t (:inherit diff-changed))))

   ;; evil
   `(evil-ex-lazy-highlight ((t (:background ,bg-highlight-2))))
   `(evil-ex-substitute-matches ((t (:background ,diff-removed-highlight))))
   `(evil-ex-substitute-replacement ((t (:background ,diff-added-highlight :underline nil :foreground ,fg))))))

;;;###autoload
(when load-file-name
  (add-to-list
   'custom-theme-load-path
   (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'shortbrain)
(provide 'shortbrain-theme)

;;; brutalist-theme.el ends here
