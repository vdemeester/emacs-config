;; getelget - el-get boostrap script
;;
;; Checks to see if el-get has been checked out, and bootstraps it if
;; it has not. After bootstrapping, calls el-get to load specified
;; packages.
;;
;; el-get-packages should be defined before including this file. Any
;; definitions from el-get-sources will be appended to el-get-packages.
;;
;; Written in 2011 by Nathan R. Yergler <nathan@yergler.net>
;;
;; To the extent possible under law, the person who associated CC0 with
;; getelget has waived all copyright and related or neighboring rights
;; to getelget.
;;
;; You should have received a copy of the CC0 legalcode along with this
;; work.  If not, see <http://creativecommons.org/publicdomain/zero/1.0/>.

;; add a hook listener for post-install el-get
(defun post-install-hook (pkg)
  ;; after installing el-get, load the local package list
  (if (string-equal pkg "el-get")
      (progn
		;; update recipes from emaccswiki
		(el-get-emacswiki-refresh nil t)
		(el-get-sync))))
(add-hook 'el-get-post-install-hooks 'post-install-hook)

;; add the el-get directory to the load path
(add-to-list 'load-path
             (concat (file-name-as-directory user-emacs-directory)
                     (file-name-as-directory "el-get")
                     "el-get"))

;; install all user's packages
(defun el-get-sync ()
  "Install all user's packages from `el-get-packages' variable."
  (interactive)
  (el-get 'sync el-get-packages))

;; try to require el-get
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let (el-get-master-branch)
      (goto-char (point-max))
      (eval-print-last-sexp))))
