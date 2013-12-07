;; el-get configuration
(setq el-get-is-lazy t)

(setq el-get-sources
      '(;; This should be in contrib/ of org-mode but this is not
        ;; shipped with Emacs. We take exactly the version we need to
        ;; match org-mode in Emacs.
        (:name org-mime
               :description "org html export for text/html MIME emails"
               :type http
               :url "https://raw.github.com/jwiegley/org-mode/release_7.9.3f/contrib/lisp/org-mime.el")

        (:name git-modes
               :description "GNU Emacs modes for various Git-related files"
               :type github
               :pkgname "magit/git-modes")
        (:name magit
               :depends (git-modes))
        (:name znc
               :type github
               :pkgname "sshirokov/ZNC.el"
               :description "ERC and ZNC interface")
        (:name gist
               :depends (gh))
        (:name gh
               :type github
               :pkgname "sigma/gh.el"
               :depends (pcache logito request)
               :description "Github API client libraries"
               :website "http://github.com/sigma/gh.el")
        (:name pcache
               :type github
               :pkgname "sigma/pcache"
               :description "persistent caching for Emacs"
               :website "http://github.com/sigma/pcache")
        (:name logito
               :type github
               :pkgname "sigma/logito"
               :description "logging library for Emacs"
               :website "http://github.com/sigma/logito")
        (:name request
               :description "Easy HTTP request for Emacs Lisp"
               :type github
               :submodule nil
               :pkgname "tkf/emacs-request")

        (:name ido-vertical-mode
               :type github
               :pkgname "rson/ido-vertical-mode.el"
               :description "makes ido-mode display vertically")
        (:name flx
               :description "Fuzzy matching with good sorting in ido"
               :type github
               :pkgname "lewang/flx")
        (:name s
               :description "The long lost Emacs string manipulation library."
               :type github
               :pkgname "magnars/s.el")
        (:name dash
               :description "A modern list api for Emacs. No 'cl required."
               :type github
               :pkgname "magnars/dash.el")
        (:name projectile
               :description "Project navigation and management library for Emacs"
               :type github
               :pkgname "bbatsov/projectile"
               :depends (dash s)
               :features projectile)))

(el-get nil
        '(
         ;; General
         naquadah-theme                ; Theme from Julien Danjou
          boxquote ; draw boxes
          multiple-cursors ; multiple cursors
          ace-jump-mode ; fast cursor movement
          auto-complete ; universal autocompletion
          auto-complete-css
          flx ; fuzzy matching for ido
          ido-vertical-mode ; vertical mode for ido
          smex ; IDO for M-x
          projectile ; handling of projects
          expand-region ; smartly expand region
         ;; Programming
         autopair                        ; Auto pairing of parentheses
          highlight-parentheses ; Highlight parentheses surrounding the cursor
         ;; rainbow-mode                        ; Display colors
         dtrt-indent                        ; Autodetect indentation
         magit                                ; Git stuff, must-have!
         ;; Modes
         ;; auctex                        ; LaTeX mode
         lua-mode                        ; Major mode for lua
         markdown-mode                        ; Major mode for markdown
          yaml-mode ; Major mode for YAML
          go-mode ; Major mode for Go
          git-commit-mode ; Mode for "git commit"
          zencoding-mode ; Mode to expand CSS tags to HTML
         ;; gnus and other stuff
         ;;bbdb                                ; Big brother database
         ;;nognus                        ; Gnus
         ;;gnus-identities                ; Manipulate Gnus identities
          ;; org stuff
          org-mime
          ;; Misc
          znc ; znc
          gist ; gist integration
         ))

