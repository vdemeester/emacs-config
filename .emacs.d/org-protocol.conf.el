;; Setup org-protocol with XDG
(let ((desktop-file (expand-file-name "~/.local/share/applications/emacsclient.desktop")))
  (unless (file-exists-p desktop-file)
    (with-temp-file desktop-file
      (insert "[Desktop Entry]
Name=Emacs Client
Exec=emacsclient %u
Icon=emacs-icon
Type=Application
Terminal=false
MimeType=x-scheme-handler/org-protocol;
"))
    (call-process "update-desktop-database" nil nil nil (file-name-directory desktop-file))
    (call-process "xdg-mime" nil nil nil "default" "emacsclient.desktop" "x-scheme-handler/org-protocol")))

;; Here is the associated bookmarklet:
;; javascript:location.href='org-protocol://store-link://'+encodeURIComponent(location.href)+'/'+encodeURIComponent(document.title)+'/'+encodeURIComponent(window.getSelection())
