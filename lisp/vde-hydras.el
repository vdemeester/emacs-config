(defhydra hydra-projects (:color blue)
  "Open projects"
  ("d" hydra-docker-projects/body "docker")
  ("e" (find-file "~/.emacs.d/init.el") "emacs")
  ("m" hydra-moby-projects/body "moby")
  ("h" (find-file "~/.config/nixpkgs/README.md") "home-manager")
  ("s" (find-file "/sudo::/etc/nixos/README.md") "nixos"))

(bind-key "C-c SPC" #'hydra-projects/body)

(defhydra hydra-docker-projects (:color blue)
  "Docker projects"
  ("a" (find-file "~/src/github.com/docker/app/README.md") "docker/app")
  ("c" (find-file "~/src/github.com/docker/cli/README.md") "docker/cli")
  ("d" (find-file "~/src/github.com/docker/docker/README.md") "docker/docker"))

(defhydra hydra-moby-projects (:color blue)
  "Moby projects"
  ("b" (find-file "~/src/github.com/moby/buildkit/README.md") "buildkit")
  ("c" (find-file "~/src/github.com/containerd/containerd/README.md") "containerd"))

(defhydra hydra-goto-line (goto-map ""
                                    :pre (linum-mode 1)
                                    :post (linum-mode -1))
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


(defhydra hydra-toggle (:color pink :hint nil)
  "
_a_ abbrev-mode:       %`abbrev-mode
_d_ debug-on-error:    %`debug-on-error
_f_ auto-fill-mode:    %`auto-fill-function
_h_ highlight          %`highlight-nonselected-windows
_t_ truncate-lines:    %`truncate-lines
_w_ whitespace-mode:   %`whitespace-mode
_l_ org link display:  %`org-descriptive-links
"
  ("a" abbrev-mode)
  ("d" toggle-debug-on-error)
  ("f" auto-fill-mode)
  ("h" (setq highlight-nonselected-windows (not highlight-nonselected-windows)))
  ("t" toggle-truncate-lines)
  ("w" whitespace-mode)
  ("l" org-toggle-link-display)
  ("q" nil "quit"))

(global-set-key (kbd "C-c C-v") 'hydra-toggle/body)


(defhydra hydra-marked-items (dired-mode-map "")
  "
Number of marked items: %(length (dired-get-marked-files))
"
  ("m" dired-mark "mark"))

(bind-key "M-y" #'hydra-yank-pop/yank-pop)
(bind-key "C-y" #'hydra-yank-pop/yank)

(provide 'vde-hydras)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
