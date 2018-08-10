(defhydra hydra-projects (:color blue)
  "Open projects"
  ("d" hydra-docker-projects/body "docker")
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

(bind-key "M-y" #'hydra-yank-pop/yank-pop)
(bind-key "C-y" #'hydra-yank-pop/yank)

(provide 'vde-hydras)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
