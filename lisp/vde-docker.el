;;; -*- lexical-binding: t; -*-
(use-package dockerfile-mode            ; Edit docker's Dockerfiles
  :mode ("Dockerfile\\'" . dockerfile-mode))

(use-package docker-compose-mode        ; Edit docker-compose files
  :mode ("docker-compose.yml\\'". docker-compose-mode))

(use-package kubernetes
  :commands (kubernetes-overview))

(provide 'vde-docker)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
