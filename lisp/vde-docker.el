(use-package dockerfile-mode            ; Edit docker's Dockerfiles
  :ensure t
  :mode ("Dockerfile\\'" . dockerfile-mode))

(use-package docker-compose-mode        ; Edit docker-compose files
  :ensure t
  :mode ("docker-compose.yml\\'". docker-compose-mode))

(provide 'vde-docker)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
