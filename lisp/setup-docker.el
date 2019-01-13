;;; setup-docker.el --- setup docker related mode(s)
;;; Commentary:
;;; Code:
;;; -*- lexical-binding: t; -*-

(use-package dockerfile-mode            ; Edit docker's Dockerfiles
  :mode ("Dockerfile\\'" . dockerfile-mode))

(provide 'setup-docker)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
