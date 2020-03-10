;;; -*- lexical-binding: t; -*-
(use-package dockerfile-mode            ; Edit docker's Dockerfiles
  :unless *sys/full*
  :mode ("Dockerfile\\'" . dockerfile-mode))

;; I have a bunch of different 'profiles' for kubernetes by different cluster so
;; i don't mess between things
;; This allow me to set the KUBECONFIG variable between those easily
;; TODO: add the current profile in modeline
(defun my-switch-kubeconfig-env (&optional kubeconfig)
  "Set KUBECONFIG environment variable for the current session"
  (interactive
   (list
    (completing-read
     "Kubeconfig: "
     (mapcar
      (lambda (x)
        (replace-regexp-in-string
         "^config\." ""
         (file-name-nondirectory(directory-file-name x))))
      (directory-files-recursively
       (expand-file-name "~/.kube") "^config\.")) nil t )))
  (setq kubeconfig (expand-file-name (format "~/.kube/config.%s" kubeconfig)))
  (if (file-exists-p kubeconfig)
      (setenv "KUBECONFIG" kubeconfig)
    (error "Cannot find kubeconfig: %s" kubeconfig)))

(provide 'setup-docker)
