(require 'flx-ido)
(require 'ido-vertical-mode)
(require 'recentf)
(setq ido-enable-flex-matching t      ; IDO flex matching
     ido-everywhere t                ; use IDO whenever possible
     ido-use-virtual-buffers t       ; use closed buffers as well
     ido-use-faces nil)              ; Let flx-ido do this job
(ido-mode 1)
(ido-vertical-mode 1)
(flx-ido-mode 1)
