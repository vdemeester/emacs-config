#!/usr/bin/env bash
set -e
docker build -t vdemeester/emacs .
docker run -it --rm \
       --user $(id -u) \
       -v /tmp/.X11-unix:/tmp/.X11-unix \
       -v $HOME/.Xauthority:/home/emacs/.Xauthority \
       -v $PWD/.emacs.d/elpa:/home/emacs/.emacs.d/elpa \
       -e DISPLAY \
       $@ \
       vdemeester/emacs emacs
