#!/usr/bin/env bash
docker build -t vdemeester/emacs .
docker run -it --rm \
       -v /tmp/.X11-unix:/tmp/.X11-unix \
       -v $HOME/.Xauthority:/home/emacs/.Xauthority \
       -e DISPLAY \
       $@ \
       vdemeester/emacs emacs
