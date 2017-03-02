FROM alpine
RUN addgroup emacs && adduser -D -G emacs emacs && \
    apk update && apk add emacs-x11 git xterm \
    ttf-ubuntu-font-family ttf-droid
COPY . /home/emacs
RUN mkdir -p /home/emacs/desktop/org/todos
WORKDIR /home/emacs
RUN chown -R emacs:emacs .
USER emacs
