FROM alpine:3.6
WORKDIR /root
# Init setup to allow user to be switched
RUN chmod o+rxw /root
# Add permanent utilities.  Replace busybox less with gnu less for color support
RUN apk add --no-cache git emacs-nox openssh-client tmux bash less
# Copy specific conf files needed
COPY docker-helpers/bashrc /root/.bashrc
COPY .gitconfig            /root/.gitconfig
COPY .tmux.conf            /root/.tmux.conf
COPY .editorconfig         /root/.editorconfig
COPY .emacs.d              /root/.emacs.d
RUN chmod -R go+rw .emacs.d .bashrc .editorconfig .tmux.conf .gitconfig
# Download cask+python, run cask install, then delete cask(mostly)+python
RUN umask 000 && \
	apk add --no-cache python && \
	git clone https://github.com/cask/cask.git && \
	cd .emacs.d && \
	~/cask/bin/cask install && \
	apk del --no-cache python && \
	rm -rf $(find ~/cask/ -mindepth 1 | grep -v cask.el | grep -v cask-bootstrap)

# Default to my personal email address
RUN echo -e "[user]\nemail=brantonhorsley@notnarb.com" > /etc/gitconfig
ENV TERM xterm-256color
ENTRYPOINT /bin/bash
