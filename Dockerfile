FROM alpine:3.6
WORKDIR /root
# TODO: a copy of the relevent dotfiles should probably be made rather than
# relying on a git clone, but ignoring that for now to get a build going
RUN umask 000 && \
	chmod o+rwx /root && \
	apk add --no-cache git emacs-nox python openssh-client tmux bash && \
	git clone https://github.com/notnarb/dotfiles.git && \
	git clone https://github.com/cask/cask.git && \
	ln -s dotfiles/.emacs.d && \
	ln -s dotfiles/.tmux.conf && \
	ln -s dotfiles/.gitconfig && \
	ln -s dotfiles/.editorconfig && \
	cd .emacs.d && \
	~/cask/bin/cask install && \
	apk del --no-cache python && \
	rm -rf $(find ~/cask/ -mindepth 1 | grep -v cask.el | grep -v cask-bootstrap)
COPY docker-helpers/.bashrc /root/.bashrc
ENV TERM xterm-256color
CMD /bin/bash
