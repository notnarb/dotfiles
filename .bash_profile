# .bash_profile

# Get the aliases and functions
if [ -f ~/.bashrc ]; then
	. ~/.bashrc
fi

# # User specific environment and startup programs

# PATH=$PATH:$HOME/bin:/sbin:/usr/sbin/

if [ -d "$HOME/bin" ] ; then
	PATH=$PATH:$HOME/bin
fi

# safe rm
alias delete='mv -t /tmp/.Trash/'

fixssh() {
  for key in SSH_AUTH_SOCK SSH_CONNECTION SSH_CLIENT; do
    if (tmux show-environment | grep "^${key}" > /dev/null); then
      value=`tmux show-environment | grep "^${key}" | sed -e "s/^[A-Z_]*=//"`
      export ${key}="${value}"
    fi
  done
}

#export PATH
