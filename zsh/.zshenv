# Add custom bin locations
export PATH="/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/Users/andrew/bin:/home/ah/bin"

# Source ~/.env if it exists
if [ -f ~/.env ]; then
  . ~/.env
fi

# Go
export PATH="$PATH:/usr/local/go/bin"
export GOPATH=$(go env GOPATH)
export PATH="$PATH:$GOPATH/bin"

# Pollev
export PATH="$PATH:$HOME/.pollev/bin"

# Pip bins
export PATH="$PATH:$HOME/.local/bin"

# Docker for mac
export DOCKER_HOST="unix:///var/run/docker.sock"

# nvm
export NVM_DIR="$HOME/.nvm"

# gpg config
export GPG_TTY=$(tty)

# nvm
[ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"  # This loads nvm

# yarn bins
export PATH="$HOME/.yarn/bin:$PATH"

# Ruby
if [[ -f /usr/local/share/chruby/chruby.sh ]] then
    source /usr/local/share/chruby/chruby.sh
    source /usr/local/share/chruby/auto.sh
    chruby ruby-2.4.3
fi
