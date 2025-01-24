# Add custom bin locations
export PATH="/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/Users/ah/bin:/home/ah/bin:/usr/local/sbin"

eval "$(/opt/homebrew/bin/brew shellenv)"

# Add kitty to the path
export PATH="$PATH:/Applications/kitty.app/Contents/MacOS"

# Source ~/.env if it exists
if [ -f ~/.env ]; then
  . ~/.env
fi

# Go
export PATH="$PATH:/usr/local/go/bin"
export GOPATH=$(go env GOPATH)
export PATH="$PATH:$GOPATH/bin"

# Pip bins
export PATH="$HOME/.local/bin:$PATH"

# Docker for mac
export DOCKER_HOST="unix:///var/run/docker.sock"
export DOCKER_BUILDKIT=1

# gpg config
export GPG_TTY=$(tty)

# nvm
# export NVM_DIR="$HOME/.nvm"
# [ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
# [ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
export PATH="/opt/homebrew/Cellar/node@22/22.13.1/bin:$PATH"

# chruby
if [[ -f /usr/local/share/chruby/chruby.sh ]] then
    source /usr/local/share/chruby/chruby.sh
    source /usr/local/share/chruby/auto.sh
    chruby ruby-2.7.4
fi

# rbenv
export PATH="/Users/ah/.rbenv/shims:$PATH"

# cargo
export PATH="$HOME/.cargo/bin:$PATH"

# fzf
export FZF_DEFAULT_COMMAND='rg --files --follow --hidden --line-buffered --smart-case --color=always'
#export FZF_DEFAULT_OPTS='--reverse --prompt="❯ "'

# Prefer brew's openssl
export PATH="/opt/homebrew/opt/openssl@3/bin:$PATH"

# Rust bins
export PATH="$HOME/.cargo/bin:$PATH"

# set rg config file
export RIPGREP_CONFIG_PATH=~/.rgconfig

export EDITOR=nvim

eval "$(pyenv init -)"
. "$HOME/.cargo/env"

# eval "$(github-copilot-cli alias -- "$0")"

unset PREFIX

export CDPATH=$HOME:$HOME/code
