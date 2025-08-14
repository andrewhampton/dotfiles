# Add custom bin locations
export PATH="/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/Users/andrewhampton/bin:/home/andrewhampton/bin:/usr/local/sbin"

if [[ -x "$(command -v /opt/homebrew/bin/brew)" ]]; then
  eval "$(/opt/homebrew/bin/brew shellenv)"
fi

# Add kitty to the path
export PATH="$PATH:/Applications/kitty.app/Contents/MacOS"

# Add lm studio binary to the path
export PATH="$PATH:/Users/andrewhampton/.lmstudio/bin"

# Source brew node
export PATH="/opt/homebrew/opt/node@22/bin:$PATH"

# Source ~/.env if it exists
if [ -f ~/.env ]; then
  . ~/.env
fi

# Go
export PATH="$PATH:/usr/local/go/bin"
if command -v go &> /dev/null; then
  export GOPATH=$(go env GOPATH)
fi
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
export PATH="/Users/andrewhampton/.rbenv/shims:$PATH"

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

if command -v pyenv &> /dev/null; then
  eval "$(pyenv init -)"
fi
if [ -f "$HOME/.cargo/env" ]; then
  . "$HOME/.cargo/env"
fi

# eval "$(github-copilot-cli alias -- "$0")"

unset PREFIX

export CDPATH=$HOME:$HOME/code
