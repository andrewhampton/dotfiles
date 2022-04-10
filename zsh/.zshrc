[[ $TERM == "dumb" ]] && unsetopt zle && PS1='$ ' && return

# Source ~/.zshenv if it exists
[ -f ~/.zshenv ] && source ~/.zshenv

# Path to your oh-my-zsh installation.
export ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME=""

plugins=(git)

source $ZSH/oh-my-zsh.sh

# User configuration

# aliases
alias fs="foreman start| grep web.1"
alias om="overmind"

alias br="bin/rails"
alias brc="bin/rails console"
alias ber="bin/rspec"
alias rs="bin/rspec"
alias nfrs="bin/rspec --next-failure"
alias ofrs="bin/rspec --only-failure"
alias ffrs="bin/rspec --fail-fast"

alias dc="docker compose"

alias ag="ag --hidden --path-to-ignore ~/.agignore"

# Better du
alias ncdu="ncdu --color dark -rr -x --exclude .git --exclude node_modules"

# k8s
alias k="kubectl"
alias kns="kubens"
alias kctx="kubectx"

# Colorized man pages
# http://boredzo.org/blog/archives/2016-08-15/colorized-man-pages-understood-and-customized
man() {
  env \
  LESS_TERMCAP_mb=$(printf "\e[1;31m") \
  LESS_TERMCAP_md=$(printf "\e[1;31m") \
  LESS_TERMCAP_me=$(printf "\e[0m") \
  LESS_TERMCAP_se=$(printf "\e[0m") \
  LESS_TERMCAP_so=$(printf "\e[1;44;33m") \
  LESS_TERMCAP_ue=$(printf "\e[0m") \
  LESS_TERMCAP_us=$(printf "\e[1;32m") \
  man "$@"
}

[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

# zsh completions
autoload -Uz compinit && compinit -i

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

# Source gh completions if they exist
[ -f /usr/local/share/zsh/site-functions/_gh ] && source /usr/local/share/zsh/site-functions/_gh

# Bootstrap rbenv if it exists
(( $+commands[rbenv] )) && eval "$(rbenv init - zsh)"

# git helpers
alias coauth='printf "Co-authored-by: %s" "$(git log --pretty=format:"%an <%ae>" -1000 | sort | uniq | fzf)" | pbcopy'
alias reviewer='gh api --paginate repos/:owner/:repo/collaborators | jq ".[].login" | tr -d \"| fzf'
alias branch='git branch --show-current |tr -d \"'

# Check if main exists and use instead of master
function git_main_branch() {
  command git rev-parse --git-dir &>/dev/null || return
  local ref
  for ref in refs/{heads,remotes/{origin,upstream}}/{main,trunk,dev}; do
    if command git show-ref -q --verify $ref; then
      echo ${ref:t}
      return
    fi
  done
  echo master
}
