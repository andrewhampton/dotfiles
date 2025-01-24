if [[ "$TERM" == "dumb" ]]
then
  unsetopt zle
  unsetopt prompt_cr
  unsetopt prompt_subst
  if whence -w precmd >/dev/null; then
      unfunction precmd
  fi
  if whence -w preexec >/dev/null; then
      unfunction preexec
  fi
  PS1='$ '
  return
fi

HISTSIZE=10000
SAVEHIST=10000

# Source ~/.zshenv if it exists
[ -f ~/.zshenv ] && source ~/.zshenv

# Only check for a zsh update very 30 days
UPDATE_ZSH_DAYS=30

# Path to your oh-my-zsh installation.
export ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME=""

# plugins=(git)

source $ZSH/oh-my-zsh.sh

# User configuration

# aliases
alias fs="foreman start| grep web.1"
alias om="overmind"

alias br="bin/rails"
alias brc="bin/rails console"
alias ber="bin/rspec"
alias rs="NO_COVERAGE=true bin/rspec"
alias nfrs="NO_COVERAGE=true bin/rspec --next-failure"
alias ofrs="NO_COVERAGE=true bin/rspec --only-failure"
alias ffrs="NO_COVERAGE=true bin/rspec --fail-fast"

alias dc="docker compose"
alias d="docker compose run --rm web"

alias dr="d bin/rails"
alias dbr="d bin/rails"
alias dbrc="d bin/rails console"
alias drs="d bin/rspec"
alias nfdrs="d bin/rspec --next-failure"
alias ofdrs="d bin/rspec --only-failure"
alias ffdrs="d bin/rspec --fail-fast"

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

# zsh completions
# autoload -Uz compinit && compinit -i

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

# Source gh completions if they exist
# [ -f /usr/local/share/zsh/site-functions/_gh ] && source /usr/local/share/zsh/site-functions/_gh

# Bootstrap rbenv if it exists
(( $+commands[rbenv] )) && eval "$(rbenv init - zsh)"

# git helpers
alias git_main_branch='git rev-parse --abbrev-ref origin/HEAD | cut -d/ -f2'
alias ghpr='gh pr view -w'
alias reviewer="(gh api --paginate repos/:owner/:repo/collaborators | jq -r \".[].login\"; gh api --paginate /orgs/:owner/teams | jq -r '\"@workramp/\" + .[].slug') | fzf"
alias label='gh api --paginate repos/:owner/:repo/labels | jq ".[].name" | tr -d \"| fzf'
alias branch='git branch --show-current |tr -d \"'
alias gmru="git for-each-ref --sort=-committerdate --count=50 refs/heads/ --format='%(HEAD) %(refname:short) | %(committerdate:relative) | %(contents:subject)'| fzf | sed -e 's/^[^[[:alnum:]]]*[[:space:]]*//' | cut -d' ' -f1| xargs -I _ git checkout _"
alias gfx='git commit --fixup $(git log $(git_main_branch)..HEAD --oneline| fzf| cut -d" " -f1)'
alias grbi="git rebase -i --autosquash"
alias grbm='git rebase --autosquash $(git_main_branch)'
alias gpa='gpf origin $(git log $(git_main_branch)..HEAD --graph --abbrev-commit --decorate --date=relative --format=format:"%D" | rev | cut -d" " -f1| rev| awk NF|fzf --select-1 --multi| grep -v origin| tr "\n" " ")'
alias gd='git diff --color-moved --color-moved-ws=allow-indentation-change'
alias gdd='GIT_EXTERNAL_DIFF=difft git diff'
alias gab='git absorb'
alias gabr'git absorb --and-rebase'
alias gco='git checkout'
alias gst='git status'
alias gl='git pull'
alias gcm='git checkout $(git_main_branch)'
alias gpf='git push --force-with-lease'

alias todo='jira issue list -a$(jira me) -sopen'
alias next='jira sprint list --current -ax -sOpen --order-by rank --reverse'
alias jira_ticket="git rev-parse --abbrev-ref HEAD | grep -oE '^[A-Z0-9]+-[0-9]+'"
alias jo='open "https://workramp.atlassian.net/browse/$(jira_ticket)"'
alias qai='jira issue edit $(jira_ticket) --no-input -l QAIgnored'

function gspin() {
  if [ $# -ne 1 ]; then
    echo "Usage: gspin <branch_name>"
    return 1
  fi
  spinoff_from_branch=$(git branch --show-current)
  spinoff_from_reset_sha=$(git rev-parse origin/$spinoff_from_branch)
  git checkout -b $1
  git update-ref -m "gspin: moving $spinoff_from_branch to $spinoff_from_reset_sha" refs/heads/$spinoff_from_branch $spinoff_from_reset_sha
}

function gpop() {
  git stash list | fzf --preview 'git stash show --color -p $(cut -d: -f1 <<< {})'| cut -d: -f1 | xargs git stash pop
}

function coauth() {
  gh api repos/{owner}/{repo}/collaborators --paginate --jq '.[] | .login + " <" + .html_url + ">"' | fzf --multi | xargs -I _ printf "Co-authored-by: %s\n" "_"| pbcopy
}

# added by Snowflake SnowSQL installer v1.2
export PATH=/Applications/SnowSQL.app/Contents/MacOS:$PATH

# bun completions
# [ -s "/Users/ah/.bun/_bun" ] && source "/Users/ah/.bun/_bun"

# bun
export BUN_INSTALL="$HOME/.bun"
export PATH="$BUN_INSTALL/bin:$PATH"

# export NVM_DIR="$HOME/.nvm"
# [ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm

# pnpm
export PNPM_HOME="/Users/ah/Library/pnpm"
case ":$PATH:" in
  *":$PNPM_HOME:"*) ;;
  *) export PATH="$PNPM_HOME:$PATH" ;;
esac
# pnpm end

[[ -f ~/.secrets ]] && source ~/.secrets

# Added by Windsurf
export PATH="/Users/ah/.codeium/windsurf/bin:$PATH"
