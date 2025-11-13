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
ZSH_THEME="andrewhampton"

# plugins=(git)

source $ZSH/oh-my-zsh.sh

# User configuration

# fzf shell completions
source <(fzf --zsh)

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
alias grbi="git rebase -i"
alias grbm='git rebase $(git_main_branch)'
alias grba='git rebase --abort'
alias grbc='git rebase --continue'
alias gpa='gpf origin $(git log $(git_main_branch)..HEAD --graph --abbrev-commit --decorate --date=relative --format=format:"%D" | rev | cut -d" " -f1| rev| awk NF|fzf --select-1 --multi| grep -v origin| tr "\n" " ")'
alias gd='git diff --color-moved --color-moved-ws=allow-indentation-change'
alias gds='git diff --staged'
alias gdd='GIT_EXTERNAL_DIFF=difft git diff'
alias gab='git absorb'
alias gabr'git absorb --and-rebase'
alias gco='git checkout'
alias gc='git commit'
alias gst='git status'
alias gl='git pull'
alias gcm='git checkout $(git_main_branch)'
alias gp='git push'
alias gpf='git push --force-with-lease'
alias grs='git restore'
alias grst='git restore --stage'
alias ga='git add'
alias gsh='git show'
alias gapa='git add --patch'
alias gcb='git checkout -b'
alias gc!='git commit --amend'
alias gcn!='git commit --amend --no-edit'
alias glg='git lg'
alias gsta='git stash'
alias gstp='git stash pop'
alias g='git'
alias gm='git merge'
alias gcp='git cherry-pick'

alias t='bin/rails test'
alias fix='bin/biome check --write && bin/rubocop -a'
alias merge='git fetch origin && git rebase origin/main && gpf && bin/ci && git switch --ignore-other-worktrees $(git_main_branch) && gm - && gp && echo "merged! 🎉" && gco -'

alias jc='jj commit'
alias jn='jj new'
alias jd='jj describe'
alias jbs='jj bookmark set'
alias jbs!='jj bookmark set --allow-backwards'
alias jbc='jj bookmark create'
alias jlm='jj log -r "mine()"'
alias jgf='jj git fetch'
alias jgp='jj git push'
alias jgp!='jj git push --allow-new'
alias jrbm='jj rebase -d "trunk()"'
alias jres='jj resolve --tool mergiraf'
jmerge() {
  jj git fetch &&
  jj rebase -d "trunk()" || return $?

  local -a bookmarks=(${(f)"$(jj bookmark list -r @ -T 'name ++ "\n"' | grep -v '^push-' | sort -u)"})
  if (( ${#bookmarks} )); then
    jj git push ${bookmarks[@]/#/-b}
  else
    jj git push -c @
  fi &&

  bin/ci &&
  jj bookmark set main -r "latest(heads(::@ & ~empty()))" &&
  jj git push -b main || return $?

  if jj bookmark list | grep -q 'push-'; then
    jj bookmark delete glob:"push-*" &&
    jj git push --deleted
  fi
}
alias jmain='jj log -r ::main' # pronounced "juh-main" like flight of the concords

# Default revset used by all helpers
export JJ_REVSET_CURRENT_BRANCH='trunk()::@ | @::'

# 1) Pick a commit in the revset, preview with jj show, print short commit id
jj_pick_commit() {
  emulate -L zsh
  setopt pipefail
  local revset=${1:-$JJ_REVSET_CURRENT_BRANCH}
  jj log -r "$revset" --no-graph \
    -T 'separate(" ", commit_id.short(), change_id.short(), bookmarks, description.first_line()) ++ "\n"' \
  | fzf --prompt='rev> ' \
        --preview 'jj show -r {1} --color=always' \
  | awk '{print $2}'
}

# 2) Pick one or more bookmarks (branches) in the same revset, print names (NL-separated)
jj_pick_bookmarks() {
  emulate -L zsh
  setopt pipefail
  local revset=${1:-$JJ_REVSET_CURRENT_BRANCH}

  # derive bookmark names from changes in revset, then de-duplicate and sort
  local names
  names=$(jj log -r "($revset) & bookmarks()" -T 'bookmarks ++ "\n"' --no-graph \
          | tr ' ' '\n' | sed '/^$/d' | sort -u) || return 1
  [[ -n $names ]] || { print -u2 "no bookmarks in revset"; return 1 }

  print -r -- "$names" \
  | fzf --multi --prompt='bookmark> ' \
        --preview 'jj show --color=always -r "{}"' \
        --preview-window=right:80%:border
}

# 3) Combine: pick bookmarks, then pick a commit, then move the bookmarks
jbu() {
  emulate -L zsh
  setopt pipefail
  local revset=${1:-$JJ_REVSET_CURRENT_BRANCH}

  local -a bms
  bms=(${(f)"$(jj_pick_bookmarks "$revset")"}) || return 1
  (( ${#bms} )) || { print -u2 "no bookmarks selected"; return 1 }

  local target
  target=$(jj_pick_commit "$revset") || return 1
  [[ -n $target ]] || { print -u2 "no revision selected"; return 1 }

  jj bookmark move -B --to "$target" "${bms[@]}"
}

# Push a selected commit and auto-create branch name
jpa() {
  emulate -L zsh
  setopt pipefail
  local revset=${1:-$JJ_REVSET_CURRENT_BRANCH}

  local target
  target=$(jj_pick_commit "$revset") || return 1
  [[ -n $target ]] || { print -u2 "no revision selected"; return 1 }

  jj git push -c "$target"
}

# Wrapper so `gh` works both in plain Git repos and jj workspaces (no .git dir)
function jh() {
  emulate -L zsh
  setopt pipefail

  local git_dir work_tree is_jj=0
  if git_dir=$(git rev-parse --absolute-git-dir 2>/dev/null) \
     && work_tree=$(git rev-parse --show-toplevel 2>/dev/null); then
    # Check if this is actually a jj-managed repo
    if jj git root &>/dev/null; then
      is_jj=1
    fi
  else
    if ! git_dir=$(jj git root 2>/dev/null); then
      echo "jh: not inside a git or jj repository" >&2
      return 1
    fi
    is_jj=1
    if ! work_tree=$(jj workspace root 2>/dev/null); then
      work_tree=$(dirname "$git_dir")
    fi
  fi

  local head_file="$git_dir/HEAD"
  local head_contents restore_head=0 branch_commit branch_name

  if [[ -f $head_file ]]; then
    head_contents=$(<"$head_file")
    if [[ $head_contents != ref:\ * ]]; then
      # For jj repos, get current commit from jj; otherwise use HEAD file
      if (( is_jj )); then
        branch_commit=$(jj log -r @ --no-graph -T 'commit_id' 2>/dev/null)
      else
        branch_commit=${head_contents//$'\n'/}
      fi

      branch_name=$(
        GIT_DIR="$git_dir" git for-each-ref --format='%(refname:short)' \
          --points-at "$branch_commit" refs/heads 2>/dev/null | head -n1
      )

      if [[ -n $branch_name ]]; then
        printf 'ref: refs/heads/%s\n' "$branch_name" >| "$head_file"
        restore_head=1
      fi
    fi
  fi

  GIT_DIR="$git_dir" GIT_WORK_TREE="$work_tree" gh "$@"
  local exit_code=$?

  if (( restore_head )); then
    printf '%s\n' "$head_contents" >| "$head_file"
  fi

  return $exit_code
}


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

# Function to switch Kubernetes context namespace and update kubeconfig symlink
# Usage: ks <environment>
function ks() {
  if [ -z "$1" ]; then
    echo "Usage: ks <environment>"
    echo "Switches Kubernetes namespace and updates kubeconfig symlink"
    return 1
  fi

  local env="$1"
  local config_path="/Users/ah/.pollev/config/k8s/teleport-${env}.k8s.ops.pe"

  # Check if the target config file exists
  if [ ! -f "$config_path" ]; then
    echo "Error: Kubernetes config for environment '$env' not found at:"
    echo "$config_path"
    return 1
  fi

  # Update symlink
  echo "Updating kubeconfig symlink to point to: $config_path"
  ln -sf "$config_path" ~/.kube/config

  # Set namespace
  echo "Setting Kubernetes namespace to: $env"
  kubectl config set-context --current --namespace="$env"

  echo "Kubernetes environment switched to: $env"
}

# Random artemis pod
function artemis_pod() {
  return kubectl get pods -l app=artemis -l deployment=artemis -o jsonpath='{.items[*].metadata.name}' | tr ' ' '\n' | awk 'BEGIN{srand()} {a[NR]=$0} END{print a[int(rand()*NR)+1]}'
}

[[ -f ~/.secrets ]] && source ~/.secrets

export PATH="$PATH:/Users/ah/.lmstudio/bin"

# Added by Antigravity
export PATH="/Users/ah/.antigravity/antigravity/bin:$PATH"
