# Base16 Color Definitions
# These color codes are based on the standard Base16 scheme
# You may need to adjust for your specific terminal configuration
base00="%F{black}"        # Default Background
base01="%F{brightblack}"  # Lighter Background
base02="%F{240}"          # Selection Background
base03="%F{243}"          # Comments, Invisibles, Line Highlighting
base04="%F{248}"          # Dark Foreground
base05="%F{white}"        # Default Foreground
base06="%F{brightwhite}"  # Light Foreground
base07="%F{252}"          # Light Background
base08="%F{red}"          # Variables, XML Tags, Markup Link Text, Markup Lists
base09="%F{yellow}"       # Integers, Boolean, Constants
base0A="%F{yellow}"       # Classes, Markup Bold, Search Text Background
base0B="%F{green}"        # Strings, Inherited Class, Markup Code
base0C="%F{cyan}"         # Support, Regular Expressions, Escape Characters
base0D="%F{blue}"         # Functions, Methods, Attribute IDs, Headings
base0E="%F{magenta}"      # Keywords, Storage, Selector, Markup Italic
base0F="%F{red}"          # Deprecated, Opening/Closing Embedded Language Tags

reset_color="%f"

# Function to display the execution time of the last command
function preexec() {
  cmd_start_time=$SECONDS
}

function precmd() {
  if [ $cmd_start_time ]; then
    cmd_duration=$(($SECONDS - $cmd_start_time))
    [ $cmd_duration -gt 1 ] && cmd_time=" ${base03}[${cmd_duration}s]${reset_color}" || cmd_time=""
    unset cmd_start_time
  fi
}

# Git info with Base16 colors
ZSH_THEME_GIT_PROMPT_PREFIX="${base0B}"
ZSH_THEME_GIT_PROMPT_SUFFIX="${reset_color}"
ZSH_THEME_GIT_PROMPT_ADDED="${base0B} ✚"
ZSH_THEME_GIT_PROMPT_MODIFIED="${base0D} 🟉"
ZSH_THEME_GIT_PROMPT_DELETED="${base08} ✖"
ZSH_THEME_GIT_PROMPT_RENAMED="${base0E} →"
ZSH_THEME_GIT_PROMPT_UNMERGED="${base0A} ⇅"
ZSH_THEME_GIT_PROMPT_UNTRACKED="${base01} 🟄"
ZSH_THEME_GIT_PROMPT_UNPUSHED="${base0A} ↑"
ZSH_THEME_GIT_PROMPT_UNPULLED="${base0A} ↓"

# JJ info with Base16 colors (using same colors as git for consistency)
ZSH_THEME_JJ_PROMPT_PREFIX="${base0C}"
ZSH_THEME_JJ_PROMPT_SUFFIX="${reset_color}"
ZSH_THEME_JJ_PROMPT_ADDED="${base0B} ✚"
ZSH_THEME_JJ_PROMPT_MODIFIED="${base0D} 🟉"
ZSH_THEME_JJ_PROMPT_DELETED="${base08} ✖"

# Function for git status
function git_prompt_status() {
  local INDEX STATUS
  INDEX=$(command git status --porcelain -b 2> /dev/null)
  STATUS=""
  if $(echo "$INDEX" | grep -E '^\?\? ' &> /dev/null); then
    STATUS="$ZSH_THEME_GIT_PROMPT_UNTRACKED$STATUS"
  fi
  if $(echo "$INDEX" | grep '^A  ' &> /dev/null); then
    STATUS="$ZSH_THEME_GIT_PROMPT_ADDED$STATUS"
  elif $(echo "$INDEX" | grep '^M  ' &> /dev/null); then
    STATUS="$ZSH_THEME_GIT_PROMPT_ADDED$STATUS"
  fi
  if $(echo "$INDEX" | grep '^ M ' &> /dev/null); then
    STATUS="$ZSH_THEME_GIT_PROMPT_MODIFIED$STATUS"
  elif $(echo "$INDEX" | grep '^AM ' &> /dev/null); then
    STATUS="$ZSH_THEME_GIT_PROMPT_MODIFIED$STATUS"
  elif $(echo "$INDEX" | grep '^ T ' &> /dev/null); then
    STATUS="$ZSH_THEME_GIT_PROMPT_MODIFIED$STATUS"
  fi
  if $(echo "$INDEX" | grep '^R  ' &> /dev/null); then
    STATUS="$ZSH_THEME_GIT_PROMPT_RENAMED$STATUS"
  fi
  if $(echo "$INDEX" | grep '^ D ' &> /dev/null); then
    STATUS="$ZSH_THEME_GIT_PROMPT_DELETED$STATUS"
  elif $(echo "$INDEX" | grep '^D  ' &> /dev/null); then
    STATUS="$ZSH_THEME_GIT_PROMPT_DELETED$STATUS"
  elif $(echo "$INDEX" | grep '^AD ' &> /dev/null); then
    STATUS="$ZSH_THEME_GIT_PROMPT_DELETED$STATUS"
  fi
  if $(echo "$INDEX" | grep '^UU ' &> /dev/null); then
    STATUS="$ZSH_THEME_GIT_PROMPT_UNMERGED$STATUS"
  fi
  if $(echo "$INDEX" | grep '^## [^ ]\+ .*ahead' &> /dev/null); then
    STATUS="$ZSH_THEME_GIT_PROMPT_UNPUSHED$STATUS"
  fi
  if $(echo "$INDEX" | grep '^## [^ ]\+ .*behind' &> /dev/null); then
    STATUS="$ZSH_THEME_GIT_PROMPT_UNPULLED$STATUS"
  fi

  echo $STATUS
}

# JJ status function
function jj_prompt_status() {
  local INDEX STATUS
  INDEX=$(command jj status 2> /dev/null)
  STATUS=""
  if $(echo "$INDEX" | grep -E '^Working copy changes:' &> /dev/null); then
    if $(echo "$INDEX" | grep -E '^A ' &> /dev/null); then
      STATUS="$ZSH_THEME_JJ_PROMPT_ADDED$STATUS"
    fi
    if $(echo "$INDEX" | grep -E '^M ' &> /dev/null); then
      STATUS="$ZSH_THEME_JJ_PROMPT_MODIFIED$STATUS"
    fi
    if $(echo "$INDEX" | grep -E '^D ' &> /dev/null); then
      STATUS="$ZSH_THEME_JJ_PROMPT_DELETED$STATUS"
    fi
  fi

  echo $STATUS
}

# JJ/Git detection function
is_jj_repo() {
  command jj root &> /dev/null
}

# JJ prompt info function
jj_prompt_info() {
  local jj_info bookmarks distance bookmarked_commit

  # Find the nearest ancestor with bookmarks
  local bookmark_info
  bookmark_info=$(command jj log -r "ancestors(@, 10)" --no-graph -T 'if(bookmarks, bookmarks ++ "|" ++ change_id.short() ++ "\n", "")' 2> /dev/null | grep -v '^$' | head -1) || return 0

  if [ -n "$bookmark_info" ]; then
    bookmarks=$(echo "$bookmark_info" | cut -d'|' -f1)
    bookmarked_commit=$(echo "$bookmark_info" | cut -d'|' -f2)

    # Count distance from @ to the bookmarked commit
    distance=$(command jj log -r "$bookmarked_commit::@" --no-graph -T 'change_id.short() ++ "\n"' 2> /dev/null | wc -l) || return 0
    distance=$((distance - 1))  # Subtract 1 because the range includes both endpoints

    if [ $distance -eq 0 ]; then
      jj_info="$bookmarks"
    else
      jj_info="${bookmarks}+${distance}"
    fi
  else
    jj_info="(no bookmarks)"
  fi

  echo "$ZSH_THEME_JJ_PROMPT_PREFIX$jj_info$ZSH_THEME_JJ_PROMPT_SUFFIX"
}

# Unified VCS prompt info function
vcs_prompt_info() {
  local info=""
  if is_jj_repo; then
    info="$(jj_prompt_info)"
  fi
  if git rev-parse --git-dir &> /dev/null; then
    if [ -n "$info" ]; then
      info="$info $(git_prompt_info)"
    else
      info="$(git_prompt_info)"
    fi
  fi
  echo "$info"
}

# Unified VCS prompt status function
vcs_prompt_status() {
  local vcs_status=""
  if is_jj_repo; then
    vcs_status="$(jj_prompt_status)"
  fi
  if git rev-parse --git-dir &> /dev/null; then
    local git_status="$(git_prompt_status)"
    if [ -n "$vcs_status" ] && [ -n "$git_status" ]; then
      vcs_status="$vcs_status$git_status"
    elif [ -n "$git_status" ]; then
      vcs_status="$git_status"
    fi
  fi
  echo "$vcs_status"
}

# Git branch display function
git_prompt_info() {
  local ref
  ref=$(command git symbolic-ref HEAD 2> /dev/null) || \
  ref=$(command git rev-parse --short HEAD 2> /dev/null) || return 0
  echo "$ZSH_THEME_GIT_PROMPT_PREFIX${ref#refs/heads/}$ZSH_THEME_GIT_PROMPT_SUFFIX"
}

# Set prompt content with Base16 colors
# PROMPT='${base0D}%~${reset_color} $(vcs_prompt_info)$(vcs_prompt_status)${cmd_time} ${base02}%D{%H:%M:%S}${reset_color}
# %(?.${base0D}.${base08})${reset_color} '
PROMPT='${base0D}%~${reset_color} $(vcs_prompt_info)$(vcs_prompt_status)${cmd_time} ${base02}%D{%H:%M:%S}${reset_color}
%(?.${base0D}.${base08})${reset_color} '
