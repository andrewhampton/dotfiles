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
  echo $STATUS
}

# Git branch display function
git_prompt_info() {
  local ref
  ref=$(command git symbolic-ref HEAD 2> /dev/null) || \
  ref=$(command git rev-parse --short HEAD 2> /dev/null) || return 0
  echo "$ZSH_THEME_GIT_PROMPT_PREFIX${ref#refs/heads/}$ZSH_THEME_GIT_PROMPT_SUFFIX"
}

# Set prompt content with Base16 colors
PROMPT='
${base05}%~${reset_color} $(git_prompt_info)$(git_prompt_status)${cmd_time}
%(?.${base0D}.${base08})${reset_color} '
