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

# JJ info with Base16 colors
ZSH_THEME_JJ_PROMPT_PREFIX="${base0C}"
ZSH_THEME_JJ_PROMPT_SUFFIX="${reset_color}"
ZSH_THEME_JJ_PROMPT_ADDED="${base0B} ✚"
ZSH_THEME_JJ_PROMPT_MODIFIED="${base0D} 🟉"
ZSH_THEME_JJ_PROMPT_DELETED="${base08} ✖"


# JJ status function
function jj_prompt_status() {
  local INDEX STATUS
  INDEX=$(command jj status --ignore-working-copy 2> /dev/null)
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


# JJ prompt info function
jj_prompt_info() {
  local jj_info bookmarks distance bookmarked_commit

  # Find the nearest ancestor with bookmarks
  local bookmark_info
  bookmark_info=$(command jj log --ignore-working-copy -r "ancestors(@, 10)" --no-graph -T 'if(bookmarks, bookmarks ++ "|" ++ change_id.short() ++ "\n", "")' 2> /dev/null | grep -v '^$' | head -1) || return 0

  if [ -n "$bookmark_info" ]; then
    bookmarks=$(echo "$bookmark_info" | cut -d'|' -f1)
    bookmarked_commit=$(echo "$bookmark_info" | cut -d'|' -f2)

    # Count distance from @ to the bookmarked commit
    distance=$(command jj log --ignore-working-copy -r "$bookmarked_commit::@" --no-graph -T 'change_id.short() ++ "\n"' 2> /dev/null | wc -l) || return 0
    distance=$((distance - 1))  # Subtract 1 because the range includes both endpoints

    if [ $distance -eq 0 ]; then
      jj_info="$bookmarks"
    else
      jj_info="${bookmarks}+${distance}"
    fi
  else
    jj_info=""
  fi

  echo "$ZSH_THEME_JJ_PROMPT_PREFIX$jj_info$ZSH_THEME_JJ_PROMPT_SUFFIX"
}




# Set prompt content with Base16 colors
# %(?.${base0D}.${base08})${reset_color} '
PROMPT='${base0D}%~${reset_color} $(jj_prompt_info)$(jj_prompt_status)${cmd_time} ${base02}%D{%H:%M:%S}${reset_color}
%(?.${base0D}.${base08})${reset_color} '
