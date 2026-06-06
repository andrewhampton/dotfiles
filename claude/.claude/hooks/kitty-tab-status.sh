#!/bin/zsh
#
# kitty-tab-status.sh — prefix this kitty tab's title with a Claude Code status
# emoji, WITHOUT ever changing the tab's name.
#
#   ❓  Claude is asking you something (waiting on your input)
#   👀  Claude finished its turn and handed back
#   (no emoji while Claude is working)
#
# It only ever swaps the LEADING emoji and preserves the rest of the title as
# the name. That's deliberate: external tab managers (e.g. the `o` orchestrator)
# identify their tabs by name, so the name must stay untouched. Consequently it
# holds an explicit title override at all times rather than handing the tab back
# to Claude Code's live "⠂ <summary>" spinner title.
#
# Invoked from Claude Code hooks; the hook JSON payload arrives on stdin.
# Arg $1 selects the mode:
#   working   UserPromptSubmit  → <name>            (no emoji)
#   question  PreToolUse(AskUserQuestion|ExitPlanMode) → ❓ <name>
#   stop      Stop              → ❓ <name> if my final message is a question,
#                                 else 👀 <name>
#
# Mechanism: `kitty @ set-tab-title` sets an EXPLICIT tab-title override that
# takes precedence over the `{title}` template Claude Code's OSC writes feed,
# and survives them. kitty remote control is reached over $KITTY_LISTEN_ON, not
# the TTY, so this works even though hooks run without a controlling terminal.
#
# Requires: kitty with `allow_remote_control yes`, jq, perl (all stock on macOS).

emulate -L zsh
setopt extended_glob

mode="${1:-stop}"
payload="$(cat)"

command -v kitty >/dev/null 2>&1 || exit 0
[[ -n "$KITTY_LISTEN_ON" ]] || exit 0

# Locate this window's tab → "<tab-id>\t<tab-title>"
row="$(kitty @ ls 2>/dev/null | jq -r --argjson wid "${KITTY_WINDOW_ID:-0}" \
  '.[].tabs[] | select(any(.windows[]?; .id == $wid)) | "\(.id)\t\(.title)"' | head -1)"
tabid="${row%%$'\t'*}"
title="${row#*$'\t'}"

# Target the resolved tab id when we have one; otherwise let kitty self-resolve.
set_title() {
  if [[ "$tabid" == <-> ]]; then
    kitty @ set-tab-title --match "id:$tabid" -- "$1" 2>/dev/null
  else
    kitty @ set-tab-title -- "$1" 2>/dev/null
  fi
}

# Name = current title minus any leading status/spinner glyph (braille spinner
# U+2800–U+28FF, our own ❓👀⚙️🔒🤔, variation selectors). This preserves
# whatever owns the name — Claude Code, or an orchestrator like `o`. Fall back
# to the basename of the hook's cwd only if the title is empty.
base="$(print -r -- "$title" \
  | perl -CSDA -pe 's/^[\x{2800}-\x{28FF}\x{2753}\x{1F440}\x{2699}\x{1F512}\x{1F914}\x{FE0F}\s]+//' 2>/dev/null)"
if [[ -z "${base// /}" ]]; then
  cwd="$(print -r -- "$payload" | jq -r '.cwd // empty' 2>/dev/null)"
  base="${${cwd:-$PWD}:t}"
fi

prefix=""   # working: no emoji while Claude is busy
case "$mode" in
  question) prefix="❓ " ;;
  stop)
    prefix="👀 "
    # ❓ when my last visible message ends in a question mark.
    transcript="$(print -r -- "$payload" | jq -r '.transcript_path // empty' 2>/dev/null)"
    if [[ -n "$transcript" && -f "$transcript" ]]; then
      last="$(jq -rs '[.[] | select(.type=="assistant") | .message.content[]?
                        | select(.type=="text") | .text] | last // ""' "$transcript" 2>/dev/null)"
      last="${last%%[[:space:]]##}"
      [[ "$last" == *'?' ]] && prefix="❓ "
    fi
    ;;
esac

set_title "${prefix}${base}"
