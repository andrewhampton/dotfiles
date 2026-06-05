#!/bin/zsh
#
# kitty-tab-status.sh — set this kitty tab's title with a Claude Code status prefix.
#
#   ❓  Claude is asking you something (waiting on your input)
#   👀  Claude finished its turn and handed back
#   (while working there is NO override, so Claude Code's own "⠂ <summary>"
#    spinner title shows through)
#
# Invoked from Claude Code hooks; the hook JSON payload arrives on stdin.
# Arg $1 selects the mode:
#   working   UserPromptSubmit  → clear the override so CC's live summary shows
#   question  PreToolUse(AskUserQuestion|ExitPlanMode) → force ❓
#   stop      Stop              → ❓ if my final message is a question, else 👀
#
# Mechanism: `kitty @ set-tab-title` sets an EXPLICIT tab-title override that
# takes precedence over the `{title}` template Claude Code's OSC writes feed,
# and survives them. Clearing it (set-tab-title "") hands the tab back to the
# template. kitty remote control is reached over $KITTY_LISTEN_ON, not the TTY,
# so this works even though hooks run without a controlling terminal.
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

# Working: drop the override so Claude Code's spinner + AI summary shows.
if [[ "$mode" == "working" ]]; then
  set_title ""
  exit 0
fi

# Base name = current title minus any leading status/spinner glyph
# (braille spinner U+2800–U+28FF, our own ❓👀⚙️🔒🤔, variation selectors),
# falling back to the basename of the hook's cwd.
base="$(print -r -- "$title" \
  | perl -CSDA -pe 's/^[\x{2800}-\x{28FF}\x{2753}\x{1F440}\x{2699}\x{1F512}\x{1F914}\x{FE0F}\s]+//' 2>/dev/null)"
if [[ -z "${base// /}" ]]; then
  cwd="$(print -r -- "$payload" | jq -r '.cwd // empty' 2>/dev/null)"
  base="${${cwd:-$PWD}:t}"
fi

prefix="👀 "
if [[ "$mode" == "question" ]]; then
  prefix="❓ "
elif [[ "$mode" == "stop" ]]; then
  # ❓ when my last visible message ends in a question mark.
  transcript="$(print -r -- "$payload" | jq -r '.transcript_path // empty' 2>/dev/null)"
  if [[ -n "$transcript" && -f "$transcript" ]]; then
    last="$(jq -rs '[.[] | select(.type=="assistant") | .message.content[]?
                      | select(.type=="text") | .text] | last // ""' "$transcript" 2>/dev/null)"
    last="${last%%[[:space:]]##}"
    [[ "$last" == *'?' ]] && prefix="❓ "
  fi
fi

set_title "${prefix}${base}"
