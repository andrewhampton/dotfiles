#!/bin/zsh
#
# kitty-tab-status.sh — prefix this kitty tab's title with a Claude Code status
# emoji, WITHOUT ever changing the tab's name.
#
#   ❓  Claude is asking you something (waiting on your input)
#   👀  Claude finished its turn and handed back
#   (no emoji while Claude is working)
#
# Each of ❓ and 👀 also plays a short, distinct audio cue (see play_cue and
# sounds/). Silence them with CLAUDE_TAB_SOUND=0.
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

# Resolve this script's own directory ONCE, here at top level. Inside a function
# zsh sets $0 to the function name (FUNCTION_ARGZERO), so ${0:A:h} evaluated in
# play_cue would resolve against $PWD — the project dir when Claude Code runs the
# hook — and the sound file would never be found. Capture it while $0 is still
# the script path (symlink-resolved via :A, since this file is symlinked into
# ~/.claude/hooks).
script_dir="${0:A:h}"

command -v kitty >/dev/null 2>&1 || exit 0
[[ -n "$KITTY_LISTEN_ON" ]] || exit 0

# Sub-agent origin. PreToolUse fires for a sub-agent's OWN tool calls too (e.g. a
# Task sub-agent invoking AskUserQuestion), and those payloads carry an .agent_id
# that the main agent's own events lack. Audio cues are for the main agent only —
# a sub-agent finishing an internal step shouldn't ping you — so detect it once
# here and gate play_cue on it. The tab emoji still updates regardless.
subagent_id="$(print -r -- "$payload" | jq -r '.agent_id // empty' 2>/dev/null)"

# Snapshot kitty's window tree once, then derive (a) this window's tab as
# "<tab-id>\t<tab-title>" and (b) whether the user is currently looking at this
# pane — a focused OS window whose active tab's active window is ours. (b) lets
# play_cue stay silent when you're already watching. (b) is fail-safe: if the
# query returns empty/false the cue still plays, so a detection miss never eats
# a sound.
ls_json="$(kitty @ ls 2>/dev/null)"
row="$(print -r -- "$ls_json" | jq -r --argjson wid "${KITTY_WINDOW_ID:-0}" \
  '.[].tabs[] | select(any(.windows[]?; .id == $wid)) | "\(.id)\t\(.title)"' | head -1)"
tabid="${row%%$'\t'*}"
title="${row#*$'\t'}"
viewing="$(print -r -- "$ls_json" | jq -r --argjson wid "${KITTY_WINDOW_ID:-0}" \
  'any(.[]; .is_focused and any(.tabs[]; .is_active and any(.windows[]?; .id==$wid and .is_active)))' 2>/dev/null)"

# Target the resolved tab id when we have one; otherwise let kitty self-resolve.
set_title() {
  if [[ "$tabid" == <-> ]]; then
    kitty @ set-tab-title --match "id:$tabid" -- "$1" 2>/dev/null
  else
    kitty @ set-tab-title -- "$1" 2>/dev/null
  fi
}

# Play a short audio cue alongside the emoji, so you don't have to be watching
# the tab to know Claude wants you. $1 is the cue name → sounds/<name>.wav
# (sibling of this script). Two distinct cues: "question" (❓, rising/unresolved)
# vs "review" (👀, resolved arpeggio). See sounds/generate.py.
#
#   - Non-blocking: afplay is detached so the hook returns instantly.
#   - Main-agent only: silent when the triggering event came from a sub-agent
#     (payload carried .agent_id). The emoji still updates.
#   - Focus-aware: silent when you're already looking at this pane — the point of
#     a sound is the tab you're NOT watching. The emoji still updates. Set
#     CLAUDE_TAB_SOUND=always to hear it even when this pane is focused.
#   - Mutable: CLAUDE_TAB_SOUND=0 silences cues entirely (emoji still updates).
#   - Debounced per (window,cue): a rapid re-fire of the same event is suppressed
#     (best-effort, ~3s; not a hard guarantee).
play_cue() {
  local pref="${CLAUDE_TAB_SOUND:-1}"
  [[ "$pref" != 0 ]] || return                              # muted
  [[ -z "$subagent_id" ]] || return                         # main agent only
  command -v afplay >/dev/null 2>&1 || return
  [[ "$pref" == always || "$viewing" != true ]] || return  # silent when watching
  local snd="$script_dir/sounds/$1.wav"
  [[ -f "$snd" ]] || return

  local key="${KITTY_WINDOW_ID:-${tabid:-$$}}"
  local mark="${TMPDIR:-/tmp}/claude-tab-sound-${key}-$1"
  local now last=0
  now="$(date +%s)"
  [[ -f "$mark" ]] && last="$(stat -f %m "$mark" 2>/dev/null || print 0)"
  (( now - last < 3 )) && return
  : > "$mark"

  ( afplay "$snd" >/dev/null 2>&1 & )   # detached; orphaned afplay keeps playing
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
cue=""      # which audio cue to play (empty = silent, e.g. while working)
case "$mode" in
  question) prefix="❓ "; cue="question" ;;
  stop)
    prefix="👀 "; cue="review"
    # ❓ when my last visible message ends in a question mark.
    transcript="$(print -r -- "$payload" | jq -r '.transcript_path // empty' 2>/dev/null)"
    if [[ -n "$transcript" && -f "$transcript" ]]; then
      last="$(jq -rs '[.[] | select(.type=="assistant") | .message.content[]?
                        | select(.type=="text") | .text] | last // ""' "$transcript" 2>/dev/null)"
      last="${last%%[[:space:]]##}"
      [[ "$last" == *'?' ]] && { prefix="❓ "; cue="question" }
    fi
    ;;
esac

set_title "${prefix}${base}"
[[ -n "$cue" ]] && play_cue "$cue"

# Never let the script's exit status ride on the conditional above: in "working"
# mode $cue is empty, so the `[[ -n … ]]` test fails (exit 1) and — being the
# last command — would make the whole hook exit 1, which Claude Code reports as
# a UserPromptSubmit hook error on every prompt.
exit 0
