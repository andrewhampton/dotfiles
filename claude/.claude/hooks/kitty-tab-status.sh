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
# sounds/). Silence them with CLAUDE_TAB_SOUND=0. The 👀 "handed back" cue is
# muted while this session still has background sub-agents running (see
# bg_agents_active): each one finishing wakes the main agent, which stops again,
# and you don't want a ping per wake — only the final, genuine hand-back.
#
# The tab that JUST signalled also gets a moving color marker (see mark_fresh):
# it pulses through an orange ramp and comes to rest on an amber tint, and its
# pane gets a faint background wash. Only ever ONE tab is marked at a time — the
# most recent to signal — so when several tabs already show ❓/👀 you can tell
# which one just pinged. Disable with CLAUDE_TAB_COLOR=0.
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

# Opt-in diagnostic: with CLAUDE_TAB_DEBUG set, append every invocation's mode +
# raw payload to that path so we can see exactly what a sub-agent event carries.
# No-op unless the env var is set, so it never affects normal runs.
if [[ -n "$CLAUDE_TAB_DEBUG" ]]; then
  printf '=== %s | mode=%s ===\n%s\n' "$(date '+%H:%M:%S')" "$mode" "$payload" >> "$CLAUDE_TAB_DEBUG" 2>/dev/null
fi

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

# True when this session still has a background sub-agent RUNNING. Used to mute the
# 👀 "handed back" cue: a background/parallel sub-agent finishing wakes the main
# agent, which processes the result and stops again — firing a genuine main-agent
# Stop (no .agent_id, so agent_id gating can't catch it). We don't want a ping per
# wake; only the FINAL stop, when nothing is left running, should sound.
#
# Sub-agent transcripts sit beside the main one at <transcript dir>/subagents/
# agent-*.jsonl. A sub-agent is single-turn, so its transcript ends with an
# assistant `end_turn` line EXACTLY when it's done; any other tail means it's
# mid-run. A stale file (dead/interrupted agent) ages out after 120s so a crashed
# agent can't silence you for the rest of the session. Fail-safe: on any error we
# return 1 (not active), so a detection miss never eats a legitimate ping.
bg_agents_active() {
  local tpath subdir f last mt now
  tpath="$(print -r -- "$payload" | jq -r '.transcript_path // empty' 2>/dev/null)"
  [[ -n "$tpath" ]] || return 1
  subdir="${tpath:h}/subagents"
  [[ -d "$subdir" ]] || return 1
  now="$(date +%s)"
  for f in "$subdir"/agent-*.jsonl(N); do
    mt="$(stat -f %m "$f" 2>/dev/null)" || continue
    (( now - mt < 120 )) || continue                      # stale → finished/dead
    last="$(tail -1 "$f" 2>/dev/null)"
    [[ "$(print -r -- "$last" \
        | jq -r '(.type=="assistant" and .message.stop_reason=="end_turn")' 2>/dev/null)" \
      == true ]] && continue                              # this one is done
    return 0                                              # still running
  done
  return 1
}

# ── Attention marker: a transient tab pulse + a moving pane wash ──────────────
# When a cue fires (same trigger as the sound: main agent, and you're not looking
# at the pane), the just-signalled tab breathes an orange pulse — from its normal
# color up to orange and back, a few times — then returns to its default color on
# its own. The pane, meanwhile, gets a faint PERSISTENT background wash, and that
# wash is the lingering "most recent" marker: only ONE pane is washed at a time,
# so among several ❓/👀 tabs you can tell which just pinged. The wash is reverted
# when a newer cue supersedes it, or when you visit the pane (clear_fresh_here).
# The tab pulse itself is fire-and-forget — it never leaves a lasting tint.
#
#   - Detached: the ~3s pulse runs in the background so the hook returns at once.
#   - Superseded-safe: each pulse step re-reads the shared state file and bows out
#     (clearing its tab + pane) the instant a newer pane becomes the freshest.
#   - Disable with CLAUDE_TAB_COLOR=0.
#
# State file holds one line "<socket>\t<tabid>\t<windowid>" naming the currently
# washed spot, so the next cue can revert it — even in another kitty instance.
CLAUDE_TAB_STATE="${TMPDIR:-/tmp}/claude-tab-freshest"
typeset -ga CLAUDE_TAB_RAMP=(                          # inactive_tab_background → orange:
  "#181825" "#322421" "#4b2f1d" "#653b19" "#7f4615"    # the breath ramp. [1] ≈ the tab's
  "#985210" "#b25d0c" "#cc6908" "#e57404" "#ff8000")   # Catppuccin default; breath clears to it
CLAUDE_TAB_WASH="#211e28"                              # base #1e1e2e nudged barely warm

# Revert a mark: restore the tab's default colors and reset the pane's colors.
# The socket may name a different kitty instance than ours, so target it via --to.
clear_mark() {   # $1=socket $2=tabid $3=windowid
  clear_tab "$2" "$1"
  [[ -n "$3" ]] && kitty @ --to "$1" set-colors --reset --match "id:$3" 2>/dev/null
}

# Clear ONLY the tab's color override (leave the pane wash — the lasting marker).
# $2 is an optional socket for cross-instance targeting; default is our own.
clear_tab() {   # $1=tabid [$2=socket]
  [[ -n "$1" ]] && kitty @ --to "${2:-$KITTY_LISTEN_ON}" set-tab-color --match "id:$1" \
    active_bg=NONE inactive_bg=NONE active_fg=NONE inactive_fg=NONE 2>/dev/null
}

# The detached pulse. Breathes CLAUDE_TAB_RAMP from the tab's default up to orange
# and back a few times, then returns the tab to its default color — the pane wash
# is what lingers. Aborts and reverts tab+pane if we stop being the freshest.
pulse_loop() {
  local sweep i w
  local -a seq
  for sweep in up down up down up down; do
    if [[ "$sweep" == up ]]; then seq=({1..10}); else seq=({10..1}); fi
    for i in "${seq[@]}"; do
      [[ -f "$CLAUDE_TAB_STATE" ]] || { clear_mark "$KITTY_LISTEN_ON" "$tabid" "$KITTY_WINDOW_ID"; return }
      w="$(<"$CLAUDE_TAB_STATE")"; w="${w##*$'\t'}"
      [[ "$w" == "$KITTY_WINDOW_ID" ]] || { clear_mark "$KITTY_LISTEN_ON" "$tabid" "$KITTY_WINDOW_ID"; return }
      # Only touch the background; leave the foreground at the tab's default
      # (light) text so titles stay legible as the background darkens/brightens.
      kitty @ set-tab-color --match "id:$tabid" \
        active_bg="$CLAUDE_TAB_RAMP[$i]" inactive_bg="$CLAUDE_TAB_RAMP[$i]" 2>/dev/null
      sleep 0.06
    done
  done
  clear_tab "$tabid"   # breath done → tab back to default; pane wash stays
}

# Move the marker to THIS tab: revert the previous mark, claim the state, wash our
# pane, and launch the detached pulse. Same gate as the sound (minus the mute).
mark_fresh() {
  [[ "${CLAUDE_TAB_COLOR:-1}" != 0 ]] || return             # disabled
  [[ -z "$subagent_id" ]] || return                         # main agent only
  [[ "$viewing" != true ]] || return                        # you're already here
  [[ "$tabid" == <-> ]] || return                           # need a real tab id

  if [[ -f "$CLAUDE_TAB_STATE" ]]; then
    local psock ptab pwin
    IFS=$'\t' read -r psock ptab pwin < "$CLAUDE_TAB_STATE"
    [[ "$ptab" == "$tabid" && "$pwin" == "$KITTY_WINDOW_ID" ]] || clear_mark "$psock" "$ptab" "$pwin"
  fi

  # Claim the mark atomically so a concurrent pulse elsewhere sees the handover.
  printf '%s\t%s\t%s\n' "$KITTY_LISTEN_ON" "$tabid" "$KITTY_WINDOW_ID" > "${CLAUDE_TAB_STATE}.$$" \
    && mv -f "${CLAUDE_TAB_STATE}.$$" "$CLAUDE_TAB_STATE"
  kitty @ set-colors --match "id:$KITTY_WINDOW_ID" background="$CLAUDE_TAB_WASH" 2>/dev/null

  ( pulse_loop & )   # detached; orphaned pulse keeps breathing after we exit
}

# When we ARE the pane being looked at, drop our mark (revert colors, clear state).
clear_fresh_here() {
  [[ -f "$CLAUDE_TAB_STATE" ]] || return
  local psock ptab pwin
  IFS=$'\t' read -r psock ptab pwin < "$CLAUDE_TAB_STATE"
  if [[ "$ptab" == "$tabid" || "$pwin" == "$KITTY_WINDOW_ID" ]]; then
    clear_mark "$psock" "$ptab" "$pwin"
    rm -f "$CLAUDE_TAB_STATE"
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

# You're looking at this pane → drop any marker it's holding.
[[ "$viewing" == true ]] && clear_fresh_here

# Mute the 👀 review cue (sound AND color marker) while background sub-agents are
# still running: each one completing wakes the main agent, which stops again, and
# we don't want a ping per wake — only the final stop, once nothing's left running.
# The ❓ question cue is deliberately exempt: that means Claude is blocked on YOU,
# so it should sound regardless. The 👀 emoji itself still updates either way.
if [[ "$cue" == review ]] && bg_agents_active; then
  cue=""
fi

# Sound + moving color marker share one trigger: a real cue you're not watching.
if [[ -n "$cue" ]]; then
  play_cue "$cue"
  mark_fresh
fi

# Never let the script's exit status ride on the conditionals above: in "working"
# mode $cue is empty and you may not be viewing, so the last test can fail (exit
# 1) and — being the last command — would make the whole hook exit 1, which
# Claude Code reports as a UserPromptSubmit hook error on every prompt.
exit 0
