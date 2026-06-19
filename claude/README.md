# claude

Stow package for [Claude Code](https://claude.com/claude-code) config under `~/.claude`.

```sh
stow --dir="$HOME/dotfiles" --target="$HOME" claude
```

## What's here

- `.claude/CLAUDE.md` — global user instructions (jj workflow, commit conventions, prefs).
- `.claude/statusline.sh` — custom status line (referenced by `settings.json`).
- `.claude/agents/` — custom agent definitions (`librarian`, `oracle`).
- `.claude/hooks/kitty-tab-status.sh` — prefixes this kitty tab's title with a
  status emoji **and plays a short, distinct audio cue** so a glance — or just
  your ears — tells you what Claude Code wants:
  - **❓** Claude is asking you something (waiting on input) — a rising,
    *unresolved* two-note "?" (the melody of a spoken question)
  - **👀** Claude finished its turn and handed back — a brighter three-note
    arpeggio that *resolves* up to the tonic (a soft "ta-da, ready")
  - no emoji and no sound while Claude is working.

  It only ever swaps the **leading emoji** and leaves the tab's **name**
  untouched, so external tab managers that identify tabs by name keep working
  (e.g. the `o` orchestrator, which matches `^(?:\S+ )?<name>$` and owns the
  name). It writes via `kitty @ set-tab-title` over the remote-control socket
  (`$KITTY_LISTEN_ON`) — an explicit override that wins over Claude Code's own
  OSC title writes. Requires kitty with `allow_remote_control yes`.

- `.claude/hooks/sounds/` — the audio cues and their generator:
  - `review.wav` / `question.wav` — the active cues the hook plays. The hook
    finds them as siblings of itself (`${0:A:h}/sounds/…`), resolving through
    the stow symlink, so they work without re-stowing once the hook is linked.
  - `generate.py` — renders the cues with the pure Python standard library (no
    numpy/sox/ffmpeg). Re-run after editing: `python3 generate.py`.
  - `variants/` — three timbral personalities rendered for both cues:
    `arcade` (the shipped pair), `musicbox`, and `bell`.
  - `audition.sh [name…]` — play the variants to pick by ear;
    `use.sh <name>` — make a personality the active pair.
  - Cues are **non-blocking** (detached `afplay`), **focus-aware** (silent when
    you're already looking at this pane — the point of a sound is the tab you're
    *not* watching), **debounced** per window+cue (rapid re-fires of the same
    event are suppressed, ~3s), and **mutable**: `CLAUDE_TAB_SOUND=0` keeps the
    emoji but silences the sound, `CLAUDE_TAB_SOUND=always` plays even when the
    pane is focused. Requires `afplay` (stock on macOS); no new `settings.json`
    wiring — the cues live inside the hook the existing `PreToolUse`/`Stop`
    hooks already call.

## Not tracked here

`~/.claude/settings.json` is your full, private Claude Code config (personal
permission allowlist, `0600`, and Claude Code rewrites it on its own), so it's
left as a local file. To activate the tab-status hook on a new machine, merge
this into it:

```json
{
  "hooks": {
    "UserPromptSubmit": [
      { "hooks": [ { "type": "command", "command": "$HOME/.claude/hooks/kitty-tab-status.sh working" } ] }
    ],
    "PreToolUse": [
      { "matcher": "AskUserQuestion|ExitPlanMode",
        "hooks": [ { "type": "command", "command": "$HOME/.claude/hooks/kitty-tab-status.sh question" } ] }
    ],
    "PostToolUse": [
      { "matcher": "AskUserQuestion|ExitPlanMode",
        "hooks": [ { "type": "command", "command": "$HOME/.claude/hooks/kitty-tab-status.sh working" } ] }
    ],
    "Stop": [
      { "hooks": [ { "type": "command", "command": "$HOME/.claude/hooks/kitty-tab-status.sh stop" } ] }
    ]
  }
}
```

Machine-local state (`projects/`, `sessions/`, `history.jsonl`, `~/.claude.json`,
`plugins/`, caches) and Keychain-stored credentials are intentionally left out.
