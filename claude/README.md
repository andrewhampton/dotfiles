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
  status emoji so a glance tells you what Claude Code wants:
  - **❓** Claude is asking you something (waiting on input)
  - **👀** Claude finished its turn and handed back
  - no emoji while Claude is working.

  It only ever swaps the **leading emoji** and leaves the tab's **name**
  untouched, so external tab managers that identify tabs by name keep working
  (e.g. the `o` orchestrator, which matches `^(?:\S+ )?<name>$` and owns the
  name). It writes via `kitty @ set-tab-title` over the remote-control socket
  (`$KITTY_LISTEN_ON`) — an explicit override that wins over Claude Code's own
  OSC title writes. Requires kitty with `allow_remote_control yes`.

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
