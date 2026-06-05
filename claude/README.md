# claude

Stow package for [Claude Code](https://claude.com/claude-code) config under `~/.claude`.

```sh
stow --dir="$HOME/dotfiles" --target="$HOME" claude
```

## What's here

- `.claude/CLAUDE.md` — global user instructions (jj workflow, commit conventions, prefs).
- `.claude/statusline.sh` — custom status line (referenced by `settings.json`).
- `.claude/agents/` — custom agent definitions (`librarian`, `oracle`).
- `.claude/hooks/kitty-tab-status.sh` — sets this kitty tab's title with a
  status prefix so a glance tells you what Claude Code wants:
  - **❓** Claude is asking you something (waiting on input)
  - **👀** Claude finished its turn and handed back
  - while it works there's no override, so Claude Code's own `⠂ <task summary>`
    spinner title shows through.

  It sets the title via `kitty @ set-tab-title` over the remote-control socket
  (`$KITTY_LISTEN_ON`) — an explicit tab-title override that wins over, and
  coexists with, Claude Code's own OSC title writes. Requires kitty with
  `allow_remote_control yes`.

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
    "Stop": [
      { "hooks": [ { "type": "command", "command": "$HOME/.claude/hooks/kitty-tab-status.sh stop" } ] }
    ]
  }
}
```

Machine-local state (`projects/`, `sessions/`, `history.jsonl`, `~/.claude.json`,
`plugins/`, caches) and Keychain-stored credentials are intentionally left out.
