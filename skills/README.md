# skills

Agent skills shared between Claude Code and Amp, managed as a stow package.

## Layout

```
skills/
├── .config/agents/skills/<name>/SKILL.md   # canonical copy (Amp reads this dir)
└── .claude/skills/<name> -> ../../.config/agents/skills/<name>   # Claude Code reads this dir
```

Each skill lives **once** under `.config/agents/skills/`. The entry under
`.claude/skills/` is a relative symlink back to it, so both tools see the same
files and there is nothing to keep in sync.

- Claude Code discovers skills in `~/.claude/skills/<name>/SKILL.md`.
- Amp discovers skills in `~/.config/agents/skills/<name>/SKILL.md` (and also
  reads `~/.claude/skills/` unless `amp.skills.disableClaudeCodeSkills` is set).

## Stowing

Both `~/.claude/skills` and `~/.config/agents/skills` already exist and hold
other (non-dotfiles) skills, so stow must link per-skill rather than replacing
the directory. Always use `--no-folding`:

```shell
cd ~
stow --no-folding -d dotfiles skills
```

Result:

```
~/.config/agents/skills/teach/SKILL.md -> ../../../../dotfiles/skills/.config/agents/skills/teach/SKILL.md   # per-file
~/.claude/skills/teach                 -> ../../dotfiles/skills/.claude/skills/teach   # a link to the repo's relative symlink
```

To unlink: `stow -D -d dotfiles skills`. To re-link after adding a skill:
`stow -R --no-folding -d dotfiles skills`.

If stow reports a conflict, an unmanaged file/dir with the same name already
exists in the target; move or remove it, then re-run.

## Adding a skill

```shell
cd ~/dotfiles/skills
mkdir -p .config/agents/skills/<name>
$EDITOR .config/agents/skills/<name>/SKILL.md
ln -s ../../.config/agents/skills/<name> .claude/skills/<name>
cd ~ && stow -R --no-folding -d dotfiles skills
```

`SKILL.md` needs YAML frontmatter with `name` and `description`; the
description is what the agent uses to decide when to load the skill. Put long
prompts or reference material in sibling files and have `SKILL.md` point at
them so the frontmatter/description stays short.

## Skills

| Skill | Purpose |
| --- | --- |
| `teach` | Adaptive guided walkthrough for deeply understanding a branch / PR series across three levels: mental model → design rationale → learning path. |
