# dotfiles

Personal dotfiles, managed with GNU [stow](https://www.gnu.org/software/stow/)
(`xstow` works too — the commands below are compatible).

Each top-level directory is a **stow package** whose internal layout mirrors
`$HOME`. Stowing a package symlinks its contents into your home directory — e.g.
`tmux/.tmux.conf` → `~/.tmux.conf`, and `nvim/.config/nvim/` → `~/.config/nvim`.

## Setup

1. Install stow: `brew install stow` (or `apt install stow`).
2. Clone this repo to `~/dotfiles`:
   ```shell
   git clone git@github.com:andrewhampton/dotfiles.git ~/dotfiles
   ```
3. From `~`, stow the packages you want — one at a time:
   ```shell
   stow -d dotfiles tmux
   ```
   …or stow the common set in one go:
   ```shell
   cd ~
   for pkg in claude cmux editor-config git hammerspoon irb jj kitty nvim \
              oh-my-zsh opencode ripgrep screen ssh tmux uv zed zsh; do
     stow -d dotfiles "$pkg"
   done
   ```
   To remove a package's symlinks: `stow -D -d dotfiles tmux`.

> **Folding note:** stow symlinks a *whole directory* when its target doesn't
> yet exist in `~` ("tree folding"). Apps can then write new files straight
> *into this repo* (that's how a stray 46 MB `.venv` once landed under `nvim/`).
> For folding-prone packages like `nvim` and `zed`, `stow --no-folding` links
> per-file so app-generated state stays out of the repo.

## Packages

| Package | Configures | Requires |
| --- | --- | --- |
| `claude` | Claude Code (`~/.claude`) | Claude Code |
| `cmux` | cmux terminal (`~/.config/cmux`) + `cmux-spawn`/`cmux-open`/`cmux-reap` (`~/.local/bin`) | cmux, Claude Code |
| `editor-config` | EditorConfig defaults (`~/.editorconfig`) | — |
| `emacs` | Emacs (`~/.emacs.d`) — legacy, unused | Emacs |
| `git` | Git config, attributes, message, global ignore | git |
| `hammerspoon` | Hammerspoon (`~/.hammerspoon`) | Hammerspoon + secrets (see below) |
| `irb` | Ruby IRB (`~/.irbrc`) | Ruby |
| `jj` | Jujutsu (`~/.config/jj`) | jj |
| `kitty` | kitty terminal (`~/.config/kitty`) | kitty |
| `nvim` | Neovim (`~/.config/nvim`) | Neovim |
| `oh-my-zsh` | Custom zsh theme (`~/.oh-my-zsh/themes`) | oh-my-zsh (installed separately) |
| `opencode` | opencode (`~/.config/opencode`) | opencode |
| `ripgrep` | ripgrep (`~/.rgconfig`, `~/.rgignore`) | ripgrep |
| `screen` | GNU screen (`~/.screenrc`) | screen |
| `ssh` | ssh agent-forward helper (`~/.ssh/rc`) | — |
| `tmux` | tmux (`~/.tmux.conf` + includes) | tmux |
| `uv` | uv (`~/.config/uv`) | uv |
| `zed` | Zed editor (`~/.config/zed`) | Zed |
| `zsh` | zsh (`~/.zshrc`, `~/.zshenv`) | zsh + oh-my-zsh |

## Not stow packages

These top-level directories are **not** meant to be stowed into `~` — don't pass
them to `stow`:

- `ergodox/`, `infinity/` — keyboard **firmware** (Ergodox Infinity, Infinity
  60%) flashed with `dfu-util`. See their READMEs.
- `iris/` — Iris keyboard layout export (`iris.json`).
- `screenshots/` — image assets used by [hammerspoon/README.md](hammerspoon/README.md).

## Secrets

Some packages expect hand-created, **gitignored** files that are not tracked here:

- `hammerspoon`: `~/.hammerspoon/forecast_io_api_key.lua` and
  `~/.hammerspoon/gmail_creds.lua` — see
  [hammerspoon/README.md](hammerspoon/README.md) for the expected format.

The zsh config also sources `~/.secrets` and `~/.env` if present (both untracked).
