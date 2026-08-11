Near-vanilla nvim config. Everything here is stock Neovim except one plugin
(`jjsigns.nvim`), installed by the built-in `vim.pack` — no third-party plugin
manager, nothing to bootstrap. Requires Neovim 0.12+ for `vim.pack`.

```shell
# Install neovim from head of master
brew install neovim --HEAD
# You can upgrade it later with
brew upgrade neovim --fetch-head

# Install git and stow to use this dotfiles repo
brew install git stow

# Clone this repo to your home dir
cd ~
git clone git@github.com:andrewhampton/dotfiles.git

# Install neovim config using stow
stow -d dotfiles nvim
```

That symlinks these files into `~/.config/nvim` and you're done. In the example
above I'm using brew to install stow, but it's a gnu utility and may already be
available on your linux distro.

## Layout

- `lua/vimconfig.lua` — options, autocmds, the `:Jump` command
- `lua/plugins.lua` — the one plugin, via `vim.pack`
- `lua/finders.lua` — incremental fuzzy find/grep/recent on the command line
- `lua/maps.lua` — key mappings
- `lua/short-comments.lua` — narrower `textwidth` inside comments, via built-in treesitter
- `lua/commit-message-formatting.lua` — 50/72 `textwidth` in commit messages
- `spell/en.utf-8.add` — personal spelling dictionary
