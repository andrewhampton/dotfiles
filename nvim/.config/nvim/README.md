Self-bootstrapping-ish config for nvim on the CLI and using the vscode plugin.
To use, just follow these steps:

```shell
# Install neovim from head of measter
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

At this point, you'll have these config files symlinked in `~./config/nvim`. In
the example code I'm using brew to install stow, but it's a gnu until and may
be already available on your linux distro.

Next, we'll have to bootstrap the package manager, Packer. This will take a few
cranks, so just repeat this step a few times until everything is installed:

```shell
nvim
:PackerSync
:qa
```
