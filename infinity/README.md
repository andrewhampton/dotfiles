Infinity 60% (MD1) keyboard firmware (built with [kiibohd](https://github.com/kiibohd)/KLL).
**Not a stow package** — don't symlink this into your home directory. The `.kll`
files are KLL keymap layers, `MD1-HackerBlank.json` is the configurator export, and
`kiibohd.dfu.bin` is the compiled firmware flashed below.

# Update procedure

1. `brew install dfu-util`
1. `cd ~/dotfiles/infinity`
1. Click button on back of keyboard
1. `dfu-util -D kiibohd.dfu.bin`
