# Update procedure

1. `brew install dfu-util`
1. `cd ~/dotfiles/ergodox`
1. Hook each keyboard directly up to mac
1. Click button on back of right keyboard
1. `dfu-util -D right_kiibohd.dfu.bin`
1. Click button on back of left keyboard
1. `dfu-util -D left_kiibohd.dfu.bin`
