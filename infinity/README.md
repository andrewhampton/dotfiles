# Infinity Keyboard Config

## Modify

1. Edit here: http://configurator.input.club/
2. to import: `cat MD1-HackerBlank.json| pbcopy` > import map > paste
3. download firmware

## Update keyboard

1. Extract downloaded firmware
2. cd into extracted dir
3. press button on bottom of keyboard and confirm led lights up
4. `dfu-util -D kiibohd.dfu.bin`
