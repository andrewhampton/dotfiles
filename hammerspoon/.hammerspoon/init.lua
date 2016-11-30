local grid = require "grid"
local menubar = require "menubar"
local hyperterm = require "hyperterm"
local anycomplete = require "anycomplete"

-- hyperterm
hyperterm.init()

-- menubar
menubar.init()

local mash =    {"cmd", "shift", "ctrl"}
local mashalt = {"ctrl", "shift", "alt"}

-- anycomplete
hs.hotkey.bind(mash, "A", anycomplete.complete)
hs.hotkey.bind(mashalt, "A", anycomplete.complete)

-- window hints
hs.hints.showTitleThresh = 0
hs.hotkey.bind(mash, 'H', hs.hints.windowHints)
hs.hotkey.bind(mashalt, 'H', hs.hints.windowHints)

-- Change screen
hs.hotkey.bind(mash, 'N', grid.pushwindow_nextscreen)
hs.hotkey.bind(mashalt, 'N', grid.pushwindow_nextscreen)

-- Snap Window
hs.hotkey.bind(mash, 'U', grid.snap_northwest)
hs.hotkey.bind(mash, 'I', grid.snap_north)
hs.hotkey.bind(mash, 'O', grid.snap_northeast)

hs.hotkey.bind(mash, 'J', grid.snap_west)
hs.hotkey.bind(mash, 'K', grid.maximize_window)
hs.hotkey.bind(mash, 'L', grid.snap_east)

hs.hotkey.bind(mash, 'M', grid.snap_southwest)
hs.hotkey.bind(mash, ',', grid.snap_south)
hs.hotkey.bind(mash, '.', grid.snap_southeast)

hs.hotkey.bind(mashalt, 'U', grid.snap_northwest)
hs.hotkey.bind(mashalt, 'I', grid.snap_north)
hs.hotkey.bind(mashalt, 'O', grid.snap_northeast)

hs.hotkey.bind(mashalt, 'J', grid.snap_west)
hs.hotkey.bind(mashalt, 'K', grid.maximize_window)
hs.hotkey.bind(mashalt, 'L', grid.snap_east)

hs.hotkey.bind(mashalt, 'M', grid.snap_southwest)
hs.hotkey.bind(mashalt, ',', grid.snap_south)
hs.hotkey.bind(mashalt, '.', grid.snap_southeast)
