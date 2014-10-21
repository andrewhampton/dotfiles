local grid = require "grid"
local hotkey = require "mjolnir.hotkey"
local application = require "mjolnir.application"

local mash =    {"cmd", "shift", "ctrl"}
local mashalt = {"cmd", "shift", "alt"}

hotkey.bind(mash, 'N', grid.pushwindow_nextscreen)

-- Snap Window
hotkey.bind(mash, 'U', grid.snap_northwest)
hotkey.bind(mash, 'I', grid.snap_north)
hotkey.bind(mash, 'O', grid.snap_northeast)

hotkey.bind(mash, 'J', grid.snap_west)
hotkey.bind(mash, 'K', grid.maximize_window)
hotkey.bind(mash, 'L', grid.snap_east)

hotkey.bind(mash, 'M', grid.snap_southwest)
hotkey.bind(mash, ',', grid.snap_south)
hotkey.bind(mash, '.', grid.snap_southeast)

hotkey.bind({'cmd'}, '1', function() application.launchorfocus("Firefox") end)
hotkey.bind({'cmd'}, '2', function() application.launchorfocus("Sublime Text") end)
hotkey.bind({'cmd'}, '3', function() application.launchorfocus("iTerm") end)
hotkey.bind({'cmd'}, '4', function() application.launchorfocus("Colloquy") end)
