
local mash =    {"cmd", "shift", "ctrl"}
local mashalt = {"cmd", "shift", "alt"}

hotkey.bind(mash, ';', function() ext.grid.snap(window.focusedwindow()) end)
hotkey.bind(mash, "'", function() fnutils.map(window.visiblewindows(), ext.grid.snap) end)

hotkey.bind(mash, '=', function() ext.grid.adjustwidth( 1) end)
hotkey.bind(mash, '-', function() ext.grid.adjustwidth(-1) end)

hotkey.bind(mash, 'N', ext.grid.pushwindow_nextscreen)

-- Snap Window
hotkey.bind(mash, 'U', ext.grid.snap_northwest)
hotkey.bind(mash, 'I', ext.grid.snap_north)
hotkey.bind(mash, 'O', ext.grid.snap_northeast)

hotkey.bind(mash, 'J', ext.grid.snap_west)
hotkey.bind(mash, 'K', ext.grid.maximize_window)
hotkey.bind(mash, 'L', ext.grid.snap_east)

hotkey.bind(mash, 'M', ext.grid.snap_southwest)
hotkey.bind(mash, ',', ext.grid.snap_south)
hotkey.bind(mash, '.', ext.grid.snap_southeast)

-- Nudge Window
hotkey.bind(mashalt, 'U', ext.grid.nudge_northwest)
hotkey.bind(mashalt, 'I', ext.grid.nudge_north)
hotkey.bind(mashalt, 'O', ext.grid.nudge_northeast)

hotkey.bind(mashalt, 'J', ext.grid.nudge_west)
hotkey.bind(mashalt, 'L', ext.grid.nudge_east)

hotkey.bind(mashalt, 'M', ext.grid.nudge_southwest)
hotkey.bind(mashalt, ',', ext.grid.nudge_south)
hotkey.bind(mashalt, '.', ext.grid.nudge_southeast)

hotkey.bind(mash, 'X', logger.show)
hotkey.bind(mash, "R", repl.open)

hotkey.bind({'ctrl'}, '3', function() application.launchorfocus("Firefox") end)
hotkey.bind({'ctrl'}, '4', function() application.launchorfocus("Sublime Text") end)
hotkey.bind({'ctrl'}, '5', function() application.launchorfocus("iTerm") end)
hotkey.bind({'ctrl'}, '6', function() application.launchorfocus("Colloquy") end)
