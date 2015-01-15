local grid = {}
local window = hs.window

function grid.snap(win, x, y, w, h)
  local newframe = {
    x = x,
    y = y,
    w = w,
    h = h,
  }
  win:setFrame(newframe, 0)
end

-- |XX|
-- |XX|
function grid.maximize_window()
  local win = window.focusedWindow()
  local s = win:screen():frame()
  grid.snap(win, s.x, 0, s.w, s.h)
end

-- |X |
-- |  |
function grid.snap_northwest()
  local win = window.focusedWindow()
  local s = win:screen():frame()
  grid.snap(win, s.x, 0, s.w/2, s.h/2)
end

-- |XX|
-- |  |
function grid.snap_north()
  local win = window.focusedWindow()
  local s = win:screen():frame()
  grid.snap(win, s.x, 0, s.w, s.h/2)
end

-- | X|
-- |  |
function grid.snap_northeast()
  local win = window.focusedWindow()
  local s = win:screen():frame()
  grid.snap(win, s.x+s.w/2, 0, s.w/2, s.h/2)
end

-- |X |
-- |X |
function grid.snap_west()
  local win = window.focusedWindow()
  local s = win:screen():frame()
  grid.snap(win, s.x, 0, s.w/2, s.h)
end


-- | X|
-- | X|
function grid.snap_east()
  local win = window.focusedWindow()
  local s = win:screen():frame()
  grid.snap(win, s.x+s.w/2, 0, s.w/2, s.h)
end

-- |  |
-- |X |
function grid.snap_southwest()
  local win = window.focusedWindow()
  local s = win:screen():frame()
  grid.snap(win, s.x, s.y+(s.h/2), s.w/2, s.h/2)
end

-- |  |
-- |XX|
function grid.snap_south()
  local win = window.focusedWindow()
  local s = win:screen():frame()
  grid.snap(win, s.x, s.y+(s.h/2), s.w, s.h/2)
end

-- |  |
-- | X|
function grid.snap_southeast()
  local win = window.focusedWindow()
  local s = win:screen():frame()
  grid.snap(win, s.x+s.w/2, s.y+(s.h/2), s.w/2, s.h/2)
end


function grid.pushwindow_nextscreen()
  local win = window.focusedWindow()
  local new_screen = win:screen():next():frame()
  local old_screen = win:screen():frame()
  local h_perc = win:size().h / old_screen.h
  local w_perc = win:size().w / old_screen.w
  local x_perc = math.abs(old_screen.x-win:topLeft().x) / old_screen.w
  local y_perc = (win:topLeft().y - old_screen.y) / old_screen.h
  grid.snap(win,
    new_screen.x + (new_screen.w * x_perc),
    new_screen.y + (new_screen.h * y_perc),
    new_screen.w * w_perc,
    new_screen.h * h_perc
  )
end

return grid
