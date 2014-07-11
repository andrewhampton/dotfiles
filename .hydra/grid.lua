ext.grid = {}

ext.grid.MARGINX = 1
ext.grid.MARGINY = 1
ext.grid.GRIDWIDTH = 2
ext.grid.NUDGE_SIZE = 75
ext.grid.DOCK_HEIGHT = 23

local function round(num, idp)
  local mult = 10^(idp or 0)
  return math.floor(num * mult + 0.5) / mult
end

function ext.grid.get(win)
  local winframe = win:frame()
  local screenrect = win:screen():frame_without_dock_or_menu()
  local thirdscreenwidth = screenrect.w / ext.grid.GRIDWIDTH
  local halfscreenheight = screenrect.h / 2
  return {
    x = round((winframe.x - screenrect.x) / thirdscreenwidth),
    y = round((winframe.y - screenrect.y) / halfscreenheight),
    w = math.max(1, round(winframe.w / thirdscreenwidth)),
    h = math.max(1, round(winframe.h / halfscreenheight)),
  }
end

function ext.grid.set(win, grid, screen)
  local screenrect = screen:frame_without_dock_or_menu()
  local thirdscreenwidth = screenrect.w / ext.grid.GRIDWIDTH
  local halfscreenheight = screenrect.h / 2
  local newframe = {
    x = (grid.x * thirdscreenwidth) + screenrect.x,
    y = (grid.y * halfscreenheight) + screenrect.y,
    w = grid.w * thirdscreenwidth,
    h = grid.h * halfscreenheight,
  }

  newframe.x = newframe.x + ext.grid.MARGINX
  newframe.y = newframe.y + ext.grid.MARGINY
  newframe.w = newframe.w - (ext.grid.MARGINX * 2)
  newframe.h = newframe.h - (ext.grid.MARGINY * 2)

  win:setframe(newframe)
end

function ext.grid.snap(win)
  if win:isstandard() then
    ext.grid.set(win, ext.grid.get(win), win:screen())
  end
end

function ext.grid.adjustwidth(by)
  ext.grid.GRIDWIDTH = math.max(1, ext.grid.GRIDWIDTH + by)
  hydra.alert("grid is now " .. tostring(ext.grid.GRIDWIDTH) .. " tiles wide", 1)
  fnutils.map(window.visiblewindows(), ext.grid.snap)
end

function ext.grid.adjust_focused_window(fn)
  local win = window.focusedwindow()
  local f = ext.grid.get(win)
  fn(f)
  ext.grid.set(win, f, win:screen())
end

function ext.grid.maximize_window()
  local win = window.focusedwindow()
  local f = {x = 0, y = 0, w = ext.grid.GRIDWIDTH, h = 2}
  ext.grid.set(win, f, win:screen())
end

function ext.grid.pushwindow_nextscreen()
  local win = window.focusedwindow()
  ext.grid.set(win, ext.grid.get(win), win:screen():next())
end

function ext.grid.pushwindow_prevscreen()
  local win = window.focusedwindow()
  ext.grid.set(win, ext.grid.get(win), win:screen():previous())
end

--- Snaps ---
function ext.grid.snap(win, x, y, w, h)
  local newframe = {
    x = x,
    y = y,
    w = w,
    h = h,
  }
  win:setframe(newframe)
end

function ext.grid.snap_northwest()
  local win = window.focusedwindow()
  local s = win:screen():frame_without_dock_or_menu()
  ext.grid.snap(win, 0, 0, s.w/2, s.h/2)
end

function ext.grid.snap_north()
  local win = window.focusedwindow()
  local s = win:screen():frame_without_dock_or_menu()
  ext.grid.snap(win, 0, 0, s.w, s.h/2)
end

function ext.grid.snap_northeast()
  local win = window.focusedwindow()
  local s = win:screen():frame_without_dock_or_menu()
  ext.grid.snap(win, s.w/2, 0, s.w/2, s.h/2)
end

function ext.grid.snap_west()
  local win = window.focusedwindow()
  local s = win:screen():frame_without_dock_or_menu()
  ext.grid.snap(win, 0, 0, s.w/2, s.h)
end

function ext.grid.snap_east()
  local win = window.focusedwindow()
  local s = win:screen():frame_without_dock_or_menu()
  ext.grid.snap(win, s.w/2, 0, s.w/2, s.h)
end

function ext.grid.snap_southwest()
  local win = window.focusedwindow()
  local s = win:screen():frame_without_dock_or_menu()
  ext.grid.snap(win, 0, (s.h/2)+ext.grid.DOCK_HEIGHT, s.w/2, s.h/2)
end

function ext.grid.snap_south()
  local win = window.focusedwindow()
  local s = win:screen():frame_without_dock_or_menu()
  ext.grid.snap(win, 0, (s.h/2)+ext.grid.DOCK_HEIGHT, s.w, s.h/2)
end

function ext.grid.snap_southeast()
  local win = window.focusedwindow()
  local s = win:screen():frame_without_dock_or_menu()
  ext.grid.snap(win, s.w/2, (s.h/2)+ext.grid.DOCK_HEIGHT, s.w/2, s.h/2)
end

--- Nudges ---
function ext.grid.nudge(win, x, y)
  local oldframe = win:frame()

  local newframe = {
    x = oldframe.x + x,
    y = oldframe.y + y,
    w = oldframe.w,
    h = oldframe.h,
  }
  win:setframe(newframe)
end

function ext.grid.nudge_west()
  ext.grid.nudge(window.focusedwindow(), -ext.grid.NUDGE_SIZE, 0)
end

function ext.grid.nudge_northwest()
  ext.grid.nudge(window.focusedwindow(), -ext.grid.NUDGE_SIZE, -ext.grid.NUDGE_SIZE)
end

function ext.grid.nudge_north()
  ext.grid.nudge(window.focusedwindow(), 0, -ext.grid.NUDGE_SIZE)
end

function ext.grid.nudge_northeast()
  ext.grid.nudge(window.focusedwindow(), ext.grid.NUDGE_SIZE, -ext.grid.NUDGE_SIZE)
end

function ext.grid.nudge_east()
  ext.grid.nudge(window.focusedwindow(), ext.grid.NUDGE_SIZE, 0)
end

function ext.grid.nudge_southeast()
  ext.grid.nudge(window.focusedwindow(), ext.grid.NUDGE_SIZE, ext.grid.NUDGE_SIZE)
end

function ext.grid.nudge_south()
  ext.grid.nudge(window.focusedwindow(), 0, ext.grid.NUDGE_SIZE)
end

function ext.grid.nudge_southwest()
  ext.grid.nudge(window.focusedwindow(), -ext.grid.NUDGE_SIZE, ext.grid.NUDGE_SIZE)
end
