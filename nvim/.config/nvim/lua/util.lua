local p = require 'plenary.path'

local util = {}

function util.gitRoot ()
  local path = p:new('.')
  local failsafe = 100
  repeat
    path = path:parent()
    failsafe = failsafe - 1
    if util.hasGit(path) then
      return path:absolute()
    end
  until (path:absolute() == "/" and failsafe > 0) or util.hasGit(path)

  path = p:new('.')
  return path:absolute()
end

function util.hasGit(path)
  local gitFolder = path:joinpath('.git')
  return gitFolder:exists() and gitFolder:is_dir()
end

return util
