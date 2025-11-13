local p = require 'plenary.path'

local util = {}

function util.gitRoot ()
  local path = util.repoRootPath()
  if not path then
    path = p:new('.')
  end

  return path:absolute()
end

function util.repoRootPath ()
  local path = p:new('.')
  local failsafe = 100
  repeat
    path = path:parent()
    failsafe = failsafe - 1
    if util.hasRepo(path) then
      return path
    end
  until (path:absolute() == "/" and failsafe > 0) or util.hasRepo(path)
end

function util.hasRepo(path)
  local gitFolder = path:joinpath('.git')
  local jjFolder = path:joinpath('.jj')
  return gitFolder:exists() or jjFolder:exists()
end

function util.currentFileRelativeToGitRoot()
  local currentBuffer = vim.fn.expand('%:p')
  if not currentBuffer then
    return p:new('.'):absolute()
  end

  local gitRoot = util.repoRootPath()
  if not gitRoot then
    return currentBuffer
  end

  return p:new(currentBuffer):make_relative(gitRoot:absolute())
end

function util.editNorgToday ()
  local filename = os.date("~/neorg/%Y-%m-%d.norg")
  vim.cmd('edit ' .. filename)
end


return util
