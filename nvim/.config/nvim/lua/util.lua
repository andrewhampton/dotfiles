local p = require 'plenary.path'

local util = {}

function util.gitRoot ()
  local path = util.gitRootPath()
  if not path then
    path = p:new('.')
  end

  return path:absolute()
end

function util.gitRootPath ()
  local path = p:new('.')
  local failsafe = 100
  repeat
    path = path:parent()
    failsafe = failsafe - 1
    if util.hasGit(path) then
      return path
    end
  until (path:absolute() == "/" and failsafe > 0) or util.hasGit(path)
end

function util.hasGit(path)
  local gitFolder = path:joinpath('.git')
  return gitFolder:exists()
end

function util.currentFileRelativeToGitRoot()
  local currentBuffer = vim.fn.expand('%:p')
  if not currentBuffer then
    return p:new('.'):absolute()
  end

  local gitRoot = util.gitRootPath()
  if not gitRoot then
    return currentBuffer
  end

  return p:new(currentBuffer):make_relative(gitRoot:absolute())
end

function util.editNorgToday ()
  filename = os.date("~/neorg/%Y-%m-%d.norg")
  vim.cmd('edit ' .. filename)
end


return util
