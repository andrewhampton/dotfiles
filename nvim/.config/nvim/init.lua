require('bootstrap')

if _G.vscode then
  -- Vim in vscode config
  require('vscode-plugins')
else
  -- Vim on the CLI config
  require('plugins')
  require('maps')
  require('vimconfig')
end

