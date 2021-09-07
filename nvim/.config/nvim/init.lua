require('bootstrap')

if _G.vscode then -- Vim in vscode config
  require('vscode-plugins')
else -- Vim on the CLI config
  -- Include 3rd party plugins
  require('plugins')

  -- Wire up key mappings
  require('maps')

  -- Vim settings customizations
  require('vimconfig')
end

