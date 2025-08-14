-- Ensure PATH includes opencode binary location (must be before bootstrap)
vim.env.PATH = vim.env.PATH .. ':' .. os.getenv('HOME') .. '/.opencode/bin'

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

  -- Short comment config
  require('short-comments')

  -- Git commit textwidth config
  require('commit-message-formatting')
end

