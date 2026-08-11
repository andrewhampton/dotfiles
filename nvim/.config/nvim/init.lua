-- Vim settings customizations
require('vimconfig')

-- Plugins, via the built-in vim.pack
require('plugins')

-- Incremental fuzzy finders on the command line
require('finders').setup()

-- Wire up key mappings
require('maps')

-- Short comment config
require('short-comments')

-- Git commit textwidth config
require('commit-message-formatting')
