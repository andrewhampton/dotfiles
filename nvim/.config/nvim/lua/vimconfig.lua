local o = vim.o   -- Global options
local wo = vim.wo -- Window options
local bo = vim.bo -- Buffer options

o.hidden = true
o.wrap = false
o.number = true
o.smartcase = true
o.splitbelow = true
o.splitright = true
o.termguicolors = true
o.undofile = true

o.clipboard = 'unnamedplus'

-- Remove node_modules from vim-rooter
o.rooter_patterns = {'.git', '_darcs', '.hg', '.bzr', '.svn'}

vim.cmd('colorscheme base16-tomorrow-night-eighties')

require('gitlinker').setup()

