local o = vim.o   -- Global options
local wo = vim.wo -- Window options
local bo = vim.bo -- Buffer options

o.autochdir = true
o.hidden = true
o.ignorecase = true
o.number = true
o.smartcase = true
o.splitbelow = true
o.splitright = true
o.termguicolors = true
o.undofile = true
o.wrap = false
o.clipboard = 'unnamedplus'

vim.cmd('colorscheme base16-tomorrow-night-eighties')

-- Auto-rebalance windows on resize
vim.cmd('augroup autoResize')
vim.cmd('  autocmd!')
vim.cmd('  autocmd VimResized * wincmd =')
vim.cmd('augroup END')

-- Special config for text files
vim.cmd('augroup textBuffers')
vim.cmd('  autocmd!')
vim.cmd('  autocmd FileType markdown,text setlocal wrap spell list')
vim.cmd('augroup end')
