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
o.signcolumn = 'yes'

vim.cmd('colorscheme base16-tomorrow-night-eighties')

vim.cmd('hi SpellBad gui=undercurl')

-- Auto-rebalance windows on resize
vim.cmd([[
augroup autoResize
  autocmd!
  autocmd VimResized * wincmd =
augroup END
]])

-- Special config for text files
vim.cmd([[
augroup textBuffers
  autocmd!
  autocmd FileType markdown,text setlocal wrap spell list
augroup end
]])
