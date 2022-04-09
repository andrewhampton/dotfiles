local o = vim.o   -- Global options
local wo = vim.wo -- Window options
local bo = vim.bo -- Buffer options
local api = vim.api -- Vim API

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

-- vim.cmd('colorscheme base16-tomorrow-night-eighties')

vim.cmd('hi SpellBad gui=undercurl')

api.nvim_create_augroup("autoResize", { clear = true })
api.nvim_create_autocmd("VimResized", {
  group = "autoResize",
  command = "wincmd ="
})

-- Special config for text files
api.nvim_create_augroup("textBuffers", { clear = true })
api.nvim_create_autocmd("FileType", {
  group = "textBuffers",
  pattern = { "markdown", "text"},
  callback = function ()
    o.wrap = true
    o.spell = true
    o.list = true
  end
})

-- Add the Jump command for git jump
vim.cmd([[
command! -bar -nargs=* Jump cexpr system('git jump ' . expand(<q-args>))
]])
