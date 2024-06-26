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
o.mouse = 'c'
o.swapfile = false

vim.g.editorconfig = true

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
  pattern = { "gitcommit", "*" },
  callback = function ()
    local textFiles = { 'gitcommit', 'markdown' }

    local data = {
      buf = vim.fn.expand("<abuf>"),
      file = vim.fn.expand("<afile>"),
      match = vim.fn.expand("<amatch>"),
    }

    if vim.tbl_contains(textFiles, data.match) then
      wo.spell = true
      -- wo.list = true
    else
      wo.wrap = false
      wo.spell = false
      wo.list = false
    end
  end
})

api.nvim_create_augroup("prettier", { clear = true })
api.nvim_create_autocmd("BufWritePre", {
  group = "prettier",
  pattern = { "*.js", "*.ts", "*.jsx", "*.tsx", "*.css", "*.sass", "*.scss" },
  callback = function ()
    vim.cmd('Neoformat')
  end
})

-- api.nvim_create_augroup("ruby-lsp-formatting", { clear = true })
-- api.nvim_create_autocmd("BufWritePre", {
--   group = "ruby-lsp-formatting",
--   pattern = { "*.rb" },
--   callback = function ()
--     vim.cmd('FormatRuby')
--   end
-- })

-- Add the Jump command for git jump
vim.cmd([[
command! -bar -nargs=* Jump cexpr system('git jump ' . expand(<q-args>))
]])

-- [[ Highlight on yank ]]
-- See `:help vim.highlight.on_yank()`
local highlight_group = vim.api.nvim_create_augroup('YankHighlight', { clear = true })
vim.api.nvim_create_autocmd('TextYankPost', {
  callback = function()
    vim.highlight.on_yank()
  end,
  group = highlight_group,
  pattern = '*',
})
