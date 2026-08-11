-- Make <Space> the leader
vim.g.mapleader = ' '
vim.keymap.set('n', '<Space>', '', { noremap = true, silent = true, desc = "Set leader" })

-- Default options for all mappings
local opts = { noremap = true, silent = true }

---------------------------------------
-- Normal mode maps (no leader)
---------------------------------------

vim.keymap.set('n', 'u', '<cmd>earlier<CR>', { desc = "Undo using undo trees", unpack(opts) })
vim.keymap.set('n', '<C-r>', '<cmd>later<CR>', { desc = "Redo using undo trees", unpack(opts) })

-- Quickfix
vim.keymap.set('n', 'gqq', '<cmd>copen<CR>', { desc = "Open quickfix", unpack(opts) })
vim.keymap.set('n', 'gqc', '<cmd>cclose<CR>', { desc = "Close quickfix", unpack(opts) })
vim.keymap.set('n', 'gqn', '<cmd>cnext<CR>', { desc = "Next quickfix item", unpack(opts) })
vim.keymap.set('n', 'gqp', '<cmd>cprev<CR>', { desc = "Previous quickfix item", unpack(opts) })
vim.keymap.set('n', 'gqon', '<cmd>cnewer<CR>', { desc = "Next quickfix list", unpack(opts) })
vim.keymap.set('n', 'gqoo', '<cmd>colder<CR>', { desc = "Previous quickfix list", unpack(opts) })

-- Clear search highlights
vim.keymap.set('n', '<C-l>', ':noh<CR>', { noremap = true, silent = false, desc = "Clear search highlights" })

---------------------------------------
-- Normal mode maps with <leader>
---------------------------------------

vim.keymap.set('n', '<leader>yf', function()
  local file = vim.fn.expand('%:p')
  local root = vim.fs.root(0, { '.git', '.jj' })
  local path = root and vim.fs.relpath(root, file) or file
  vim.fn.setreg('+', path)
  vim.notify('Copied ' .. path .. ' to clipboard')
end, { desc = "Copy file path to clipboard", unpack(opts) })

-- Find (see lua/finders.lua) -- deliberately not silent: these open the cmdline
vim.keymap.set('n', '<leader>ff', ':find ', { noremap = true, desc = "Find file by name" })
vim.keymap.set('n', '<leader>fs', ':Grep ', { noremap = true, desc = "Search project contents" })
vim.keymap.set('n', '<leader>fr', ':Recent ', { noremap = true, desc = "Open recent file" })

-- Diagnostics
vim.keymap.set('n', '<leader>dn', function() vim.diagnostic.jump({ count = 1 }) end, { desc = "Go to next diagnostic", unpack(opts) })
vim.keymap.set('n', '<leader>dp', function() vim.diagnostic.jump({ count = -1 }) end, { desc = "Go to previous diagnostic", unpack(opts) })
vim.keymap.set('n', '<leader>dd', function() vim.diagnostic.open_float({ scope = 'cursor' }) end, { desc = "Show diagnostic details", unpack(opts) })

-- Utilities
vim.keymap.set('n', '<leader>ur', ':checktime<CR>', { desc = "Reload file from disk", unpack(opts) })

-- JJ blame (jjsigns.nvim)
vim.keymap.set('n', '<leader>hb', '<cmd>JJSigns blame<CR>', { desc = "Full file blame", unpack(opts) })
vim.keymap.set('n', '<leader>ht', '<cmd>JJSigns toggle_current_line_blame<CR>', { desc = "Toggle line blame", unpack(opts) })
vim.keymap.set('n', '<leader>hs', '<cmd>JJSigns show_line_commit<CR>', { desc = "Show commit for current line", unpack(opts) })
