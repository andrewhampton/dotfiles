local util = require 'util'

-- Make <Space> the leader
vim.g.mapleader = ' '
vim.keymap.set('n', '<Space>', '', { noremap = true, silent = true, desc = "Set leader" })

-- Helper function from your original code
local function copyAndOpenGitHubLink(mode)
  local gl = require("gitlinker")
  gl.get_buf_range_url(mode, {
    action_callback = function (url)
      require("gitlinker.actions").open_in_browser(url)
      require("gitlinker.actions").copy_to_clipboard(url)
    end
  })
end

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

-- Git hunk navigation
vim.keymap.set('n', ']h', function() require("gitsigns").next_hunk() end, { desc = "Next git hunk", unpack(opts) })
vim.keymap.set('n', '[h', function() require("gitsigns").prev_hunk() end, { desc = "Previous git hunk", unpack(opts) })

---------------------------------------
-- Normal mode maps with <leader>
---------------------------------------

-- AI (Copilot)
vim.keymap.set('n', '<leader>cp', '<cmd>Copilot panel<CR>', { desc = "Open Copilot panel", unpack(opts) })

vim.keymap.set('n', '<leader>yf', function()
  local path = util.currentFileRelativeToGitRoot()
  vim.fn.setreg('+', path)
  vim.notify('Copied ' .. path .. ' to clipboard')
end, { desc = "Copy file path to clipboard", unpack(opts) })

-- Diagnostics
vim.keymap.set('n', '<leader>dn', vim.diagnostic.goto_next, { desc = "Go to next diagnostic", unpack(opts) })
vim.keymap.set('n', '<leader>dp', vim.diagnostic.goto_prev, { desc = "Go to previous diagnostic", unpack(opts) })
vim.keymap.set('n', '<leader>dd', vim.diagnostic.open_float, { desc = "Show diagnostic details", unpack(opts) })
vim.keymap.set('n', '<leader>da', '<cmd>Telescope diagnostics<CR>', { desc = "List all diagnostics", unpack(opts) })

-- Find/Telescope
vim.keymap.set('n', '<leader>fs', function()
  require('telescope.builtin').live_grep({ hidden = true, cwd = require('util').gitRoot() })
end, { desc = "Search project with live_grep", unpack(opts) })

vim.keymap.set('n', '<leader>ff', function()
  require('telescope.builtin').find_files({ hidden = true, cwd = require('util').gitRoot() })
end, { desc = "Find files in project", unpack(opts) })

vim.keymap.set('n', '<leader>fb', '<cmd>Telescope buffers<CR>', { desc = "List open buffers", unpack(opts) })
vim.keymap.set('n', '<leader>fr', '<cmd>Telescope oldfiles<CR>', { desc = "List recent files", unpack(opts) })
vim.keymap.set('n', '<leader>fq', '<cmd>Telescope quickfix<CR>', { desc = "List quickfix items", unpack(opts) })
-- vim.keymap.set('n', '<leader>ft', function() require('nvim-tree').toggle() end, { desc = "Toggle file tree", unpack(opts) })

-- Git related
vim.keymap.set('n', '<leader>gl', '<cmd>Telescope git_commits<CR>', { desc = "List commits", unpack(opts) })
vim.keymap.set('n', '<leader>gg', '<cmd>Telescope git_bcommits<CR>', { desc = "List buffer commits", unpack(opts) })
vim.keymap.set('n', '<leader>gr', '<cmd>Telescope git_branches<CR>', { desc = "List branches", unpack(opts) })
vim.keymap.set('n', '<leader>gs', '<cmd>Telescope git_status<CR>', { desc = "Git status", unpack(opts) })
vim.keymap.set('n', '<leader>gt', '<cmd>Telescope git_stash<CR>', { desc = "List git stashes", unpack(opts) })
vim.keymap.set('n', '<leader>gy', function() copyAndOpenGitHubLink('n') end, { desc = "Open GitHub link", unpack(opts) })

-- Git hunks
vim.keymap.set('n', '<leader>ghj', '<cmd>Gitsigns next_hunk<CR>', { desc = "Next git hunk", unpack(opts) })
vim.keymap.set('n', '<leader>ghk', '<cmd>Gitsigns prev_hunk<CR>', { desc = "Previous git hunk", unpack(opts) })
vim.keymap.set('n', '<leader>ght', '<cmd>Gitsigns toggle_current_line_blame<CR>', { desc = "Toggle current line blame", unpack(opts) })

-- Utilities
vim.keymap.set('n', '<leader>ur', ':checktime<CR>', { desc = "Reload file from disk", unpack(opts) })
vim.keymap.set('n', '<leader>uc', function() require('telescope.builtin').find_files({ cwd = '~/dotfiles/nvim' }) end, { desc = "Open Neovim config", unpack(opts) })

-- Additional hunk mappings under <leader>h
vim.keymap.set('n', '<leader>hs', function() require('gitsigns').stage_hunk() end, { desc = "Stage hunk", unpack(opts) })
vim.keymap.set('n', '<leader>hr', function() require('gitsigns').reset_hunk() end, { desc = "Reset hunk", unpack(opts) })
vim.keymap.set('n', '<leader>hu', function() require('gitsigns').undo_stage_hunk() end, { desc = "Unstage hunk", unpack(opts) })
vim.keymap.set('n', '<leader>hb', function() require('gitsigns').blame_line({full=true}) end, { desc = "Blame line (full)", unpack(opts) })
vim.keymap.set('n', '<leader>hd', function() require('gitsigns').diffthis() end, { desc = "Diff this", unpack(opts) })

---------------------------------------
-- Visual mode maps
---------------------------------------
vim.keymap.set('v', '<leader>gy', function() copyAndOpenGitHubLink('v') end, { desc = "GitHub link for selection", unpack(opts) })

vim.keymap.set('v', '<leader>hs', function() require('gitsigns').stage_hunk({vim.fn.line("'<"), vim.fn.line("'>")}) end,
  { desc = "Stage selected hunk", unpack(opts) })
vim.keymap.set('v', '<leader>hr', function() require('gitsigns').reset_hunk({vim.fn.line("'<"), vim.fn.line("'>")}) end,
  { desc = "Reset selected hunk", unpack(opts) })
vim.keymap.set('v', '<leader>hu', function() require('gitsigns').undo_stage_hunk({vim.fn.line("'<"), vim.fn.line("'>")}) end,
  { desc = "Unstage selected hunk", unpack(opts) })

---------------------------------------
-- Other modes
---------------------------------------
-- Pane navigation with Alt+hjkl
vim.keymap.set('n', '<M-h>', '<C-w>h', { desc = "Move to left pane", unpack(opts) })
vim.keymap.set('n', '<M-j>', '<C-w>j', { desc = "Move to bottom pane", unpack(opts) })
vim.keymap.set('n', '<M-k>', '<C-w>k', { desc = "Move to top pane", unpack(opts) })
vim.keymap.set('n', '<M-l>', '<C-w>l', { desc = "Move to right pane", unpack(opts) })

-- Clear search highlights
vim.keymap.set('n', '<C-l>', ':noh<CR>', { noremap = true, silent = false, desc = "Clear search highlights" })

-- Insert mode Copilot panel
vim.keymap.set('i', '<C-c>', '<cmd>Copilot panel<CR>', { noremap = true, silent = true, desc = "Open Copilot panel" })

vim.keymap.set('n', '<leader>ac', '<cmd>ClaudeCode<cr>', { desc = "Toggle Claude Code", noremap = true, silent = true })
vim.keymap.set('n', '<leader>af', '<cmd>ClaudeCodeFocus<cr>', { desc = "Focus Claude Code", noremap = true, silent = true })
vim.keymap.set('n', '<leader>ar', '<cmd>ClaudeCode --resume<cr>', { desc = "Resume Claude Code", noremap = true, silent = true })
vim.keymap.set('n', '<leader>aC', '<cmd>ClaudeCode --continue<cr>', { desc = "Continue Claude Code", noremap = true, silent = true })
vim.keymap.set('n', '<leader>ab', '<cmd>ClaudeCodeAdd %<cr>', { desc = "Add current buffer to Claude Code", noremap = true, silent = true })
vim.keymap.set('v', '<leader>as', '<cmd>ClaudeCodeSend<cr>', { desc = "Send selection to Claude Code", noremap = true, silent = true })
vim.keymap.set('n', '<leader>aa', '<cmd>ClaudeCodeDiffAccept<cr>', { desc = "Accept Claude Code diff", noremap = true, silent = true })
vim.keymap.set('n', '<leader>ad', '<cmd>ClaudeCodeDiffDeny<cr>', { desc = "Deny Claude Code diff", noremap = true, silent = true })
