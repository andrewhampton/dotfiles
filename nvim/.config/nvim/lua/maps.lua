local builtin = require('telescope.builtin')
local map = vim.api.nvim_set_keymap

-- Make space the leader
map('n', '<Space>', '', {})
vim.g.mapleader = ' '

options = { noremap = true }

-- Quickly edit config
map('n', '<leader>ev', ':vsplit $MYVIMRC<cr>', options)

-- Reload file from disk
map('n', '<leader>ct', ':checktime<CR>', options)

-- File switching
map('n', '<leader>fs', "<cmd>Telescope live_grep<CR>", options)
map('n', '<leader>ff', "<cmd>Telescope find_files<CR>", options)
map('n', '<leader>fb', "<cmd>Telescope buffers<CR>", options)
map('n', '<leader>fr', "<cmd>Telescope oldfiles<CR>", options)

-- Vim suggest
map('n', '<leader>fq', "<cmd>Telescope quickfix<CR>", options)
map('n', '<leader>fp', "<cmd>Telescope spell_suggest<CR>", options)

-- LSP
map('n', 'gr', "<cmd>Telescope lsp_references<CR>", options)
map('n', 'gd', "<cmd>Telescope lsp_definitions<CR>", options) -- Goto the definition of the word under the cursor, if there's only one, otherwise show all options in Telescope
map('n', 'gi', "<cmd>Telescope lsp_implementations<CR>", options) -- Goto the implementation of the word under the cursor if there's only one, otherwise show all options in Telescope
map('n', '<leader>lbs', "<cmd>Telescope lsp_document_symbols<CR>", options) -- Lists LSP document symbols in the current buffer
map('n', '<leader>lws', "<cmd>Telescope lsp_workspace_symbols<CR>", options) -- Lists LSP document symbols in the current workspace
map('n', '<leader>lca', "<cmd>Telescope lsp_code_actions<CR>", options) -- Lists any LSP actions for the word under the cursor, that can be triggered with <cr>
map('n', '<leader>lbd', "<cmd>Telescope lsp_document_diagnostics<CR>", options) -- Lists LSP diagnostics for the current buffer
map('n', '<leader>lwd', "<cmd>Telescope lsp_workspace_diagnostics<CR>", options) -- Lists LSP diagnostics for the current workspace if supported, otherwise searches in all open buffers

-- git
map('n', '<leader>gl', '<cmd>Telescope git_commits<CR>', options) -- Lists git commits with diff preview, checkout action <cr>, reset mixed <C-r>m, reset soft <C-r>s and reset hard <C-r>h
map('n', '<leader>gbl', '<cmd>Telescope git_bcommits<CR>', options) -- Lists buffer's git commits with diff preview and checks them out on <cr>
map('n', '<leader>gb', '<cmd>Telescope git_branches<CR>', options) -- Lists all branches with log preview, checkout action <cr>, track action <C-t> and rebase action<C-r>
map('n', '<leader>gs', '<cmd>Telescope git_status<CR>', options) -- Lists current changes per file with diff preview and add action. (Multi-selection still WIP)
map('n', '<leader>gt', '<cmd>Telescope git_stash<CR>', options) -- Lists stash items in current repository with ability to apply them on <cr>
