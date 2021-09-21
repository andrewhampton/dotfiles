local map = vim.api.nvim_set_keymap

-- Make space the leader
map('n', '<Space>', '', {})
vim.g.mapleader = ' '

options = { noremap = true }

-- Quickly edit config
map('n', '<leader>ev', ':vsplit ~/dotfiles/nvim/.config/nvim/init.lua<cr>', options)

-- Reload file from disk
map('n', '<leader>ct', ':checktime<CR>', options)

map('i', '<c-space>', '<cmd>lua vim.lsp.omnifunc()<cr>', options)

-- File switching
map('n', '<leader>fs', "<cmd>lua require('telescope.builtin').live_grep({hidden=true, cwd=require('util').gitRoot()})<CR>", options)  -- Grep the current project (respects .gitconfig)
map('n', '<leader>ff', "<cmd>lua require('telescope.builtin').find_files({hidden=true, cwd=require('util').gitRoot()})<CR>", options) -- Jump to files in the current project
map('n', '<leader>fb', "<cmd>Telescope buffers<CR>", options)    -- Jump to a different open buffer
map('n', '<leader>fr', "<cmd>Telescope oldfiles<CR>", options)   -- Jump to a recent file (cross project)

-- Vim suggest
map('n', '<leader>fq', "<cmd>Telescope quickfix<CR>", options)
map('n', '<leader>fp', "<cmd>Telescope spell_suggest<CR>", options)

-- LSP navigation
map('n', 'gr', "<cmd>Telescope lsp_references<CR>", options)
map('n', 'gd', "<cmd>Telescope lsp_definitions<CR>", options) -- Goto the definition of the word under the cursor, if there's only one, otherwise show all options in Telescope
map('n', 'gi', "<cmd>Telescope lsp_implementations<CR>", options) -- Goto the implementation of the word under the cursor if there's only one, otherwise show all options in Telescope
map('n', '<leader>lbs', "<cmd>Telescope lsp_document_symbols<CR>", options) -- Lists LSP document symbols in the current buffer
map('n', '<leader>lws', "<cmd>Telescope lsp_workspace_symbols<CR>", options) -- Lists LSP document symbols in the current workspace
map('n', '<leader>lca', "<cmd>Telescope lsp_code_actions<CR>", options) -- Lists any LSP actions for the word under the cursor, that can be triggered with <cr>
map('n', '<leader>lbd', "<cmd>Telescope lsp_document_diagnostics<CR>", options) -- Lists LSP diagnostics for the current buffer
map('n', '<leader>lwd', "<cmd>Telescope lsp_workspace_diagnostics<CR>", options) -- Lists LSP diagnostics for the current workspace if supported, otherwise searches in all open buffers
map('n', '<leader>ltd', "<cmd>Telescope lsp_type_definitions<Cr>", options) -- Goto the definition of the type of the word under the cursor, if there's only one, otherwise show all options in Telescope|

-- git
map('n', '<leader>gl', '<cmd>Telescope git_commits<CR>', options) -- Lists git commits with diff preview, checkout action <cr>, reset mixed <C-r>m, reset soft <C-r>s and reset hard <C-r>h
map('n', '<leader>gbl', '<cmd>Telescope git_bcommits<CR>', options) -- Lists buffer's git commits with diff preview and checks them out on <cr>
map('n', '<leader>gb', '<cmd>Telescope git_branches<CR>', options) -- Lists all branches with log preview, checkout action <cr>, track action <C-t> and rebase action<C-r>
map('n', '<leader>gs', '<cmd>Telescope git_status<CR>', options) -- Lists current changes per file with diff preview and add action. (Multi-selection still WIP)
map('n', '<leader>gt', '<cmd>Telescope git_stash<CR>', options) -- Lists stash items in current repository with ability to apply them on <cr>


-- neorg
-- Open today's notes
function openNorgToday ()
  filename = os.date("~/neorg/%Y-%m-%d.norg")
  vim.cmd('edit ' .. filename)
end

map('n', '<leader>ot', '<cmd>lua openNorgToday()<cr>', options)
