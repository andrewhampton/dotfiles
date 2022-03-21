local map = vim.api.nvim_set_keymap
local wk = require('which-key')

-- Make space the leader
map('n', '<Space>', '', {})
vim.g.mapleader = ' '

options = { noremap = true }

-- LSP navigation
map('n', 'gr', "<cmd>Telescope lsp_references<CR>", options)
map('n', 'gd', "<cmd>Telescope lsp_definitions<CR>", options) -- Goto the definition of the word under the cursor, if there's only one, otherwise show all options in Telescope
map('n', 'gi', "<cmd>Telescope lsp_implementations<CR>", options) -- Goto the implementation of the word under the cursor if there's only one, otherwise show all options in Telescope

wk.register({
  ["<s-space>"] = { vim.lsp.omnifunc },
}, { mode = "i" })

-- Normal mode maps
wk.register({
  f = {
    name = "find",
    s = { function () require('telescope.builtin').live_grep({hidden=true, cwd=require('util').gitRoot()}) end, "rg" },  -- Grep the current project (respects .gitconfig)
    f = { function () require('telescope.builtin').find_files({hidden=true, cwd=require('util').gitRoot()}) end, "files" }, -- Jump to files in the current project
    b = { "<cmd>Telescope buffers<CR>", "buffers" },    -- Jump to a different open buffer
    r = { "<cmd>Telescope oldfiles<CR>", "recent" },   -- Jump to a recent file (cross project)
    q = { "<cmd>Telescope quickfix<CR>", "quickfix" },
    t = { function () require('nvim-tree').toggle() end, 'tree' },
  },
  g = {
    name = "git",
    l = { '<cmd>Telescope git_commits<CR>', "commits" }, -- Lists git commits with diff preview, checkout action <cr>, reset mixed <C-r>m, reset soft <C-r>s and reset hard <C-r>h
    g = { '<cmd>Telescope git_bcommits<CR>', "buffer commits" }, -- Lists buffer's git commits with diff preview and checks them out on <cr>
    r = { '<cmd>Telescope git_branches<CR>', "branches" }, -- Lists all branches with log preview, checkout action <cr>, track action <C-t> and rebase action<C-r>
    s = { '<cmd>Telescope git_status<CR>', "status" }, -- Lists current changes per file with diff preview and add action. (Multi-selection still WIP)
    t = { '<cmd>Telescope git_stash<CR>', "stash" }, -- Lists stash items in current repository with ability to apply them on <cr>
    b = { '<cmd>GitBlameToggle<CR>', 'blame' },
    y = { function () copyAndOpenGitHubLink('n') end, 'github link' },
    h = {
      name = "hunk",
      j = { '<cmd>Gitsigns next_hunk<CR>', 'next hunk' },
      k = { '<cmd>Gitsigns prev_hunk<CR>', 'prev hunk' },
      s = { '<cmd>Gitsigns stage_hunk<CR>', 'stage hunk' },
      u = { '<cmd>Gitsigns undo_stage_hunk<CR>', 'unstage hunk' },
      r = { '<cmd>Gitsigns reset_hunk<CR>', 'reset hunk' },
      b = { '<cmd>Gitsigns blame_line<CR>', 'blame line' },
      t = { '<cmd>Gitsigns toggle_current_line_blame<CR>', 'toggle current line blame' },
    }
  },
  l = {
    name = "lsp",
    s = { "<cmd>Telescope lsp_document_symbols<CR>", "symbols" }, -- Lists LSP document symbols in the current buffer
    a = { "<cmd>Telescope lsp_code_actions<CR>", "actions" }, -- Lists any LSP actions for the word under the cursor, that can be triggered with <cr>
    d = { "<cmd>Telescope lsp_document_diagnostics<CR>", "diagnostics" }, -- Lists LSP diagnostics for the current buffer
    t = { "<cmd>Telescope lsp_type_definitions<Cr>", "types" }, -- Goto the definition of the type of the word under the cursor, if there's only one, otherwise show all options in Telescope|
    w = {
      name = "workspace",
      s = { "<cmd>Telescope lsp_workspace_symbols<CR>", "symbols" }, -- Lists LSP document symbols in the current workspace
      d = { "<cmd>Telescope lsp_workspace_diagnostics<CR>", "diagnostics" }, -- Lists LSP diagnostics for the current workspace if supported, otherwise searches in all open buffers
    }
  },
  o = {
    name = 'notes',
    t = { function () vim.cmd(os.date(':vsplit ~/notes/%Y-%m-%d.md')) end, 'open today' },
  },
  u = {
    name = 'util',
    r = { ':checktime<CR>', 'reload file' },
    c = { ':vsplit ~/dotfiles/nvim/.config/nvim/init.lua<cr>', 'edit vim config' },
  }
}, { prefix = "<leader>"})

-- Visual mode maps
wk.register({
  g = {
    y = { function () copyAndOpenGitHubLink('v') end, 'github link' },
  },
}, {
  prefix = "<leader>",
  mode = "v",
})

-- C-* maps
wk.register({
  ["<c-l>"] = { ':noh<cr>', 'clear highlights' },
}, {silent = false})

function copyAndOpenGitHubLink (mode)
  local gl = require("gitlinker")
  gl.get_buf_range_url(mode, {action_callback = function (url)
      require("gitlinker.actions").open_in_browser(url)
      require("gitlinker.actions").copy_to_clipboard(url)
    end
  })
end
