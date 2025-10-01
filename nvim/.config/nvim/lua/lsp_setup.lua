local telescope = require('telescope.builtin')
-- local wk = require('which-key')

-- Setup LSP capabilities
local capabilities = vim.lsp.protocol.make_client_capabilities()
-- Enable snippet support even without nvim-cmp
capabilities.textDocument.completion.completionItem.snippetSupport = true
capabilities.textDocument.completion.completionItem.resolveSupport = {
  properties = {
    'documentation',
    'detail',
    'additionalTextEdits',
  }
}

-- Modern diagnostic configuration
vim.diagnostic.config({
  virtual_text = {
    severity = { min = vim.diagnostic.severity.WARN },
    source = "if_many",
  },
  signs = true,
  underline = true,
  update_in_insert = false,
  severity_sort = true,
  -- float = {
  --   border = 'rounded',
  --   source = 'always',
  --   header = '',
  --   prefix = '',
  -- },
})

local on_attach = function(client)
  local opts = { noremap = true, silent = true, buffer = 0 }

  vim.keymap.set('n', 'K', vim.lsp.buf.hover, { desc = 'Hover details', unpack(opts) })
  vim.keymap.set('n', 'gd', function()
    local clients = vim.lsp.get_clients({ bufnr = 0 })
    local has_definition_support = false
    for _, client in ipairs(clients) do
      if client.server_capabilities.definitionProvider then
        has_definition_support = true
        break
      end
    end

    if has_definition_support then
      telescope.lsp_definitions()
    else
      vim.notify("LSP definition not supported, falling back to tag search")
      vim.cmd('normal! gd')
    end
  end, { desc = 'Go to definition', unpack(opts) })
  vim.keymap.set('n', 'gr', telescope.lsp_references, { desc = 'Go to references', unpack(opts) })
  vim.keymap.set('n', 'gI', telescope.lsp_implementations, { desc = 'Go to implementations', unpack(opts) })
  vim.keymap.set('n', 'gT', telescope.lsp_type_definitions, { desc = 'Go to type definition', unpack(opts) })

  vim.keymap.set('n', '<leader>lr', vim.lsp.buf.rename, { desc = 'LSP rename', unpack(opts) })
  vim.keymap.set('n', '<leader>lc', vim.lsp.buf.code_action, { desc = 'LSP code action', unpack(opts) })
  vim.keymap.set('n', '<leader>ld', telescope.lsp_document_symbols, { desc = 'LSP document symbols', unpack(opts) })

  vim.bo[0].omnifunc = 'v:lua.vim.lsp.omnifunc'
end

vim.lsp.config.ts_ls = {
  cmd = { 'typescript-language-server', '--stdio' },
  filetypes = { 'javascript', 'javascriptreact', 'javascript.jsx', 'typescript', 'typescriptreact', 'typescript.tsx' },
  root_markers = { "package.json", "tsconfig.json" },
  capabilities = capabilities,
  on_attach = on_attach,
  settings = {
    documentFormatting = false
  },
}
vim.lsp.enable('ts_ls')

-- lspconfig.solargraph.setup {
--   on_attach = on_attach,
--   capabilities = {
--     "definitions",
--     "typeDefinition",
--     "hover",
--     "signatureHelp",
--     "rename",
--     "publishDiagnostics",
--   },
--   filetypes = {"ruby", "rakefile"},
--   flags = {
--     debounce_text_changes = 150
--   },
--   root_dir = util.root_pattern("Gemfile", ".git"),
--   init_options = {
--     formatting = false
--   }
-- }

vim.lsp.config.ruby_lsp = {
  cmd = { 'ruby-lsp' },
  filetypes = { 'ruby', 'rakefile' },
  root_markers = { 'Gemfile', '.git' },
  capabilities = capabilities,
  on_attach = on_attach,
  settings = {
    formatter = 'none',  -- Disable LSP formatting, use Neoformat instead
  }
}
vim.lsp.enable('ruby_lsp')

vim.g.markdown_fenced_languages = {
  "ts=typescript"
}

vim.lsp.config.denols = {
  cmd = { 'deno', 'lsp' },
  filetypes = { 'javascript', 'javascriptreact', 'javascript.jsx', 'typescript', 'typescriptreact', 'typescript.tsx' },
  root_markers = { "deno.json", "deno.jsonc" },
  capabilities = capabilities,
  on_attach = on_attach,
}
vim.lsp.enable('denols')

vim.lsp.config.rust_analyzer = {
  cmd = { "rustup", "run", "stable", "rust-analyzer" },
  filetypes = { 'rust' },
  root_markers = { 'Cargo.toml', 'rust-project.json' },
  capabilities = capabilities,
  on_attach = on_attach,
  -- settings = {
  --   ["rust-analyzer"] = {
  --     assist = {
  --       importGranularity = "module",
  --       importPrefix = "by_self",
  --     },
  --     cargo = {
  --       loadOutDirsFromCheck = true
  --     },
  --     procMacro = {
  --       enable = true
  --     }
  --   }
  -- }
}
vim.lsp.enable('rust_analyzer')

vim.lsp.config.herb_ls = {
  cmd = { "herb-language-server", "--stdio" },
  filetypes = { "html", "eruby" },
  root_markers = { "Gemfile", ".git" },
  capabilities = capabilities,
  on_attach = on_attach,
}
vim.lsp.enable('herb_ls')
