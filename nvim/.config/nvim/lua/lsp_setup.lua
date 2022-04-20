local nvim_lsp = require('lspconfig')
local telescope = require('telescope.builtin')
local wk = require('which-key')
local cmp = require('cmp_nvim_lsp')

local on_attach = function(client)
  wk.register({
    K = { vim.lsp.buf.hover, 'Hover details' },
    g = {
      name = 'goto',
      d = { telescope.lsp_definitions, 'definition' },
      r = { telescope.lsp_references, 'references' },
      i = { telescope.lsp_implementations, 'implementations' },
      T = { telescope.lsp_type_definitions, 'type definition' },
    },
    ["<leader>l"] = {
      name = 'lsp',
      r = { vim.lsp.buf.rename, 'rename' },
      c = { vim.lsp.buf.code_action, 'code action' },
    }
  }, {
    buffer = 0 -- only the current buffer so we don't get lsp mappings in non-lsp buffers
  })

  vim.api.nvim_buf_set_option(0, 'omnifunc', 'v:lua.vim.lsp.omnifunc')
  vim.api.nvim_buf_set_option(0, 'formatexpr', 'v:lua.vim.lsp.formatexpr()')
end

local capabilities = cmp.update_capabilities(vim.lsp.protocol.make_client_capabilities())

nvim_lsp.tsserver.setup {
  on_attach = on_attach,
  flags = {
    debounce_text_changes = 150
  },
  init_options = {
    documentFormatting = false
  },
  capabilities = capabilities
}

nvim_lsp.solargraph.setup {
  on_attach = on_attach,
  flags = {
    debounce_text_changes = 150
  },
  init_options = {
    formatting = false
  },
  capabilities = capabilities
}
