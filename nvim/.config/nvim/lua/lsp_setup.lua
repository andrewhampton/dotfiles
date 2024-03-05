local lspconfig = require('lspconfig')
local telescope = require('telescope.builtin')
local wk = require('which-key')
local configs = require("lspconfig.configs")
local util = require("lspconfig.util")

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
  -- vim.api.nvim_buf_set_option(0, 'formatexpr', 'v:lua.vim.lsp.formatexpr()')

end

lspconfig.tsserver.setup {
  on_attach = on_attach,
  root_dir = lspconfig.util.root_pattern("package.json"),
  flags = {
    debounce_text_changes = 150
  },
  init_options = {
    documentFormatting = false
  },
}

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

if not configs.ruby_lsp then
	configs.ruby_lsp = {
		default_config = {
			cmd = { "ruby-lsp" },
			filetypes = { "ruby" },
			root_dir = util.root_pattern("Gemfile", ".git"),
			init_options = {
				enabledFeatures = {
          "formatting",
          "definition",
          "textdefinitions",
          "typeDefinition",
          "hover",
          "signatureHelp",
          "rename",
          "publishDiagnostics",
        },
        formatter = 'auto',
			},
			settings = {},
		},
		commands = {
			FormatRuby = {
				function()
					vim.lsp.buf.format({
						name = "ruby_lsp",
						async = true,
					})
				end,
				description = "Format using ruby-lsp",
			},
		},
	}
end

lspconfig.ruby_lsp.setup({ on_attach = on_attach })


vim.g.markdown_fenced_languages = {
  "ts=typescript"
}

lspconfig.denols.setup {
  on_attach = on_attach,
  root_dir = lspconfig.util.root_pattern("deno.json", "deno.jsonc")
}

lspconfig.rust_analyzer.setup {
  on_attach = on_attach,
  cmd = { "rustup", "run", "stable", "rust-analyzer" },
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
