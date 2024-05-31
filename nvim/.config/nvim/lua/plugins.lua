local o = vim.o

require("lazy").setup({
  'chriskempson/base16-vim',
  'kyazdani42/nvim-web-devicons',
  'nvim-lua/plenary.nvim',
  -- 'gpanders/editorconfig.nvim',
  -- 'tpope/vim-commentary',
  'tpope/vim-surround',
  'nvim-telescope/telescope-fzy-native.nvim',
  'nvim-telescope/telescope-fzf-native.nvim',
  'nvim-telescope/telescope-ui-select.nvim',
  -- 'williamboman/mason.nvim,
  --

  {
    'github/copilot.vim',
    config = function ()
      vim.g.copilot_node_command = "/opt/homebrew/bin/node"
    end
  },

  {
    'arcticicestudio/nord-vim',
    config = function ()
      vim.cmd('colorscheme nord')
    end
  },

  {
    'j-hui/fidget.nvim',
    config = function () require('fidget').setup() end,
    branch = 'legacy' -- Stay on the legacy branch until the new version is rewritten
  },

  -- Make <leader>gy yank a link to the current line in GitHub
  {'ruifm/gitlinker.nvim', dependencies = 'nvim-lua/plenary.nvim', config = function() require('gitlinker').setup({mappings = nil}) end},

  {'hoob3rt/lualine.nvim', dependencies = {'kyazdani42/nvim-web-devicons', lazy = true}, config = function() require('evil_lualine') end},

  {'neovim/nvim-lspconfig', dependencies = 'j-hui/fidget.nvim', config = function() require('lsp_setup') end},

  -- {
  --   "jackMort/ChatGPT.nvim",
  --     event = "VeryLazy",
  --     config = function()
  --       require("chatgpt").setup({
  --         edit_with_instructions = {
  --           diff = true,
  --         },
  --         openai_params = {
  --           model = "gpt-4-turbo-preview",
  --           temperature = 0.2,
  --           top_p = 0.1,
  --           max_tokens = 1000,
  --         },
  --         openai_edit_params = {
  --           model = "gpt-4-turbo-preview",
  --           temperature = 0.2,
  --           top_p = 0.1,
  --         },
  --       })
  --     end,
  --     dependencies = {
  --       "MunifTanjim/nui.nvim",
  --       "nvim-lua/plenary.nvim",
  --       "nvim-telescope/telescope.nvim"
  --     }
  -- },

  {
    'TimUntersberger/neogit',
    dependencies = 'nvim-lua/plenary.nvim',
    branch = 'nightly', -- Stay on nightly until support for nvim 0.11 is merged
    config = function ()
      local neogit = require('neogit')
      neogit.setup {}
    end
  },

  {
    'nvim-telescope/telescope.nvim',
    dependencies = {
      'nvim-lua/plenary.nvim',
      {
        'nvim-telescope/telescope-fzf-native.nvim',
        build = 'make',
        cond = function()
          return vim.fn.executable 'make' == 1
        end,
      },
      { 'nvim-telescope/telescope-ui-select.nvim' },
      'nvim-telescope/telescope-fzf-native.nvim',
      'nvim-telescope/telescope-fzy-native.nvim',
      'neovim/nvim-lspconfig'
    },
    config = function()
      local telescope = require('telescope')
      telescope.setup({
        defaults = {
          path_display = { shorten = { len = 3, exclude = { 4, -2, -1 } } },
          layout_strategy = 'vertical',
          layout_config = {
            vertical = {
              width = function(_, max_cols, _)
                local border = math.floor(max_cols / 100 * 10)
                return max_cols - border
              end,
              height = function(_, _, max_rows)
                local border = math.floor(max_rows / 100 * 10)
                return max_rows - border
              end,
              preview_height = function(_, _, max_rows)
                local border = math.floor(max_rows / 100 * 10)
                return max_rows - 10 - border
              end,
            },
          },
        },
        extensions = {
          fzy_native = {
            override_generic_sorter = true,
            override_file_sorter = true,
          },
        },
      })
      -- telescope.load_extension('fzf')
      -- telescope.load_extension('fzy_native')
      pcall(require('telescope').load_extension, 'fzf')
      pcall(require('telescope').load_extension, 'ui-select')
    end
  },

  {
    'sbdchd/neoformat',
    config = function ()
      vim.g.neoformat_try_node_exe = 1
    end
  },

  {
    'nvim-treesitter/playground',
    config = function ()
      require "nvim-treesitter.configs".setup {
        playground = {
          enable = true,
          disable = {},
          updatetime = 25, -- Debounced time for highlighting nodes in the playground from source code
          persist_queries = false, -- Whether the query persists across vim sessions
          keybindings = {
            toggle_query_editor = 'o',
            toggle_hl_groups = 'i',
            toggle_injected_languages = 't',
            toggle_anonymous_nodes = 'a',
            toggle_language_display = 'I',
            focus_language = 'f',
            unfocus_language = 'F',
            update = 'R',
            goto_node = '<cr>',
            show_help = '?',
          },
        }
      }
    end
  },

  {
    'nvim-treesitter/nvim-treesitter',
    build = ':TSUpdate',
    config = function ()
      require('nvim-treesitter.configs').setup {
        highlight = {
          enable = true
        },
        indent = {
          enabled = true
        },
        ensure_installed = {
          "bash",
          "cmake",
          "css",
          "dockerfile",
          "graphql",
          "html",
          "http",
          "javascript",
          "jsdoc",
          "json",
          "latex",
          "lua",
          "make",
          "regex",
          "ruby",
          "scss",
          "toml",
          "tsx",
          "typescript",
          "vim",
          "yaml",
        }
      }
    end
  },

  {"folke/which-key.nvim", config = function() require("which-key").setup() end},

  {'lewis6991/gitsigns.nvim', dependencies = {'nvim-lua/plenary.nvim'}, config = function() require('gitsigns').setup() end},

  {
    'karb94/neoscroll.nvim',
    config = function()
      require('neoscroll').setup({
        easing_function = 'quadratic'
      })
    end
  },

  {
    'numToStr/Comment.nvim',
    config = function()
      require('Comment').setup()
    end
  },
})

