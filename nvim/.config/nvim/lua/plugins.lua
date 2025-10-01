local o = vim.o

-- Ensure common bin paths are available inside Neovim (esp. for GUI/launcher sessions)
local hb = "/opt/homebrew/bin"
local ul = "/usr/local/bin"
local lb = (os.getenv("HOME") or "") .. "/.local/bin"
local ocb = (os.getenv("HOME") or "") .. "/.opencode/bin"
vim.env.PATH = table.concat({ hb, ul, lb, ocb, vim.env.PATH or "" }, ":")
vim.fn.setenv('PATH', vim.env.PATH)

require("lazy").setup({
  'chriskempson/base16-vim',
  'kyazdani42/nvim-web-devicons',
  'nvim-lua/plenary.nvim',
  'gpanders/editorconfig.nvim',
  'tpope/vim-surround',
  'nvim-telescope/telescope-fzy-native.nvim',
  'nvim-telescope/telescope-fzf-native.nvim',
  'nvim-telescope/telescope-ui-select.nvim',
  'github/copilot.vim',
  {
    "sourcegraph/amp.nvim",
    branch = "main",
    lazy = false,
    opts = { auto_start = true, log_level = "info" },
  },
  {
    'williamboman/mason.nvim',
    cmd = 'Mason',
    keys = { { '<leader>cm', '<cmd>Mason<cr>', desc = 'Mason' } },
    build = ':MasonUpdate',
    event = 'VeryLazy',
    opts = {
      ensure_installed = {
        'typescript-language-server',
        'ruby-lsp',
        'herb-language-server',
        'rubocop',
        'prettier',
      },
    },
    dependencies = {
      'williamboman/mason-lspconfig.nvim',
    },
    config = function(_, opts)
      require('mason').setup(opts)
      local mr = require('mason-registry')
      mr:on('package:install:success', function()
        vim.defer_fn(function()
          require('lazy.core.handler.event').trigger({
            event = 'FileType',
            buf = vim.api.nvim_get_current_buf(),
          })
        end, 100)
      end)
      local function ensure_installed()
        for _, tool in ipairs(opts.ensure_installed or {}) do
          local p = mr.get_package(tool)
          if not p:is_installed() then
            p:install()
          end
        end
      end
      if mr.refresh then
        mr.refresh(ensure_installed)
      else
        ensure_installed()
      end

      -- Setup mason-lspconfig integration
      require('mason-lspconfig').setup({
        ensure_installed = { 'ts_ls', 'ruby_lsp' },
        automatic_installation = true,
      })
    end,
  },

  -- {
  --   'arcticicestudio/nord-vim',
  --   config = function ()
  --     vim.cmd('colorscheme nord')
  --   end
  -- },
  {
    "catppuccin/nvim",
    name = "catppuccin",
    priority = 1000,
    config = function()
      require("catppuccin").setup({
          styles = {
              conditionals = {},
          },
      })

      vim.cmd.colorscheme('catppuccin-mocha')
    end
  },

  {
    'j-hui/fidget.nvim',
    opts = {
      progress = {
        suppress_on_insert = true,
        ignore_done_already = true,
        display = {
          render_limit = 16,
          done_ttl = 3,
        },
      },
      notification = {
        window = {
          winblend = 0,
        },
      },
    },
  },

  -- Make <leader>gy yank a link to the current line in GitHub
  {'ruifm/gitlinker.nvim', dependencies = 'nvim-lua/plenary.nvim', config = function() require('gitlinker').setup({mappings = nil}) end},

  {'hoob3rt/lualine.nvim', dependencies = {'kyazdani42/nvim-web-devicons', lazy = true}, config = function() require('evil_lualine') end},

  {'neovim/nvim-lspconfig', dependencies = 'j-hui/fidget.nvim', config = function() require('lsp_setup') end},

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
    'nvim-treesitter/nvim-treesitter',
    build = ':TSUpdate',
    config = function ()
      require('nvim-treesitter.configs').setup {
        highlight = {
          enable = true
        },
        indent = {
          enable = true
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

  -- {
  --   'coder/claudecode.nvim',
  --   config = true,
  -- },
  {
    "knubie/vim-kitty-navigator",
    build = "cp ./*.py ~/.config/kitty/"
  }
})

