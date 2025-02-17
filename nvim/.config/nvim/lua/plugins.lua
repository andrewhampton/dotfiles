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
  'github/copilot.vim',
  -- 'williamboman/mason.nvim,
  --


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

