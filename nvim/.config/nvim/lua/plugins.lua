local o = vim.o

return require('packer').startup(function ()
  use 'chriskempson/base16-vim'
  use 'kyazdani42/nvim-web-devicons'
  use 'nvim-lua/plenary.nvim'
  use 'sgur/vim-editorconfig'
  use 'tpope/vim-commentary'
  use 'tpope/vim-endwise'
  use 'tpope/vim-surround'
  use 'wbthomason/packer.nvim'

  -- Make <leader>gy yank a link to the current line in GitHub
  use {
    'ruifm/gitlinker.nvim',
    requires = 'nvim-lua/plenary.nvim',
    config = function ()
      require('gitlinker').setup({
        mappings = nil
      })
    end
  }

  use {
    'hoob3rt/lualine.nvim',
    requires = {'kyazdani42/nvim-web-devicons', opt = true},
    config = function ()
      require('evil_lualine')
    end
  }

  use {
    'neovim/nvim-lspconfig',
    config = function ()
      require('lsp_setup')
    end
  }

  use {
    'nvim-telescope/telescope-fzf-native.nvim',
    run = 'make'
  }

  use {
    'nvim-telescope/telescope.nvim',
    requires = { 'nvim-lua/plenary.nvim', 'nvim-telescope/telescope-fzf-native.nvim' },
    config = function ()
      require('telescope').setup({ })
      require('telescope').load_extension('fzf')
    end
  }

  use {
    'nvim-treesitter/nvim-treesitter',
    run = ':TSUpdate',
    config = function ()
      local parser_configs = require('nvim-treesitter.parsers').get_parser_configs()

      parser_configs.norg = {
        install_info = {
          url = "https://github.com/nvim-neorg/tree-sitter-norg",
          files = { "src/parser.c", "src/scanner.cc" },
          branch = "main"
        },
      }

      require'nvim-treesitter.configs'.setup {
        ensure_installed = "maintained",
        highlight = {
          enable = true,
          additional_vim_regex_highlighting = false,
        },
        indent = {
          enable = true
        }
      }
    end
  }

  use {
    "nvim-neorg/neorg",
      config = function()
        require('neorg').setup {
          -- Tell Neorg what modules to load
          load = {
            ["core.defaults"] = {}, -- Load all the default modules
            ["core.norg.concealer"] = {}, -- Allows for use of icons
            ["core.norg.dirman"] = { -- Manage your directories with Neorg
              config = {
                workspaces = {
                  my_workspace = "~/neorg"
                }
              }
            }
          },
        }
      end,
    requires = "nvim-lua/plenary.nvim",
    after = "nvim-treesitter"
  }

  use {
    'lewis6991/spellsitter.nvim',
    after = 'nvim-treesitter',
    config = function ()
      require('spellsitter').setup()
    end
  }

  use {
    "folke/which-key.nvim",
    config = function()
      require("which-key").setup {
        plugins = {
          spelling = { enabled = true }
        }
      }
    end
  }

  -- I want neogit and gitsigns, but they give me "EMFILE: too many open files"
  -- errors when lsp things are happening.
  --
  -- Seems to be related to these closed issues:
  -- https://github.com/lewis6991/gitsigns.nvim/issues/168
  -- https://github.com/TimUntersberger/neogit/issues/160
  --
  -- And potentially this open issue:
  -- https://github.com/nvim-lua/plenary.nvim/issues/222
  --
  -- use {
  --   'TimUntersberger/neogit',
  --   requires = 'nvim-lua/plenary.nvim',
  --   config = function ()
  --     require('neogit').setup({})
  --   end
  -- }
  -- use {
  --   'lewis6991/gitsigns.nvim',
  --   requires = {
  --     'nvim-lua/plenary.nvim'
  --   },
  --   config = function ()
  --     require('gitsigns').setup()
  --   end
  -- }
end)

