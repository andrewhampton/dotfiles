local o = vim.o

return require('packer').startup(function()
  use 'chriskempson/base16-vim'
  use 'kyazdani42/nvim-web-devicons'
  use 'nvim-lua/plenary.nvim'
  use 'gpanders/editorconfig.nvim'
  use 'tpope/vim-commentary'
  use 'tpope/vim-surround'
  use 'wbthomason/packer.nvim'
  use 'nvim-telescope/telescope-fzy-native.nvim'

  use {
    'github/copilot.vim',
    config = function ()
      vim.g.copilot_node_command = "$NVM_DIR/versions/node/v16.15.0/bin/node"
    end
  }

  use {
    'arcticicestudio/nord-vim',
    config = function ()
      vim.cmd('colorscheme nord')
    end
  }

  -- Make <leader>gy yank a link to the current line in GitHub
  use {'ruifm/gitlinker.nvim', requires = 'nvim-lua/plenary.nvim', config = function() require('gitlinker').setup({mappings = nil}) end}

  use {'hoob3rt/lualine.nvim', requires = {'kyazdani42/nvim-web-devicons', opt = true}, config = function() require('evil_lualine') end}

  use {'neovim/nvim-lspconfig', config = function() require('lsp_setup') end}

  use {
    'TimUntersberger/neogit',
    requires = 'nvim-lua/plenary.nvim',
    config = function ()
      local neogit = require('neogit')
      neogit.setup {}
    end
  }

  use {
    'nvim-telescope/telescope.nvim',
    requires = {'nvim-lua/plenary.nvim', 'nvim-telescope/telescope-fzy-native.nvim'},
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
      telescope.load_extension('fzy_native')
    end
  }

  use {
    'sbdchd/neoformat',
    config = function ()
      vim.g.neoformat_try_node_exe = 1
    end
  }

  use {
    'nvim-treesitter/nvim-treesitter',
    run = ':TSUpdate',
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
  }

  use {"folke/which-key.nvim", config = function() require("which-key").setup() end}

  use {'lewis6991/gitsigns.nvim', requires = {'nvim-lua/plenary.nvim'}, config = function() require('gitsigns').setup() end}

  use {
    'karb94/neoscroll.nvim',
    config = function()
      require('neoscroll').setup({
        easing_function = 'quadratic'
      })
    end
  }
end)

