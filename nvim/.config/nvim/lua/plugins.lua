local o = vim.o

return require('packer').startup(function ()
  use 'chriskempson/base16-vim'
  use 'kyazdani42/nvim-web-devicons'
  use 'sgur/vim-editorconfig'
  use 'tpope/vim-commentary'
  use 'tpope/vim-endwise'
  use 'tpope/vim-surround'
  use 'wbthomason/packer.nvim'

  -- Make <leader>gy yank a link to the current line in github
  use {
    'ruifm/gitlinker.nvim',
    requires = 'nvim-lua/plenary.nvim',
    config = function ()
      require('gitlinker').setup()
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
    'TimUntersberger/neogit',
    requires = 'nvim-lua/plenary.nvim',
    config = function ()
      require('neogit').setup({})
    end
  }

  use {
    'lewis6991/gitsigns.nvim',
    requires = {
      'nvim-lua/plenary.nvim'
    },
    config = function ()
      require('gitsigns').setup()
    end
  }

  use {
    'neovim/nvim-lspconfig',
    config = function ()
      require('lsp_setup')
    end
  }

  use {
    'nvim-telescope/telescope.nvim',
    requires = { 'nvim-lua/plenary.nvim' },
    config = function ()
      require('telescope').setup({ })
    end
  }

  use {
    'nvim-treesitter/nvim-treesitter',
    run = ':TSUpdate'
  }
end)

