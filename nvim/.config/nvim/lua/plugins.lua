local o = vim.o

return require('packer').startup(function ()
  use 'chriskempson/base16-vim'
  use 'kyazdani42/nvim-web-devicons'
  use 'neovim/nvim-lspconfig'
  use 'sgur/vim-editorconfig'
  use 'tpope/vim-commentary'
  use 'tpope/vim-endwise'
  use 'tpope/vim-surround'
  use 'wbthomason/packer.nvim'
  use 'vim-airline/vim-airline'

  -- Make <leader>gy yank a link to the current line in github
  use {
    'ruifm/gitlinker.nvim',
    requires = 'nvim-lua/plenary.nvim',
    config = function ()
      require('gitlinker').setup()
    end
  }

  -- Airline
  use {
    'vim-airline/vim-airline-themes',
    requires = 'vim-airline/vim-airline',
    config = function ()
      o.airline_theme = 'base16_tomorrow_night_eighties'
    end
  }

  -- lsp

  -- lsp completion engine
  use {
    "hrsh7th/nvim-cmp",
    requires = {
      "hrsh7th/vim-vsnip",
      "hrsh7th/cmp-buffer",
      "hrsh7th/cmp-nvim-lsp"
    }
  }

  -- telescope
  use {
    'nvim-telescope/telescope.nvim',
    requires = { 'nvim-lua/plenary.nvim' }
  }

  -- treesitter
  use {
    'nvim-treesitter/nvim-treesitter',
    run = ':TSUpdate'
  }
end)

