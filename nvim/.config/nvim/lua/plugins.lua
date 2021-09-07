local o = vim.o

return require('packer').startup(function ()
  use 'airblade/vim-rooter'
  use 'chriskempson/base16-vim'
  use 'kyazdani42/nvim-web-devicons'
  use 'tpope/vim-commentary'
  use 'tpope/vim-surround'
  use 'wbthomason/packer.nvim'
  use 'sgur/vim-editorconfig'
  use 'tpope/vim-endwise'

  -- Airline
  use 'vim-airline/vim-airline'
  use 'vim-airline/vim-airline-themes'
  _G.airline_theme = 'base16_tomorrow_night_eighties'

  -- lsp
  use 'neovim/nvim-lspconfig'

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
    requires = { {'nvim-lua/plenary.nvim'} }
  }

  -- treesitter
  use {
    'nvim-treesitter/nvim-treesitter',
    run = ':TSUpdate'
  }
end)
