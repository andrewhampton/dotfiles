local o = vim.o

return require('packer').startup(function ()
  use 'chriskempson/base16-vim'
  use 'kyazdani42/nvim-web-devicons'
  use 'nvim-lua/plenary.nvim'
  use 'sgur/vim-editorconfig'
  use 'tpope/vim-commentary'
  use 'tpope/vim-endwise'
  use 'tpope/vim-surround'
  use 'tversteeg/registers.nvim'
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
    run = ':TSUpdate'
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

