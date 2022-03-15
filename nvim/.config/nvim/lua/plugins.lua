local o = vim.o

return require('packer').startup(function()
  use 'chriskempson/base16-vim'
  use 'kyazdani42/nvim-web-devicons'
  use 'nvim-lua/plenary.nvim'
  use 'sgur/vim-editorconfig'
  use 'tpope/vim-commentary'
  use 'tpope/vim-surround'
  use 'wbthomason/packer.nvim'
  use 'github/copilot.vim'

  use {
    'arcticicestudio/nord-vim',
    config = function ()
      vim.cmd('colorscheme nord')
    end
  }

  -- Make <leader>gy yank a link to the current line in GitHub
  use {'ruifm/gitlinker.nvim', requires = 'nvim-lua/plenary.nvim', config = function() require('gitlinker').setup({mappings = nil}) end}

--   use {'hoob3rt/lualine.nvim', requires = {'kyazdani42/nvim-web-devicons', opt = true}, config = function() require('evil_lualine') end}

  use {'neovim/nvim-lspconfig', config = function() require('lsp_setup') end}

  use {'nvim-telescope/telescope-fzf-native.nvim', run = 'make'}

  use {
    'nvim-telescope/telescope.nvim',
    requires = {'nvim-lua/plenary.nvim', 'nvim-telescope/telescope-fzf-native.nvim'},
    config = function()
      require('telescope').setup({})
      require('telescope').load_extension('fzf')
    end
  }

  use {'nvim-treesitter/nvim-treesitter', run = ':TSUpdate'}

  use {"folke/which-key.nvim", config = function() require("which-key").setup() end}

  -- use {'lewis6991/gitsigns.nvim', requires = {'nvim-lua/plenary.nvim'}, config = function() require('gitsigns').setup() end}
end)

