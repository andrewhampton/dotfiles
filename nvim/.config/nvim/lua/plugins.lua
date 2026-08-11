-- Plugins, managed by the built-in vim.pack (Neovim 0.12+).
-- Keep this list short. Everything else here should be stock Neovim.

vim.pack.add({
  { src = 'https://github.com/andrewhampton/jjsigns.nvim' },
})

require('jjsigns').setup({
  current_line_blame = true,
  current_line_blame_opts = {
    delay = 500,
    virt_text_pos = 'eol',
  },
})
