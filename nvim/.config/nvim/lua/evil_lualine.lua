-- Modified from https://gist.github.com/hoob3rt/b200435a765ca18f09f83580a606b878
local lualine = require 'lualine'
local util = require 'util'

-- Color table for highlights based on tomorrow night eighties
-- local colors = {
--   background = '#2d2d2d',
--   current_line = '#393939',
--   selection = '#515151',
--   foreground = '#cccccc',
--   comment = '#999999',
--   red = '#f2777a',
--   orange = '#f99157',
--   yellow = '#ffcc66',
--   green = '#99cc99',
--   aqua = '#66cccc',
--   blue = '#6699cc',
--   purple = '#cc99cc',
-- }
-- Color table for highlights based on Nord
local colors = {
  background = '#2E3440',
  current_line = '#3b4252',
  selection = '#d8dee9',
  foreground = '#d8dee9',
  comment = '#5e81ac',
  red = '#BF616A',
  orange = '#d08770',
  yellow = '#ebcb8b',
  green = '#a3be8c',
  aqua = '#88c0d0',
  blue = '#5e81ac',
  purple = '#b48ead',
}

local conditions = {
  buffer_not_empty = function() return vim.fn.empty(vim.fn.expand('%:t')) ~= 1 end,
  hide_in_width = function() return vim.fn.winwidth(0) > 80 end,
  check_git_workspace = function()
    local filepath = vim.fn.expand('%:p:h')
    local gitdir = vim.fn.finddir('.git', filepath .. ';')
    return gitdir and #gitdir > 0 and #gitdir < #filepath
  end
}

-- Config
local config = {
  options = {
    -- Disable sections and component separators
    component_separators = "",
    section_separators = "",
    theme = {
      -- We are going to use lualine_c an lualine_x as left and
      -- right section. Both are highlighted by c theme .  So we
      -- are just setting default looks o statusline
      normal = {c = {fg = colors.foreground, bg = colors.current_line}},
      inactive = {c = {fg = colors.foreground, bg = colors.current_line}}
    }
  },
  sections = {
    -- these are to remove the defaults
    lualine_a = {},
    lualine_b = {},
    lualine_y = {},
    lualine_z = {},
    -- These will be filled later
    lualine_c = {},
    lualine_x = {}
  },
  inactive_sections = {
    -- these are to remove the defaults
    lualine_a = {},
    lualine_v = {},
    lualine_y = {},
    lualine_z = {},
    lualine_c = {},
    lualine_x = {}
  }
}

-- Inserts a component in lualine_c at left section
local function ins_left(component)
  table.insert(config.sections.lualine_c, component)
end

-- Inserts a component in lualine_x ot right section
local function ins_right(component)
  table.insert(config.sections.lualine_x, component)
end

ins_left {
  function () return util.currentFileRelativeToGitRoot() end,
  color = {fg = colors.blue, gui = 'bold'},
}

table.insert(config.inactive_sections.lualine_c, {
  function () return util.currentFileRelativeToGitRoot() end,
  color = {fg = colors.blue, gui = 'bold'},
})

ins_left {
  'branch',
  icon = '',
  condition = conditions.check_git_workspace,
  color = {fg = colors.aqua, gui = 'bold'}
}

ins_left {
  'diff',
  -- Is it me or the symbol for modified us really weird
  symbols = {added = ' ', modified = '柳 ', removed = ' '},
  color_added = colors.green,
  color_modified = colors.orange,
  color_removed = colors.red,
  condition = conditions.hide_in_width
}


ins_right {
  'diagnostics',
  sources = {'nvim_lsp'},
  symbols = {error = ' ', warn = ' ', info = ' '},
  color_error = colors.red,
  color_warn = colors.yellow,
  color_info = colors.aqua
}

ins_right {
  -- Lsp server name .
  function()
    local msg = ''
    local buf_ft = vim.bo[0].filetype
    local clients = vim.lsp.get_clients()
    if next(clients) == nil then return msg end
    for _, client in ipairs(clients) do
      local filetypes = client.config.filetypes
      if filetypes and vim.fn.index(filetypes, buf_ft) ~= -1 then
        return client.name
      end
    end
    return msg
  end,
  icon = '',
  color = {fg = colors.yellow, gui = 'bold'}
}

-- Add components to right sections
ins_right {
  'o:encoding', -- option component same as &encoding in viml
  upper = true, -- I'm not sure why it's upper case either ;)
  condition = conditions.hide_in_width,
  color = {fg = colors.comment, gui = 'bold'}
}

ins_right {
  'fileformat',
  upper = true,
  icons_enabled = false, -- I think icons are cool but Eviline doesn't have them. sigh
  color = {fg = colors.comment, gui = 'bold'}
}

ins_right {
  'location',
  color = {fg = colors.aqua, gui = 'bold'}
}

ins_right {'progress', color = {fg = colors.purple, gui = 'bold'}}

-- local api = vim.api
-- local ts_utils = require("nvim-treesitter.ts_utils")
--
-- -- Add treesitter info at cursor location to statusline
-- ins_right {
--   function ()
--     local cursor = api.nvim_win_get_cursor(0)
--     local cursor_node = ts_utils.get_node_at_cursor()
--     return (cursor_node:type()) .. ' ' .. cursor[1]-1 .. ':' .. cursor[2]-1
--   end,
--   color = {fg = colors.purple, gui = 'bold'}
-- }
--
-- -- Add treesitter info at character before cursor location to statusline
-- ins_right {
--   function ()
--     local cursor = api.nvim_win_get_cursor(0)
--     local row, col = cursor[1] - 1, cursor[2] - 2
--     local prev_cursor_node = vim.treesitter.get_node({bufnr = 0, pos = {row, col}})
--     return (prev_cursor_node and prev_cursor_node:type() or 'ukn') .. ' ' .. row .. ':' .. col
--   end,
--   color = {fg = colors.red, gui = 'bold'}
-- }
--
-- -- Add current `textwidth` config to status line
-- ins_right {
--   function ()
--     return vim.api.nvim_buf_get_option(0, 'textwidth')
--   end
-- }

ins_right {
  -- filesize component
  function()
    local function format_file_size(file)
      local size = vim.fn.getfsize(file)
      if size <= 0 then return '' end
      local sufixes = {'b', 'k', 'm', 'g'}
      local i = 1
      while size > 1024 do
        size = size / 1024
        i = i + 1
      end
      return string.format('%.1f%s', size, sufixes[i])
    end
    local file = vim.fn.expand('%:p')
    if string.len(file) == 0 then return '' end
    return format_file_size(file)
  end,
  condition = conditions.buffer_not_empty,
  color = {fg = colors.comment, gui = 'bold'}
}

-- Now don't forget to initialize lualine
lualine.setup(config)
