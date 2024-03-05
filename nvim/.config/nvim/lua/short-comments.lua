local api = vim.api
local ts_utils = require("nvim-treesitter.ts_utils")

local function get_previous_node_type()
  local cursor = api.nvim_win_get_cursor(0)
  local row, col = cursor[1] - 1, math.max(0, cursor[2] - 2)
  local prev_cursor_node = vim.treesitter.get_node({bufnr = 0, pos = {row, col}})
  return prev_cursor_node and prev_cursor_node:type() or nil
end

local function set_textwidth_for_comments()
  local bufnr = api.nvim_get_current_buf()
  local cursor_node = ts_utils.get_node_at_cursor()

  if cursor_node then
    local node_type = cursor_node:type()
    local prev_node_type = get_previous_node_type()
    local should_set_textwidth = node_type == "comment" or prev_node_type == "comment"

    if should_set_textwidth then
      local ok, original_textwidth = pcall(api.nvim_buf_get_var, bufnr, 'original_textwidth')
      if not ok then
        original_textwidth = api.nvim_buf_get_option(bufnr, 'textwidth')
        api.nvim_buf_set_var(bufnr, 'original_textwidth', original_textwidth)
      end
      api.nvim_buf_set_option(bufnr, 'textwidth', 80)
    else
      local ok, original_textwidth = pcall(api.nvim_buf_get_var, bufnr, 'original_textwidth')
      if ok then
        api.nvim_buf_set_option(bufnr, 'textwidth', original_textwidth)
      end
    end
  end
end

local treesitter_comment_textwidth_group = api.nvim_create_augroup('TreesitterCommentTextwidth', {clear = true})

api.nvim_create_autocmd({'CursorMoved', 'CursorMovedI', 'InsertCharPre'}, {
  group = treesitter_comment_textwidth_group,
  pattern = '*',
  callback = set_textwidth_for_comments,
})
