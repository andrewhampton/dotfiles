local api = vim.api

-- Function to set textwidth based on the current line
function git_commit_textwidth()
  print('entering')
  local function set_textwidth(width)
    api.nvim_buf_set_option(0, 'textwidth', width)
  end

  -- Set initial textwidth based on the first line
  local line_number = api.nvim_win_get_cursor(0)[1]
  if line_number == 1 then
    set_textwidth(50)
  else
    set_textwidth(72)
  end

  -- Update textwidth when the cursor moves
  local function on_cursor_moved()
    print('updating')
    local new_line_number = api.nvim_win_get_cursor(0)[1]
    if new_line_number ~= line_number then
      if new_line_number == 1 then
        set_textwidth(50)
      else
        set_textwidth(72)
      end
      line_number = new_line_number
    end
  end

  local git_commit_cursor_moved = api.nvim_create_augroup('git_commit_cursor_moved', {clear = true})

  api.nvim_create_autocmd({'CursorMoved', 'CursorMovedI'}, {
    group = git_commit_cursor_moved,
    pattern = '*',
    callback = on_cursor_moved,
  })
end

local git_commit_tw = api.nvim_create_augroup('git_commit_tw', {clear = true})

api.nvim_create_autocmd({'FileType'}, {
  group = git_commit_tw,
  pattern = 'gitcommit',
  callback = git_commit_textwidth,
})
