-- md-file-complete: @file reference with Telescope picker in markdown buffers
-- Type @ in insert mode to open a file finder; selection is inserted as @path

local M = {}

local util = require('util')

local function insert_ref(selection)
  -- Insert the selected path at the cursor (after the @)
  local row, col = unpack(vim.api.nvim_win_get_cursor(0))
  local line = vim.api.nvim_get_current_line()
  local before = line:sub(1, col)
  local after = line:sub(col + 1)
  vim.api.nvim_set_current_line(before .. selection .. after)
  vim.api.nvim_win_set_cursor(0, { row, col + #selection })
end

function M.pick_file()
  local repo = util.repoRootPath()
  local root = repo and repo:absolute() or vim.fn.expand('~/code')
  local actions = require('telescope.actions')
  local action_state = require('telescope.actions.state')

  require('telescope.builtin').find_files({
    cwd = root,
    prompt_title = '@ File Reference',
    hidden = false,
    file_ignore_patterns = { '%.git/', '%.jj/' },
    attach_mappings = function(prompt_bufnr, map)
      actions.select_default:replace(function()
        actions.close(prompt_bufnr)
        local entry = action_state.get_selected_entry()
        if entry then
          insert_ref(entry[1])
        end
      end)
      return true
    end,
  })
end

function M.setup()
  vim.api.nvim_create_autocmd('FileType', {
    pattern = 'markdown',
    callback = function(ev)
      vim.keymap.set('i', '@', function()
        -- Insert the @ character, then open Telescope
        vim.api.nvim_feedkeys('@', 'n', false)
        vim.schedule(M.pick_file)
      end, { buffer = ev.buf, desc = '@file reference via Telescope' })
    end,
  })
end

return M
