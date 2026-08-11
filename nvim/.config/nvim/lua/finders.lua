-- Incremental fuzzy finders built on Neovim's command line.
--
-- No plugins and no picker UI: the completion popup IS the UI. Typing in the
-- command line fires wildtrigger() on every keystroke, so matches update live.
-- See :h cmdline-autocompletion, :h fuzzy-file-picker, :h live-grep — this is
-- the documented recipe, ported to Lua and backed by ripgrep.
--
--   <leader>ff  :find    fuzzy match on file name
--   <leader>fs  :Grep    ripgrep the project, jump to the match
--   <leader>fr  :Recent  fuzzy match on recently opened files

local M = {}

local function project_root()
  return vim.fs.root(0, { '.git', '.jj' }) or assert(vim.uv.cwd())
end

---------------------------------------
-- :find — fuzzy file names
---------------------------------------

-- Rebuilt once per command line, then reused for every keystroke.
local cache = { root = nil, files = {} }

local function list_files(root)
  if vim.fn.executable('rg') == 1 then
    local out = vim.fn.systemlist({
      'rg', '--files', '--hidden', '-g', '!.git/', '-g', '!.jj/', root,
    })
    if vim.v.shell_error == 0 then
      return vim.tbl_map(function(f) return vim.fs.relpath(root, f) or f end, out)
    end
  end
  -- Fallback: stock globbing, no ripgrep required.
  local out = vim.fn.globpath(root, '**', true, true)
  return vim.iter(out)
    :filter(function(f) return vim.fn.isdirectory(f) == 0 end)
    :map(function(f) return vim.fs.relpath(root, f) or f end)
    :totable()
end

-- 'findfunc' contract: return display paths while completing, real paths when
-- :find actually opens something. Keeps the popup readable without breaking
-- the open, which matters because 'autochdir' moves cwd out from under us.
function M.findfunc(arg, is_completion)
  if vim.tbl_isempty(cache.files) then
    cache.root = project_root()
    cache.files = list_files(cache.root)
  end

  local matches = arg == '' and cache.files or vim.fn.matchfuzzy(cache.files, arg)
  if is_completion then
    return matches
  end
  return vim.tbl_map(function(f) return cache.root .. '/' .. f end, matches)
end

---------------------------------------
-- :Grep — live project search
---------------------------------------

-- Set by CmdlineLeavePre below, consumed when :Grep finally runs.
local grep_selected = nil

local function grep_complete(arglead)
  -- Wait for two characters; one-letter searches match half the repo.
  if #arglead < 2 then
    return {}
  end
  local prg = vim.o.grepprg
  if not prg:find('%$%*') then
    prg = prg .. ' $*'
  end
  local pattern = vim.fn.shellescape(vim.fn.escape(arglead, '\\'))
  -- Function replacement, so a '%' in the pattern isn't treated as a capture.
  local cmd = prg:gsub('%$%*', function() return pattern end)
  local out = vim.fn.systemlist(cmd)
  return vim.v.shell_error <= 1 and out or {}
end

local function grep_visit()
  if not grep_selected then
    return
  end
  -- Let quickfix parse the grep line rather than reinventing the format.
  local item = vim.fn.getqflist({ lines = { grep_selected } }).items[1]
  grep_selected = nil
  if not item or item.bufnr == 0 then
    return
  end
  vim.bo[item.bufnr].buflisted = true
  vim.cmd.buffer(item.bufnr)
  pcall(vim.api.nvim_win_set_cursor, 0, { item.lnum, math.max(0, item.col - 1) })
end

---------------------------------------
-- :Recent — fuzzy recent files
---------------------------------------

local function recent_files()
  return vim.iter(vim.v.oldfiles)
    :filter(function(f) return vim.fn.filereadable(f) == 1 end)
    :map(function(f) return vim.fn.fnamemodify(f, ':~') end)
    :totable()
end

local function recent_complete(arglead)
  local files = recent_files()
  if arglead == '' then
    return files
  end
  return vim.fn.matchfuzzy(files, arglead)
end

---------------------------------------
-- Setup
---------------------------------------

function M.setup()
  -- Respect ignore files. The default grepprg passes -uu, which drags in
  -- node_modules, .jj, and friends.
  vim.o.grepprg = 'rg --vimgrep'
  vim.o.wildmode = 'noselect:lastused,full'
  vim.o.wildoptions = 'pum,tagfile,fuzzy'

  vim.o.findfunc = "v:lua.require'finders'.findfunc"

  vim.api.nvim_create_user_command('Grep', grep_visit, {
    nargs = '+',
    complete = grep_complete,
    desc = 'Live ripgrep search of the project',
  })

  vim.api.nvim_create_user_command('Recent', function(o)
    vim.cmd.edit(vim.fn.fnameescape(vim.fn.expand(o.args)))
  end, {
    nargs = 1,
    complete = recent_complete,
    desc = 'Open a recently used file',
  })

  local group = vim.api.nvim_create_augroup('Finders', { clear = true })

  -- The whole trick: re-run completion on every keystroke.
  vim.api.nvim_create_autocmd('CmdlineChanged', {
    group = group,
    pattern = ':',
    callback = function() vim.fn.wildtrigger() end,
  })

  -- Drop the file cache so each new command line sees the current project.
  vim.api.nvim_create_autocmd('CmdlineEnter', {
    group = group,
    pattern = ':',
    callback = function() cache = { root = nil, files = {} } end,
  })

  -- On <CR> with nothing explicitly selected, take the top match. Without
  -- this you'd have to <Tab> into the popup before every accept.
  vim.api.nvim_create_autocmd('CmdlineLeavePre', {
    group = group,
    pattern = ':',
    callback = function()
      local info = vim.fn.cmdcomplete_info()
      local matches = info.matches or {}
      if vim.tbl_isempty(matches) then
        return
      end
      local selected = info.selected ~= -1 and matches[info.selected + 1] or matches[1]
      local line = vim.fn.getcmdline()

      local cmd = line:match('^%s*(find?)%s') or line:match('^%s*(Recent)%s')
      if cmd and info.selected == -1 then
        vim.fn.setcmdline(('%s %s'):format(cmd, selected))
      elseif line:match('^%s*Grep%s') then
        -- :Grep takes no real argument; stash the hit and restore the line so
        -- the pattern lands in command-line history.
        grep_selected = selected
        vim.fn.setcmdline(info.cmdline_orig)
      end
    end,
  })

  -- Keep <Up>/<Down> on history instead of the popup.
  vim.keymap.set('c', '<Up>', function()
    return vim.fn.wildmenumode() == 1 and '<C-e><Up>' or '<Up>'
  end, { expr = true, replace_keycodes = true })
  vim.keymap.set('c', '<Down>', function()
    return vim.fn.wildmenumode() == 1 and '<C-e><Down>' or '<Down>'
  end, { expr = true, replace_keycodes = true })
end

return M
