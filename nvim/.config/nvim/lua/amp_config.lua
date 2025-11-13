-- Amp.nvim configuration and keybindings
local M = {}

local util = require('util')

-- Helper to get file path (prefer git-relative)
local function get_file_path()
  return util.currentFileRelativeToGitRoot() or vim.fn.expand('%:p')
end

-- Helper to get visual selection
local function get_selection(opts)
  local start_line = opts.line1
  local end_line = opts.line2
  local lines = vim.api.nvim_buf_get_lines(0, start_line - 1, end_line, false)
  return {
    text = table.concat(lines, "\n"),
    start_line = start_line,
    end_line = end_line,
  }
end

-- Helper to send message with notification
local function send_to_amp(message)
  require("amp.message").send_message(message)
  vim.notify("Sent to Amp", vim.log.levels.INFO)
end

-- Send raw message to Amp
vim.api.nvim_create_user_command("AmpSend", function(opts)
  local message = opts.args
  if message == "" then
    vim.notify("Please provide a message to send", vim.log.levels.WARN)
    return
  end
  send_to_amp(message)
end, {
  nargs = "*",
  desc = "Send a message to Amp",
})

-- Edit selection with custom instructions
vim.api.nvim_create_user_command("AmpEdit", function(opts)
  local sel = get_selection(opts)
  local file_path = get_file_path()

  vim.ui.input({ prompt = "Edit instruction: " }, function(instruction)
    if not instruction or instruction == "" then return end

    local message = string.format(
      "Edit the selection in @%s#L%d-L%d\n\nCurrent content:\n```\n%s\n```\n\nInstruction: %s",
      file_path, sel.start_line, sel.end_line, sel.text, instruction
    )
    send_to_amp(message)
  end)
end, {
  range = true,
  desc = "Edit selection with Amp",
})

-- Explain selection
vim.api.nvim_create_user_command("AmpExplain", function(opts)
  local sel = get_selection(opts)
  local file_path = get_file_path()

  local message = string.format(
    "Explain this code from @%s#L%d-L%d\n\n```\n%s\n```",
    file_path, sel.start_line, sel.end_line, sel.text
  )
  send_to_amp(message)
end, {
  range = true,
  desc = "Explain selection with Amp",
})

-- Generate tests for selection
vim.api.nvim_create_user_command("AmpTests", function(opts)
  local sel = get_selection(opts)
  local file_path = get_file_path()

  local instruction = "Ensure tests exist for the following code"

  local message = string.format(
    "%s from @%s#L%d-L%d\n\n```\n%s\n```",
    instruction, file_path, sel.start_line, sel.end_line, sel.text
  )
  send_to_amp(message)
end, {
  range = true,
  desc = "Generate tests for selection with Amp",
})

-- Add documentation/comments to selection
vim.api.nvim_create_user_command("AmpDocs", function(opts)
  local sel = get_selection(opts)
  local file_path = get_file_path()

  local message = string.format(
    "Add documentation comments to this code in @%s#L%d-L%d\n\n```\n%s\n```\n\nUse the appropriate doc format for the language.",
    file_path, sel.start_line, sel.end_line, sel.text
  )
  send_to_amp(message)
end, {
  range = true,
  desc = "Add docs to selection with Amp",
})

-- Fix all diagnostics in file
vim.api.nvim_create_user_command("AmpFixDiagnostics", function()
  local file_path = get_file_path()

  -- Get all diagnostics in current buffer
  local diagnostics = vim.diagnostic.get(0)

  if #diagnostics == 0 then
    vim.notify("No diagnostics in file", vim.log.levels.WARN)
    return
  end

  -- Format diagnostics with line numbers
  local diag_messages = {}
  for _, d in ipairs(diagnostics) do
    table.insert(diag_messages, string.format(
      "- Line %d: [%s] %s",
      d.lnum + 1,
      d.source or "unknown",
      d.message
    ))
  end

  local message = string.format(
    "Fix all diagnostics in @%s\n\nDiagnostics:\n%s",
    file_path, table.concat(diag_messages, "\n")
  )
  send_to_amp(message)
end, {
  desc = "Fix all diagnostics in file with Amp",
})

-- Quick ask about selection
vim.api.nvim_create_user_command("AmpAsk", function(opts)
  local sel = get_selection(opts)
  local file_path = get_file_path()

  vim.ui.input({ prompt = "Question: " }, function(question)
    if not question or question == "" then return end

    local message = string.format(
      "Question about @%s#L%d-L%d\n\n```\n%s\n```\n\n%s",
      file_path, sel.start_line, sel.end_line, sel.text, question
    )
    send_to_amp(message)
  end)
end, {
  range = true,
  desc = "Ask about selection with Amp",
})

function M.setup()
  local opts = { noremap = true, silent = true }

  -- Register which-key group for <leader>a
  local ok, wk = pcall(require, "which-key")
  if ok then
    wk.add({
      { "<leader>a", group = "amp", mode = { "n", "v" } },
    })
  end

  -- Visual mode mappings (all under <leader>a for Amp)
  vim.keymap.set('v', '<leader>e',  ':AmpEdit<CR>', { desc = "Edit selection with Amp", unpack(opts) })
  vim.keymap.set('v', '<leader>as', ':AmpSend<CR>', { desc = "Send selection to Amp", unpack(opts) })
  vim.keymap.set('v', '<leader>ax', ':AmpExplain<CR>', { desc = "Explain selection", unpack(opts) })
  vim.keymap.set('v', '<leader>at', ':AmpTests<CR>', { desc = "Generate tests", unpack(opts) })
  vim.keymap.set('v', '<leader>ao', ':AmpDocs<CR>', { desc = "Add documentation", unpack(opts) })
  vim.keymap.set('v', '<leader>aq', ':AmpAsk<CR>', { desc = "Ask about selection", unpack(opts) })

  -- Normal mode mappings
  vim.keymap.set('n', '<leader>ai', ':AmpFixDiagnostics<CR>', { desc = "Fix all diagnostics", unpack(opts) })
end

return M
