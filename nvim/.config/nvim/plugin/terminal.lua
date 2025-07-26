--- Opens a terminal
---@param cmd string
---@param type 'horizontal'|'vertical'|'float'
---@param name string|nil
local function open_terminal(cmd, type, name)
  local editor_width = vim.o.columns
  local editor_height = vim.o.lines

  local width = math.floor(editor_width * 0.8)
  local height = math.floor(editor_height * 0.8)

  local row = math.floor((editor_height - height) / 2)
  local col = math.floor((editor_width - width) / 2)
  local split = 'below'
  if type == 'vertical' then
    split = 'right'
  end

  local buf = vim.api.nvim_create_buf(true, true)
  if name ~= nil then
    vim.api.nvim_buf_set_name(buf, name)
  end

  if type == 'horizontal' or type == 'vertical' then
    vim.api.nvim_open_win(buf, true, {
      win = -1,
      split = split,
    })
  else
    vim.api.nvim_open_win(buf, true, {
      relative = 'editor',
      row = row,
      col = col,
      width = width,
      height = height,
      style = 'minimal',
      border = 'rounded'
    })
  end

  vim.cmd(string.format("term %s", cmd))
end

vim.api.nvim_create_user_command("HTerm", function(args)
  open_terminal(args['args'], 'horizontal')
end, { desc = "Open a horizontal terminal", nargs = '?' })

vim.api.nvim_create_user_command("VTerm", function(args)
  open_terminal(args['args'], 'vertical')
end, { desc = "Open a horizontal terminal", nargs = '?' })

vim.api.nvim_create_user_command("FTerm", function(args)
  open_terminal(args['args'], 'float')
end, { desc = "Open a horizontal terminal", nargs = '?' })

return {
  open_terminal = open_terminal,
}
