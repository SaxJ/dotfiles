local function open_terminal(cmd, vertical)
  local split = 'below'
  if vertical then
    split = 'right'
  end

  local buf = vim.api.nvim_create_buf(true, true)
  vim.api.nvim_open_win(buf, true, {
    win = -1,
    split = split,
  })

  vim.cmd(string.format("term %s", cmd))
end

vim.api.nvim_create_user_command("HTerm", function (args)
  open_terminal(args['args'], false)
end, {desc = "Open a horizontal terminal", nargs = '?'})

vim.api.nvim_create_user_command("VTerm", function (args)
  open_terminal(args['args'], true)
end, {desc = "Open a horizontal terminal", nargs = '?'})
