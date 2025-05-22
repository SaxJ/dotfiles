vim.api.nvim_create_user_command("JiraList", function()
  local buf = vim.api.nvim_create_buf(true, true)
  local win = vim.api.nvim_open_win(buf, true, {
    win = -1,
    split = 'below',
  });

  vim.api.nvim_set_option_value('wrap', false, {win = win})
  vim.api.nvim_set_option_value('filetype', 'jira-issues', {buf = buf})
  vim.api.nvim_set_option_value('tabstop', 8, {buf = buf})

  vim.keymap.set('n', 'q', ':bd<CR>', {buffer = buf})

  vim.cmd([[.! jira issue list -s "~✅ Done" --columns key,status,summary --plain --no-headers]])
  vim.api.nvim_set_option_value('modifiable', false, {buf = buf})
end, { desc = "List Jira issues" })
