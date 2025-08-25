vim.api.nvim_create_user_command("RefreshBuffers", function()
  local buffers = vim.api.nvim_list_bufs()
  local refreshed = 0

  for _, buf in ipairs(buffers) do
    if vim.api.nvim_buf_is_loaded(buf) and vim.api.nvim_buf_get_name(buf) ~= '' then
      local ok, _ = pcall(vim.api.nvim_buf_call, buf, function()
        vim.cmd('edit')
      end)

      if ok then
        refreshed = refreshed + 1
      end
    end
  end

  print(string.format('Refreshed %d buffers.', refreshed))
end, { desc = "Refreshes buffer contents from disk" })
