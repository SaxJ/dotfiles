local function cmd_on_visual_selection(cmd)
  local mode = vim.api.nvim_get_mode()['mode']
  local new_buf = vim.api.nvim_create_buf(false, true)
  if new_buf == 0 then
    vim.notify('Failed to create AI buffer.', "error")
    return
  end

  if mode == 'n' then
    vim.cmd('normal! ggVGy')
  else
    vim.cmd('normal! y')
  end

  vim.cmd('vsplit')
  vim.api.nvim_set_current_buf(new_buf)
  vim.cmd('normal! P')
  vim.cmd("%! " .. cmd)
end

vim.keymap.set({ 'n', 'v' }, "<leader>aie", function()
  cmd_on_visual_selection("sc explain")
end, { desc = "Explain current selection or buffer." })

vim.keymap.set({ 'n', 'v' }, "<leader>air", function()
  cmd_on_visual_selection("sc review")
end, { desc = "Ask the AI to review some code." })

vim.keymap.set({ 'n' }, "<leader>aia", function()
  local prompt = vim.fn.input("Prompt: ")
  local new_buf = vim.api.nvim_create_buf(false, true)
  if new_buf == 0 then
    vim.notify('Failed to create AI buffer.', "error")
    return
  end

  vim.cmd('vsplit')
  vim.api.nvim_set_current_buf(new_buf)
  vim.cmd(string.format([[r! sc '%s']], prompt))
end, { desc = "Ask the AI a question from the prompt" })
