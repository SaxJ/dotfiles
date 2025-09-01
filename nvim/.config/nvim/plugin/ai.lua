local function bufferToTmpFile()
  local buf = vim.api.nvim_get_current_buf()
  local allLines = vim.api.nvim_buf_get_lines(buf, 0, -1, false)
  local content = table.concat(allLines, '\n')

  local tempFile = vim.fn.tempname()
  local tempHandle = io.open(tempFile, "w")

  if tempHandle ~= nil then
    tempHandle:write(content)
    tempHandle:close()
  end

  return tempFile
end

local function regionToTmpFile()
  local buf = vim.api.nvim_get_current_buf()
  local from = vim.fn.getpos("'<")
  local to = vim.fn.getpos("'>")

  local startLine = from[2] - 1
  local startCol = from[3] - 1

  local endLine = to[2] - 1
  local endCol = to[3] - 1
  if from[4] == 0 then
    endCol = to[3]
  end

  local lines = vim.api.nvim_buf_get_lines(buf, startLine, endLine + 1, false)

  local tempFile = vim.fn.tempname()
  if #lines == 0 then
    -- no selection
    return tempFile
  end

  if #lines == 1 then
    lines[1] = string.sub(lines[1], startCol + 1, endCol + 1)
  else
    lines[1] = string.sub(lines[1], startCol + 1)
    lines[#lines] = string.sub(lines[#lines], 1, endCol + 1)
  end

  local content = table.concat(lines, '\n')
  local tempHandle = io.open(tempFile, "w")

  if tempHandle ~= nil then
    tempHandle:write(content)
    tempHandle:close()
  end

  return tempFile
end

--- Filters the current buffer using an external command and writes the output to a new buffer.
-- @param mode The mode in which to execute the command, either 'n' for normal mode or 'v' for visual mode.
-- @param cmd The command to execute for filtering the buffer contents.
local function filterBufferToNew(cmd)
  local tempFile = mode == 'n' and bufferToTmpFile() or regionToTmpFile()

  local outputBuf = vim.api.nvim_create_buf(false, true)
  vim.api.nvim_buf_call(outputBuf, function()
    vim.cmd('read ' .. tempFile)
    vim.cmd("%! " .. cmd)
  end)

  vim.cmd('vsplit')
  vim.api.nvim_set_current_buf(outputBuf)
end

vim.api.nvim_create_user_command(
  'AiExplain',
  function()
    filterBufferToNew('sc "Concisely explain this buffer contents."')
  end,
  { desc = 'Explain the selection or buffer.', range = true }
)

vim.api.nvim_create_user_command(
  'AiPrompt',
  function()
    local prompt = vim.fn.input("Prompt> ")
    filterBufferToNew(string.format('sc "%s"', prompt))
  end,
  { desc = 'Just prompt.', range = true }
)
