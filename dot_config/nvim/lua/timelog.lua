local function timeclock_in(note)
  local cache_dir = vim.fn.stdpath("data")
  local log_file = cache_dir .. '/timelog'
  local file = io.open(log_file, "a+")
  local last_in = false
  if file ~= nil then
    for line in file:lines() do
      local c = string.sub(line, 1, 1)
      last_in = c == 'i'
    end

    local datetime = os.date("%Y/%m/%d %I:%M:%S")

    if last_in then
      file:write(string.format("o %s\n", datetime))
    end
    file:write(string.format("i %s %s\n", datetime, note))

    file:close()
  end
end

local function timeclock_out(note)
  local cache_dir = vim.fn.stdpath("data")
  local log_file = cache_dir .. '/timelog'
  local file = io.open(log_file, "a+")
  local last_in = false
  if file ~= nil then
    for line in file:lines() do
      local c = string.sub(line, 1, 1)
      last_in = c == 'i'
    end

    local datetime = os.date("%Y/%m/%d %I:%M:%S")

    if last_in then
      file:write(string.format("i %s %s\n", datetime, note))
    end

    file:close()
  end
end

vim.api.nvim_create_user_command("ClockIn", function (args)
  timeclock_in(args['args'])
end, {})

vim.api.nvim_create_user_command("ClockOut", function (args)
  timeclock_out(args['args'])
end, {})
