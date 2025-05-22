local funcs = require('functions')

local function open_log_file(mode)
  local cache_dir = vim.fn.stdpath("data")
  local log_file = cache_dir .. '/timelog'
  if vim.g.timelog_file ~= nil then
    log_file = vim.g.timelog_file
  end
  return io.open(log_file, mode)
end

local function timeclock_in(note)
  local file = open_log_file("a+")
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
  local file = open_log_file("a+")
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

vim.api.nvim_create_user_command("ClockIn", function(args)
  timeclock_in(args['args'])
end, {
  desc = "Clock in",
  nargs = 1,
  complete = function()
    local file = open_log_file("r")
    local suggestions = {}
    if file ~= nil then
      local lines = {}
      for line in file:lines() do
        table.insert(lines, line)
      end

      for _, line in ipairs(lines) do
        local m = ""
        for match in string.gmatch(line, "%a %d+/%d+/%d+ %d+:%d+:%d+ (.*)") do
          m = funcs.string.trim(match)
        end

        if m ~= "" then
          table.insert(suggestions, m)
        end
      end
    end
    return funcs.table.unique(suggestions)
  end
})

vim.api.nvim_create_user_command("ClockOut", function(args)
  timeclock_out(args['args'])
end, { desc = "Clock in", nargs = 1 })
