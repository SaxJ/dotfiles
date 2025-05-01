--- Trim whitespace from start and end of string
---@param str string The string to trim
---@return (string) trimmed The trimmed string
local trim = function(str)
	return (string.gsub(str, "^%s*(.-)%s*$", "%1"))
end

--- Split a string by lines
---@param str string
---@return string[]
local lines = function(str)
  local result = {}
  for line in str:gmatch('[^\n]+') do
    table.insert(result, line)
  end

  return result
end

--- Concat two tables
---@param t1 table
---@param t2 table
---@return table
local concat_tables = function(t1, t2)
  local result = {}
  table.move(t1, 1, #t1, 1, result)
  table.move(t2, 1, #t2, #t1 + 1, result)

  return result
end

local log_level_to_urgency = {
	[1] = "low",
	[2] = "low",
	[3] = "normal",
	[4] = "critical",
}

--- Log an entry to my work log
local log_work_date = function()
	local date_cmd = vim.system({ "date", "+%Y-%m-%d" }, { text = true }):wait()
	local date = trim(date_cmd.stdout)

	local hostname_cmd = vim.system({ "hostname" }, { text = true }):wait()
	local hostname = trim(hostname_cmd.stdout)

	local line = string.format("* %s :%s:", date, hostname)

	os.execute(
		string.format("grep -qxF '%s' ~/Documents/wiki/log.org || echo '%s' >> ~/Documents/wiki/log.org", line, line)
	)
end

--- Open a floating window
---@param content string[]
---@return integer,integer
local function open_float(content)
  local editor_width = vim.o.columns
  local editor_height = vim.o.lines

  local width = math.floor(editor_width * 0.8)
  local height = math.floor(editor_height * 0.8)

  local row = math.floor((editor_height - height) / 2)
  local col = math.floor((editor_width - width) / 2)

  local buf = vim.api.nvim_create_buf(false, true)
  local win = vim.api.nvim_open_win(buf, true, {
    relative = 'editor',
    row = row,
    col = col,
    width = width,
    height = height,
    style = 'minimal',
    border = 'rounded'
  })

  vim.api.nvim_set_option_value('ft', 'markdown', {buf = buf})
  vim.api.nvim_buf_set_keymap(buf, 'n', 'q', '', {
    callback = function ()
      vim.api.nvim_win_close(win, true)
      vim.api.nvim_buf_delete(buf, {
        force = true,
        unload = true
      })
    end,
    noremap = true,
    silent = true,
  })

  vim.api.nvim_buf_set_lines(buf, 0, -1, false, content)

  return buf, win
end

--- Open a popup window
---@return integer,integer
local function open_popup()
  local editor_width = vim.o.columns
  local editor_height = vim.o.lines

  local width = editor_width
  local height = math.floor(editor_height * 0.25)

  local row = editor_height - height - 1
  local col = 0

  local buf = vim.api.nvim_create_buf(false, true)
  local win = vim.api.nvim_open_win(buf, true, {
    relative = 'editor',
    row = row,
    col = col,
    width = width,
    height = height,
    style = 'minimal',
    border = 'single'
  })


  return buf, win
end

local function open_split()
  local buf = vim.api.nvim_create_buf(true, true)
  local win = vim.api.nvim_open_win(buf, true, {
    win = -1,
  split = 'below',
  })

  return buf, win
end

--- Send a system notification
---@param title string Notification title
---@param msg string Notification content
---@param log_level integer The notification urgency
local notify_send = function(title, msg, log_level)
	local urgency = log_level_to_urgency[log_level] or "normal"
	local command = { "notify-send", "-u", urgency, "-i", "nvim", "-a", "Neovim", title, msg }

	vim.system(command)
end

local table_contains = function(key, table)
	return table[key] ~= nil
end

local file_watcher_map = {}
local on_fs_change = function(err, fname, status)
	vim.api.nvim_cmd({ "checktime", fname }, {})
end
local watch_file = function(fname)
	if not table_contains(fname, file_watcher_map) then
		file_watcher_map[fname] = vim.uv.new_fs_event()
	end
	local watcher = file_watcher_map[fname]
	watcher:start(
		fname,
		{},
		vim.schedule_wrap(function(...)
			on_fs_change(...)
		end)
	)
end

local function map(t, func)
	local result = {}
	for i, v in ipairs(t) do
		result[i] = func(v)
	end
	return result
end

--- Runs a callback every <interval> seconds
---@param interval integer
---@param callback function
---@return uv_timer_t
local function at_interval(interval, callback)
  local timer = vim.uv.new_timer()
  timer:start(interval * 1000, interval * 1000, function ()
    callback()
  end)
  return timer
end

--- Stops the supplied timer and releases resources
---@param timer uv_timer_t
local function stop_interval(timer)
  timer:stop()
  timer:close()
end

return {
	string = {
		trim = trim,
    lines = lines,
	},
	table = {
		table_contains = table_contains,
		map = map,
    concat_tables = concat_tables,
	},
	util = {
		log_work_date = log_work_date,
    at_interval = at_interval,
    stop_interval = stop_interval,
	},
	system = {
		notify_send = notify_send,
    watch_file = watch_file,
	},
  windows = {
    open_float = open_float,
    open_popup = open_popup,
    open_split = open_split,
  }
}
