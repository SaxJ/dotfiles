--- Trim whitespace from start and end of string
---@param str string The string to trim
---@return (string) trimmed The trimmed string
local trim = function(str)
	return (string.gsub(str, "^%s*(.-)%s*$", "%1"))
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
	vim.api.nvim_cmd({ "checktime", fname })
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
vim.api.nvim_create_autocmd("BufRead", {
	pattern = { "*" },
	callback = function(event)
		local fname = event["match"]
		watch_file(fname)
	end,
})

vim.api.nvim_create_autocmd("BufRead", {
	pattern = { "*" },
	callback = function(event)
		local fname = event["match"]
		if table_contains(fname, file_watcher_map) then
			file_watcher_map[fname]:stop()
		end

		file_watcher_map[fname] = nil
	end,
})

local function map(t, func)
	local result = {}
	for i, v in ipairs(t) do
		result[i] = func(v)
	end
	return result
end

return {
	string = {
		trim = trim,
	},
	table = {
		table_contains = table_contains,
		map = map,
	},
	util = {
		log_work_date = log_work_date,
	},
	system = {
		notify_send = notify_send,
	},
}
