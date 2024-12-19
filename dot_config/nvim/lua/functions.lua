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

return {
	string = {
		trim = trim,
	},
	util = {
		log_work_date = log_work_date,
	},
	system = {
		notify_send = notify_send,
	},
}
