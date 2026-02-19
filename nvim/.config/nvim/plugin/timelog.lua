local funcs = require("functions")

local module = {}

---@param datetime_str string
local function parse_datetime(datetime_str)
	local fmt = "(%d+)/(%d+)/(%d+) (%d+):(%d+):(%d+)"
	local year, month, day, hour, min, sec = datetime_str:match(fmt)
	if not year then
		error("Problem parsing datetime format.")
	end
	return os.time({
		year = tonumber(year),
		month = tonumber(month),
		day = tonumber(day),
		hour = tonumber(hour),
		min = tonumber(min),
		sec = tonumber(sec),
	})
end

---@param seconds integer
local function format_duration(seconds)
	seconds = math.abs(seconds)
	if seconds < 60 then
		return string.format("%ds", seconds)
	elseif seconds < 3600 then
		local min = seconds / 60
		local sec = seconds % 60
		return string.format("%dm %ds", min, sec)
	elseif seconds < 86400 then
		local hrs = seconds / 3600
		local min = (seconds % 3600) / 60
		return string.format("%dh %dm", hrs, min)
	else
		local days = seconds / 86400
		local hrs = (seconds % 86400) / 3600
		return string.format("%dd %dh", days, hrs)
	end
end

local function open_log_file(mode)
	local cache_dir = vim.fn.stdpath("data")
	local log_file = cache_dir .. "/timelog"

	if vim.env.TIMELOG_FILE ~= nil then
		log_file = vim.env.TIMELOG_FILE
	end

	if vim.g.timelog_file ~= nil then
		log_file = vim.g.timelog_file
	end

	return io.open(log_file, mode)
end

local function get_unique_projects()
	local file = open_log_file("r")
	if file == nil then
		return {}
	end

	local projects = {}
	for line in file:lines() do
		local io = string.match(line, "[io]")
		if io == "i" then
			local cleaned, _ = string.gsub(line, "[io] %d+/%d+/%d+ %d+:%d+:%d+", "")
			cleaned = vim.trim(cleaned)

			if cleaned ~= "" then
				table.insert(projects, cleaned)
			end
		end
	end
	file:close()

	return funcs.table.unique(projects)
end

local function timeclock_status()
	local file = open_log_file("r")
	local last_in = false
	local project = nil
	local datetime = nil
	if file ~= nil then
		for line in file:lines() do
			local c = string.sub(line, 1, 1)
			last_in = c == "i"

			if c == "i" then
				local extracted_project, _ = string.gsub(line, "[io] %d+/%d+/%d+ %d+:%d+:%d+", "")
				local extracted = vim.split(line, " ", { plain = true, trimempty = true })

				datetime = string.format("%s %s", extracted[2], extracted[3])
				project = vim.trim(extracted_project)
			end
		end

		file:close()
	end

	if last_in then
		local duration = parse_datetime(datetime) - os.time()
		return string.format("%s %s", project, format_duration(duration))
	end
	return "Clocked Out"
end

local function timeclock_in(note)
	local file = open_log_file("a+")
	local last_in = false
	if file ~= nil then
		for line in file:lines() do
			local c = string.sub(line, 1, 1)
			last_in = c == "i"
		end

		local datetime = os.date("%Y/%m/%d %H:%M:%S")

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
			last_in = c == "i"
		end

		local datetime = os.date("%Y/%m/%d %H:%M:%S")

		if last_in then
			file:write(string.format("o %s %s\n", datetime, note))
		end

		file:close()
	end
end

local function timeclock_summary()
	local f = open_log_file("r")

	local lastEntry = nil
	local daysSummary = {}

	while true do
		local line = f:read()
		if line == nil then
			break
		end

		local io = string.match(line, "[io]")
		if io == "i" then
			local year, month, day, hour, minute, second = line:match("(%d+)/(%d+)/(%d+) (%d+):(%d+):(%d+)")
			local dateTable = { year = year, month = month, day = day, hour = hour, minute = minute, second = second }

			local project, _ = string.gsub(line, "[%w%p]+", 3)
			project = vim.trim(project)

			lastEntry = { datetime = dateTable, project = project }
		else
			if lastEntry == nil then
				error("Checkout with no corresponding checkin")
			end

			local year, month, day, hour, minute, second = line:match("(%d+)/(%d+)/(%d+) (%d+):(%d+):(%d+)")
			local dateTable = { year = year, month = month, day = day, hour = hour, minute = minute, second = second }

			local lastDate = lastEntry["datetime"]
			local project = lastEntry["project"]

			local dayKey = string.format("%d/%d/%d", lastDate["day"], lastDate["month"], lastDate["year"])
			if daysSummary[dayKey] == nil then
				daysSummary[dayKey] = {}
			end

			if daysSummary[dayKey][project] == nil then
				daysSummary[dayKey][project] = 0
			end

			local timeDifference = os.difftime(os.time(dateTable), os.time(lastDate))
			daysSummary[dayKey][project] = daysSummary[dayKey][project] + math.abs(timeDifference)
		end
	end

	return daysSummary
end

vim.api.nvim_create_user_command("ClockIn", function(args)
	timeclock_in(args["args"])
end, {
	desc = "Clock in",
	nargs = 1,
	complete = get_unique_projects,
})

vim.api.nvim_create_user_command("ClockSummary", function()
	local summary = timeclock_summary()
	local buf, _ = funcs.windows.open_split()

	vim.api.nvim_buf_set_lines(buf, 0, -1, false, { vim.json.encode(summary) })
end, {
	desc = "Summary of clocked times",
})

vim.api.nvim_create_user_command("ClockOut", function(args)
	timeclock_out(args["args"])
end, { desc = "Clock out" })

vim.api.nvim_create_autocmd("VimLeavePre", {
	callback = function()
		local file = open_log_file("a+")
		local last_in = false
		if file ~= nil then
			for line in file:lines() do
				local c = string.sub(line, 1, 1)
				last_in = c == "i"
			end

			file:close()
		end
		if last_in then
			local choice = vim.fn.confirm("You are clocked in. Continue to exit?", "&Yes\n&No", 2)
			if choice == 1 then
				return true
			elseif choice == 2 then
				return false
			end
		end
		return true
	end,
})

module.status = timeclock_status

_G.TimeClock = module
