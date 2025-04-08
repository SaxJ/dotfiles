local f = require("functions")

vim.g.jira_api_key = f.string.trim(vim.system({ "pass", "jira" }, { text = true }):wait().stdout)
vim.g.jira_host = "https://hejira.atlassian.net"
vim.g.jira_user = f.string.trim(vim.system({ "jira", "me" }, { text = true }):wait().stdout)

--- Runs the given JQL query, returning the results
---@param query string The JQL query to execute
local function jql_query(query)
	local curl_cmd = vim.system({
		"curl",
		"-s",
		"-G",
		"--user",
		string.format("%s:%s", vim.g.jira_user, vim.g.jira_api_key),
		"-H",
		"Accept: application/json",
		"--data-urlencode",
		string.format("jql=%s", query),
		"--data-urlencode",
		"maxResults=1000",
		"--data-urlencode",
		"fields=summary",
		string.format("%s/rest/api/3/search/jql", vim.g.jira_host),
	}, { text = true })

	local curl_result = curl_cmd:wait()
	if curl_result.code == 0 then
		local results = vim.json.decode(curl_result.stdout)
		return f.table.map(results["issues"], function(issue)
			return { key = issue["key"], summary = issue["fields"]["summary"] }
		end)
	else
		return {}
	end
end

--- Gets a list of valid jira issue transitions
---@param key string The jira issue key
local function issue_transitions(key)
	local curl_cmd = vim.system({
		"curl",
		"-s",
		"-G",
		"--user",
		string.format("%s:%s", vim.g.jira_user, vim.g.jira_api_key),
		"-H",
		"Accept: application/json",
		string.format("%s/rest/api/3/issue/%s/transitions", vim.g.jira_host, key),
	}, { text = true })

	local curl_result = curl_cmd:wait()
	if curl_result.code == 0 then
		local results = vim.json.decode(curl_result.stdout)
		return results["transitions"]
	else
		return {}
	end
end

--- Perform the given transition on the issue
---@param issue_key string The key of the issue
---@param transition_id string|number The numeric ID of the transition
---@return boolean If the transition was successful
local function perform_transition(issue_key, transition_id)
	local curl_cmd = vim.system({
		"curl",
		"-s",
		"-X",
		"POST",
		"--user",
		string.format("%s:%s", vim.g.jira_user, vim.g.jira_api_key),
		"-H",
		"Accept: application/json",
		"-H",
		"Content-Type: application/json",
		"-d",
		vim.json.encode({ transition = { id = transition_id } }),
		string.format("%s/rest/api/3/issue/%s/transitions", vim.g.jira_host, issue_key),
	}, { text = true })

	local curl_result = curl_cmd:wait()
	return curl_result.code == 0
end

local function get_issue_details(issue_key)
	local curl_cmd = vim.system({
		"curl",
		"-s",
		"-G",
		"--user",
		string.format("%s:%s", vim.g.jira_user, vim.g.jira_api_key),
		"-H",
		"Accept: application/json",
		"--data-urlencode",
		"fields=summary,description",
		string.format("%s/rest/api/2/issue/%s", vim.g.jira_host, issue_key),
	}, { text = true })

	local curl_result = curl_cmd:wait()
	if curl_result.code == 0 then
		local result = vim.json.decode(curl_result.stdout)
		local summary = result["fields"]["summary"]
		local key = result["key"]
		local raw_description = result["fields"]["description"]

		local description = { "No description :(" }
		if type(raw_description) == "string" then
			description = f.string.lines(raw_description)
		end

		local buf, win = f.windows.open_popup()

		local content = f.table.concat_tables({
			string.format("# %s - %s", key, summary),
			"",
		}, description)

		vim.api.nvim_set_option_value("ft", "markdown", { buf = buf })
		vim.api.nvim_set_option_value("wrap", true, { win = win })

		vim.api.nvim_buf_set_keymap(buf, "n", "q", "", {
			callback = function()
				vim.api.nvim_win_close(win, true)
				vim.api.nvim_buf_delete(buf, {
					force = true,
					unload = true,
				})
			end,
			noremap = true,
			silent = true,
		})

		vim.api.nvim_buf_set_lines(buf, 0, -1, false, content)

		vim.api.nvim_set_option_value("modifiable", false, { buf = buf })
		vim.api.nvim_set_option_value("readonly", true, { buf = buf })
		vim.api.nvim_set_option_value("modifiable", false, { buf = buf })
	else
		f.system.notify_send("Jira Error", "Could not get issue details", 1)
	end
end

--- Display current jira issues in a popup
local function jira_action()
	local results =
		jql_query("assignee = currentUser() AND project = MKT AND statusCategory != Done ORDER BY created DESC")

	local items = {}
	for i, item in ipairs(results) do
		table.insert(items, {
			idx = i,
			score = i,
			key = item["key"],
			summary = item["summary"],
			text = string.format("%s %s", item["key"], item["summary"]),
		})
	end

	Snacks.picker({
		items = items,
		format = function(item)
			local ret = {}
			ret[#ret + 1] = { item.key, "SnacksPickerLabel" }
			ret[#ret + 1] = { string.rep(" ", 24 - #item.key), virtual = true }
			ret[#ret + 1] = { item.summary, "SnacksPickerComment" }
			return ret
		end,
		matcher = {
			ignorecase = true,
		},
		confirm = function(picker, item)
			Snacks.picker({
				items = f.table.map(issue_transitions(item.key), function(t)
					return {
						name = t.name,
						text = t.name,
						id = t.id,
					}
				end),
				format = function(tran)
					return { { tran.name, "SnacksPickerLabel" } }
				end,
				matcher = {
					ignorecase = true,
				},
				confirm = function(picker2, tran)
					local success = perform_transition(item.key, tran.id)

					local notification_title = string.format("Issue %s", item.key)
					if success then
						f.system.notify_send(notification_title, tran.name, 1)
					else
						f.system.notify_send(notification_title, "Did not transition", 1)
					end
					picker2:close()
				end,

				picker:close(),
			})
		end,
	})
end

local function jira_display_details()
	local results =
		jql_query("assignee = currentUser() AND project = MKT AND statusCategory != Done ORDER BY created DESC")

	local items = {}
	for i, item in ipairs(results) do
		table.insert(items, {
			idx = i,
			score = i,
			key = item["key"],
			summary = item["summary"],
			text = string.format("%s %s", item["key"], item["summary"]),
		})
	end

	Snacks.picker({
		items = results,
		format = function(item)
			local ret = {}
			ret[#ret + 1] = { item.key, "SnacksPickerLabel" }
			ret[#ret + 1] = { string.rep(" ", 24 - #item.key), virtual = true }
			ret[#ret + 1] = { item.summary, "SnacksPickerComment" }
			return ret
		end,
		matcher = {
			ignorecase = true,
		},
		confirm = function(picker, item)
			get_issue_details(item.key)
			picker:close()
		end,
	})
end

return {
	jql_query = jql_query,
	jira_action = jira_action,
	jira_display_details = jira_display_details,
}
