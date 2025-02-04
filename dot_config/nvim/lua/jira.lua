local fzf = require("fzf-lua")
local f = require("functions")

vim.g.jira_api_key = f.string.trim(vim.system({ "pass", "jira" }, { text = true }):wait().stdout)
vim.g.jira_host = "https://hejira.atlassian.net"
vim.g.jira_user = f.string.trim(vim.system({ "jira", "me" }, { text = true }):wait().stdout)

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
			return string.format("%s\t%s", issue["key"], issue["fields"]["summary"])
		end)
	else
		return {}
	end
end

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

local function jira_action()
	local results =
		jql_query('assignee = currentUser() AND project = MKT AND status != "✅ Done" ORDER BY created DESC')
	fzf.fzf_exec(results, {
		actions = {
			["default"] = function(selection)
				local selected = selection[1]
				local key = string.match(selected, "^(%a+-%d+)")
				local transitions = issue_transitions(key)
				local scratch = vim.api.nvim_create_buf(true, true)
				vim.api.nvim_buf_set_lines(scratch, 0, -1, false, { vim.json.encode(transitions) })
				local transition_map = {}
				for _, v in ipairs(transitions) do
					transition_map[v["name"]] = v["id"]
				end

				fzf.fzf_exec(
					f.table.map(transitions, function(t)
						return t["name"]
					end),
					{
						actions = {
							["default"] = function(selected_transition)
								local transition_id = transition_map[selected_transition[1]]
								local success = perform_transition(key, transition_id)

								local notification_title = string.format("Issue %s", key)
								if success then
									f.system.notify_send(notification_title, selected_transition[1], 1)
								else
									f.system.notify_send(notification_title, "Did not transition", 1)
								end
							end,
						},
					}
				)
			end,
		},
	})
end

return {
	jql_query = jql_query,
	jira_action = jira_action,
}
