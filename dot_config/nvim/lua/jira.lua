local f = require("functions")

vim.g.jira_api_key = f.string.trim(vim.system({ "pass", "jira" }, { text = true }):wait().stdout)
vim.g.jira_host = "https://hejira.atlassian.net"
vim.g.jira_user = "saxon.jensen@healthengine.com.au"
-- vim.g.jira_user = f.string.trim(vim.system({ "jira", "me" }, { text = true }):wait().stdout)

local function get_issue(issue_key)
	local curl_cmd = vim.system({
		"curl",
		"-s",
		"-G",
		"--user",
		string.format("%s:%s", vim.g.jira_user, vim.g.jira_api_key),
		"-H",
		"Accept: application/json",
		string.format("%s/rest/api/2/issue/%s", vim.g.jira_host, issue_key),
	}, { text = true })

	local curl_result = curl_cmd:wait()
	if curl_result.code == 0 then
		return vim.json.decode(curl_result.stdout)
  else
    return nil
  end
end

local function download_images(issue)
  vim.fn.mkdir("~/.local/state/nvim/image_cache")

  local attachments = issue['attachment']
  for _, att in ipairs(attachments) do
    local curl_cmd = vim.system({
      "curl",
      "-s",
      "-G",
      "--user",
      string.format("%s:%s", vim.g.jira_user, vim.g.jira_api_key),
      "-H",
      "Accept: application/json",
      "-L",
      "--output",
      string.format("~/.local/state/nvim/image_cache/%s", att['filename']),
    })

    curl_cmd:wait()
  end
end

local function display_issue(issue_key)
  local issue = get_issue(issue_key)
  if issue == nil then
    return
  end

  download_images(issue)

  local buf, win = f.windows.open_split()

  -- Set issue information in jira wiki format
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, f.string.lines(issue['fields']['description']))

  -- convert to markdown
  vim.api.nvim_command("%! pandoc --from jira --to markdown")

  -- set buffer properties
  vim.api.nvim_buf_set_option_value('modifiable', false, {buf = buf})
  vim.api.nvim_buf_set_option_value('wrap', true, {buf = buf})
end

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
		"fields=summary,created,status",
		string.format("%s/rest/api/3/search/jql", vim.g.jira_host),
	}, { text = true })

	local curl_result = curl_cmd:wait()
	if curl_result.code == 0 then
		local results = vim.json.decode(curl_result.stdout)
		return f.table.map(results["issues"], function(issue)
			return {
				key = issue["key"],
				summary = issue["fields"]["summary"],
        created = issue["fields"]['created'],
        status = issue['fields']['status']['name'],
        statusCategory = issue['fields']['status']['statusCategory']['name'],
			}
		end)
	else
		return {}
	end
end

local function issues_to_tsv(issues)
  return f.table.map(issues, function (issue)
    local values = {issue['key'],issue['status'],issue['summary']}
    return table.concat(values, '||')
  end)
end

local function display_issues_table()
  local issues = jql_query("project = MKT")
  local tsv = issues_to_tsv(issues)

  local buf, win = f.windows.open_split()
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, tsv)
  vim.api.nvim_buf_set_option_value('modifiable', false, {buf = buf})

  vim.api.nvim_command("%! column --table -s '||' --table-columns 'KEY,STATUS,SUMMARY'")

  -- keymaps on this buffer
  vim.keymap.set("n", "v", function ()
    local word = vim.fn.expand("<cname>")
    vim.cmd(string.format([[echo "%s"]], word))
    -- display_issue()
  end, {buffer = buf})
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


return {
	jql_query = jql_query,
  issues_to_tsv = issues_to_tsv,
  display_issues_table = display_issues_table,
  display_issue = display_issue,
  get_issue = get_issue,
}
