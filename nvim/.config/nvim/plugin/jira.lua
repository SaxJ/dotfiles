vim.g.jira_domain = ""
vim.g.jira_email = ""
vim.g.jira_token = ""

local function make_jira_auth()
	local basic = string.format("%s:%s", vim.g.jira_email, vim.g.jira_token)
	local b64 = vim.base64.encode(basic)
	return string.format("Basic %s", b64)
end

local function make_jira_url(path)
	return string.format("https://%s%s", vim.g.jira_domain, path)
end
