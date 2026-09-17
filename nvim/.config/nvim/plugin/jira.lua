local function jira_pick_issue(callback)
	local cmd = string.format("jira issue list --raw")
	local output = vim.fn.system(cmd)
	local items = vim.json.decode(output)

	vim.ui.select(items, {
		prompt = "Issue",
		format_item = function(item)
			return string.format("%s - %s", item["key"], item["fields"]["summary"])
		end,
	}, callback)
end

vim.api.nvim_create_user_command("JiraIssues", function()
	jira_pick_issue(function(issue)
		vim.cmd("tabnew")
		vim.cmd(string.format("terminal jira issue view %s", issue["key"]))
		vim.cmd("stopinsert")

		vim.keymap.set({ "n", "t" }, "q", "<cmd>bd<CR>", { buffer = 0, desc = "Quit" })
		vim.keymap.set({ "n", "t" }, "<C-c>", function()
			vim.system({ "git", "checkout", "-b", issue["key"], "--track" }):wait()
			vim.notify("Switched to branch " .. issue["key"], vim.log.levels.INFO)
		end)
	end)
end)
