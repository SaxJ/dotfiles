-- vim: ts=2 sts=2 sw=2 et
local function gh_pick_pr()
	local cmd_result = vim.system({ "gh", "pr", "list", "--json", "url,title" }, { text = true }):wait()
	local results = vim.json.decode(cmd_result.stdout)
	local display_lines = vim.tbl_map(function(item)
		return item["title"]
	end, results)

	local title_map = {}
	for _, value in ipairs(results) do
		title_map[value["title"]] = value["url"]
	end

	require("fzf-lua").fzf_exec(display_lines, {
		prompt = "PR> ",
		actions = {
			["default"] = function(selected)
				vim.system({ "gh", "pr", "checkout", title_map[selected[1]] })
				vim.notify("Changed branch to PR", vim.log.levels.WARN)
			end,
		},
	})
end

vim.keymap.set("n", "<leader>gP", gh_pick_pr)
