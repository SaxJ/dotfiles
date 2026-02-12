local tasks = {
	["Hannibal Dev"] = {
		dir = "~/Documents/hannibal/",
		cmd = "yarn dev -- -p 4000",
	},
	["Unicron Dev"] = {
		dir = "~/Documents/unicron/",
		cmd = "npm run dev -- -p 3000",
	},
}

local pickers = require("telescope.pickers")
local finders = require("telescope.finders")
local conf = require("telescope.config").values

local actions = require("telescope.actions")
local action_state = require("telescope.actions.state")

local function runner()
	local results = vim.fn.keys(tasks)

	pickers
		.new(opts, {
			prompt_title = "Background",
			finder = finders.new_table({
				results = results,
			}),
			sorter = conf.generic_sorter(),
			attach_mappings = function(prompt_buf, map)
				actions.select_default:replace(function()
					actions.close(prompt_buf)
					local selection = action_state.get_selected_entry()
					local task = tasks[selection[1]]

					local cmd = string.format([[TTerm cd %s && %s]], task["dir"], task["cmd"])
					vim.cmd(cmd)
				end)

				return true
			end,
		})
		:find()
end

vim.api.nvim_create_user_command("Background", function()
	runner()
end, { desc = "Run background task" })
-- vim: ts=2 sts=2 sw=2 et
