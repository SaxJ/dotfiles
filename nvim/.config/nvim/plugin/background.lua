local pickers = require("telescope.pickers")
local finders = require("telescope.finders")
local conf = require("telescope.config").values

local actions = require("telescope.actions")
local action_state = require("telescope.actions.state")

if vim.g.background_tasks_count == nil then
	vim.g.background_tasks_count = 0
end

--- @param opts table
local function runner(opts)
	local tasks = vim.g.background_tasks
	if tasks == nil then
		print("No background tasks set!")
	end

	local results = vim.fn.keys(tasks)

	pickers
		.new(opts, {
			prompt_title = "Background",
			finder = finders.new_table({
				results = results,
			}),
			sorter = conf.generic_sorter(),
			attach_mappings = function(prompt_buf, _)
				actions.select_default:replace(function()
					actions.close(prompt_buf)
					local selection = action_state.get_selected_entry()
					local task = tasks[selection[1]]

					local buf = vim.api.nvim_create_buf(true, false)

					vim.api.nvim_create_autocmd({ "BufDelete" }, {
						buffer = buf,
						callback = function()
							vim.g.background_tasks_count = vim.fn.max({ 0, vim.g.background_tasks_count - 1 })
						end,
					})

					vim.api.nvim_buf_call(buf, function()
						local cmd = string.format([[term cd %s && %s]], task["dir"], task["cmd"])
						vim.cmd(cmd)
					end)

					vim.g.background_tasks_count = vim.g.background_tasks_count + 1
				end)

				return true
			end,
		})
		:find()
end

vim.api.nvim_create_autocmd("VimLeavePre", {
	callback = function()
		if vim.g.background_tasks_count > 0 then
			local choice = vim.fn.confirm("You have tasks running. Continue to exit?", "&Yes\n&No", 2)
			if choice == 1 then
				return true
			else
				return false
			end
		end
	end,
})

vim.api.nvim_create_user_command("Background", function()
	runner({})
end, { desc = "Run background task" })
-- vim: ts=2 sts=2 sw=2 et
