local pickers = require("telescope.pickers")
local finders = require("telescope.finders")
local conf = require("telescope.config").values

local actions = require("telescope.actions")
local action_state = require("telescope.actions.state")

--- @return 'composer'|'yarn'|'npm'|nil
local function detect_project()
	if vim.fn.filereadable("composer.json") == 1 then
		return "composer"
	elseif vim.fn.filereadable("yarn.lock") == 1 then
		return "yarn"
	elseif vim.fn.filereadable("package.json") == 1 then
		return "npm"
	else
		return nil
	end
end

local function runner(opts)
	local project_type = detect_project()
	local results = {}
	local cmd_root = ""
	if project_type == "composer" then
		local project_file = table.concat(vim.fn.readfile("composer.json"), "\n")
		results = vim.fn.keys(vim.json.decode(project_file)["scripts"])
		cmd_root = "composer "
	elseif project_type == "yarn" then
		local project_file = table.concat(vim.fn.readfile("package.json"), "\n")
		results = vim.fn.keys(vim.json.decode(project_file)["scripts"])
		cmd_root = "yarn "
	elseif project_type == "npm" then
		local project_file = table.concat(vim.fn.readfile("package.json"), "\n")
		results = vim.fn.keys(vim.json.decode(project_file)["scripts"])
		cmd_root = "npm run "
	end

	opts = opts or {}
	pickers
		.new(opts, {
			prompt_title = "Runner",
			finder = finders.new_table({
				results = results,
			}),
			sorter = conf.generic_sorter(opts),
			attach_mappings = function(prompt_buf, map)
				actions.select_default:replace(function()
					actions.close(prompt_buf)
					local selection = action_state.get_selected_entry()

					vim.cmd("HTerm " .. cmd_root .. selection[1])
				end)

				return true
			end,
		})
		:find()
end

vim.api.nvim_create_user_command("Runner", function()
	runner()
end, { desc = "Run project command in new tab" })
-- vim: ts=2 sts=2 sw=2 et
