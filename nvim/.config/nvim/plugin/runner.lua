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

local function runner()
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

	vim.ui.select(results, nil, function(choice)
		local cmd = cmd_root .. choice
		Terminal.open_terminal(cmd, "horizontal")

		vim.keymap.set("n", "q", "<cmd>bd<CR>", { buffer = 0, desc = "Quit" })

		vim.keymap.set("n", "gr", string.format("<cmd>terminal %s<CR>", cmd), { buffer = 0, desc = "Recompile" })
	end)
end

vim.api.nvim_create_user_command("Runner", function()
	runner()
end, { desc = "Run project command in new tab" })
-- vim: ts=2 sts=2 sw=2 et
