if vim.g.background_tasks_count == nil then
	vim.g.background_tasks_count = 0
end

local function runner()
	local tasks = vim.g.background_tasks
	if tasks == nil then
		print("No background tasks set!")
	end

	local results = vim.fn.keys(tasks)
	vim.ui.select(results, {}, function(choice)
		local task = tasks[choice]

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
	runner()
end, { desc = "Run background task" })
-- vim: ts=2 sts=2 sw=2 et
